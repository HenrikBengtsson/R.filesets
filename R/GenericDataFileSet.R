###########################################################################/**
# @RdocClass GenericDataFileSet
#
# @title "The GenericDataFileSet class"
#
# \description{
#  @classhierarchy
#
#  A GenericDataFileSet object represents a set of @see "GenericDataFile"s.
# }
# 
# @synopsis
#
# \arguments{
#   \item{files}{A @list of @see "GenericDataFile":s.}
#   \item{tags}{A @character @vector of tags to be used for this file set.
#      The string \code{"*"} indicates that it should be replaced by the
#      tags part of the file set pathname.}
#   \item{alias}{A @character string specifying a name alias overriding the
#      name inferred from the pathname.}
#   \item{...}{Not used.}
#   \item{.onUnknownArgs}{A @character string specifying what should occur
#      if there are unknown arguments in \code{...}.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @examples "../incl/GenericDataFileSet.Rex"
# 
# @author
#*/###########################################################################
setConstructorS3("GenericDataFileSet", function(files=NULL, tags="*", alias=NULL, ..., .onUnknownArgs=c("error", "warning", "ignore")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments 'files':
  if (is.null(files)) {
  } else if (is.list(files)) {
    reqFileClass <- GenericDataFileSet$getFileClass();
    base::lapply(files, FUN=function(df) {
      Arguments$getInstanceOf(df, reqFileClass)
    });
  } else {
    throw("Argument 'files' is of unknown type: ", mode(files)[1]);
  }

  # Arguments '.onUnknownArgs':
  .onUnknownArgs <- match.arg(.onUnknownArgs);

  # Arguments '...':
  args <- list(...);
  if (length(args) > 0) {
    if (is.element(.onUnknownArgs, c("error", "warning"))) {
      argsStr <- paste(names(args), collapse=", ");
      msg <- sprintf("Unknown arguments: %s", argsStr);
      if (.onUnknownArgs == "error") {
        throw(msg);
      } else if (.onUnknownArgs == "warning") {
        warning(msg);
      }
    }
  }


  this <- extend(Object(), c("GenericDataFileSet", uses("FullNameInterface")),
    files = as.list(files),
    .alias = NULL,
    .tags = NULL
  );

  setTags(this, tags);
  setAlias(this, alias);

  this;
})


###########################################################################/**
# @RdocMethod as.character
#
# @title "Returns a short string describing the file set"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @character string.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("as.character", "GenericDataFileSet", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- sprintf("%s:", class(this)[1]);

  # Name and tags of file set
  s <- c(s, sprintf("Name: %s", getName(this)));
  tags <- getTags(this, collapse=",");
  if (!is.null(tags)) {
    s <- c(s, sprintf("Tags: %s", tags));
  }

  # Full names of file set
  s <- c(s, sprintf("Full name: %s", getFullName(this)));

  # Number of files in set
  n <- nbrOfFiles(this);
  s <- c(s, sprintf("Number of files: %d", n));

  # Names of files
  names <- getNames(this);
  if (n >= 5)
    names <- c(names[1:2], "...", names[n]);
  names <- paste(names, collapse=", ");
  s <- c(s, sprintf("Names: %s [%d]", names, n));

  # Pathname
  path <- getPath(this);
  if (!is.na(path)) {
    pathR <- getRelativePath(path);
    if (nchar(pathR) < nchar(path)) {
      path <- pathR;
    }
  }
  s <- c(s, sprintf("Path (to the first file): %s", path));

  # File size
  fileSizeB <- sprintf("%.2f MB", getFileSize(this, "numeric")/1024^2);
  s <- c(s, sprintf("Total file size: %s", fileSizeB));

  # RAM
  s <- c(s, sprintf("RAM: %.2fMB", objectSize(this)/1024^2));

  class(s) <- "GenericSummary";
  s;
}, private=TRUE)




setMethodS3("clearCache", "GenericDataFileSet", function(this, ...) {
  # Clear the cache of all files
  lapply(this, clearCache);

  # Clear cached values
  fields <- c(".fileSize");
  for (field in fields) {
    this[[field]] <- NULL;
  }

  # Then for this object
  NextMethod("clearCache", this);
})



setMethodS3("clone", "GenericDataFileSet", function(this, clear=TRUE, ...) {
  # Clone itself
  object <- NextMethod("clone", this, ...);

  # Clone each file object
  files <- as.list(object);
  for (kk in seq(along=files)) {
    files[[kk]] <- clone(files[[kk]], clear=TRUE);
  }
  object$files <- files;

  # Clear the cached fields?
  if (clear)
    clearCache(object);

  object;
}, private=TRUE)



setMethodS3("getFileClass", "GenericDataFileSet", function(static, ...) {
  # By default, infer the file class from the set class.
  name <- class(static)[1];
  name <- gsub("Set$", "", name);
  if (regexpr("File$", name) == -1) {
    name <- paste(name, "File", sep="");
  }
  name;
}, static=TRUE, protected=FALSE)



setMethodS3("validate", "GenericDataFileSet", function(this, ...) {
  invisible(TRUE);
}, protected=FALSE)





###########################################################################/**
# @RdocMethod getAlias
#
# @title "Gets the alias of the file set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character, or @NULL if no alias is set.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getAlias", "GenericDataFileSet", function(this, ...) {
  this$.alias;
})



###########################################################################/**
# @RdocMethod setAlias
#
# @title "Sets the alias of the file set"
#
# \description{
#   @get "title".
#   If specified, the alias overrides the name inferred from the pathname
#   of the file set.  This can be used in order to use another name of the
#   output data set than the input data set of many transforms and models.
# }
#
# @synopsis
#
# \arguments{
#  \item{alias}{A @character string for the new alias of the file set.
#   The alias must consists of valid filename characters, and must not
#   contain commas, which are used to separate tags.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("setAlias", "GenericDataFileSet", function(this, alias=NULL, ...) {
  # Argument 'alias':
  if (!is.null(alias)) {
    alias <- Arguments$getFilename(alias);  # Valid filename?

    # Assert that no commas are used.
    if (regexpr("[,]", alias) != -1) {
      throw("File-set aliases (names) must not contain commas: ", alias);
    }
  }

  this$.alias <- alias;
})






setMethodS3("getFileSize", "GenericDataFileSet", function(this, ..., force=FALSE) {
  fileSize <- this$.fileSize;
  if (force || is.null(fileSize)) {
    fileSize <- sum(unlist(lapply(this, FUN=getFileSize), use.names=FALSE));
    this$.fileSize <-  fileSize;
  }
  fileSize;
})



###########################################################################/**
# @RdocMethod getPath
#
# @title "Gets the path (directory) of the file set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getPath", "GenericDataFileSet", function(this, ...) {
  files <- getFiles(this);
  if (length(files) == 0) {
    naValue <- as.character(NA);
    return(naValue);
  }
  getPath(files[[1]]);
})



###########################################################################/**
# @RdocMethod length
# @aliasmethod nbrOfFiles
#
# @title "Gets the number of files in the file set"
#
# \description{
#   @get "title".
# }
#
# \usage{
#  \method{length}{GenericDataFileSet}(x)
#  \method{nbrOfFiles}{GenericDataFileSet}(this, ...)
# }
#
# \value{
#   Returns an @integer.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("length", "GenericDataFileSet", function(x) {
  # To please R CMD check
  this <- x;

  length(this$files);
}, private=TRUE, appendVarArgs=FALSE)


setMethodS3("nbrOfFiles", "GenericDataFileSet", function(this, ...) {
  length(this, ...);
})



###########################################################################/**
# @RdocMethod lapply
#
# @title "Applies a function to each of the data files"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a named @list where the names are the names of the date files
#   according to @seemethod "getNames".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("lapply", "GenericDataFileSet", function(this, ...) {
  res <- base::lapply(this$files, ...);
  names(res) <- unlist(lapply(as.list(this), FUN=getFullName));
  res;
})

setMethodS3("sapply", "GenericDataFileSet", function(this, ...) {
  res <- sapply(this$files, ...);
  names(res) <- unlist(lapply(as.list(this), FUN=getFullName));
  res;
})


setMethodS3("reorder", "GenericDataFileSet", function(x, order, ...) {
  # To please R CMD check
  this <- x;

  # Argument 'order':
  if (is.character(order)) {
    # Assume 'order' contains names
    names <- getNames(this);

    # Identify special tags and remove them
    idx <- (order == "*");
    if (sum(idx) > 1)
      throw("Argument 'order' contains more than one asterix.");
    pos <- match(order[!idx], names);
    if (any(is.na(pos))) {
      bad <- order[!idx][is.na(pos)];
      throw("Argument 'order' contains unknown sample names: ", 
                                                 paste(bad, collapse=", "));
    }
    if (sum(idx) == 0) {
      order <- pos;
    } else {
      order <- as.list(order);
      order[!idx] <- names[pos];
      order[[whichVector(idx)]] <- setdiff(names, names[pos]);
      order <- unlist(order, use.names=FALSE);
      order <- match(order, names);
    }
  }

  order <- Arguments$getIndices(order, max=nbrOfFiles(this));
  if (any(duplicated(order))) {
    bad <- order[duplicated(order)];
    throw("Argument 'order' contains duplicates: ", 
                                                 paste(bad, collapse=", "));
  }

  this$files <- this$files[order];
  invisible(this);
}, private=TRUE)



###########################################################################/**
# @RdocMethod sortBy
#
# @title "Sorts the set"
#
# \description{
#   @get "title" by one of several ordering schema.
# }
#
# @synopsis
#
# \arguments{
#  \item{by}{A @character string specifying the ordering scheme.}
#  \item{caseSensitive}{If @TRUE, the ordering is case sensitive, 
#        otherwise not.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns returns itself (invisibly) with the set ordered accordingly.
# }
#
# \details{
#   The set is ordering by the fullnames.
#   If \code{by="lexicographic"}, lexicographic ordering is used, 
#   sometimes also referred to as alphabetic ordering.
#   If \code{by="mixedsort"}, mixedsort order is used, 
#   cf. @see "gtools::mixedsort".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("sortBy", "GenericDataFileSet", function(this, by=c("lexicographic", "mixedsort"), caseSensitive=FALSE, ...) {
  # Argument 'by':
  by <- match.arg(by);

  # Argument 'caseSensitive':
  caseSensitive <- Arguments$getLogical(caseSensitive);

  # Get the fullnames
  fullnames <- getFullNames(this);
  if (!caseSensitive) {
    fullnames <- tolower(fullnames);
  }

  if (by == "lexicographic") {
    order <- order(fullnames);
  } else if (by == "mixedsort") {
    order <- gtools::mixedorder(fullnames);
  }

  # Sanity check
  stopifnot(!any(is.na(order)));
  stopifnot(length(unique(order)) == length(order));

  this$files <- this$files[order];
  invisible(this);
})


###########################################################################/**
# @RdocMethod getNames
# @aliasmethod getFullNames
#
# @title "Gets the names (or fullnames) of the files in the file set"
#
# \description{
#   @get "title".
# }
#
# \usage{
#  \method{getNames}{GenericDataFileSet}(this, ...)
#  \method{getFullNames}{GenericDataFileSet}(this, ...)
# }
#
# \arguments{
#  \item{...}{Arguments passed to \code{getName()} (\code{getFullName()})
#    of each file.}
# }
#
# \value{
#   Returns a @character @vector of length equal to the number of files
#   in the set.
# }
#
# @author
#
# \seealso{
#   @seemethod "indexOf"
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getNames", "GenericDataFileSet", function(this, ...) {
  res <- unlist(lapply(this, FUN=getName, ...));
  unname(res);
})

setMethodS3("getFullNames", "GenericDataFileSet", function(this, ...) {
  res <- unlist(lapply(this, FUN=getFullName, ...));
  unname(res);
})



###########################################################################/**
# @RdocMethod indexOf
#
# @title "Finds indices of a subset of files"
#
# \description{
#   @get "title" whose names match a given set of names or name patterns.
# }
#
# @synopsis
#
# \arguments{
#  \item{patterns}{A @character @vector of length K of names and/or 
#   regular expressions to be matched.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns an @integer @vector of length K with elements in
#   [1,@seemethod "nbrOfFiles"] or (integer) @NA (for non-matched names).
# }
#
# \details{
#   If an element of \code{patterns} contains a comma, then that element
#   is matched against the @seemethod "getFullNames", otherwise it is
#   matched against @seemethod "getNames".
#   First each pattern is matched by regular expression rules, and if
#   there is not match, then by fixed strings.
# }
#
# @author
#
# \seealso{
#   @seemethod "getNames"
#   @seeclass
# }
#*/###########################################################################
setMethodS3("indexOf", "GenericDataFileSet", function(this, patterns=NULL, ..., onMissing=c("NA", "error")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'onMissing':
  onMissing <- match.arg(onMissing);

  names <- getNames(this);

  # Return all indices
  if (is.null(patterns)) {
    res <- seq(along=names);
    names(res) <- names;
    return(res);
  }

  fullnames <- getFullNames(this);

  naValue <- as.integer(NA);

  patterns0 <- patterns;
  res <- lapply(patterns, FUN=function(pattern) {
    # First try matching a regular expression, then a fixed string.
    pattern0 <- pattern;
    hasTags <- (regexpr(",", pattern, fixed=TRUE) != -1);
    if (hasTags) {
      searchStrings <- fullnames;
    } else {
      searchStrings <- names;
    }

    # - - - - - - - - - - - -
    # 1. Regular expression
    # - - - - - - - - - - - -
    # Assert that the regular expression has a "head" and a "tail".
    pattern <- sprintf("^%s$", pattern);
    pattern <- gsub("\\^\\^", "^", pattern);
    pattern <- gsub("\\$\\$", "$", pattern);

    # Escape '+', and '*', if needed
    lastPattern <- "";
    while (pattern != lastPattern) {
      lastPattern <- pattern;
      pattern <- gsub("(^|[^\\]{1})([+*])", "\\1\\\\\\2", pattern);
    }

    # Match
    idxs <- grep(pattern, searchStrings, fixed=FALSE);

    # - - - - - - - - - - - -
    # 2. Fixed string?
    # - - - - - - - - - - - -
    if (length(idxs) == 0) {
      pattern <- pattern0;
      idxs <- grep(pattern, searchStrings, fixed=TRUE);
    }

    # Nothing matched?
    if (length(idxs) == 0) {
      idxs <- naValue;
    }

    # Note that 'idxs' may return more than one match
    idxs;
  });

  ns <- sapply(res, FUN=length);
  names <- NULL;
  for (kk in seq(along=ns)) {
    names <- c(names, rep(patterns0[kk], times=ns[kk]));
  }
  res <- unlist(res, use.names=FALSE);
  names(res) <- names;

  # Not allowing missing values?
  if (onMissing == "error" && any(is.na(res))) {
    names <- names(res)[is.na(res)];
    throw("Some names where not match: ", paste(names, collapse=", "));
  }

  res;
})


###########################################################################/**
# @RdocMethod getPathnames
#
# @title "Gets the pathnames of the files in the file set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character @vector.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getPathnames", "GenericDataFileSet", function(this, ...) {
  unlist(lapply(this, FUN=getPathname))
})



###########################################################################/**
# @RdocMethod seq
#
# @title "Gets an vector of file indices"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns an @integer @vector in [1,N] where N is the number of files,
#   or an empty vector if the file set is empty.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("seq", "GenericDataFileSet", function(this, ...) {
  seq(length=nbrOfFiles(this));
})


###########################################################################/**
# @RdocMethod as.list
#
# @title "Returns the files of the file set"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#  Returns a @list of files, each of class @see "getFileClass".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("as.list", "GenericDataFileSet", function(x, ...) {
  # To please R CMD check.
  this <- x;

  this$files;
})


###########################################################################/**
# @RdocMethod getFile
#
# @title "Get a particular file of the file set"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{idx}{An @integer index specifying the file to be returned.}
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns a @GenericDataFile.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("getFile", "GenericDataFileSet", function(this, idx, ...) {
  if (length(idx) != 1)
    throw("Argument 'idx' must be a single index.");
  res <- this$files;
  n <- length(res);
  idx <- Arguments$getIndex(idx, max=n);
  res[[idx]];
})


setMethodS3("getFiles", "GenericDataFileSet", function(this, idxs=NULL, ...) {
  res <- this$files;
  if (is.null(idxs)) {
  } else {
    n <- length(res);
    idxs <- Arguments$getIndices(idxs, max=n);
    res <- res[idxs];
  }
  res;
}, private=TRUE)



###########################################################################/**
# @RdocMethod appendFiles
#
# @title "Appends a list of files to a data set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{files}{A single @see "GenericDataFile" or a @list of such to
#    be appended.}
#  \item{clone}{If @TRUE, each file is cloned before being appened.}
#  \item{...}{Additional arguments passed to @see "base::append".}
#  \item{verbose}{...}
# }
#
# \value{
#   Returns (invisible) the appended data set (itself).
# }
#
# \details{
#   The files appended must inherit the same class as the first file
#   of the data set, otherwise an exception is thrown.
# }
#
# @author
#
# \seealso{
#   To append a data set, see @see "append".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("appendFiles", "GenericDataFileSet", function(this, files, clone=TRUE, ..., verbose=FALSE) {
  # Argument 'files':
  if (!is.list(files)) {
    files <- list(files);
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "Appending ", length(files), " files");
  if (length(files) == 0) {
    verbose && cat(verbose, "No files to append. Skipping.");
  } else {
    # Validate classes?
    if (nbrOfFiles(this) > 0) {
      verbose && enter(verbose, "Validating file classes");
      aFile <- this$files[[1]];
      className <- class(aFile)[1];
      isValid <- unlist(lapply(files, FUN=inherits, className));
      if (!all(isValid)) {
        throw("Some of the elements in argument 'files' are not '", 
          className, "'");
      }
      verbose && exit(verbose);
    }

    # Clone file objects?
    if (clone) {
      verbose && enter(verbose, "Cloning files");
      files <- base::lapply(files, FUN=function(file) clone(file));    
      verbose && exit(verbose);
    }

    # Append
    this$files <- base::append(this$files, files, ...);

    # Some cached values are incorrect now.
    clearCache(this);
  }

  verbose && exit(verbose);

  invisible(this);
})


###########################################################################/**
# @RdocMethod append
#
# @title "Appends one data set to an existing one"
#
# \description{
#   @get "title".
#   The fullname of the merged data set is that of the first data set.
# }
#
# @synopsis
#
# \arguments{
#  \item{values}{The data set to be appended to this data set.}
#  \item{...}{Additional arguments passed to @seemethod "appendFiles".}
# }
#
# \value{
#   Returns a merged @see "GenericDataFileSet" of the same class as the
#   first data set.
# }
#
# \details{
#   By default, in order to avoid downstream side effects on the data set
#   being appended, each of its file is cloned before being appended.
# }
#
# @author
#
# \seealso{
#   To append one or more files, see @see "appendFiles".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("append", "GenericDataFileSet", function(x, values, ...) {
  # To please R CMD check
  this <- x;
  other <- values;

  # Argument 'other':
  if (inherits(other, "GenericDataFileSet")) {
    other <- Arguments$getInstanceOf(other, class(this)[1]);
    files <- getFiles(other);
  } else {
    files <- other;
  }

  appendFiles(this, files, ...);
})



###########################################################################/**
# @RdocMethod extract
#
# @title "Extract a subset of the file set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{files}{An @integer or a @logical @vector indicating which data files
#    to be extracted.  Negative indices are excluded.}
#  \item{...}{Not used.}
#  \item{onMissing}{A @character specifying the action if a requested file 
#    does not exist.  If \code{"error"}, an error is thrown.  If \code{"NA"},
#    a @see "GenericDataFile" refering to an @NA pathname is used in place.
#    If \code{"drop"}, the missing file is dropped.}
# }
#
# \value{
#   Returns an @see "GenericDataFileSet" (or a subclass) object.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("extract", "GenericDataFileSet", function(this, files, ..., onMissing=c("NA", "error", "drop"), onDuplicates=c("ignore", "drop", "error")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'files':
  nbrOfFiles <- length(this);
  if (is.logical(files)) {
    files <- Arguments$getVector(files, length=rep(nbrOfFiles, 2));
    files <- whichVector(files);
  } else if (is.character(files)) {
    files <- indexOf(this, files, ...);
  } else if (is.numeric(files)) {
    files <- Arguments$getIntegers(files, disallow="NaN");

    # Exclude indices?
    if (any(files < 0, na.rm=TRUE)) {
      incl <- files[files > 0];
      if (length(incl) == 0) {
        incl <- seq(this);
      }
      excl <- na.omit(files[files < 0]);
      files <- setdiff(incl, -excl);
      rm(incl, excl);
    }
  }

  # Argument 'onMissing':
  onMissing <- match.arg(onMissing);

  # Argument 'onDuplicates':
  onDuplicates <- match.arg(onDuplicates);


  if (onMissing == "error") {
    disallow <- c("NA", "NaN");
  } else if (is.element(onMissing, c("NA", "drop"))) {
    disallow <- c("NaN");
  }
  files <- Arguments$getIndices(files, max=nbrOfFiles, disallow=disallow);
  missing <- which(is.na(files));

  # Drop non-existing files?
  if (length(missing) > 0 && onMissing == "drop") {
    files <- files[is.finite(files)];
    missing <- integer(0);
  }

  # Check for duplicates?
  if (onDuplicates != "ignore") {
    dups <- which(is.finite(files) & duplicated(names(files)));
    if (length(dups) > 0) {
      dupNames <- names(files)[head(dups)];
      dupNames <- paste(dupNames, collapse=", ");
      if (onDuplicates == "error") {
        throw("Cannot extract file subset. Files with identical names detected: ", dupNames);
      } else if (onDuplicates == "drop") {
        warning("Dropping files with duplicated names: ", dupNames);
        files <- files[-dups];
        missing <- which(is.na(files));
      }
    }
  }

  res <- clone(this);
  files <- this$files[files];

  # Any missing files?
  if (length(missing) > 0) {
    className <- class(this$files[[1]])[1];
    if (is.null(className)) {
      className <- getFileClass(this);
    }
    clazz <- Class$forName(className);
    naValue <- newInstance(clazz, NA, mustExist=FALSE);
    for (idx in missing) {
      files[[idx]] <- naValue;
    }
  }
  res$files <- files;
  rm(files);

  # Some cached values are incorrect now.
  clearCache(res);

  res;
})




###########################################################################/**
# @RdocMethod byPath
# @aliasmethod fromFiles
#
# @title "Defines a GenericDataFileSet by searching for files in a directory"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{path}{The directory where to search for files.}
#  \item{pattern}{The filename pattern for match files. 
#     If @NULL, filename extensions corresponding to known subclasses
#     of the abstract @see "GenericDataFile" class are search for.}
#  \item{recursive}{If @TRUE, subdirectories are search recursively,
#     otherwise not.}
#  \item{...}{Optional arguments passed to the constructor of the
#     static (calling) class.}
# }
#
# \value{
#   Returns an @see "GenericDataFileSet" object.
# }
#
# \section{Reserved filenames}{
#   Note that files with names starting with a period \code{.} are not 
#   searched for.  Such files are considered "private" and have to be
#   included explicitly, if wanted.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("byPath", "GenericDataFileSet", function(static, path=NULL, pattern=NULL, recursive=FALSE, fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'path':
  path <- Arguments$getReadablePath(path, mustExist=TRUE);

  # Argument 'pattern':
  if (!is.null(pattern)) {
    pattern <- Arguments$getRegularExpression(pattern);
  }

  # Argument 'fileClass':
  clazz <- Class$forName(fileClass);
  dfStatic <- getStaticInstance(clazz);
  dfStatic <- Arguments$getInstanceOf(dfStatic, getFileClass(static));

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "Defining an ", class(static)[1], " object from files");
  verbose && cat(verbose, "Path: ", path);
  verbose && cat(verbose, "Pattern: ", pattern);
  verbose && cat(verbose, "File class: ", class(dfStatic)[1]);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Create set of GenericDataFile objects from matching files
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Scan for files
  verbose && enter(verbose, "Scanning directory for files");
  pathnames <- list.files(path=path, pattern=pattern, full.names=TRUE, 
                                   all.files=FALSE, recursive=recursive);
  verbose && printf(verbose, "Found %d files/directories.\n", length(pathnames));
  if (length(pathnames) > 0) {
    # Keep only files
    keep <- sapply(pathnames, FUN=isFile);
    pathnames <- pathnames[keep];
  }
  verbose && printf(verbose, "Found %d files.\n", length(pathnames));
  verbose && exit(verbose);

  if (length(pathnames) > 0) {
    # Sort files in lexicographic order
    pathnames <- sort(pathnames);

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Build list of GenericDataFile objects
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    verbose && enter(verbose, "Defining ", length(pathnames), " files");
    files <- list();
    for (kk in seq(along=pathnames)) {
      if (as.logical(verbose)) cat(kk, ", ", sep="");
      df <- fromFile(dfStatic, pathnames[kk], recursive=recursive, .checkArgs=FALSE, verbose=less(verbose));
      files[[kk]] <- df;
      if (kk == 1) {
        # Update the static class instance.  The reason for this is
        # that if the second file cannot be instanciated with the same
        # class as the first one, then the files are incompatible.
        # Note that 'df' might be of a subclass of 'dfStatic'.
        clazz <- Class$forName(class(df)[1]);
        dfStatic <- getStaticInstance(clazz);
      }
    }
    if (as.logical(verbose)) cat("\n");
    verbose && exit(verbose);
  } else {
    files <- list();
  }

  # Create the file set object
  if (inherits(static, "Class")) {
    className <- getName(static);
  } else {
    className <- class(static)[1];
  }
  verbose && enter(verbose, "Allocating a new ", className, " instance");
  verbose && cat(verbose, "Arguments:");
  verbose && cat(verbose, "Number of files: ", length(files));
  verbose && str(verbose, list(...));
  set <- newInstance(static, files, ...);
  verbose && exit(verbose);

  # Allow the file set to update itself according to these new rules.
  verbose && enter(verbose, "Updating newly allocated ", className);
  update2(set, ..., verbose=less(verbose, 5));
  verbose && exit(verbose);

  # Validate?
  if (.validate) {
    validate(set, verbose=less(verbose, 5));
  }

  verbose && exit(verbose);

  set;
}, static=TRUE)



###########################################################################/**
# @RdocMethod copyTo
#
# @title "Copies a data set to another directory"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{path}{The destination directory.  If missing, it is created.}
#  \item{...}{Not used.}
#  \item{verbose}{...}
# }
#
# \value{
#   Returns a @see "GenericDataFileSet" consisting the new file copies.
# }
# 
# \details{
#   Each file is copied safely, but if this method is interrupted, it
#   may results in a data set consisting of fewer than the original
#   data set.
#   FUTURE: In order to minimize the risk for this, we may consider to
#   first copy to a temporary directory, which is then renamed, cf. how
#   individual files are safely copied.
# }
# 
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("copyTo", "GenericDataFileSet", function(this, path=NULL, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'path':
  path <- Arguments$getWritablePath(path);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  nbrOfFiles <- nbrOfFiles(this);
  verbose && enter(verbose, sprintf("Copying %d files", nbrOfFiles));
  verbose && cat(verbose, "Output path for files: ", path);

  for (kk in seq_len(nbrOfFiles)) {
    verbose && enter(verbose, sprintf("File %d of %d", kk, nbrOfFiles));
    cf <- getFile(this, kk);
    if (isFile(cf)) {
      cfCopy <- copyTo(cf, path=path, ..., verbose=less(verbose));
    }
    verbose && exit(verbose);
  }

  # Return new instance
  res <- byPath(this, path=path, ...);

  verbose && exit(verbose);

  res;
}, protected=TRUE)



###########################################################################/**
# @RdocMethod findByName
#
# @title "Locates all file sets that match the requested name"
#
# \description{
#   @get "title", tags, and sub directories, in any of the root paths.
# }
#
# @synopsis
#
# \arguments{
#  \item{name, tags}{The name and the tags of the file set to be located.}
#  \item{subdirs}{A @character @vector of the subpath where the file
#     set is located.}
#  \item{paths}{A @character @vector of root paths where to look for 
#     the file set.}
#  \item{mustExist}{If @TRUE, an exception is thrown if the file set was
#     not found, otherwise not.}
#  \item{...}{Not used.}
#  \item{verbose}{...}
# }
#
# \value{
#   Returns a @character @vector of paths.
#   If no file sets where found, @NULL is returned.
# }
# 
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("findByName", "GenericDataFileSet", function(static, name, tags=NULL, subdirs=NULL, paths=NULL, firstOnly=TRUE, mustExist=FALSE, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments 'name':
  name <- Arguments$getCharacter(name);
  if (nchar(name) == 0) {
    throw("A ", class(static)[1], " must have a non-empty name: ''");
  }
  name <- Arguments$getFilename(name, .type="name");

  # Arguments 'tags':
  if (!is.null(tags)) {
    tags <- sapply(tags, Arguments$getFilename, .type="name", .name="tags");
  }

  # Arguments 'paths':
  if (is.null(paths)) {
    paths <- ".";
  }

  # Argument 'firstOnly':
  firstOnly <- Arguments$getLogical(firstOnly);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }



  verbose && enter(verbose, "Locating data sets");

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify existing root directories
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  rootPaths <- sapply(paths, FUN=filePath, expandLinks="any");
  rootPaths <- paths[sapply(rootPaths, FUN=isDirectory)];
  if (length(rootPaths) == 0) {
    if (mustExist) {
      throw("None of the data directories exist: ", 
                                           paste(paths, collapse=", "));
    }
    return(NULL);
  }

  verbose && cat(verbose, "Search root path:");
  verbose && print(verbose, rootPaths);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify existing data set directories
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # The full name of the data set
  fullname <- paste(c(name, tags), collapse=",");
  verbose && cat(verbose, "Fullname: ", fullname);

  # Look for matching data sets
  dataSetPaths <- file.path(rootPaths, fullname);

  # Look for existing directories
  dataSetPaths <- sapply(dataSetPaths, FUN=filePath, expandLinks="any");
  dataSetPaths <- dataSetPaths[sapply(dataSetPaths, FUN=isDirectory)];
  dataSetPaths <- unname(dataSetPaths);

  verbose && cat(verbose, "Search dataset paths:");
  verbose && print(verbose, dataSetPaths);


  paths <- NULL;
  if (length(dataSetPaths) > 0) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Identify existing subdirectories
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(subdirs) >= 1) {
      verbose && enter(verbose, "Search subdirectories");
      verbose && print(verbose, subdirs);

      for (kk in seq(along=subdirs)) {
        dir <- subdirs[kk];
        verbose && enter(verbose, sprintf("Subdirectory #%d ('%s') of %d", kk, dir, length(subdirs)));

        # Smart directory?
        if (identical(dir, "*"))
          dir <- ":.*:";
        pattern <- "^:([^:]*):$";
        isSmart <- (regexpr(pattern, dir) != -1);
        if (isSmart) {
          verbose && enter(verbose, "Processing \"smart\" path");

          # Regular expression pattern for subsetting directories
          pattern <- gsub(pattern, "\\1", dir);
          pattern <- Arguments$getRegularExpression(pattern);

          pathsKK <- sapply(dataSetPaths, FUN=function(path) {
            # List all directories and files
            dirsT <- list.files(path=path, pattern=pattern, full.names=TRUE);
            if (length(dirsT) == 0)
              return(NULL);
            # Keep only directories
            dirsT <- sapply(dirsT, FUN=filePath, expandLinks="any");
            dirsT <- dirsT[sapply(dirsT, FUN=isDirectory)];
            if (length(dirsT) == 0)
              return(NULL);
            # Work only with the directory names
            dirsT <- basename(dirsT);

            # Keep only the first match
            # TO DO: Find a more powerful set of selecting directories
            # /HB 2009-02-11
            dir <- dirsT[1];

            file.path(path, dir);
          });

          verbose && exit(verbose);
        } else {
          pathsKK <- file.path(dataSetPaths, dir);
        } # if (isSmart)
 
        # In case there are NULLs
        pathsKK <- unlist(pathsKK, use.names=FALSE);
        # Keep only directories
        pathsKK <- sapply(pathsKK, FUN=filePath, expandLinks="any");
        pathsKK <- pathsKK[sapply(pathsKK, FUN=isDirectory)];
        pathsKK <- unname(pathsKK);

        verbose && cat(verbose, "Existing paths:");
        verbose && print(verbose, pathsKK);

        paths <- c(paths, pathsKK);

        verbose && exit(verbose);
      } # for (kk ...)

      verbose && cat(verbose, "All existing paths:");
      verbose && print(verbose, paths);

      verbose && exit(verbose);
    } else {
      paths <- dataSetPaths;
    }# if (length(subdirs) >= 1)
  
    if (length(paths) > 1) {
      if (firstOnly) {
        warning("Found duplicated data set: ", paste(paths, collapse=", "));
        paths <- paths[1];
        verbose && cat(verbose, "Dropped all but the first path.");
      }
    }
  } # if (length(dataSetPaths) > 0)
  
  if (length(paths) == 0) {
    paths <- NULL;

    if (mustExist) {
      msg <- sprintf("Failed to locate data set '%s'", fullname);
      if (!is.null(subdirs)) {
        subdirsStr <- paste(subdirs, collapse=", ");
        msg <- sprintf("%s (with subdirectory '%s')", msg, subdirsStr);
      }
        msg <- sprintf("%s in search path (%s)", 
                                msg, paste(rootPaths, collapse=", "));

      throw(msg);
    }
  }

  verbose && exit(verbose);

  paths;
}, protected=TRUE, static=TRUE)




###########################################################################/**
# @RdocMethod byName
#
# @title "Locates and sets up a file set by its name"
#
# \description{
#   @get "title", tags, root and sub directories.
# }
#
# @synopsis
#
# \arguments{
#  \item{name, tags}{The name and the tags of the file set to be located.}
#  \item{subdirs}{A @character @vector of the subpath where the file
#     set is located.}
#  \item{paths}{A @character @vector of root paths where to look for 
#     the file set.}
#  \item{...}{Not used.}
#  \item{verbose}{...}
# }
#
# \value{
#   Returns a @see "GenericDataFileSet".
#   If not found, an exception is thrown.
# }
# 
# @author
#
# \seealso{
#   Internally, @seemethod "findByName" is used to locate the data set,
#   and @seemethod "byPath" to then set it up.
#   @seeclass
# }
#*/###########################################################################
setMethodS3("byName", "GenericDataFileSet", function(static, name, tags=NULL, subdirs=NULL, paths=NULL, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, sprintf("Setting up %s by its name", class(static)[1]));
  verbose && cat(verbose, "Name: ", name);
  verbose && cat(verbose, "Tags: ", paste(tags, collapse=","));

  suppressWarnings({
    path <- findByName(static, name=name, tags=tags, subdirs=subdirs, 
            paths=paths, firstOnly=TRUE, mustExist=TRUE, verbose=verbose);
  })

  verbose && cat(verbose, "Path to data set:");
  verbose && print(verbose, path);

  suppressWarnings({
    res <- byPath(static, path=path, ..., verbose=verbose);
  })

  verbose && exit(verbose);

  res;
}, static=TRUE) 



setMethodS3("hasFile", "GenericDataFileSet", function(this, file, ...) {
  # Argument 'file':
  file <- Arguments$getInstanceOf(file, "GenericDataFile");

  files <- getFiles(this);
  for (kk in seq(along=files)) {
    if (identical(file, files[[kk]]))
      return(TRUE);
  }

  return(FALSE);
})


setMethodS3("equals", "GenericDataFileSet", function(this, other, ..., verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }
 

  # Default values
  notEqual <- FALSE;
  msg <- NULL;
  attr(notEqual, "thisSet") <- getPath(this);
  attr(notEqual, "otherSet") <- getPath(other);

  if (!inherits(other, "GenericDataFileSet")) {
    msg <- sprintf("The 'other' is not a GenericDataFileSet: %s",
                                                 class(other)[1]);
    attr(notEqual, "reason") <- msg;
    return(notEqual);
  }

  nbrOfFiles <- nbrOfFiles(this);

  value <- nbrOfFiles;
  valueOther <- nbrOfFiles(other);
  if (value != valueOther) {
    msg <- sprintf("The number of files differ: %d != %d",
                                              value, valueOther);
    attr(notEqual, "reason") <- msg;
    return(notEqual);
  }

  if (identical(getPathnames(this), getPathnames(other)))
    return(TRUE);

  for (kk in seq(this)) {
    verbose && enter(verbose, sprintf("File #%d of %d", kk, nbrOfFiles));

    df1 <- getFile(this, 1);
    df2 <- getFile(other, 1);
    eqls <- equals(df1, df2, ...);
    if (!eqls) {
      verbose && cat(verbose, "Not equal");
      return(eqls);
    }

    verbose && exit(verbose);
  }

  TRUE;
})



setMethodS3("update2", "GenericDataFileSet", function(this, ...) {
}, protected=TRUE)





# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FULLNAME TRANSLATORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

###########################################################################/**
# @RdocMethod getDefaultFullName
#
# @title "Gets the default full name of the file set"
#
# \description{
#   @get "title", that is the name of the directory without parent directories.
# }
#
# @synopsis
#
# \arguments{
#  \item{parent}{The number of generations up in the directory tree the
#    directory name should be retrieved.  By default the current directory
#    is used.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# \details{
#  By default, the full name of a file set is the name of the directory 
#  containing all the files, e.g. the name of file set 
#  \code{path/foo,c/to,a,b/*} is \code{to,a,b}.
#  Argument \code{parent=1} specifies that the parent directory should be
#  used, e.g. \code{foo,c}.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getDefaultFullName", "GenericDataFileSet", function(this, parent=0, ...) {
  # Argument 'parent':
  parent <- Arguments$getInteger(parent, range=c(0,32));

  # The name of a file set is inferred from the pathname of the directory
  # of the set assuming path/to/<fullname>/<something>/<subdir>/

  # Get the path of this file set
  path <- getPath(this);
  if (is.na(path)) {
    naValue <- as.character(NA);
    return(naValue);
  }

  while (parent > 0) {
    # path/to/<fullname>/<something>
    path <- dirname(path);
    parent <- parent - 1;
  }

  # <fullname>
  fullname <- basename(path);

  fullname;
})


setMethodS3("updateFullName", "GenericDataFileSet", function(this, ...) {
  update2(this, ...);
}, protected=TRUE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FULLNAME*S* TRANSLATORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("updateFullNames", "GenericDataFileSet", function(this, ...) {
  updateFullName(this, ...);
}, protected=TRUE)


setMethodS3("clearFullNamesTranslator", "GenericDataFileSet", function(this, ...) {
  sapply(this, clearFullNameTranslator, ...);
  invisible(this);
}, protected=TRUE)

setMethodS3("appendFullNamesTranslatorByNULL", "GenericDataFileSet", function(this, ...) {
  sapply(this, appendFullNameTranslatorByNULL, NULL, ...);
  invisible(this);
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByfunction", "GenericDataFileSet", function(this, fcn, ...) {
  sapply(this, appendFullNameTranslatorByfunction, fcn, ...);
  invisible(this);
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorBydata.frame", "GenericDataFileSet", function(this, fcn, ...) {
  sapply(this, appendFullNameTranslatorBydata.frame, fcn, ...);
  invisible(this);
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByTabularTextFile", "GenericDataFileSet", function(this, fcn, ...) {
  sapply(this, appendFullNameTranslatorByTabularTextFile, fcn, ...);
  invisible(this);
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByTabularTextFileSet", "GenericDataFileSet", function(this, fcn, ...) {
  sapply(this, appendFullNameTranslatorByTabularTextFileSet, fcn, ...);
  invisible(this); 
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorBylist", "GenericDataFileSet", function(this, fcn, ...) {
  sapply(this, appendFullNameTranslatorBylist, fcn, ...);
  invisible(this);
}, protected=TRUE)


setMethodS3("appendFullNamesTranslator", "GenericDataFileSet", function(this, by, ...) {
  # Arguments 'by':
  classNames <- class(by);
  methodNames <- sprintf("appendFullNamesTranslatorBy%s", classNames);

  # Dispatch on the 'by' argument...
  keep <- sapply(methodNames, FUN=exists, mode="function");
  methodNames <- methodNames[keep];

  if (length(methodNames) > 0) {
    methodName <- methodNames[1];
    fcn <- get(methodName, mode="function");
    res <- fcn(this, by, ...);
  } else {
    # ...otherwise, apply the fullname translator to each file
    res <- sapply(this, appendFullNameTranslator, by, ...);
  }

  # Allow the object to update itself according to these new rules.
  updateFullNames(this);

  invisible(res);
}, protected=TRUE)


setMethodS3("setFullNamesTranslator", "GenericDataFileSet", function(this, ...) {
  clearFullNamesTranslator(this);
  appendFullNamesTranslator(this, ...);
}, protected=TRUE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Deprecated
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
setMethodS3("fromFiles", "GenericDataFileSet", function(static, ...) {
  byPath(static, ...);
}, static=TRUE, deprecated=TRUE, protected=TRUE)




############################################################################
# HISTORY:
# 2011-02-13
# o GENERALIZATION: Now append() for GenericDataFileSet tries to also
#   append non-GenericDataFileSet object by passing them down to
#   appendFiles() assuming they are GenericDataFile:s.
# o GENERALIZATION: Now appendFiles() for GenericDataFileSet also accepts
#   a single item.  Thus, there is no longer a need to wrap up single
#   items in a list.
# 2010-11-19
# o ROBUSTNESS: Now GenericDataFileSet$byName() asserts that arguments
#   'name' and 'tags' contain only valid characters.  This will for
#   instance prevent passing paths or pathnames by mistake.
# 2010-08-03
# o Added sortBy() to GenericDataFileSet, which can sort files either
#   by lexicographic or mixedsort ordering.
# 2010-07-06
# o BUG FIX: indexOf() for GenericDataFileSet/List would return NA if the
#   search pattern/string contained parentheses.  The reason is that
#   such have a special meaning in regular expression.  Now indexOf()
#   first search by regular expression patterns, then by fixed strings.
# 2010-05-26
# o Now GenericDataFileSet$findByName(..., mustExist=FALSE) do no longer
#   throw an exception even if there is no existing root path.
# o Added argument 'firstOnly=TRUE' to findByName() for GenericDataFileSet.
# o Added appendFullNameTranslatorBy...() method for TabularTextFileSet:s.
# 2010-05-25
# o Added appendFullNameTranslatorBy...() method for data frames and
#   TabularTextFile:s. 
# 2010-02-13
# o Added argument '.onUnknownArgs' to GenericDataFileSet().
# 2010-02-07
# o BUG FIX: indexOf() of GenericDataFileSet did not handle names with
#   regular expression symbols '+' and '*'.
# 2010-01-31
# o DOCUMENTATION: Added Rd help for more methods.
# o Deprecated static fromFiles() of GenericDataFileSet.  Use byPath() 
#   instead.
# 2010-01-24
# o ROBUSTNESS: If argument 'files' is logical, then extract() of
#   GenericDataFileSet now asserts that the length of 'files' matches
#   the number of available files.
# 2009-12-31
# o BUG FIX: byPath() of GenericDataFileSet would give "Error in
#   pathnames[keep] : invalid subscript type 'list'" if there was no files.
# 2009-12-30
# o BUG FIX: Changed the default to 'parent=0' for getDefaultFullName() of
#   GenericDataFileSet to be consistent with the documentation.
# o BUG FIX: Now byPath() of GenericDataFileSet returns only files. Before
#   it would also return directories.
# o Now extract(ds, c(1,2,NA,4), onMissing="NA") returns a valid
#   GenericDataFileSet where missing files are returned as missing
#   GenericDataFile:s.
# o copyTo() of GenericDataFileSet quietly ignores missing files.
# o Now a GenericDataFileSet may contain GenericDataFile:s refering to 
#   missing files.
# o BUG FIX: getPath() and getDefaultFullName() of GenericDataFileSet would
#   return a *logical* instead of *character* value.
# o BUG FIX: indexOf(ds, names) of GenericDataFileSet would return a
#   *logical* instead of an *integer* vector of NA:s if none of the names 
#   existed.
# 2009-12-25
# o Added Rd help for indexOf() of GenericDataFileSet.
# 2009-10-30
# o BUG FIX: Appending empty data sets using append() of GenericDataFileSet
#   would give error Error in this$files[[1]] : subscript out of bounds.
# o Now append() clears the cache.
# o Now clearCache() of GenericDataFileSet clears the total file size.
# o Added argument 'force=FALSE' to getFileSize() of GenericDataFileSet.
# 2009-10-23
# o Added hasExtension() and getDefaultFullNameAndExtension().
# 2009-10-22
# o Rename previous setFullNamesNnn() to appendFullNamesNnn().
# 2009-10-02
# o CLEAN UP: Renamed fromFiles() to byPath().  For backward compatibility
#   the former calls the latter.
# o Now setFullNamesTranslator() for GenericDataFileSet dispatches on the
#   'by' argument.  If that is not possible, it call setFullNameTranslator()
#   for each file in the set (as before).
# o CLEAN UP: Removed setFullName() for GenericDataFileSet, because there
#   is not a "default" on.
# 2009-08-12
# o Now findByName() of GenericDataFileSet follows Windows Shortcut links
#   also for subdirectories.
# 2009-05-19
# o Now setFullNameTranslator() for GenericDataFileSet asserts that the 
#   fullname translator function accepts also argument 'set'.
# 2009-05-04
# o Now static fromFiles() of GenericDataFileSet supports empty data sets.
# o BUG FIX: as.character() of GenericDataFileSet would throw an error if
#   the data set was empty, because then there was no path.
# 2009-02-26
# o Now hasTags(..., tags) splits the 'tags' argument.
# 2009-02-08
# o Now argument 'files' in extract() of GenericDataFileSet can 
#   also be a vector of string.
# o ALPHA: Added support for "smart" subdirectories in static findByName() 
#   of GenericDataFileSet.
# 2008-07-21
# o Now findByName() assert that the data set name is not empty.
# 2008-07-17
# o Added setFullName(), and setName().
# o Added setFullNameTranslator() for the data set itself.
# 2008-07-14
# o Added update2().
# o Added setFullNamesTranslator() to set it for all files.
# 2008-06-07
# o Added argument 'recursive' to fromFiles().
# 2008-05-16
# o Now ... is passed down by getFullNames().
# 2008-05-15
# o Added equals().
# 2008-05-14
# o Added hasFile() to GenericDataFileSet.
# 2008-05-11
# o Added static findByName() and byName() to GenericDataFileSet.
# o If argument '.validate' to fromFiles() is TRUE, validate() is called.
# o Added validate().
# o Now argument 'fileClass' of fromFiles() defaults to getFileClass().
# o Added static and protected getFileClass().
# 2008-03-22
# o Now getNames() of GenericDataFileSet passes '...' to getName() of 
#   each file.
# 2007-09-14
# o Extracted from AffymetrixFileSet.
# 2007-03-06
# o Added indexOf().
# 2007-02-15
# o Added getFullNames().
# 2007-02-07
# o Added sapply().
# 2007-01-14
# o Added a test for "unknown" (=unused) arguments to constructor.
# 2006-01-07
# o Added hasTags() and hasTag().
# o Now arguments '...' in fromFiles() are passed to the constructor of
#   the static class, i.e. via newInstance(static, ...).  Requested by KS.
# o Added argument 'alias' to constructor.
# o Added getAlias() and setAlias(), where the latter replaces setName().
# 2006-12-02
# o Added reorder().
# 2006-12-01
# o Now lapply() add data file names to returned list.
# 2006-11-20
# o Added support to override name of file set.
# o Added support for optional tags.
# 2006-11-02
# o Added getFullName(), getTags() and redefined getName().
# 2006-10-22
# o Now 'recursive' of fromFiles() defaults to FALSE.
# o Added getFiles() again.
# 2006-09-11
# o Added getPathnames().
# 2006-08-27
# o Added getFile() and getFiles().
# o Made filenames starting with a period reserved for internal use.
# 2006-08-26
# o Now getName() of a file set is inferred from the pathname:
#     path/to/<name>/chip_files/<"chip type">/
# 2006-08-21
# o Renamed 'array' to 'file'.
# o Extracted from AffymetrixCelSet.R.
# 2006-08-11
# o Added clearCache() which also clears the cache of all data file object.
# 2006-05-16
# o Redefined "[" to extract arrays.
# 2006-04-13
# o Added Rdoc comments for all methods.
# 2006-04-09
# o Now the read map is loaded automatically when fromFiles() used.
# 2006-03-30
# o Updated to new aroma.apd.
# 2006-03-18
# o Added argument 'subset' to calcAvgCellSignals() & normalizeQuantile().
# 2006-03-15
# o Now nbrOfCells() returns the number of cells for the first file only.
# o Now the fromFiles(static, ...) creates an object of the same class as 
#   the static object.
# 2006-03-04
# o Added mapping functions.
# o Added writeApd().
# 2006-03-03
# o Added lapply().
# 2006-03-02
# o Updated to deal with AffymetrixDataFile object instead of CEL directly.
# 2006-02-21
# o Letting readCelUnits() transform signals improves speed substantially.
# o Making use of new multi-array readCelUnits().
# 2006-02-20
# o Created.
############################################################################
