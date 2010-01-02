###########################################################################/**
# @RdocClass GenericDataFileSetList
#
# @title "The GenericDataFileSetList class"
#
# \description{
#  @classhierarchy
#
#  A GenericDataFileSetList object represents a list of
#  @see "GenericDataFileSet"s.
# }
# 
# @synopsis
#
# \arguments{
#   \item{dsList}{A single or a @list of @see "GenericDataFileSet":s.}
#   \item{tags}{A @character @vector of tags.}
#   \item{...}{Not used.}
#   \item{allowDuplicates}{If @FALSE, files with duplicated names are not
#     allowed and an exception is thrown, otherwise not.}
#   \item{.setClass}{A @character string specifying a name of the
#     class that each data set must be an instance of.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @examples "../incl/GenericDataFileSetList.Rex"
# 
# @author
#*/###########################################################################
setConstructorS3("GenericDataFileSetList", function(dsList=list(), tags="*", ..., allowDuplicates=TRUE, .setClass="GenericDataFileSet") {
  # Argument '.setClass':
  .setClass <- Arguments$getCharacter(.setClass, length=c(1,1));

  # Argument 'dsList':
  if (!is.list(dsList)) {
    dsList <- list(dsList);
  }

  if (is.list(dsList)) {
    for (ds in dsList) {
      ds <- Arguments$getInstanceOf(ds, .setClass);
    }
  } else {
    throw("Argument 'dsList' is not a list: ", class(dsList)[1]);
  }

  # Arguments 'allowDuplicates':
  allowDuplicates <- Arguments$getLogical(allowDuplicates);

  this <- extend(Object(), c("GenericDataFileSetList", uses("FullNameInterface")),
    .dsList = dsList,
    .tags = NULL,
    .allowDuplicates = allowDuplicates
  );

  setTags(this, tags);

  assertDuplicates(this);

  this;
})

setMethodS3("assertDuplicates", "GenericDataFileSetList", function(this, ...) {
  allowDuplicates <- this$.allowDuplicates;

  if (!allowDuplicates) {
    names <- getNames(this);
    dups <- names[duplicated(names)];
    n <- length(dups);
    if (n > 0) {
      if (n > 5) {
        dups <- c(dups[1:2], "...", dups[n]);
      }
      dups <- paste(dups, collapse=", ");
      throw(sprintf("Detected %n files with duplicated names, which are not allowed (onDuplicates=FALSE): %s", n, dups));
    }
  }
}, protected=TRUE)



setMethodS3("as.GenericDataFileSetList", "GenericDataFileSetList", function(this, ...) {
  # Nothing to do
  this;
})



setMethodS3("as.character", "GenericDataFileSetList", function(x, ...) {
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

  # Unique names
  names <- getNames(this);
  n <- length(names);
  if (n >= 5)
    names <- c(names[1:2], "...", names[n]);
  names <- paste(names, collapse=", ");
  s <- c(s, sprintf("Names: %s [%d]", names, n));

  # Number of file sets
  n <- nbrOfSets(this);
  s <- c(s, sprintf("Number of file sets: %d", n));

  for (kk in seq(length=n)) {
    ds <- getSet(this, kk);
    s <- c(s, sprintf("<File set #%d ('%s') of %d>", kk, getName(ds), n));
    s <- c(s, as.character(ds));
  }

  GenericSummary(s);
})

setMethodS3("clone", "GenericDataFileSetList", function(this, ...) {
  res <- NextMethod("clone", this);
  dsList <- getSets(this);
  for (kk in seq(along=dsList)) {
    dsList[[kk]] <- clone(dsList[[kk]]);
  }
  res$.dsList <- dsList;
  res;
})


setMethodS3("getAsteriskTags", "GenericDataFileSetList", function(this, ..., collapse=",") {
  # Get the tags of each data set
  dsList <- getSets(this);
  tags <- sapply(dsList, getTags, collapse=",");
  tags <- tags[nchar(tags) > 0];
  tags <- paste(tags, collapse=collapse);
  tags;
}, protected=TRUE)


setMethodS3("setTags", "GenericDataFileSetList", function(this, tags=NULL, ...) {
  # Argument 'tags':
  if (!is.null(tags)) {
    tags <- Arguments$getCharacters(tags);
    tags <- trim(unlist(strsplit(tags, split=",")));
    tags <- tags[nchar(tags) > 0];
  }

  this$.tags <- tags;

  invisible(this);
})


setMethodS3("getTags", "GenericDataFileSetList", function(this, ...) {
  # AD HOC; custom tags are taken care of in getDefaultFullName().
  NextMethod("getTags", this, ..., collapse=NULL, useCustomTags=FALSE);
}, protected=TRUE)


setMethodS3("getDefaultFullName", "GenericDataFileSetList", function(this, collapse="+", ...) {
  # Get the names of each data set
  dsList <- getSets(this);
  names <- sapply(dsList, getName);

  # By default, merge names
  name <- mergeByCommonTails(names, collapse=collapse);

  # Get the tags
  tags <- this$.tags;
  tags[tags == "*"] <- getAsteriskTags(this, collapse=",");
  tags <- tags[nchar(tags) > 0];

  fullname <- paste(c(name, tags), collapse=",");

  fullname;
})


setMethodS3("as.list", "GenericDataFileSetList", function(x, ...) {
  # To please R CMD check
  this <- x;

  getSets(this, ...);
})


setMethodS3("getSets", "GenericDataFileSetList", function(this, idxs=NULL, ...) {
  res <- this$.dsList;
  if (is.null(idxs)) {
  } else {
    n <- length(res);
    idxs <- Arguments$getIndices(idxs, max=n);
    res <- res[idxs];
  }
  res;
})

setMethodS3("getSet", "GenericDataFileSetList", function(this, idx, ...) {
  if (length(idx) != 1)
    throw("Argument 'idx' must be a single index.");
  res <- this$.dsList;
  n <- length(res);
  idx <- Arguments$getIndex(idx, max=n);
  res[[idx]];
})

setMethodS3("nbrOfSets", "GenericDataFileSetList", function(this, ...) {
  length(getSets(this));
})


setMethodS3("getFileListClass", "GenericDataFileSetList", function(this, ...) {
  classNames <- class(this);
  pattern <- "(.*)(List|Tuple)$";
  exts <- gsub(pattern, "\\2", classNames);
  keep <- is.element(exts, c("List", "Tuple"));
  classNames <- classNames[keep];
  exts <- exts[keep];
  classNames <- gsub("FileSet", "Set", classNames);
  classNames <- gsub("Set", "File", classNames);

  # Keep only existing class names
  keep <- sapply(classNames, FUN=exists, mode="function");
  classNames <- classNames[keep];

  if (length(classNames) == 0) {
    throw("Failed to locate a file list class for this set list: ", 
                                                      class(this)[1]);
  }

  className <- classNames[1];
    
  clazz <- Class$forName(className);
  classNames <- c(getKnownSubclasses(clazz), className);
  clazz <- NULL;
  for (kk in seq(along=classNames)) {
     className <- classNames[kk];
     tryCatch({
       clazz <- Class$forName(className);
     }, error = function(ex) {});
     if (!is.null(clazz)) {
       return(className);
     }
  } # for (kk ...)

  throw("Failed to locate a file list class for this set list: ", 
                                                      class(this)[1]);
}, protected=TRUE)



setMethodS3("getFullNames", "GenericDataFileSetList", function(this, ...) {
  dsList <- getSets(this);
  names <- lapply(dsList, FUN=getFullNames);
  names <- unlist(names, use.names=FALSE);
  names <- unique(names);
  names <- sort(names);
  names;
})


setMethodS3("getNames", "GenericDataFileSetList", function(this, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  unionWithDuplicates <- function(x, y) {
    keep <- is.element(x, y);
    x <- x[keep];
    keep <- is.element(y, x);
    y <- y[keep];
    z <- if(length(x) > length(y)) x else y;
    sort(z);
  }

  dsList <- getSets(this);
  names <- lapply(dsList, FUN=getNames);

  # Note, we cannot use union() if there may be duplicates
  names <- Reduce(unionWithDuplicates, names);

  names <- unlist(names, use.names=FALSE);

  # Return unique names?
  allowDuplicates <- this$.allowDuplicates;
  unique <- (!allowDuplicates);
  if (unique) {
    names <- unique(names);
  }

  names <- sort(names);
  names;
})


setMethodS3("nbrOfFiles", "GenericDataFileSetList", function(this, ...) {
  names <- getNames(this, ...);
  length(names);
})

setMethodS3("length", "GenericDataFileSetList", function(x, ...) {
  # To please R CMD check
  this <- x;
  nbrOfFiles(this, ...);
})


setMethodS3("indexOf", "GenericDataFileSetList", function(this, patterns=NULL, ..., onMissing=c("NA", "error")) {
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
    pattern <- sprintf("^%s$", pattern);
    pattern <- gsub("\\^\\^", "^", pattern);
    pattern <- gsub("\\$\\$", "$", pattern);

    # Specifying tags?
    if (regexpr(",", pattern) != -1) {
      idxs <- grep(pattern, fullnames);
    } else {
      idxs <- grep(pattern, names);
    }
    if (length(idxs) == 0)
      idxs <- naValue;

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



setMethodS3("extract", "GenericDataFileSetList", function(this, files, ..., drop=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'files':
  if (is.null(files)) {
    files <- seq(length=nbrOfFiles(this));
  } else if (is.logical(files)) {
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

  # Avoid return duplicates if not allowed
  allowDuplicates <- this$.allowDuplicates;
  if (!allowDuplicates) {
    dups <- files[duplicated(files)];
    n <- length(dups);
    if (n > 0) {
      if (n > 1) {
        dups <- c(dups[1:2], "...", dups[n]);
      }
      dups <- paste(dups, collapse=", ");
      throw(sprintf("Argument 'files' contains %s duplicates, which is not allowed (allowDuplicates=FALSE): %s", n, dups));
    }
  }

  names <- getNames(this);
  n <- nbrOfFiles(this);
  files <- Arguments$getIndices(files, max=n, disallow="NaN");
  files <- names[files];

  dsList <- getSets(this);
  setNames <- sapply(dsList, FUN=getFullName);
  dsList <- lapply(dsList, FUN=extract, files, ...);
  names(dsList) <- setNames; 

  # Drop empty data sets?
  if (drop) {
    ns <- sapply(dsList, nbrOfFiles);
    dsList <- dsList[ns > 0];
  }

  res <- clone(this);
  res$.dsList <- dsList;

  assertDuplicates(res);

  res;
}, protected=TRUE)



setMethodS3("as.data.frame", "GenericDataFileSetList", function(x, row.names=NULL, ..., names=NULL, onDuplicates=c("drop", "error")) {
  # To please R CMD check
  this <- x;

  # Argument 'names':
  if (is.null(names)) {
    names <- getNames(this);
  } else {
    names <- Arguments$getCharacters(names);
  }

  # Argument 'row.names':
  if (is.null(row.names)) {
    if (any(is.na(names))) {
      row.names <- seq(along=names);
    } else {
      row.names <- names;
    }
  } else {
    row.names <- Arguments$getCharacters(row.names, 
                                         length=rep(length(names),2));
  }

  # Argument 'onDuplicates':
  onDuplicates <- match.arg(onDuplicates);


  # Extract subset of interests (missing files ok; duplicates too)
  if (is.element(onDuplicates, c("drop", "error"))) {
    onDuplicates2 <- onDuplicates;
  } else {
    onDuplicates2 <- "ignore";
    # Not supported
    throw("Unsupported value of 'onDuplicates': ", onDuplicates);
  }

  res <- extract(this, names, onMissing="NA", onDuplicates=onDuplicates2, ...);

  # Get list of files
  dsList <- getSets(res);
  dfList <- lapply(dsList, FUN=getFiles);
  rm(res, dsList);

  # Sanity check
  ns <- sapply(dfList, FUN=length);
  if (length(unique(ns)) != 1) {
    throw("Internal error. Cannot extract data frame. Non-balanced data sets.");
  }

  attr(dfList, "row.names") <- row.names;
  attr(dfList, "class") <- "data.frame";

  dfList;
}, protected=TRUE)


setMethodS3("getFileList", "GenericDataFileSetList", function(this, ..., dropMissing=TRUE) {
  dsList <- extract(this, ..., onDuplicated="error");
  dfList <- lapply(dsList, FUN=getFile, 1);

  if (dropMissing) {
    keep <- sapply(dfList, FUN=isFile);
    dfList <- dfList[keep];
  }

  # Coerce to a file list
  className <- getFileListClass(this);
  clazz <- Class$forName(className);
  dfList <- newInstance(clazz, dfList);

  dfList;
})



setMethodS3("getFileListV0", "GenericDataFileSetList", function(this, name, dropMissing=TRUE, ...) {
  throw("DEPRECATED");

  # Argument 'name':
  name <- Arguments$getCharacter(name);

  dsList <- getSets(this);

  dfList <- list();
  names <- character(0);
  for (kk in seq(along=dsList)) {
    ds <- dsList[[kk]];
    idx <- indexOf(ds, name);
    if (!is.na(idx)) {
      dfList[[kk]] <- getFile(ds, idx);
      names[kk] <- names(dsList)[kk];
    }
  }
  if (!is.null(names(dfList))) {
    names(dfList) <- names;
  }

  if (dropMissing) {
    dfList <- dfList[!sapply(dfList, FUN=is.null)];
  }

  # Coerce to a file list
  className <- getFileListClass(this);
  clazz <- Class$forName(className);
  dfList <- newInstance(clazz, dfList);

  dfList;
}, deprecated=TRUE)


###########################################################################
# HISTORY:
# 2010-01-01
# o BUG FIX: getNames() of GenericDataFileSetList would drop duplicated
#   names if there where more than one data set.
# o Added constructor argument 'allowDuplicates=TRUE'.  If FALSE, an
#   error is thrown if a object with duplicates are tried to be created,
#   either via the constructor or via extract().
# 2009-12-30
# o Rename getFileClass() to getFileListClass().
# o Replaced getListOfSets() with getSets(), cf. getFiles().
# o Created new getFileList().
# o Turned into an Object().
# o Renamed argument '.fileSetClass' to '.setClass'.
# o Now GenericDataFileSetList is a FullNameInterface class.
# o Added Rdoc comments for GenericDataFileSetList.
# o Added protected as.data.frame() to GenericDataFileSetList.
# o Added protected extract().
# 2009-06-03
# o Added argument '.fileSetClass' to the constructor.
# 2009-05-12
# o Created.
###########################################################################
