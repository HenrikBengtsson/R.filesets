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
#   \item{files}{A @list of @see "GenericDataFile":s or
#      a @see "GenericDataFileSet".}
#   \item{tags}{A @character @vector of tags to be used for this file set.
#      The string \code{"*"} indicates that it should be replaced by the
#      tags part of the file set pathname.}
#   \item{depth}{An non-negative @integer.}
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
setConstructorS3("GenericDataFileSet", function(files=NULL, tags="*", depth=NULL, ..., .onUnknownArgs=c("error", "warning", "ignore")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments 'files':
  if (is.null(files)) {
  } else if (inherits(files, "GenericDataFileSet") || is.list(files)) {
    if (!is.list(files)) files <- as.list(files)
    reqFileClass <- GenericDataFileSet$getFileClass()
    base::lapply(files, FUN=function(df) {
      Arguments$getInstanceOf(df, reqFileClass)
    })
  } else {
    throw("Argument 'files' is of unknown type: ", mode(files)[1L])
  }

  # Arguments 'depth':
  if (!is.null(depth)) {
    depth <- Arguments$getInteger(depth, range=c(0,32))
  }

  # Arguments '.onUnknownArgs':
  .onUnknownArgs <- match.arg(.onUnknownArgs)

  # Arguments '...':
  args <- list(...)
  if (length(args) > 0L) {
    if (is.element(.onUnknownArgs, c("error", "warning"))) {
      argsStr <- paste(names(args), collapse=", ")
      msg <- sprintf("Unknown arguments: %s", argsStr)
      if (.onUnknownArgs == "error") {
        throw(msg)
      } else if (.onUnknownArgs == "warning") {
        warning(msg)
      }
    }
  }

  files <- as.list(files)


  this <- extend(Object(), c("GenericDataFileSet", uses("FullNameInterface")),
    "cached:.fileSize" = NULL,
    files = files,
    .depth = depth,
    .tags = NULL
  )

  setTags(this, tags)

  this
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
  this <- x

  s <- sprintf("%s:", class(this)[1L])

  # Name and tags of file set
  s <- c(s, sprintf("Name: %s", getName(this)))
  tags <- getTags(this, collapse=",")
  if (!is.null(tags)) {
    s <- c(s, sprintf("Tags: %s", tags))
  }

  # Full names of file set
  s <- c(s, sprintf("Full name: %s", getFullName(this)))

  # Subdirectories(?)
  subdirs <- getSubdirs(this, default=NULL)
  if (length(subdirs) > 0L) {
    s <- c(s, sprintf("Subpath: %s", subdirs))
  }

  # Number of files in set
  n <- length(this)
  s <- c(s, sprintf("Number of files: %d", n))

  # Names of files
  names <- getNames(this)
  s <- c(s, sprintf("Names: %s [%d]", hpaste(names), n))

  # Pathname
  path <- getPath(this)
  if (!is.na(path)) {
    pathR <- getRelativePath(path)
    if (nchar(pathR) < nchar(path)) path <- pathR
  }
  s <- c(s, paste("Path (to the first file): ", path, sep=""))

  # File size
  fileSize <- getFileSize(this, "units")
  if (!is.na(fileSize)) {
    fileSizeB <- sprintf("%.0f bytes", getFileSize(this, "numeric"))
    if (fileSizeB != fileSize) {
      fileSize <- sprintf("%s (%s)", fileSize, fileSizeB)
    }
  }
  s <- c(s, sprintf("Total file size: %s", fileSize))

  # Check fullnames translation
  getFullNames(this, onRemapping="warning")

  GenericSummary(s)
}, protected=TRUE)




setMethodS3("clearCache", "GenericDataFileSet", function(this, ...) {
  # Clear the cache of all files
  files <- getFiles(this)
  lapply(files, FUN=clearCache)

  # Then for this object
  NextMethod()
}, protected=TRUE)



setMethodS3("clone", "GenericDataFileSet", function(this, clear=TRUE, ...) {
  # Clone itself
  object <- NextMethod()

  # Clone each file object
  files <- as.list(object)
  for (kk in seq_along(files)) {
    files[[kk]] <- clone(files[[kk]], clear=TRUE)
  }
  object$files <- files

  # Clear the cached fields?
  if (clear)
    clearCache(object)

  object
}, protected=TRUE)



setMethodS3("getFileClass", "GenericDataFileSet", function(static, ...) {
  # By default, infer the file class from the set class.
  name <- class(static)[1L]
  name <- gsub("Set$", "", name)
  if (regexpr("File$", name) == -1L) {
    name <- paste(name, "File", sep="")
  }
  name
}, static=TRUE, protected=FALSE)



###########################################################################/**
# @RdocMethod validate
#
# @title "Validates all files in the data set"
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
#   If one of the files is invalid, then an error is thrown.
#   If all of the files are valid, then @TRUE is returned.
#   Otherwise, @NA is returned.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("validate", "GenericDataFileSet", function(this, ...) {
  I <- length(this)
  res <- rep(NA, times=I)
  for (ii in seq_len(I)) {
    res[ii] <- validate(this[[ii]], ...)
  }

  # Summarize across all files
  res <- all(res, na.rm=FALSE)

  res
}, protected=FALSE)




setMethodS3("getFileSize", "GenericDataFileSet", function(this, what=c("numeric", "units"), sep="", ..., force=FALSE) {
  # Argument 'what':
  what <- match.arg(what)

  fileSize <- this$.fileSize
  if (force || is.null(fileSize)) {
    files <- getFiles(this)
    fileSizes <- unlist(lapply(files, FUN=getFileSize), use.names=FALSE)
    fileSize <- sum(fileSizes, na.rm=TRUE)
    this$.fileSize <-  fileSize
  }

  if (what == "numeric")
    return(fileSize)

  if (is.na(fileSize))
    return(fileSize)

  hsize(fileSize, digits = 2L, standard = "IEC")
})



###########################################################################/**
# @RdocMethod getPath
# @alias getPath
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
  # Find a file with a non-missing pathname
  file <- getOneFile(this)
  getPath(file, ...)
})


setMethodS3("getDepth", "GenericDataFileSet", function(this, default=0L, ...) {
  # Argument 'default':
  if (!is.null(default)) {
    default <- Arguments$getInteger(default, range=c(0,32))
  }

  depth <- this$.depth
  if (is.null(depth)) {
    depth <- default
  }
  depth
}, private=TRUE)


setMethodS3("setDepth", "GenericDataFileSet", function(this, depth=0L, ...) {
  # Argument 'depth':
  if (!is.null(depth)) {
    depth <- Arguments$getInteger(depth, range=c(0,32))
  }

  this$.depth <- depth

  invisible(this)
}, private=TRUE)


setMethodS3("getSubdirs", "GenericDataFileSet", function(this, collapse="/", ...) {
  if (!is.null(collapse)) {
    collapse <- Arguments$getCharacter(collapse)
  }

  depth <- getDepth(this, ...)
  if (is.null(depth)) {
    return(NULL)
  }

  path <- getPath(this)
  dirs <- character(length=depth)
  for (dd in seq_len(depth)) {
    dirs[dd] <- basename(path)
    path <- dirname(path)
  }
  dirs <- rev(dirs)

  if (length(dirs) > 1L && !is.null(collapse)) {
    dirs <- paste(dirs, collapse=collapse)
  }

  dirs
}, protected=TRUE) # getSubdirs()


###########################################################################/**
# @RdocMethod length
# @aliasmethod nbrOfFiles
#
# @title "Gets the number of files in the set"
#
# \description{
#   @get "title".
# }
#
# \usage{
#  @usage length,GenericDataFileSet
#  @usage nbrOfFiles,GenericDataFileSet
# }
#
# \value{
#   Returns an non-negative @integer.
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
  this <- x

  length(this$files)
}, private=TRUE, appendVarArgs=FALSE)


setMethodS3("nbrOfFiles", "GenericDataFileSet", function(this, ...) {
  length(this, ...)
}, protected=TRUE)



setMethodS3("reorder", "GenericDataFileSet", function(x, order, ...) {
  # To please R CMD check
  this <- x

  # Argument 'order':
  if (is.character(order)) {
    # Assume 'order' contains names
    names <- getNames(this)

    # Identify special tags and remove them
    idx <- (order == "*")
    if (sum(idx) > 1)
      throw("Argument 'order' contains more than one asterix.")
    pos <- match(order[!idx], names)
    if (any(is.na(pos))) {
      bad <- order[!idx][is.na(pos)]
      throw("Argument 'order' contains unknown sample names: ",
                                                 paste(bad, collapse=", "))
    }
    if (sum(idx) == 0) {
      order <- pos
    } else {
      order <- as.list(order)
      order[!idx] <- names[pos]
      order[[which(idx)]] <- setdiff(names, names[pos])
      order <- unlist(order, use.names=FALSE)
      order <- match(order, names)
    }
  }

  order <- Arguments$getIndices(order, max=length(this))
  if (any(duplicated(order))) {
    bad <- order[duplicated(order)]
    throw("Argument 'order' contains duplicates: ",
                                                 paste(bad, collapse=", "))
  }

  this$files <- this$files[order]
  invisible(this)
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
#  \item{decreasing}{If @TRUE the sorting is done in a decreasing manner.}
#  \item{caseSensitive}{If @TRUE, the ordering is case sensitive,
#        otherwise not.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns returns itself with the set ordered accordingly.
# }
#
# \details{
#   The set is ordering by the fullnames.
#   If \code{by="lexicographic"}, lexicographic ordering is used,
#   sometimes also referred to as alphabetic ordering.
#   If \code{by="mixeddecimal"}, mixedsort ordering acknowledging
#   decimal numbers is used, cf. @see "gtools::mixedsort".
#   If \code{by="mixedroman"}, mixedsort ordering acknowledging
#   roman numerals is used, cf. @see "gtools::mixedsort".
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("sortBy", "GenericDataFileSet", function(this, by=c("lexicographic", "mixedsort", "mixeddecimal", "mixedroman", "filesize"), decreasing=FALSE, caseSensitive=FALSE, ...) {
  # Argument 'by':
  by <- match.arg(by)
  if (by == "mixedsort") by <- "mixeddecimal"

  # Argument 'decreasing':
  decreasing <- Arguments$getLogical(decreasing)

  # Argument 'caseSensitive':
  caseSensitive <- Arguments$getLogical(caseSensitive)

  if (by == "lexicographic") {
    fullnames <- getFullNames(this)
    if (!caseSensitive) fullnames <- tolower(fullnames)
    order <- order(fullnames, decreasing=decreasing, ...)
  } else if (by == "mixeddecimal") {
    fullnames <- getFullNames(this)
    if (!caseSensitive) fullnames <- tolower(fullnames)
    order <- gtools::mixedorder(fullnames, numeric.type="decimal")
    if (decreasing) order <- rev(order)
  } else if (by == "mixedroman") {
    fullnames <- getFullNames(this)
    if (!caseSensitive) fullnames <- tolower(fullnames)
    order <- gtools::mixedorder(fullnames, numeric.type="roman", roman.case="both")
    if (decreasing) order <- rev(order)
  } else if (by == "filesize") {
    sizes <- sapply(this, FUN=getFileSize)
    order <- order(sizes, decreasing=decreasing, ...)
  }

  # Sanity check
  stop_if_not(!any(is.na(order)))
  stop_if_not(length(unique(order)) == length(order))

  this$files <- this$files[order]

  this
})


###########################################################################/**
# @RdocMethod getNames
# @aliasmethod getFullNames
# @aliasmethod names
#
# @title "Gets the names (or fullnames) of the files in the file set"
#
# \description{
#   @get "title".
# }
#
# \usage{
#  @usage getNames,GenericDataFileSet
#  @usage getFullNames,GenericDataFileSet
# }
#
# \arguments{
#  \item{...}{Arguments passed to \code{getName()} (\code{getFullName()})
#    of each file.}
#  \item{onRemapping}{Action to take if the fullnames before and after
#    translation do not map consistently to the same file indices.}
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
  files <- as.list(this, useNames=FALSE)
  res <- unlist(lapply(files, FUN=getName, ...))
  unname(res)
})

setMethodS3("getFullNames", "GenericDataFileSet", function(this, ..., onRemapping=getOption("R.filesets::onRemapping", "ignore")) {
  ## Argument 'onRemapping':
  onRemapping <- match.arg(onRemapping, choices=c("ignore", "warning", "error"))

  files <- as.list(this, useNames=TRUE, translate=FALSE)
  names <- unlist(lapply(files, FUN=getFullName, ...), use.names=FALSE)

  ## Assert bijective mapping after translation?
  if (onRemapping != "ignore" && length(names) > 1L) {
    names0 <- names(files)
    idxs0 <- match(unique(names0), names0)
    idxs <- match(unique(names), names)
    if (!identical(idxs, idxs0)) {
      signal <- if (onRemapping == "warning") warning else throw

      msg <- sprintf("%s %s: Invalid full-names translation detected. One or more of the full-names translator functions need to be corrected.", class(this)[1], sQuote(getFullName(this)))

      missing <- setdiff(idxs0, idxs)
      if (length(missing) > 0L) {
        map <- sprintf("%s->%s used to map to #%d", sQuote(names0[missing]), sQuote(names[missing]), missing)
        msg <- sprintf("%s After translation, some names no longer map to an index (%s).", msg, hpaste(map, collapse="; "))
      }

      ## NB: Can this even happen?
      extra <- setdiff(idxs, idxs0)
      if (length(extra) > 0L) {
        map <- sprintf("%s->%s now maps to #%d", sQuote(names0[extra]), sQuote(names[extra]), extra)
        msg <- sprintf("%s After translation, some names map to previously unknown indices (%s).", msg, hpaste(map, collapse="; "))
      }

      ## Otherwise...
      if (length(missing) == 0L && length(extra) == 0L) {
        neq <- (idxs != idxs0)
        names <- names[neq]
        idxs <- idxs[neq]
        names0 <- names0[neq]
        idxs0 <- idxs0[neq]
        map <- sprintf("%s->#%d", names, idxs)
        map0 <- sprintf("%s->#%d", names0, idxs0)
        msg <- sprintf("%s The translated names has a different mapping than the non-translated ones: (%s) != (%s).", msg, hpaste(map, collapse="; "), hpaste(map0, collapse="; "))
      }

      signal(msg)
    } # if (!identical(idxs, idxs0))
  } ## if (validate)

  names
})

setMethodS3("names", "GenericDataFileSet", function(x, ...) {
  getFullNames(x, ...)
}, protected=TRUE)



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
#  \item{by}{A @character @vector specifying how and in what order the
#   name matching is done.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns an @integer @vector of length K with elements in
#   [1,@seemethod "length"] or (integer) @NA (for non-matched names).
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
setMethodS3("indexOf", "GenericDataFileSet", function(this, patterns=NULL, by=c("exact", "regexp", "fixed"), ..., onMissing=c("NA", "error")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument '...':
  args <- list(...)
  if (is.element("names", names(args))) {
    throw("Unknown argument 'names' to indexOf() for GenericDataFileSet.")
  }

  # Argument 'by':
  by <- match.arg(by, several.ok=TRUE)

  # Argument 'onMissing':
  onMissing <- match.arg(onMissing)


  names <- getNames(this)

  # Nothing to search for?
  if (is.null(patterns)) {
    # Return all indices
    res <- seq_along(names)
    names(res) <- names
    return(res)
  }

  fullnames <- getFullNames(this)

  patterns0 <- patterns
  res <- lapply(patterns, FUN=function(pattern) {
    pattern0 <- pattern

    # Search among fullnames or the names?
    hasTags <- (regexpr(",", pattern, fixed=TRUE) != -1L)
    if (hasTags) {
      searchStrings <- fullnames
    } else {
      searchStrings <- names
    }

    for (how in by) {
      if (how == "regexp") {
        # Regular expression:
        # Assert that the regular expression has a "head" and a "tail".
        pattern <- sprintf("^%s$", pattern)
        pattern <- gsub("\\^\\^", "^", pattern)
        pattern <- gsub("\\$\\$", "$", pattern)

        # Escape '+', and '*', if needed
        lastPattern <- ""
        while (pattern != lastPattern) {
          lastPattern <- pattern
          pattern <- gsub("(^|[^\\]{1})([+*])", "\\1\\\\\\2", pattern)
        }

        # Match
        idxs <- grep(pattern, searchStrings, fixed=FALSE)
      } else if (how == "fixed") {
        # Fixed string:
        pattern <- pattern0
        idxs <- grep(pattern, searchStrings, fixed=TRUE)
      } else if (how == "exact") {
        # Fixed string:
        pattern <- pattern0
        idxs <- which(pattern == searchStrings)
      }

      # Done?
      if (length(idxs) > 0L) break
    } # for (how ...)

    # Nothing found?
    if (length(idxs) == 0L) idxs <- NA_integer_

    # Note that 'idxs' may return more than one match
    idxs
  })

  ns <- sapply(res, FUN=length)
  names <- NULL
  for (kk in seq_along(ns)) {
    names <- c(names, rep(patterns0[kk], times=ns[kk]))
  }
  res <- unlist(res, use.names=FALSE)
  names(res) <- names

  # Not allowing missing values?
  if (onMissing == "error" && any(is.na(res))) {
    names <- names(res)[is.na(res)]
    throw("One or more files where not found: ", paste(sQuote(names), collapse=", "))
  }

  res
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
  files <- getFiles(this)
  unlist(lapply(files, FUN=getPathname))
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
#   \item{...}{Arguments passed to @seemethod "getFiles".}
#   \item{useNames}{If @TRUE, the list will be annotated with names.}
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
setMethodS3("as.list", "GenericDataFileSet", function(x, useNames=TRUE, ...) {
  getFiles(x, ..., useNames=useNames)
})



###########################################################################/**
# @RdocMethod getFile
# @aliasmethod [[
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
#   \item{idx}{A @numeric index or a @character string specifying the
#    file to be returned.}
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
#   If argument \code{idx} is a @character, then internally
#   @seemethod "indexOf" is used to identify what to return.
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("getFile", "GenericDataFileSet", function(this, idx, ...) {
  # Argument 'idx':
  if (length(idx) != 1L) {
    throw("Argument 'idx' must be a scalar.")
  }

  res <- this$files

  if (is.numeric(idx)) {
    n <- length(res)
    idx <- Arguments$getIndex(idx, max=n)
  } else if (is.character(idx)) {
    idx <- indexOf(this, idx, by="exact", onMissing="error", ...)
  } else {
    throw("Argument 'idx' must be either a numeric index or a character string: ", mode(idx))
  }

  res[[idx]]
})


setMethodS3("getFiles", "GenericDataFileSet", function(this, idxs=NULL, useNames=FALSE, ...) {
  res <- this$files

  # Subset?
  if (!is.null(idxs)) {
    n <- length(res)
    idxs <- Arguments$getIndices(idxs, max=n)
    res <- res[idxs]
  }

  # Add names?
  if (useNames) {
    names(res) <- sapply(res, FUN=getFullName, ...)
  }

  res
}, protected=TRUE)


setMethodS3("getOneFile", "GenericDataFileSet", function(this, default=NA, mustExist=is.null(default), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  getDefault <- function() {
    if (is.null(default)) return(NULL)
    if (!is.object(default) && is.na(default)) {
      className <- getFileClass(this)
      clazz <- Class$forName(className, envir=parent.frame())
      default <- newInstance(clazz, NA_character_)
    } else if (is.numeric(default)) {
      default <- this[[default]]
    }
    default <- Arguments$getInstanceOf(default, "GenericDataFile")
    default
  } # getDefault()


  # Argument 'mustExist':
  mustExist <- Arguments$getLogical(mustExist)


  files <- getFiles(this)
  I <- length(files)

  # Nothing?
  if (I == 0L) {
    if (mustExist) {
      throw("Cannot retrieve a file with a non-missing pathname. File set is empty.")
    }
    return(getDefault())
  }


  # Find first file with a non-missing pathname
  for (ii in seq_len(I)) {
    file <- files[[ii]]
    pathname <- getPathname(file)
    # Found?
    if (!is.null(pathname) && !is.na(pathname)) {
      return(file)
    }
  } # for (ii ...)


  # Nothing found
  if (mustExist) {
    throw("Cannot retrieve a file with a non-missing pathname. File set contains no such files.")
  }

  return(getDefault())
}) # getOneFile()



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
#  \item{.fileClass}{A @character string specifying the class that
#    all files must inherit from.
#    If @NULL, @seemethod "getFileClass" is used.}
#  \item{.assertSameClass}{If @TRUE, the files to be appended must inherit
#    from the same class as the existing files (the first file).}
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
setMethodS3("appendFiles", "GenericDataFileSet", function(this, files, clone=TRUE, ..., .fileClass=getFileClass(this), .assertSameClass=TRUE, verbose=FALSE) {
  # Argument 'files':
  if (!is.list(files)) {
    files <- list(files)
  }

  # Argument '.fileClass':
  if (is.null(.fileClass)) {
    .fileClass <- getFileClass(this)
  } else {
    .fileClass <- Arguments$getCharacter(.fileClass)
  }

  if (length(files) > 0L) {
    # Assert that all files are instances of the file class of this set.
    className <- .fileClass
    isValid <- unlist(lapply(files, FUN=inherits, className))
    if (!all(isValid)) {
      classNames <- sapply(files, FUN=function(x) class(x)[1L])
      classNames <- classNames[!isValid]
      classNames <- unique(classNames)
      throw(sprintf("Argument 'files' contains non-%s objects: %s",
                                    className, hpaste(classNames)))
    }

    # Must inherit from the same class as the existing files?
    if (.assertSameClass && length(this) > 0L) {
      aFile <- getOneFile(this)
      className <- class(aFile)[1L]
      isValid <- unlist(lapply(files, FUN=inherits, className))
      if (!all(isValid)) {
        classNames <- sapply(files, FUN=function(x) class(x)[1L])
        classNames <- classNames[!isValid]
        classNames <- unique(classNames)
        throw(sprintf("Argument 'files' contains non-%s objects (which is what the set already contains): %s", className, hpaste(classNames)))
      }
    }
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }


  verbose && enter(verbose, "Appending ", length(files), " files")
  if (length(files) > 0L) {
    # Clone file objects?
    if (clone) {
      verbose && enter(verbose, "Cloning files")
      files <- base::lapply(files, FUN=function(file) clone(file))
      verbose && exit(verbose)
    }

    # Append
    this$files <- base::append(this$files, files, ...)

    # Some cached values are incorrect now.
    clearCache(this)
  } else {
    verbose && cat(verbose, "No files to append. Skipping.")
  }

  verbose && exit(verbose)

  invisible(this)
})


###########################################################################/**
# @RdocMethod append
# @aliasmethod c
# @aliasmethod rep
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
  this <- x
  other <- values

  # Argument 'other':
  if (inherits(other, "GenericDataFileSet")) {
    other <- Arguments$getInstanceOf(other, class(this)[1L])
    files <- getFiles(other)
  } else {
    files <- other
  }

  appendFiles(this, files, ...)
})



###########################################################################/**
# @RdocMethod extract
# @alias extract
# @aliasmethod [
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
#    If \code{"drop"}, the missing file is dropped.
#    If \code{"dropall"}, an empty data set is return if one or more
#    missing files are requested.
#  }
# }
#
# \value{
#   Returns a @see "GenericDataFileSet" with zero of more
#   @see "GenericDataFile":s.
# }
#
# @author
#
# \seealso{
#   @see "stats::na.omit" for dropping missing files from a fileset.
#   @seeclass
# }
#*/###########################################################################
setMethodS3("extract", "GenericDataFileSet", function(this, files, ..., onMissing=c("NA", "error", "drop", "dropall"), onDuplicates=c("ignore", "drop", "error")) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'files':
  nbrOfFiles <- length(this)
  if (is.logical(files)) {
    files <- Arguments$getVector(files, length=rep(nbrOfFiles, times=2))
    files <- which(files)
  } else if (is.character(files)) {
    files <- indexOf(this, files, ...)
  } else if (is.numeric(files)) {
    files <- Arguments$getIntegers(files, disallow="NaN")

    # Exclude indices?
    if (any(files < 0L, na.rm=TRUE)) {
      incl <- files[files > 0L]
      if (length(incl) == 0L) {
        incl <- seq_along(this)
      }
      excl <- na.omit(files[files < 0L])
      files <- setdiff(incl, -excl)
      # Not needed anymore
      incl <- excl <- NULL
    }
  }

  # Argument 'onMissing':
  onMissing <- match.arg(onMissing)

  # Argument 'onDuplicates':
  onDuplicates <- match.arg(onDuplicates)


  if (onMissing == "error") {
    disallow <- c("NA", "NaN")
  } else if (is.element(onMissing, c("NA", "drop", "dropall"))) {
    disallow <- c("NaN")
  }
  files <- Arguments$getIndices(files, max=nbrOfFiles, disallow=disallow)
  missing <- which(is.na(files))


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Handle missing files
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (length(missing) > 0L) {
    # Error with missing files?
    if (onMissing == "error") {
      throw("Detected missing files, which is not allowed (onMissing='error'): ", length(missing))
    }

    # Drop non-existing files?
    if (onMissing == "drop") {
      files <- files[is.finite(files)]
      missing <- integer(0L)
    } else if (onMissing == "dropall") {
      files <- files[c()]
      missing <- integer(0L)
    }
  }

  # Check for duplicates?
  if (onDuplicates != "ignore") {
    dups <- which(is.finite(files) & duplicated(names(files)))
    if (length(dups) > 0L) {
      dupNames <- names(files)[head(dups)]
      dupNames <- paste(dupNames, collapse=", ")
      if (onDuplicates == "error") {
        throw("Cannot extract file subset. Files with identical names detected: ", dupNames)
      } else if (onDuplicates == "drop") {
        warning("Dropping files with duplicated names: ", dupNames)
        files <- files[-dups]
        missing <- which(is.na(files))
      }
    }
  }

  res <- clone(this)
  files <- this$files[files]

  # Should missing files be returned?
  if (length(missing) > 0L) {
    className <- NULL
    if (length(this$files) > 0L) {
      # TODO: Drop this? /HB 2013-11-15
      className <- class(this$files[[1L]])[1L]
    }
    if (is.null(className)) {
      className <- getFileClass(this)
    }

    # Allocate a "missing" file of the correct class
    clazz <- Class$forName(className, envir=parent.frame())
    naValue <- newInstance(clazz, NA_character_, mustExist=FALSE)
    for (idx in missing) {
      files[[idx]] <- naValue
    }
  }

  res$files <- files
  files <- NULL; # Not needed anymore

  # Some cached values are incorrect now.
  clearCache(res)

  res
}) # extract()



###########################################################################/**
# @RdocMethod anyNA
# @alias is.na.GenericDataFileSet
#
# @title "Checks whether any of the pathnames are missing"
#
# \description{
#   @get "title".
#   Note that this only tests the \emph{pathnames} of files,
#   but it does not test whether the files exists or not.
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
#   @see "stats::na.omit" for dropping missing items.
#   @seeclass
# }
#*/###########################################################################
setMethodS3("anyNA", "GenericDataFileSet", function(x, ...) {
  files <- getFiles(x)
  for (df in files) {
    if (is.na(df)) return(TRUE)
  }
  FALSE
}) # anyNA()

setMethodS3("is.na", "GenericDataFileSet", function(x) {
  files <- getFiles(x)
  unlist(lapply(files, FUN=is.na))
}, appendVarArgs=FALSE) # is.na()



###########################################################################/**
# @RdocMethod byPath
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
#  \item{private}{If @FALSE, files starting with a period are excluded,
#     otherwise not.}
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
setMethodS3("byPath", "GenericDataFileSet", function(static, path=NULL, pattern=NULL, private=FALSE, recursive=FALSE, depth=0L, fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'path':
  path <- Arguments$getReadablePath(path, mustExist=TRUE)

  # Argument 'pattern':
  if (!is.null(pattern)) {
    pattern <- Arguments$getRegularExpression(pattern)
  }

  # Argument 'private':
  private <- Arguments$getLogical(private)

  # Argument 'fileClass':
  clazz <- Class$forName(fileClass, envir=parent.frame())
  dfStatic <- getStaticInstance(clazz)
  dfStatic <- Arguments$getInstanceOf(dfStatic, getFileClass(static))

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }


  verbose && enter(verbose, "Defining an ", class(static)[1L], " object from files")
  verbose && cat(verbose, "Path: ", path)
  verbose && cat(verbose, "Depth: ", depth)
  verbose && cat(verbose, "Pattern: ", pattern)
  verbose && cat(verbose, "File class: ", class(dfStatic)[1L])

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Create set of GenericDataFile objects from matching files
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Scan for files
  verbose && enter(verbose, "Scanning directory for files")
  pathnames <- list.files(path=path, pattern=pattern, full.names=TRUE,
                                   all.files=private, recursive=recursive)
  verbose && printf(verbose, "Found %d files/directories.\n", length(pathnames))
  if (length(pathnames) > 0L) {
    # Keep only files
    keep <- sapply(pathnames, FUN=isFile)
    pathnames <- pathnames[keep]
  }
  verbose && printf(verbose, "Found %d files.\n", length(pathnames))
  verbose && exit(verbose)

  if (length(pathnames) > 0L) {
    # Sort files in lexicographic order
    pathnames <- sort(pathnames)

    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Build list of GenericDataFile objects
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    verbose && enter(verbose, "Defining ", length(pathnames), " files")
    # NOTE: Argument 'recursive' to fromFile() below should really have
    #       been named 'subclasses', because it indicates whether also
    #       subclasses of class(dfStatic) should be considered or not.
    #       Looking for compatible subclasses is very slow, which is why
    #       should avoid doing it unless really necessary. /HB 2013-11-11
    subclasses <- recursive
    files <- list()
    for (kk in seq_along(pathnames)) {
      if (as.logical(verbose)) writeRaw(verbose, kk, ", ")
      df <- fromFile(dfStatic, pathnames[kk], recursive=subclasses, .checkArgs=FALSE, verbose=less(verbose))
      files[[kk]] <- df
      if (kk == 1L) {
        # Update the static class instance.  The reason for this is
        # that if the second file cannot be instanciated with the same
        # class as the first one, then the files are incompatible.
        # Note that 'df' might be of a subclass of 'dfStatic'.
        clazz <- Class$forName(class(df)[1L], envir=parent.frame())
        dfStatic <- getStaticInstance(clazz)
        # SPEEDUP: Now we don't need to scan for subclasses anymore.
        subclasses <- FALSE
      }
    }
    if (as.logical(verbose)) writeRaw(verbose, "\n")
    verbose && exit(verbose)
  } else {
    files <- list()
  }

  # Create the file set object
  if (inherits(static, "Class")) {
    className <- getName(static)
  } else {
    className <- class(static)[1L]
  }
  verbose && enter(verbose, "Allocating a new ", className, " instance")
  verbose && cat(verbose, "Arguments:")
  verbose && cat(verbose, "Number of files: ", length(files))
  verbose && str(verbose, list(...))
  set <- newInstance(static, files, ...)
  verbose && exit(verbose)

  # Allow the file set to update itself according to these new rules.
  verbose && enter(verbose, "Updating newly allocated ", className)
  update2(set, ..., verbose=less(verbose, 5))
  verbose && exit(verbose)

  # Set depth
  setDepth(set, depth)

  # Validate?
  if (.validate) {
    validate(set, verbose=less(verbose, 5))
  }

  verbose && exit(verbose)

  set
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
#  \item{...}{Additional arguments passed to \code{copyTo()} used to copy
#   the individual @see "GenericDataFile":s in the set.}
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
  path <- Arguments$getWritablePath(path)

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }


  nbrOfFiles <- length(this)
  verbose && enter(verbose, sprintf("Copying %d files", nbrOfFiles))
  verbose && cat(verbose, "Output path for files: ", path)

  for (kk in seq_len(nbrOfFiles)) {
    verbose && enter(verbose, sprintf("File %d of %d", kk, nbrOfFiles))
    cf <- this[[kk]]
    if (isFile(cf)) {
      cfCopy <- copyTo(cf, path=path, ..., verbose=less(verbose))
    }
    verbose && exit(verbose)
  }

  # Return new instance
  res <- byPath(this, path=path)

  verbose && exit(verbose)

  res
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
#  \item{firstOnly}{If @TRUE, only the first path found, if any, is returned,
#     otherwise all found paths are returned.}
#  \item{mustExist}{If @TRUE, an exception is thrown if the file set was
#     not found, otherwise not.}
#  \item{...}{Not used.}
#  \item{verbose}{...}
# }
#
# \value{
#   Returns a @character @vector of paths.
#   If no file sets were found, @NULL is returned.
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
  name <- Arguments$getCharacter(name)
  if (nchar(name) == 0L) {
    throw("A ", class(static)[1L], " must have a non-empty name: ''")
  }
  name <- Arguments$getFilename(name, .type="name")

  # Arguments 'tags':
  if (!is.null(tags)) {
    tags <- sapply(tags, FUN=Arguments$getFilename, .type="name", .name="tags")
  }

  # Arguments 'paths':
  if (is.null(paths)) {
    paths <- "."
  } else {
    paths <- unique(paths)
  }

  # Argument 'firstOnly':
  firstOnly <- Arguments$getLogical(firstOnly)

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }



  verbose && enter(verbose, "Locating data sets")


  verbose && enter(verbose, "Expanding paths by allowing for regular expression matching of the deepest subdirectory")

  verbose && cat(verbose, "Possible search paths before expansion:")
  verbose && print(verbose, paths)
  pathsOrg <- paths

  # Expand paths by regular expressions, in case they exist
  paths <- lapply(paths, FUN=function(path) {
    parent <- dirname(path)
    subdir <- basename(path);  # This will drop trailing slashes, if any.
    pattern <- sprintf("^%s(|[.](lnk|LNK))$", subdir)
    subdirs <- list.files(pattern=pattern, path=parent, full.names=FALSE)
    file.path(parent, subdirs)
  })
  paths <- unlist(paths, use.names=FALSE)

  verbose && cat(verbose, "Possible search paths after expansion:")
  verbose && print(verbose, paths)

  if (length(paths) == 0L) {
    if (mustExist) {
      throw("No such root path directories: ", paste(sQuote(pathsOrg), collapse=", "))
    }
    verbose && exit(verbose)
    return(NULL)
  }

  verbose && exit(verbose)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify existing root directories
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  verbose && enter(verbose, "Filtering out root paths that are existing directories")

  rootPaths <- sapply(paths, FUN=function(path) {
    Arguments$getReadablePath(path, mustExist=FALSE)
  })
  if (length(rootPaths) == 0L) {
    if (mustExist) {
      throw("None of the root path directories exist: ",
                                           paste(paths, collapse=", "))
    }
    verbose && exit(verbose)
    return(NULL)
  }
  rootPaths <- rootPaths[sapply(rootPaths, FUN=isDirectory)]

  verbose && cat(verbose, "Search root path:")
  verbose && print(verbose, rootPaths)

  verbose && exit(verbose)


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify existing data set directories
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # The full name of the data set
  fullname <- paste(c(name, tags), collapse=",")
  verbose && cat(verbose, "Fullname: ", fullname)

  # Look for matching data sets
  dataSetPaths <- file.path(rootPaths, fullname)

  # Look for existing directories
  dataSetPaths <- sapply(dataSetPaths, FUN=function(path) {
    Arguments$getReadablePath(path, mustExist=FALSE)
  })
  dataSetPaths <- dataSetPaths[sapply(dataSetPaths, FUN=isDirectory)]
  dataSetPaths <- unname(dataSetPaths)

  verbose && cat(verbose, "Search dataset paths:")
  verbose && print(verbose, dataSetPaths)


  paths <- NULL
  if (length(dataSetPaths) > 0L) {
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    # Identify existing subdirectories
    # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if (length(subdirs) >= 1L) {
      verbose && enter(verbose, "Search subdirectories")
      verbose && print(verbose, subdirs)

      for (kk in seq_along(subdirs)) {
        dir <- subdirs[kk]
        verbose && enter(verbose, sprintf("Subdirectory #%d ('%s') of %d", kk, dir, length(subdirs)))

        # Smart directory?
        if (identical(dir, "*"))
          dir <- ":.*:"
        pattern <- "^:([^:]*):$"
        isSmart <- (regexpr(pattern, dir) != -1L)
        if (isSmart) {
          verbose && enter(verbose, "Processing \"smart\" path")

          # Regular expression pattern for subsetting directories
          pattern <- gsub(pattern, "\\1", dir)
          pattern <- Arguments$getRegularExpression(pattern)

          pathsKK <- sapply(dataSetPaths, FUN=function(path) {
            # List all directories and files
            dirsT <- list.files(path=path, pattern=pattern, full.names=TRUE)
            if (length(dirsT) == 0L)
              return(NULL)
            # Keep only directories
            dirsT <- sapply(dirsT, FUN=function(path) {
              Arguments$getReadablePath(path, mustExist=FALSE)
            })
            dirsT <- dirsT[sapply(dirsT, FUN=isDirectory)]
            if (length(dirsT) == 0L)
              return(NULL)
            # Work only with the directory names
            dirsT <- basename(dirsT)

            # Keep only the first match
            # TO DO: Find a more powerful set of selecting directories
            # /HB 2009-02-11
            dir <- dirsT[1L]

            file.path(path, dir)
          })

          verbose && exit(verbose)
        } else {
          pathsKK <- file.path(dataSetPaths, dir)
        } # if (isSmart)

        # In case there are NULLs
        pathsKK <- unlist(pathsKK, use.names=FALSE)
        # Keep only directories
        pathsKK <- sapply(pathsKK, FUN=function(path) {
          Arguments$getReadablePath(path, mustExist=FALSE)
        })
        pathsKK <- pathsKK[sapply(pathsKK, FUN=isDirectory)]
        pathsKK <- unname(pathsKK)

        verbose && cat(verbose, "Existing paths:")
        verbose && print(verbose, pathsKK)

        paths <- c(paths, pathsKK)

        verbose && exit(verbose)
      } # for (kk ...)

      verbose && cat(verbose, "All existing paths:")
      verbose && print(verbose, paths)

      verbose && exit(verbose)
    } else {
      paths <- dataSetPaths
    } # if (length(subdirs) >= 1L)


    if (length(paths) > 1L) {
      if (firstOnly) {
        warning("Found duplicated data set: ", paste(paths, collapse=", "))
        paths <- paths[1L]
        verbose && cat(verbose, "Dropped all but the first path.")
      }
    }
  } # if (length(dataSetPaths) > 0L)

  if (length(paths) == 0L) {
    paths <- NULL

    if (mustExist) {
      msg <- sprintf("Failed to locate data set '%s'", fullname)
      if (!is.null(subdirs)) {
        subdirsStr <- paste(subdirs, collapse=", ")
        msg <- sprintf("%s (with subdirectory '%s')", msg, subdirsStr)
      }
        msg <- sprintf("%s in search path (%s)",
                                msg, paste(rootPaths, collapse=", "))

      throw(msg)
    }
  }

  verbose && exit(verbose)

  paths
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
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }

  verbose && enter(verbose, sprintf("Setting up %s by its name", class(static)[1L]))
  verbose && cat(verbose, "Name: ", name)
  verbose && cat(verbose, "Tags: ", paste(tags, collapse=","))

  # Record the "depth"/"subdirs".
  if (length(subdirs) > 0L) {
    subdirsT <- unlist(strsplit(subdirs, split="/\\\\"), use.names=FALSE)
    depth <- length(subdirsT)
  } else {
    depth <- 0L
  }

  verbose && printf(verbose, "Subpath [%d]: %s\n",
                    depth, paste(subdirs, collapse="/"))


  suppressWarnings({
    paths <- findByName(static, name=name, tags=tags, subdirs=subdirs,
             paths=paths, firstOnly=FALSE, mustExist=TRUE, verbose=verbose)
  })

  verbose && cat(verbose, "Paths to possible data sets:")
  verbose && print(verbose, paths)

  res <- NULL
  for (kk in seq_along(paths)) {
    path <- paths[kk]
    verbose && enter(verbose, sprintf("Trying path #%d of %d", kk, length(paths)))
    verbose && cat(verbose, "Path: ", path)

    suppressWarnings({
      res <- byPath(static, path=path, depth=depth, ..., verbose=verbose)
    })

    if (!is.null(res)) {
      verbose && cat(verbose, "Successful setup of data set.")
      verbose && exit(verbose)
      break
    }

    verbose && exit(verbose)
  } # for (kk ...)

  if (is.null(res)) {
    throw(sprintf("Failed to setup a data set for any of %d data directories located.", length(paths)))
  }

  verbose && exit(verbose)

  res
}, static=TRUE)



setMethodS3("hasFile", "GenericDataFileSet", function(this, file, ...) {
  # Argument 'file':
  file <- Arguments$getInstanceOf(file, "GenericDataFile")

  files <- getFiles(this)
  for (kk in seq_along(files)) {
    if (identical(file, files[[kk]]))
      return(TRUE)
  }

  return(FALSE)
})


setMethodS3("equals", "GenericDataFileSet", function(this, other, ..., verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }


  # Default values
  notEqual <- FALSE
  msg <- NULL
  attr(notEqual, "thisSet") <- getPath(this)
  attr(notEqual, "otherSet") <- getPath(other)

  if (!inherits(other, "GenericDataFileSet")) {
    msg <- sprintf("The 'other' is not a GenericDataFileSet: %s",
                                                 class(other)[1L])
    attr(notEqual, "reason") <- msg
    return(notEqual)
  }

  nbrOfFiles <- length(this)

  value <- nbrOfFiles
  valueOther <- length(other)
  if (value != valueOther) {
    msg <- sprintf("The number of files differ: %d != %d",
                                              value, valueOther)
    attr(notEqual, "reason") <- msg
    return(notEqual)
  }

  if (identical(getPathnames(this), getPathnames(other)))
    return(TRUE)

  for (kk in seq_along(this)) {
    verbose && enter(verbose, sprintf("File #%d of %d", kk, nbrOfFiles))
    df1 <- this[[kk]]
    df2 <- other[[kk]]
    eqls <- equals(df1, df2, ...)
    if (!eqls) {
      verbose && cat(verbose, "Not equal")
      return(eqls)
    }
    verbose && exit(verbose)
  }

  TRUE
})



setMethodS3("update2", "GenericDataFileSet", function(this, ...) {
}, protected=TRUE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# COMPRESSION
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

###########################################################################/**
# @RdocMethod gzip
# @aliasmethod gunzip
#
# @title "Compresses/uncompresses a set of files"
#
# \description{
#   @get "title" using gzip compression.
#   When compressing (uncompressing), each of the @see GenericDataFile
#   of the file set are compressed (uncompressed).
# }
#
# \usage{
#  @usage gzip,GenericDataFileSet
#  @usage gunzip,GenericDataFileSet
# }
#
# \arguments{
#  \item{...}{Arguments passed to \code{gzip()/gunzip()} on each
#    of the GenericDataFile entries.}
# }
#
# \value{
#   Returns (invisibly) itself.
# }
#
# @author
#
# \seealso{
#   Internally @see "R.utils::gzip" and @see "R.utils::gunzip" are used.
#   @seeclass
# }
#*/###########################################################################
setMethodS3("gunzip", "GenericDataFileSet", function(this, ...) {
  files <- sapply(this, FUN=gunzip, ...)
  invisible(this)
})


setMethodS3("gzip", "GenericDataFileSet", function(this, ...) {
  files <- sapply(this, FUN=gzip, ...)
  invisible(this)
})



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# VECTOR-RELATED METHODS
#
# The below implementations, makes the listed "core" methods to work:

# length():
# * seq_along()
#
# length() + [():
# * rev()
# * sample()
#
# length() + [() + c():
# * append()
#
# as.list() + length():
# * lapply(), sapply()
#
# ...what else?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("[", "GenericDataFileSet", function(x, i, ...) {
  extract(x, i, ...)
}, protected=TRUE)

setMethodS3("[[", "GenericDataFileSet", function(x, i, ...) {
  if (is.numeric(i) || is.character(i)) {
    getFile(x, i, ...)
  } else {
    NextMethod()
  }
}, protected=TRUE)


setMethodS3("c", "GenericDataFileSet", function(x, ...) {
  files <- as.list(x)
  args <- list(...)
  args <- lapply(args, FUN=function(x) {
    if (inherits(x, "GenericDataFileSet")) x <- as.list(x)
    if (inherits(x, "GenericDataFile")) x <- list(x)
    x
  })
  args <- Reduce(c, args)
  files <- c(files, args)
  newInstance(x, files)
}, protected=TRUE)


setMethodS3("rep", "GenericDataFileSet", function(x, ...) {
  idxs <- seq_along(x)
  idxs <- rep(idxs, ...)
  x[idxs]
}, protected=TRUE)


setMethodS3("findDuplicated", "GenericDataFileSet", function(x, ..., fromLast=FALSE, any=FALSE) {
  # Local functions
  isDuplicated <- function(file, files, ...) {
    if (length(files) == 0L) return(FALSE)
    for (ii in seq_along(files)) {
      if (equals(file, files[[ii]], ...)) return(TRUE)
    }
    FALSE
  } # isDuplicated()

  files <- as.list(x)
  dups <- logical(length(files))
  if (length(dups) <= 1L) return(dups)

  if (!fromLast) files <- rev(files)

  for (ii in seq_along(files)) {
    file <- files[[1L]]
    files <- files[-1L]
    isDup <- isDuplicated(file, files, ...)
    dups[[ii]] <- isDup
    if (any) break
  }

  if (!fromLast) dups <- rev(dups)
  dups
}, protected=TRUE) # findDuplicated()


setMethodS3("duplicated", "GenericDataFileSet", function(x, ...) {
  findDuplicated(x, ...)
})

setMethodS3("anyDuplicated", "GenericDataFileSet", function(x, ...) {
  any(findDuplicated(x, ..., fromLast=TRUE, firstOnly=TRUE))
})

setMethodS3("unique", "GenericDataFileSet", function(x, ...) {
  dups <- duplicated(x, ...)
  # Drop duplicates?
  if (any(dups)) x <- x[!dups]
  x
})


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
setMethodS3("getDefaultFullName", "GenericDataFileSet", function(this, parent=getDepth(this, default=NULL), ...) {
  # Argument 'parent':
  if (!is.null(parent)) {
    parent <- Arguments$getInteger(parent, range=c(0,32))
  }

  # The name of a file set is inferred from the pathname of the directory
  # of the set assuming path/to/<fullname>/<something>/<subdir>/

  # Get the path of this file set
  path <- getPath(this)
  if (is.null(path) || is.na(path)) {
    return(NA_character_)
  }

  if (!is.null(parent)) {
    while (parent > 0L) {
      # path/to/<fullname>/<something>
      path <- dirname(path)
      parent <- parent - 1
    }
  }

  # <fullname>
  fullname <- basename(path)

  fullname
})


setMethodS3("updateFullName", "GenericDataFileSet", function(this, ...) {
  update2(this, ...)
}, protected=TRUE)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# FULLNAME*S* TRANSLATORS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("updateFullNames", "GenericDataFileSet", function(this, ...) {
  updateFullName(this, ...)
}, protected=TRUE)


setMethodS3("clearFullNamesTranslator", "GenericDataFileSet", function(this, ...) {
  files <- as.list(this, useNames=FALSE)
  lapply(files, FUN=clearFullNameTranslator, ...)
  invisible(this)
}, protected=TRUE)

setMethodS3("resetFullNames", "GenericDataFileSet", function(this, ...) {
  clearFullNamesTranslator(this, ...)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByNULL", "GenericDataFileSet", function(this, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorByNULL, NULL, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByfunction", "GenericDataFileSet", function(this, fcn, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorByfunction, fcn, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorBydata.frame", "GenericDataFileSet", function(this, fcn, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorBydata.frame, fcn, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByTabularTextFile", "GenericDataFileSet", function(this, fcn, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorByTabularTextFile, fcn, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorByTabularTextFileSet", "GenericDataFileSet", function(this, fcn, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorByTabularTextFileSet, fcn, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslatorBylist", "GenericDataFileSet", function(this, fcn, ...) {
  files <- as.list(this, useNames=FALSE)
  sapply(files, FUN=appendFullNameTranslatorBylist, fcn, ...)
  invisible(this)
}, protected=TRUE)


setMethodS3("appendFullNamesTranslator", "GenericDataFileSet", function(this, by, ...) {
  # Arguments 'by':
  classNames <- class(by)
  methodNames <- sprintf("appendFullNamesTranslatorBy%s", classNames)

  # Dispatch on the 'by' argument...
  keep <- sapply(methodNames, FUN=exists, mode="function")
  methodNames <- methodNames[keep]

  if (length(methodNames) > 0L) {
    methodName <- methodNames[1L]
    fcn <- get(methodName, mode="function")
    res <- fcn(this, by, ...)
  } else {
    # ...otherwise, apply the fullname translator to each file
    res <- sapply(this, FUN=appendFullNameTranslator, by, ...)
  }

  # Allow the object to update itself according to these new rules.
  updateFullNames(this)

  invisible(res)
}, protected=TRUE)


setMethodS3("setFullNamesTranslator", "GenericDataFileSet", function(this, ...) {
  clearFullNamesTranslator(this)
  appendFullNamesTranslator(this, ...)
}, protected=TRUE)
