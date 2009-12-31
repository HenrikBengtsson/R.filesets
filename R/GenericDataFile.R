###########################################################################/**
# @RdocClass GenericDataFile
#
# @title "The abstract GenericDataFile class"
#
# \description{
#  @classhierarchy
#
#  A GenericDataFile is an object refering to a data file on a file system.
#  Note that this class is abstract and can not be instanciated, but
#  instead you have to use one of the subclasses or the generic 
#  @seemethod "fromFile" method.
# }
# 
# @synopsis
#
# \arguments{
#   \item{filename}{The filename of the file.}
#   \item{path}{An optional path to the file.}
#   \item{mustExist}{If @TRUE, an exception is thrown if the file does
#     not exists, otherwise not.}
#   \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
# 
# \section{Filename convention}{
#   The filename of an \code{GenericDataFile} is structured as follows:
#   \itemize{
#    \item{filename}{\code{"sample001,a,b,c.CEL"} 
#       (this follows the \R convention (but not the Unix convention)}
#    \item{fullname}{\code{"sample001,a,b,c"}}
#    \item{name}{\code{"sample001"}}
#    \item{tags}{\code{c("a", "b", "c")}}
#    \item{extension}{\code{"CEL"}}
#   }
# }
#
# @author
#
# \seealso{
#   An object of this class is typically part of an @see "GenericDataFileSet".
# }
#*/###########################################################################
setConstructorS3("GenericDataFile", function(filename=NULL, path=NULL, mustExist=TRUE, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (!is.null(filename)) {
    pathname <- Arguments$getReadablePathname(filename, path=path, mustExist=mustExist);
    if (!is.na(pathname) && !isFile(pathname)) {
      if (isDirectory(pathname)) {
        throw("The specified pathname is a directory: ", pathname);
      }
      throw("The specified pathname is not a file: ", pathname);
    }
  } else {
    pathname <- NULL;
  }

  # Arguments '...':
  args <- list(...);

  # Ignore any argument called 'recursive'
  keep <- whichVector(regexpr("^recursive$", names(args)) == -1);
  args <- args[keep];

  if (length(args) > 0) {
    argsStr <- paste(names(args), collapse=", ");
    throw("Unknown arguments: ", argsStr);
  }

  extend(Object(), c("GenericDataFile", uses("FullNameInterface")),
    .alias = NULL,
    .pathname = pathname,
    .attributes = list()
  )
}, abstract=TRUE)



setMethodS3("getLabel", "GenericDataFile", function(this, ...) {
  label <- this$label;
  if (is.null(label))
    label <- getName(this, ...);
  label;
}, private=TRUE)

setMethodS3("setLabel", "GenericDataFile", function(this, label, ...) {
  this$label <- label;
  invisible(this);
}, private=TRUE) 


setMethodS3("clone", "GenericDataFile", function(this, clear=TRUE, ...) {
  object <- NextMethod("clone", this, ...);
  if (clear)
    clearCache(object);
  object;
}, private=TRUE)


setMethodS3("equals", "GenericDataFile", function(this, other, ...) {
  # Default values
  notEqual <- FALSE;
  attr(notEqual, "thisFile") <- getPathname(this);
  attr(notEqual, "otherFile") <- getPathname(other);
  msg <- NULL;

  if (!inherits(other, "GenericDataFile")) {
    msg <- sprintf("The 'other' is not a GenericDataFile: %s",
                                                 class(other)[1]);
    attr(notEqual, "reason") <- msg;
    return(notEqual);
  }

  if (identical(getPathname(this), getPathname(other)))
    return(TRUE);

  value <- getFileSize(this);
  valueOther <- getFileSize(other);
  if (!identical(value, valueOther)) {
    msg <- sprintf("The file sizes differ: %.0f != %.0f",
                                          value, valueOther);
    attr(notEqual, "reason") <- msg;
    return(notEqual);
  }

  value <- getChecksum(this);
  valueOther <- getChecksum(other);
  if (identical(value, valueOther)) {
    msg <- sprintf("The checksums differ: %d != %d",
                                          value, valueOther);
    attr(notEqual, "reason") <- msg;
    return(notEqual);
  }

  TRUE;
})



###########################################################################/**
# @RdocMethod as.character
#
# @title "Returns a short string describing the file"
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
setMethodS3("as.character", "GenericDataFile", function(x, ...) {
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

  # Pathname
  pathname <- getPathname(this);
  pathnameR <- getRelativePath(pathname);
  if (nchar(pathnameR) < nchar(pathname)) {
    pathname <- pathnameR;
  }
  s <- c(s, sprintf("Pathname: %s", pathname));

  # File size
  fileSize <- getFileSize(this, "units");
  if (!is.na(fileSize)) {
    fileSizeB <- sprintf("%.0f bytes", getFileSize(this, "numeric"));
    if (fileSizeB != fileSize) {
      fileSize <- sprintf("%s (%s)", fileSize, fileSizeB);
    }
  }
  s <- c(s, sprintf("File size: %s", fileSize));

  s <- c(s, sprintf("RAM: %.2f MB", objectSize(this)/1024^2));

  class(s) <- "GenericSummary";
  s;
}, private=TRUE)




###########################################################################/**
# @RdocMethod getPathname
#
# @title "Gets the pathname of the file"
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
setMethodS3("getPathname", "GenericDataFile", function(this, ...) {
  this$.pathname;
})




###########################################################################/**
# @RdocMethod getPath
#
# @title "Gets the path (directory) of the file"
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
setMethodS3("getPath", "GenericDataFile", function(this, ...) {
  dirname(this$.pathname);
})




###########################################################################/**
# @RdocMethod getFilename
#
# @title "Gets the filename of the file"
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
# \details{
#  The filename of a file is the pathname excluding any path.
#  For instance, the filename of \code{path/to/foo,a.2,b.ext} is 
#  \code{foo,a.2,b.ext}.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getFilename", "GenericDataFile", function(this, ...) {
  basename(this$.pathname);
})



###########################################################################/**
# @RdocMethod getDefaultFullName
#
# @title "Gets the full name of the file"
#
# \description{
#   @get "title", that is the filename without the filename extension.
# }
#
# @synopsis
#
# \arguments{
#  \item{aliased}{If @TRUE, and an alias has been set, the alias is 
#     returned, otherwise the default full name is returned.}
#  \item{translate}{If @TRUE, an a fullname translator is set, the fullname
#     is translated before returned.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# \details{
#  The full name of a file is the filename excluding any
#  extension (and period).
#  For instance, the full name of \code{path/to/foo,a.2,b.ext} is 
#  \code{foo,a.2,b}.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getDefaultFullName", "GenericDataFile", function(this, aliased=FALSE, ...) {
  if (aliased) {
    alias <- getAlias(this);
    if (!is.null(alias))
      fullname <- alias;
  } else {
    filename <- getFilename(this);
    if (is.null(filename))
      return("");
    pattern <- getExtensionPattern(this);
    fullname <- gsub(pattern, "", filename);
  }

  fullname;
}, protected=TRUE)


setMethodS3("getOutputExtension", "GenericDataFile", function(...) {
  getFilenameExtension(...);  
}, protected=TRUE);

setMethodS3("getFilenameExtension", "GenericDataFile", abstract=TRUE, protected=TRUE);


setMethodS3("getExtensionPattern", "GenericDataFile", function(this, ..., force=FALSE) {
  pattern <- this$.extensionPattern;
  if (force || is.null(pattern)) {
    # Default pattern is anything following the last period
    pattern <- "\\.([^.]+)$";
##   pattern <- toAsciiRegExprPattern(pattern); # Don't handle [.] and ~
    this$.extensionPattern <- pattern;
  }
  pattern;
}, static=TRUE)


setMethodS3("setExtensionPattern", "GenericDataFile", function(this, pattern, ...) {
  pattern <- Arguments$getRegularExpression(pattern);
  this$.extensionPattern <- pattern;
  invisible(this);
})



setMethodS3("getExtension", "GenericDataFile", function(this, ...) {
  filename <- getFilename(this, ...);
  fullname <- getDefaultFullName(this, ...);
  # Drop <fullname> and a possible '.'.
  substring(filename, first=nchar(fullname)+2);
})



setMethodS3("getAlias", "GenericDataFile", function(this, ...) {
  this$.alias;
})

setMethodS3("setAlias", "GenericDataFile", function(this, alias=NULL, ...) {
  if (!is.null(alias)) {
    alias <- Arguments$getFilename(alias);
  }
  
  this$.alias <- alias;
  invisible(this);
})




###########################################################################/**
# @RdocMethod getFileType
#
# @title "Gets the file type of a file"
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
#   Returns a @character in lower case letters.
# }
#
# \details{
#   By default, this methods returns the filename extension, but subclasses
#   may override this.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getFileType", "GenericDataFile", function(this, ...) {
  pattern <- "(.*)[.]([abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0-9]+)$";
  ext <- gsub(pattern, "\\2", this$.pathname);
  tolower(ext);
})


setMethodS3("isFile", "GenericDataFile", function(this, ...) {
  isFile(getPathname(this));
})


setMethodS3("getFileSize", "GenericDataFile", function(this, what=c("numeric", "units"), sep="", ...) {
  # Argument 'what':
  what <- match.arg(what);

  fileSize <-   file.info(this$.pathname)$size;
  if (what == "numeric")
    return(fileSize);

  if (is.na(fileSize))
    return(fileSize);
    
  units <- c("bytes", "kB", "MB", "GB", "TB");
  scale <- 1;
  for (kk in seq(along=units)) {
    unit <- units[kk];
    if (fileSize < 1000)
      break;
    fileSize <- fileSize/1024;
  }
  fileSize <- sprintf("%.2f %s%s", fileSize, sep, unit);
  fileSize <- gsub(".00 bytes", " bytes", fileSize, fixed=TRUE);

  fileSize;
})


setMethodS3("getCreatedOn", "GenericDataFile", function(this, ...) {
  file.info(this$.pathname)[["ctime"]];
}, protected=TRUE)


setMethodS3("getLastModifiedOn", "GenericDataFile", function(this, ...) {
  file.info(this$.pathname)[["mtime"]];
}, protected=TRUE)


setMethodS3("getLastAccessedOn", "GenericDataFile", function(this, ...) {
  file.info(this$.pathname)[["atime"]];
}, protected=TRUE)


setMethodS3("hasBeenModified", "GenericDataFile", function(this, ..., unknown=TRUE) {
  lastModifiedOn <- getLastModifiedOn(this);
  prevModifiedOn <- this$.prevModifiedOn;

  # Unknown modification timestamp on file?
  if (is.na(lastModifiedOn) || lastModifiedOn == 0) {
    res <- unknown;
    attr(res, "lastModifiedOn") <- lastModifiedOn;
    attr(res, "prevModifiedOn") <- prevModifiedOn;
    return(res);
  }

  if (is.null(prevModifiedOn)) {
    res <- unknown;
    attr(res, "lastModifiedOn") <- lastModifiedOn;
    attr(res, "prevModifiedOn") <- prevModifiedOn;
  } else {
    res <- (lastModifiedOn > prevModifiedOn);
    attr(res, "lastModifiedOn") <- lastModifiedOn;
    attr(res, "prevModifiedOn") <- prevModifiedOn;
  }

  this$.prevModifiedOn <- lastModifiedOn;

  res;
}, protected=TRUE)


setMethodS3("fromFile", "GenericDataFile", function(static, filename, path=NULL, ..., recursive=TRUE, verbose=FALSE, .checkArgs=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  if (.checkArgs) {
    # Argument 'recursive':
    recursive <- Arguments$getLogical(recursive);

    # Argument 'filename' and 'path':
    pathname <- Arguments$getReadablePathname(filename, path=path, mustExist=TRUE);
  } else {
    pathname <- filename;
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Use subclasses to setup the file?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (recursive) {
    # Get all known subclasses (bottom up)
    clazz <- Class$forName(class(static)[1]);
    knownSubclasses <- getKnownSubclasses(clazz);
    knownSubclasses <- rev(knownSubclasses);
    for (className in knownSubclasses) {
      clazz <- Class$forName(className);
  
      # Try reading the file using the static fromFile() method of each class
      static <- getStaticInstance(clazz);
      tryCatch({
        res <- fromFile(static, filename=pathname, .checkArgs=FALSE);
        return(res);
      }, error = function(ex) {})
    }
  }

  # If not "read" above, just create an instance as is.
  res <- newInstance(static, filename=pathname, ...);

  res;
}, static=TRUE)



setMethodS3("copyTo", "GenericDataFile", function(this, filename=getFilename(this), path=NULL, overwrite=FALSE, ..., verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'filename' and 'path':
  pathname <- Arguments$getWritablePathname(filename, path=path);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }
 
  # Assert that we're not trying to copy to itself
  if (identical(pathname, getPathname(this)))
    throw("Cannot copy file. Source and destination are identical: ", pathname);

  # Assert that file is not overwritten by mistake.
  pathname <- Arguments$getWritablePathname(pathname, mustNotExist=!overwrite);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Fail-safe copying
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  verbose && enter(verbose, "Copying file");
  copyFile(getPathname(this), pathname, overwrite=overwrite, verbose=less(verbose, 10));
  verbose && exit(verbose);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Create object of the same class.
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  res <- newInstance(this, pathname);

  res;
}, protected=TRUE)



setMethodS3("renameTo", "GenericDataFile", function(this, filename=getFilename(this), path=NULL, ..., verbose=TRUE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'filename' and 'path':
  pathname <- Arguments$getWritablePathname(filename, path=path);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }
 
  # Assert that the file exists
  if (!isFile(this)) {
    throw("Cannot rename file. Source file does not exist: NA");
  }   

  # Nothing to do?
  if (identical(pathname, getPathname(this)))
    return(this);

  # Assert that file is not overwritten by mistake.
  pathname <- Arguments$getWritablePathname(pathname, mustNotExist=TRUE);

  srcPathname <- getPathname(this);

  verbose && enter(verbose, "Renaming ", class(this)[1], " pathname");
  verbose && cat(verbose, "Source: ", srcPathname);
  verbose && cat(verbose, "Destination: ", pathname);

  verbose && enter(verbose, "Renaming file");
  res <- file.rename(srcPathname, pathname);
  if (!res) {
    throw("Failed to rename file: ", srcPathname, " -> ", pathname);
  }
  verbose && exit(verbose);

  # Update GenericDataFile object
  this$.pathname <- pathname;

  verbose && exit(verbose);

  invisible(this);
}, protected=TRUE)



setMethodS3("getChecksum", "GenericDataFile", function(this, ..., force=FALSE, verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  checksum <- this$.checksum;
  if (force || is.null(checksum) || hasBeenModified(this)) {
    if (isFile(this)) {
      verbose && enter(verbose, "Calculating checksum");
      pathname <- getPathname(this);
      checksum <- digest2(pathname, file=TRUE);
      verbose && exit(verbose);
    } else {
      naValue <- as.character(NA);
      checksum <- naValue;
    }
    this$.checksum <- checksum;
  }

  checksum;
})


setMethodS3("writeChecksum", "GenericDataFile", function(this, ..., skip=FALSE, verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  if (!isFile(this)) {
    throw("Cannot write checksum to file. File does not exist: NA");
  }   


  verbose && enter(verbose, "Writing checksum");

  pathname <- getPathname(this);
  outPathname <- sprintf("%s.md5", pathname);

  verbose && cat(verbose, "Pathname: ", outPathname);

  # Skip existing checksum file?
  if (skip && isFile(outPathname)) {
    verbose && cat(verbose, "Found existing checksum file");
    verbose && enter(verbose, "Reading existing checksum file");
    checksum <- readChecksum(this, verbose=less(verbose));
    verbose && cat(verbose, "Checksum (read): ", checksum);
    verbose && exit(verbose);
  } else {
    verbose && enter(verbose, "Getting checksum");
    checksum <- getChecksum(this, verbose=less(verbose));
    verbose && cat(verbose, "Checksum (generated): ", checksum);
    cat(checksum, file=outPathname);
    verbose && exit(verbose);
  }

  verbose && exit(verbose);

  invisible(outPathname);
})



setMethodS3("readChecksum", "GenericDataFile", function(this, ..., verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  if (!isFile(this)) {
    throw("Cannot read stored checksum. File does not exist: NA");
  }   

  verbose && enter(verbose, "Reading checksum");
  pathname <- getPathname(this);
  outPathname <- sprintf("%s.md5", pathname);
  verbose && cat(verbose, "Pathname: ", outPathname);
  outPathname <- Arguments$getReadablePathname(outPathname, mustExist=TRUE);

  checksum <- readLines(outPathname, warn=FALSE);

#  verbose && enter(verbose, "Trimming");
  # Trim all lines
  checksum <- trim(checksum);
  # Drop empty lines
  checksum <- checksum[nchar(checksum) > 0];
  # Drop comments
  checksum <- checksum[regexpr("^#", checksum) == -1];
#  verbose && exit(verbose);

  verbose && enter(verbose, "Validating checksum");
  if (length(checksum) == 0)
    throw("File format error. No checksum found: ", outPathname);
  if (length(checksum) > 1)
    throw("File format error. Too many possible checksums: ", outPathname);

  # A checksum should only consist of hexadecimal characters
  if (regexpr("^[0-9abcdefABCDEF]+$", checksum) == -1) {
    throw("File format error. Invalid checksum ('", checksum, "'): ", outPathname);
  }
  verbose && exit(verbose);

  verbose && exit(verbose);

  checksum;
})


setMethodS3("compareChecksum", "GenericDataFile", function(this, ..., verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  if (!isFile(this)) {
    throw("Cannot compare checksum. File does not exist: NA");
  }   

  pathname <- getPathname(this);
  outPathname <- sprintf("%s.md5", pathname);

  verbose && enter(verbose, "Comparing checksum");
  verbose && cat(verbose, "Pathname: ", outPathname);

  checksum <- getChecksum(this, verbose=less(verbose));
  if (isFile(outPathname)) {
    checksum2 <- readLines(outPathname, warn=FALSE);
  } else {
    naValue <- as.character(NA);
    checksum2 <- naValue;
  }
  res <- identical(checksum, checksum2);

  verbose && cat(verbose, res);
  verbose && exit(verbose);

  res;
})


setMethodS3("validateChecksum", "GenericDataFile", function(this, ..., verbose=FALSE) {
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  if (!isFile(this)) {
    throw("Cannot validate checksum. File does not exist: NA");
  }   

  verbose && enter(verbose, "Validating checksum");
  pathname <- getPathname(this);
  res <- compareChecksum(this, ..., verbose=less(verbose));
  if (!res) {
    throw("The calculated checksum and the checksum store on file do not match: ", pathname);
  }
  verbose && exit(verbose);

  invisible(res);
})


setMethodS3("renameToUpperCaseExt", "GenericDataFile", function(static, pathname, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # isFileCase() is a case-sensitive isFile() for Windows
  isFileCase <- function(pathname, ...) {
    # Non-case sensitive check
    if (!isFile(pathname))
      return(FALSE);

    # There can still be a case-difference
    path <- dirname(pathname);
    filename <- basename(pathname);
    filenames <- list.files(path=path, all.files=TRUE);
    res <- grep(filename, filenames, fixed=TRUE);
    res <- (length(res) >= 1);
    res;
  }


  # Identify the filename extension
  ext <- gsub("^.*[.]", "", pathname);

  # No filename extension?  Do nothing.
  if (identical(ext, pathname))
    return(pathname);

  # Generate the pathname with lower- and upper-case extensions
  extL <- tolower(ext);
  pathnameL <- gsub(sprintf("[.]%s$", ext), sprintf(".%s", extL), pathname);

  extU <- toupper(ext);
  pathnameU <- gsub(sprintf("[.]%s$", ext), sprintf(".%s", extU), pathname);

  # Does a lower-case filename exist? If not, nothing to do.
  if (!isFileCase(pathnameL))
    return(pathnameU);

  # Can we rename it?
  if (identical(pathname, pathnameL) && isFileCase(pathnameU)) {
    throw("Cannot rename pathname to have upper-case filename extension, because such a file already exists: ", pathnameU);
  }

  # Try to rename the file
  res <- file.rename(pathnameL, pathnameU);
  if (res) {
    msg <- paste("Renamed file to have an upper-case filename extension:", pathname);
    warning(msg);
  } else {
    throw("Failed to rename file such that it gets an upper-case filename extension (try to rename the file manually): ", pathname);
  }

  pathnameU;
}, static=TRUE, protected=TRUE)



setMethodS3("gzip", "GenericDataFile", function(this, ...) {
  if (!isFile(this)) {
    throw("Cannot gzip file. File does not exist: NA");
  }   

  pathname <- getPathname(this);
  if (regexpr("[.]gz$", pathname) != -1)
    throw("File is already gzip'ed: ", pathname);
  outPathname <- sprintf("%s.gz", pathname);
  gzip(pathname, destname=outPathname, ...);
  this$.pathname <- outPathname;
  invisible(pathname);
}, protected=TRUE)


setMethodS3("gunzip", "GenericDataFile", function(this, ...) {
  if (!isFile(this)) {
    throw("Cannot gunzip file. File does not exist: NA");
  }

  pathname <- getPathname(this);
  if (regexpr("[.]gz$", pathname) == -1)
    throw("File is not gzip'ed: ", pathname);
  outPathname <- gsub("[.]gz$", "", pathname);
  gunzip(pathname, destname=outPathname, ...);
  this$.pathname <- outPathname;
  invisible(pathname);
}, protected=TRUE)



############################################################################
# HISTORY:
# 2009-12-30
# o BUG FIX: Now GenericDataFile(pathname) throws an error if 'pathname'
#   is refering to a directory.
# o Now GenericDataFile(NA, mustExist=FALSE) is a valid object.  Made all
#   methods aware of such missing files.
# 2009-10-02
# o CLEAN UP: Removed setFullName() for GenericDataFile, because there
#   is not a "default" on.
# o Now setFullNameTranslator(...) for GenericDataFile dispatches on the
#   'translator' argument (2nd) to call setFullNameTranslatorBy<class>().
#   setFullNameTranslatorByFunction() and setFullNameTranslatorByNULL()
#   are defined by default.
# 2009-05-19
# o Now setFullNameTranslator() for GenericDataFile asserts that the 
#   fullname translator function accepts also argument 'file'.
# 2009-04-23
# o BUG FIX: as.character() would throw 'Error in sprintf("%d", getFileSize
#   (db, "numeric")) : use format %f, %e, %g or %a for numeric objects' if
#   file size is returned as a double, which happens for very large files.
# 2009-02-26
# o Now hasTags(..., tags) splits the 'tags' argument.
# 2009-02-23
# o (Re-)Added space between number and unit for RAM and file size.
# o Now as.character() of GenericDataFile also reports the exact file size
#   in case the file size is reported in kB, MB, etc.  It also tries to
#   report the relative pathname rather than the absolute.
# o Now getChecksum() of GenericDataFile caches results unless the file
#   has been modified since last time.
# o Added hasBeenModified() to GenericDataFile.
# 2008-09-18
# o Now readChecksum() does some validation.  It is also possible to have
#   commented rows in the checksum file.
# o Added argument 'skip' to writeChecksum().
# 2008-07-23
# o Added getCreatedOn(), getLastModifiedOn(), getLastAccessedOn().
# 2008-07-17
# o Added setFullName(), and setName().
# 2008-06-11
# o BUG FIX: Argument 'recursive' is ignored in the GenericDataFile
#   constructor.
# 2008-06-06
# o Added argument 'recursive' to fromFile().
# 2008-05-16
# o Added support for full name translation of GenericDataFile:s.
# 2008-05-15
# o Added gzip() and gunzip().
# o Update equals() to also compare classes, file sizes, and checksums.
# 2008-05-11
# o Now static fromFile() always creates an instance.
# 2008-05-09
# o Moved private get/seLabel() from AffymetrixFile to GenericDataFile.
# o Moved the attributes features from AffymetrixFile to GenericDataFile.
# 2008-03-22
# o Added 'aliased' to getFullName().
# 2007-09-25
# o Added isFile() to test if the file exists or not.
# 2007-09-15
# o Added renameTo().
# o Now copyTo() utilizes fileCopy(), which is fail safe.
# 2007-09-14
# o Extracted GenericDataFile from AffymetrixFile.
# 2007-09-13
# o Added missing setAttributesByTags().
# 2007-08-09
# o Added static renameToUpperCaseExt().
# 2007-03-20
# o Added getAlias() and setAlias().  Note, getName() etc are still
#   unaffected by these.
# 2007-03-05
# o Added setAttributesByTags(), which now also tries to coerce values.
# o Added support for (in-memory) attributes.
# 2007-02-07
# o Added getChecksum(), writeChecksum(), readChecksum(), and 
#   compareChecksum() and validateChecksum(). I did this because I noticed 
#   by chance that some of my CEL files transferred via an external HDD got
#   corrupt probe signals.
# 2007-01-14
# o Added a test for "unknown" (=unused) arguments to constructor.
# 2007-01-07
# o Added hasTags() and hasTag().
# 2006-11-02
# o Added getFullName(), getTags() and redefined getName().
# 2006-09-15
# o Added stextSize().
# 2006-08-27
# o Added stextLabel() and stextLabels(). stext is for "side text", cf. 
#   mtext for "margin text". stext() is slightly more convenient than mtext
#   when it comes to different font sizes.
# o Added copyTo().
# 2006-08-14
# o Added abstract fromFile().
# 2006-08-11
# o Created from AffymetrixDataFile in order to represent CDF files too.
############################################################################
