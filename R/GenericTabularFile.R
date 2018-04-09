###########################################################################/**
# @RdocClass GenericTabularFile
#
# @title "The abstract GenericTabularFile class"
#
# \description{
#  @classhierarchy
#
#  A TabularTextFile is an object refering to a tabular text file
#  on a file system containing data in a tabular format.
#  Methods for reading all or a subset of the tabular data exist.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @see "GenericDataFile".}
#   \item{.verify, verbose}{(Internal only) If @TRUE, the file is
#      verified while the object is instantiated by the constructor.
#      The verbose argument is passed to the verifier function.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @author
#
# \seealso{
#   An object of this class is typically part of an
#   @see "GenericTabularFileSet".
# }
#*/###########################################################################
setConstructorS3("GenericTabularFile", function(..., .verify=TRUE, verbose=FALSE) {
  this <- extend(GenericDataFile(...), c("GenericTabularFile", uses("ColumnNamesInterface")));

  if (.verify) {
    verify(this, ..., verbose=verbose);
  }

  this;
}, abstract=TRUE)


setMethodS3("as.character", "GenericTabularFile", function(x, ...) {
  this <- x;
  s <- NextMethod("as.character");
  s <- c(s, sprintf("Number of data rows: %d", nbrOfRows(this, fast=TRUE)));
  s;
}, protected=TRUE)



setMethodS3("verify", "GenericTabularFile", function(this, ..., verbose=FALSE) {
  # Nothing to do?
  pathname <- getPathname(this);
  if (is.null(pathname) || is.na(pathname))
    return(invisible(this));


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "Validating file contents");

  tryCatch({
    data <- readDataFrame(this, rows=1:10, verbose=verbose);
  }, error = function(ex) {
    throw("File format error of the tabular file ('", getPathname(this), "'): ", ex$message);
  })

  verbose && exit(verbose);

  invisible(this);
}, private=TRUE)





###########################################################################/**
# @RdocMethod nbrOfRows
# @alias nbrOfColumns.GenericTabularFile
#
# @title "Gets the number of data rows"
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
#   Returns an @integer.
# }
# @author
#
# \seealso{
#   @seemethod "dim".
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("nbrOfRows", "GenericTabularFile", abstract=TRUE);



setMethodS3("nbrOfColumns", "GenericTabularFile", function(this, ...) {
  ncols <- NextMethod("nbrOfColumns");
  if (!is.na(ncols)) return(ncols);
  data <- readDataFrame(this, colClasses=NULL, rows=1L);
  ncol(data);
})



###########################################################################/**
# @RdocMethod dim
#
# @title "Gets the dimension of data table"
#
# \description{
#  @get "title", which is the number of rows and the number of columns.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \value{
#   Returns an @integer @vector of length two.
# }
# @author
#
# \seealso{
#   @seemethod "nbrOfRows".
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("dim", "GenericTabularFile", function(x) {
  # To please R CMD check.
  this <- x;

  c(nbrOfRows(this), nbrOfColumns(this));
}, appendVarArgs=FALSE)






###########################################################################/**
# @RdocMethod readDataFrame
#
# @title "Reads the tabular data as a data frame"
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
#   Returns a @data.frame.
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
setMethodS3("readDataFrame", "GenericTabularFile", abstract=TRUE);



###########################################################################/**
# @RdocMethod readColumns
#
# @title "Reads a subset of the columns as a data frame"
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
#   Returns a @data.frame.
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
setMethodS3("readColumns", "GenericTabularFile", abstract=TRUE);




###########################################################################/**
# @RdocMethod extractMatrix
#
# @title "Reads one of the columns"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{column}{An @integer specifying the column to read.}
#   \item{drop}{If @TRUE, a @vector is returned,
#     otherwise a one-column @matrix.}
#   \item{...}{Additional arguments passed to @seemethod "readColumns".}
#   \item{verbose}{A @logical or a @see "R.utils::Verbose" object.}
# }
#
# \value{
#   Returns a Jx1 @matrix, or if \code{drop=TRUE} a @vector of length J.
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
setMethodS3("extractMatrix", "GenericTabularFile", function(this, column=1L, drop=FALSE, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  nbrOfColumns <- nbrOfColumns(this);

  # Argument 'drop':
  drop <- Arguments$getLogical(drop);

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }


  verbose && enter(verbose, "Extracting data as a single-column matrix");

  # Read data as data frame
  data <- readColumns(this, columns=column, ..., verbose=less(verbose, 5));
  # Drop dimension
  data <- data[,1,drop=TRUE];

  verbose && cat(verbose, "Raw data frame read:");
  verbose && str(verbose, data);

  # Coerce into a matrix?
  if (!drop) {
    data <- as.matrix(data);
    colnames(data) <- getName(this);
  } else {
    verbose && cat(verbose, "Dropping singleton dimensions");
  }

  verbose && cat(verbose, "Result:");
  verbose && str(verbose, data);

  verbose && exit(verbose);

  data;
})


setMethodS3("[", "GenericTabularFile", function(this, i=NULL, j=NULL, drop=FALSE) {
  # Argument 'drop':
  drop <- Arguments$getLogical(drop);

  # Read data
  if (missing(j) || is.null(j)) {
    data <- readColumns(this, rows=i);
  } else {
    data <- readColumns(this, rows=i, columns=j);
  }

  # Drop dimensions?
  if (drop) {
    if (ncol(data) == 1L) {
      data <- data[,1L];
    } else if (nrow(data) == 1L) {
      data <- data[1L,];
    }
  }

  data;
}, protected=TRUE)


setMethodS3("head", "GenericTabularFile", function(x, n=6L, ...) {
  stopifnot(length(n) == 1L);
  nrow <- nrow(x);
  if (n < 0L) {
    n <- max(nrow + n, 0L);
  } else {
    n <- min(n, nrow);
  }
  rows <- seq_len(n);
  x[rows,, drop=FALSE];
})


setMethodS3("tail", "GenericTabularFile", function(x, n=6L, ...) {
  stopifnot(length(n) == 1L);
  nrow <- nrow(x);
  if (n < 0L) {
    n <- max(nrow + n, 0L);
  } else {
    n <- min(n, nrow);
  }
  rows <- seq.int(to=nrow, length.out=n);
  x[rows,, drop=FALSE];
})
