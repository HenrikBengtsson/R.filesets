setConstructorS3("GenericTabularFile", function(..., .verify=TRUE, verbose=FALSE) {
  this <- extend(GenericDataFile(...), "GenericTabularFile");

  if (.verify)
    verify(this, ..., verbose=verbose);
  this;
})


setMethodS3("as.character", "GenericTabularFile", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- NextMethod("as.character", this, ...);
  class <- class(s);
  s <- c(s, sprintf("Number of data rows: %d", nbrOfRows(this, fast=TRUE)));

  class(s) <- class;
  s;
})



setMethodS3("verify", "GenericTabularFile", function(this, ..., verbose=FALSE) {
  # Nothing to do?
  if (is.null(getPathname(this)))
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



setMethodS3("getColumnNameTranslator", "GenericTabularFile", function(this, ...) {
  this$.columnNameTranslator;
})


setMethodS3("setColumnNameTranslator", "GenericTabularFile", function(this, fcn, ...) {
  # Arguments 'fcn':
  if (is.null(fcn)) {
  } else if (!is.function(fcn)) {
    throw("Argument 'fcn' is not a function: ", class(fcn)[1]);
  }

  this$.columnNameTranslator <- fcn;
})


setMethodS3("translateColumnNames", "GenericTabularFile", function(this, names, ...) {
  nameTranslator <- getColumnNameTranslator(this);	
  if (!is.null(nameTranslator)) {
    names2 <- nameTranslator(names);

    # Sanity check
    if (any(is.na(names2))) {
      throw("Failed to translate names. Some names were translated to NA:s ", 
            paste(head(names[is.na(names2)]), collapse=", "));
    }
    if (length(names2) != length(names)) {
      throw(sprintf("Failed to translate column names. The translator is erroneous, because it drops/adds some names (passed %d names but got %d names).", length(names), length(names2)));
    }
    names <- names2;

    if (identical(attr(names, "isFinal"), TRUE))
      return(names);
  }

  # Do nothing
  names;
}, protected=TRUE)



setMethodS3("getColumnNames", "GenericTabularFile", abstract=TRUE);

setMethodS3("readDataFrame", "GenericTabularFile", abstract=TRUE);

setMethodS3("readColumns", "GenericTabularFile", abstract=TRUE);

setMethodS3("nbrOfRows", "GenericTabularFile", abstract=TRUE);

setMethodS3("nbrOfColumns", "GenericTabularFile", function(this, ...) {
  length(getColumnNames(this));
})

setMethodS3("dim", "GenericTabularFile", function(x) {
  # To please R CMD check.
  this <- x;

  c(nbrOfRows(this), nbrOfColumns(this));
}, appendVarArgs=FALSE)



setMethodS3("extractMatrix", "GenericTabularFile", function(this, column=1, drop=FALSE, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  nbrOfColumns <- nbrOfColumns(this);

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
  data <- data[,1];

  verbose && cat(verbose, "Raw data frame read:");
  verbose && str(verbose, data);

  # Coerce into a matrix?
  if (!drop) {
    data <- as.matrix(data);
  } else {
    verbose && cat(verbose, "Dropping singleton dimensions");
  }

  verbose && cat(verbose, "Result:");
  verbose && str(verbose, data);

  verbose && exit(verbose);

  data;
})
 

############################################################################
# HISTORY:
# 2009-10-30
# o ROBUSTIFICATION: Now translateColumnNames() of GenericTabularFile throws
#   an exception if some fullnames were translated into NA.
# 2008-05-12
# o Added extractMatrix().
# o BUG FIX: getReadArguments() did not infer column classes if there was
#   no header to read but the column names was manually set.
# o BUG FIX: readDataFrame() did not read the first data row if there was
#   no column header; it was eaten up by a preceeding readHeader().
# 2008-04-29
# o Added readLines(), nbrOfLines(), nbrOfRows() and dim().
# o Now readDataFrame() keeps the row names if arguments rows != NULL.
# 2008-04-25
# o Now argument 'verbose' of the constructor is passed to verfity().
# 2008-04-24
# o Added argument 'rows' to readDataFrame() for GenericTabularFile.
# 2008-04-14
# o Renamed readData() to readDataFrame() for GenericTabularFile.
# 2008-03-22
# o Added {get|set}ColumnNameTranslator().
# 2008-03-18
# o Now any '...' arguments to getReadArguments() override the inferred 
#   read arguments, e.g. na.strings="NA".
# 2008-02-27
# o Since 'affy' defines standardGeneric("colnames") and because S3 methods
#   are not found by such S4 generic functions, we avoid that method name,
#   and instead use getColumnNames().
# 2007-09-16
# o Removed all 'camelCaseNames' arguments.  Now column names are decided 
#   by getColumnNames() and translateColumnNames(), which can be overridden.
# 2007-09-14
# o Extracted from AffymetrixTabularFile.
# 2007-09-10
# o Created from AffymetrixCsvGenomeInformation.R.
############################################################################
