setConstructorS3("TabularTextFile", function(..., sep=c("\t", ","), quote="\"", fill=FALSE, skip=0, columnNames=TRUE, .verify=TRUE, verbose=FALSE) {
  # Argument 'columnNames':
  if (is.logical(columnNames)) {
    readColumnNames <- columnNames;
    columnNames <- NULL;
  } else if (is.character(columnNames)) {
    readColumnNames <- FALSE;
  } else {
    throw("Argument 'columnNames' must be either a logical or a character vector: ", class(columnNames)[1]);
  }

  this <- extend(GenericTabularFile(..., .verify=FALSE), "TabularTextFile",
    .fileHeader = NULL,
    .columnNameTranslator = NULL,
    sep = sep,
    quote = quote,
    fill = fill,
    skip = skip,
    .columnNames = columnNames,
    readColumnNames = readColumnNames
  );

  if (.verify)
    verify(this, ..., verbose=verbose);
  this;
})


setMethodS3("as.character", "TabularTextFile", function(x, ...) {
  # To please R CMD check
  this <- x;

  s <- NextMethod("as.character", this, ...);
  class <- class(s);
  if (hasColumnHeader(this)) {
    columns <- paste("'", getColumnNames(this), "'", sep="");
    s <- c(s, sprintf("Columns [%d]: %s", length(columns), paste(columns, collapse=", ")));
  } else {
    s <- c(s, sprintf("Columns [NA]: <not reading column names>"));
  }
  s <- c(s, sprintf("Number of text lines: %d", nbrOfLines(this, fast=TRUE)));

  class(s) <- class;
  s;
})



setMethodS3("verify", "TabularTextFile", function(this, ..., verbose=FALSE) {
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
    data <- readDataFrame(this, skip=this$skip, nrow=10, verbose=verbose);
  }, error = function(ex) {
    throw("File format error of the tabular file ('", getPathname(this), "'): ", ex$message);
  })

  verbose && exit(verbose);

  invisible(this);
}, private=TRUE)



setMethodS3("readColumnNames", "TabularTextFile", function(this, ...) {
  as.logical(this$readColumnNames);
})


setMethodS3("hasColumnHeader", "TabularTextFile", function(this, ...) {
  identical(this$readColumnNames, TRUE);
})


setMethodS3("getColumnNames", "TabularTextFile", function(this, ..., translate=TRUE) {
  # Argument 'translate':
  translate <- Arguments$getLogical(translate);

  if (hasColumnHeader(this)) {
    colnames <- getHeader(this, ...)$columns;
    if (translate) {
      colnames <- translateColumnNames(this, colnames);
    }
  } else {
    colnames <- this$.columnNames;
  }
  colnames;
})



setMethodS3("getHeader", "TabularTextFile", function(this, ..., force=FALSE) {
  hdr <- this$.fileHeader;
  if (force || is.null(hdr)) {
    hdr <- readRawHeader(this, ...);
    if (hasColumnHeader(this)) {
      hdr$columns <- hdr$topRows[[1]];
    }
    this$.fileHeader <- hdr;
  }
  hdr;
})




setMethodS3("readRawHeader", "TabularTextFile", function(this, con=NULL, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Reading tabular file header from ", class(this)[1]);

  # Open a file connection?
  if (is.null(con)) {
    pathname <- getPathname(this);
    verbose && cat(verbose, "Pathname: ", pathname);

    # Open file connection
    con <- file(pathname, open="r");
    on.exit({
      if (!is.null(con)) {
        close(con);
        con <- NULL;
      }
    })
  }


  ready <- FALSE;
  comments <- c();
  skip <- this$skip;
  while (!ready) {
    line <- readLines(con, n=1);
    isComments <- (regexpr("^#", line) != -1);
    if (!isComments) {
      if (skip == 0)
        break;
      skip <- skip - 1;
    }
    comments <- c(comments, line);
  }

  verbose && cat(verbose, "Header comments:", level=-20);
  verbose && str(verbose, comments, level=-20);

  # Infer column separator?
  sep <- this$sep;
  if (length(sep) > 1) {
    verbose && enter(verbose, "Identifying the separator that returns most columns");
    verbose && cat(verbose, "Separators:");
    verbose && str(verbose, sep);
    columns <- base::lapply(sep, FUN=function(split) {
      strsplit(line, split=split)[[1]];
    });
    nbrOfColumns <- sapply(columns, FUN=length);
    max <- which.max(nbrOfColumns);
    sep <- sep[max];
    verbose && printf(verbose, "Choosen separator: '%s' (0x%s)\n", sep, charToRaw(sep));
    verbose && exit(verbose);
  }

  lines <- c(line, readLines(con, n=9));
  verbose && print(verbose, line);
  topRows <- strsplit(lines, split=sep);
  topRows <- lapply(topRows, trim);
  verbose && print(verbose, topRows);

  # Remove quotes?
  quote <- this$quote;
  if (!is.null(quote)) {
    for (pattern in c(sprintf("^%s", quote), sprintf("%s$", quote))) {
      topRows <- lapply(topRows, FUN=function(row) {
        gsub(pattern, "", row);
      })
    }
  }

  verbose && cat(verbose, "Columns: ", paste(paste("'", topRows, "'", sep=""), collapse=", "), level=-10);

  hdr <- list(
    comments=comments,
    sep=sep,
    quote=quote,
    skip=this$skip,
    topRows=topRows
  );

  verbose && str(verbose, hdr);

  verbose && exit(verbose);

  hdr;
}, protected=TRUE); # readRawHeader()



setMethodS3("getReadArguments", "TabularTextFile", function(this, fileHeader=NULL, colClassPatterns=c("*"=NA), defColClass="NULL", ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'fileHeader':
  if (is.null(fileHeader)) {
    fileHeader <- getHeader(this);
  }

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }



  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Infer column classes
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Default column classes
  columns <- getColumnNames(this);
  if (!is.null(columns)) {
    nbrOfColumns <- length(columns);
    defColClasses <- rep(defColClass, nbrOfColumns);
    defColClassPatterns <- defColClasses;

    # Default columns?
    pos <- whichVector(names(colClassPatterns) == "*");
    if (length(pos) > 0) {
      # Exclude extra '*':s
      if (length(pos) > 1) {
        colClassPatterns <- colClassPatterns[-(pos[-1])];
        pos <- pos[1];
      }
  
      # Insert defaults
      colClass <- colClassPatterns[pos];
      names <- names(colClassPatterns);
      if (length(colClassPatterns) > 1) {
        names <- insert(names[-pos], at=pos, values=rep("*", nbrOfColumns));
        idxs <- whichVector(names == "*");
        names[idxs] <- sprintf("^%s$", columns);
  
        colClassPatterns <- insert(colClassPatterns[-pos], at=pos, 
                                   values=rep("*", nbrOfColumns));
        names(colClassPatterns) <- names;
        colClassPatterns[idxs] <- colClass;
      } else {
        colClassPatterns <- rep(colClass, nbrOfColumns);
        names(colClassPatterns) <- sprintf("^%s$", columns);
      }
    }

    verbose && cat(verbose, "Pattern used to identify column classes:", level=-20);
    verbose && print(verbose, colClassPatterns, level=-20);
  
    verbose && cat(verbose, "Generate column classes:");
    # Read everything by default
    colClasses <- defColClasses;
    names(colClasses) <- columns;
  
    # Update column classes according to patterns
    for (kk in seq(along=colClassPatterns)) {
      pattern <- names(colClassPatterns)[kk];
      idxs <- whichVector(regexpr(pattern, columns) != -1);
      if (length(idxs) > 0) {
        colClass <- colClassPatterns[kk];
        colClasses[idxs] <- colClass;
      }
    }
  } else {
    args <- list(...);
    colClasses <- args$colClasses;
  }
  
  verbose && cat(verbose, "Column classes:", level=-20);
  verbose && print(verbose, colClasses, level=-20);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Setup read.table() arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Inferred arguments
  args <- list(
    header      = hasColumnHeader(this),
    colClasses  = colClasses,
    skip        = fileHeader$skip,
    sep         = fileHeader$sep,
    quote       = fileHeader$quote,
    fill        = this$fill,
    check.names = FALSE,
    na.strings  = c("---")
  );

  # Overwrite with user specified arguments, if any
  userArgs <- list(...);
  for (key in names(userArgs)) {
    args[[key]] <- userArgs[[key]];
  }

  args;
}, protected=TRUE);



setMethodS3("readDataFrame", "TabularTextFile", function(this, con=NULL, rows=NULL, nrow=NULL, ..., verbose=FALSE) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'rows' and 'nrow':
  if (!is.null(rows) && !is.null(nrow)) {
    throw("Only one of arguments 'rows' and 'nrow' can be specified.");
  }

  if (!is.null(rows)) {
    rows <- Arguments$getIndices(rows);
    nrow <- max(rows);
  }

  if (!is.null(nrow)) {
    nrow <- Arguments$getInteger(nrow, range=c(1,Inf));
  }
  
  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose);
  if (verbose) {
    pushState(verbose);
    on.exit(popState(verbose));
  }

  verbose && enter(verbose, "Reading ", class(this)[1]);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Reading header to infer read.table() arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  hdr <- getHeader(this, verbose=less(verbose, 5));

  # Get read arguments
  args <- getReadArguments(this, fileHeader=hdr, nrow=nrow, ..., 
                                               verbose=less(verbose, 5));

  verbose && cat(verbose, "Arguments inferred from file header:");
  verbose && print(verbose, args);


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Identify names of columns read
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  columns <- getColumnNames(this);
  verbose && printf(verbose, "Column names (%d):\n", length(columns));
  verbose && cat(verbose, paste(columns, collapse=", "));

  if (!is.null(columns)) {
    verbose && enter(verbose, "Matching column names:");
    verbose && printf(verbose, "Column classes (%d):\n", 
                                                length(args$colClasses));
    verbose && cat(verbose, paste(args$colClasses, collapse=", "));
    columns[args$colClasses == "NULL"] <- NA;
    columns <- columns[!is.na(columns)];
    verbose && exit(verbose);
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Open a file connection?
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (is.null(con)) {
    pathname <- getPathname(this);
    verbose && cat(verbose, "Pathname: ", pathname);

    # Open file connection
    con <- file(pathname, open="r");
    on.exit({
      if (!is.null(con)) {
        close(con);
        con <- NULL;
      }
    })
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Reading data
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  fcnName <- "read.table";
#  fcnName <- "readTable";  ## BUGGY /HB 2008-06-17
  verbose && enter(verbose, sprintf("Calling %s()", fcnName));

  verbose && cat(verbose, "Arguments used to read tabular file:");
  args <- c(list(con), args);
  verbose && print(verbose, args);
  data <- do.call(fcnName, args=args);
  nbrOfRowsRead <- nrow(data);
  verbose && cat(verbose, "Raw data read by read.table():");
  verbose && cat(verbose, "Number of rows read: ", nbrOfRowsRead);
  verbose && str(verbose, data);

  # Extract subset of rows?
  if (fcnName == "read.table") {
    if (!is.null(rows)) {
      if (max(rows) > nbrOfRowsRead) {
        rows <- intersect(rows, 1:nbrOfRowsRead);
        warning("Argument 'rows' was out of range [1,", nbrOfRowsRead,
                                  "]. Ignored rows beyond this range.");
      }
      data <- data[rows,,drop=FALSE];
    } else {
      rownames(data) <- NULL;
    }
  } else {
    rownames(data) <- NULL;
  }

  # Sanity check
  if (length(columns) > 0) {
    if (ncol(data) != length(columns)) {
      throw("Number of read data columns does not match the number of column headers: ", ncol(data), " !=", length(columns));
    }
    colnames(data) <- columns;
  }

  verbose && str(verbose, data);
  verbose && exit(verbose);

  attr(data, "fileHeader") <- hdr;

  verbose && exit(verbose);

  data;
})


setMethodS3("[", "TabularTextFile", function(this, i=NULL, j=NULL, drop=FALSE) {
  # Read data
  data <- readDataFrame(this, rows=i, columns=j);

  # Drop dimensions?
  if (drop) {
    if (ncol(data) == 1) {
      data <- data[,1];
    } else if (nrow(data) == 1) {
      data <- data[1,];
    }
  }
  
  data;
})


setMethodS3("readColumns", "TabularTextFile", function(this, columns, colClasses=rep("character", length(columns)), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Arguments 'columns':
  if (is.numeric(columns)) {
    columnNames <- getColumnNames(this);
    columns <- Arguments$getIndices(columns, range=c(1, length(columnNames)));
    columnNames <- columnNames[columns];
  } else {
    columnNames <- Arguments$getCharacters(columnNames);
  }


  colClassPatterns <- colClasses;
  names(colClassPatterns) <- sprintf("^%s$", columnNames);
  readDataFrame(this, colClassPatterns=colClassPatterns, ...);
}, protected=TRUE)



# AD HOC fix to speed up ll(), which calls dimension() on each object, 
# which in turn calls dim() and dim() is really slow for this class,
# because it has to infer the number of rows by reading the complete
# file. The fix is to return NA for the number of rows if the file size
# is larger than 10MB unless nbrOfLines() has already been called.
# /HB 2008-07-22
setMethodS3("dimension", "TabularTextFile", function(this, ...) {
  c(nbrOfRows(this, fast=TRUE), nbrOfColumns(this, fast=TRUE));
}, private=TRUE);


setMethodS3("nbrOfRows", "TabularTextFile", function(this, fast=FALSE, ...) {
  hdr <- getHeader(this, ...);
  n <- nbrOfLines(this, fast=fast);
  n <- n - hdr$skip - as.integer(length(hdr$columns) > 0);
  n <- as.integer(n);
  n;
})


setMethodS3("nbrOfLines", "TabularTextFile", function(this, fast=FALSE, ...) {
  pathname <- getPathname(this);

  n <- this$.nbrOfLines;
  mtime <- getLastModifiedOn(this);
  if (is.null(n) || is.na(mtime) || !identical(mtime, attr(n, "mtime"))) {
    if (fast) {
      if (getFileSize(this) < 10e6)
        fast <- FALSE;
    }

    if (fast) {
      n <- as.integer(NA);
    } else {
      n <- countLines(pathname, ...);
      attr(n, "mtime") <- mtime;
      this$.nbrOfLines <- n;
    }
  }

  n;
})


setMethodS3("readLines", "TabularTextFile", function(con, ...) {
  # To please R CMD check
  this <- con;
  pathname <- getPathname(this);
  readLines(pathname, ...);
})




############################################################################
# HISTORY:
# 2009-10-06
# o Added subsetting via [() to TabularTextFile.
# 2009-05-09
# o Added argument 'translate' to getColumnNames() of TabularTextFile.
# 2008-07-23
# o Now nbrOfLines() cache the results and only recount if the file has
#   been modified since last time (or the file system does not provide
#   information on last modification time).  It also uses the new 
#   countLines() in R.utils.
# 2008-07-22
# o Added ad hoc dimension() to speed up ll(). It may return NA for the
#   number of rows.
# 2008-06-12
# o Added readRawHeader().  Removed readHeader() [now in getHeader()].
# o Added hasColumnHeader() to TabularTextFile.
# 2008-05-16
# o Took the text-file based parts of GenericTabularFile and placed them
#   in new subclass TabularTextFile.
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
# o Added argument 'rows' to readDataFrame() for TabularTextFile.
# 2008-04-14
# o Renamed readData() to readDataFrame() for TabularTextFile.
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
