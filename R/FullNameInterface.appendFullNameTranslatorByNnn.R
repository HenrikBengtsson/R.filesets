setMethodS3("appendFullNameTranslatorBycharacter", "FullNameInterface", function(this, fullname, ...) {
  # Append a translator function that always returns a constant string
  appendFullNameTranslator(this, function(...) { fullname });
}, protected=TRUE)


setMethodS3("appendFullNameTranslatorByfunction", "FullNameInterface", function(this, fcn, ...) {
  # Arguments 'fcn':
  if (!is.function(fcn)) {
    throw("Argument 'fcn' is not a function: ", class(fcn)[1]);
  }

  # Sanity check
  names <- c("foo bar");
  names <- fcn(names, file=this);

  fnList <- getListOfFullNameTranslators(this);
  fnList <- c(fnList, fcn);
  setListOfFullNameTranslators(this, fnList);
}, protected=TRUE)


setMethodS3("appendFullNameTranslatorBydata.frame", "FullNameInterface", function(this, df, ...) {
  # Arguments 'df':
  if (!is.data.frame(df)) {
    throw("Argument 'df' is not a data.frame: ", class(df)[1]);
  }

  reqColNames <- c("pattern", "replacement");
  colnames <- colnames(df);
  if (is.null(colnames) && ncol(df) == 2) {
    colnames <- reqColNames;
    colnames(df) <- colnames;
  } else {
    res <- all(is.element(reqColNames, colnames));
    if (!res) {
      msg <- sprintf("The specified data frame does not have all of the required columns (%s): %s", paste(reqColNames, collapse=", "), paste(colnames, collapse=", "));
      throw(msg);
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Build function
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Generate regular expression
  patterns <- df[,"pattern"];
  replacements <- df[,"replacement"];
  nbrOfRules <- length(patterns);

  # Generate translator function
  fcn <- function(names, ...) {
    # For each rule
    for (kk in seq(length=nbrOfRules)) {
      pattern <- patterns[kk];
      idxs <- grep(pattern, names, fixed=FALSE);
      # No matches?
      if (length(idxs) == 0)
        next;

      # Translate
      replacement <- replacements[kk];
      names[idxs] <- gsub(pattern, replacement, names[idxs], fixed=FALSE);
    } # for (kk ...)

    names;
  } # fcn()

  appendFullNameTranslator(this, fcn);
}, protected=TRUE)


setMethodS3("appendFullNameTranslatorByTabularTextFile", "FullNameInterface", function(this, df, ...) {
  # Arguments 'df':
  if (!inherits(df, "TabularTextFile")) {
    throw("Argument 'df' is not a TabularTextFile: ", class(df)[1]);
  }

  df <- readDataFrame(df);

  appendFullNameTranslator(this, df, ...);
})



############################################################################
# HISTORY:
# 2010-05-25
# o Added appendFullNameTranslatorBy...() method for data frames and
#   TabularTextFile:s.
# o Moved to its own file.
############################################################################
