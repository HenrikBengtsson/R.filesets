###########################################################################/**
# @RdocClass GenericTabularFileSet
#
# @title "The GenericTabularFileSet class"
#
# \description{
#  @classhierarchy
#
#  An GenericTabularFileSet object represents a set of
#  @see "GenericTabularFile"s.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @see "GenericDataFileSet".}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @author
#*/###########################################################################
setConstructorS3("GenericTabularFileSet", function(...) {
  extend(GenericDataFileSet(...), "GenericTabularFileSet");
})



setMethodS3("extractMatrix", "GenericTabularFileSet", function(this, ..., drop=FALSE) {
  args <- list(...)
  
  if ("files" %in% names(args)) {
    .Defunct("Argument 'files' of extractMatrix() for GenericTabularFileSet is deprecated. Use extractMatrix(ds[files], ...) instead.")
  }

  nbrOfFiles <- length(this)
  data <- NULL;
  for (kk in seq_len(nbrOfFiles)) {
    dataFile <- this[[kk]]
    argsKK <- c(list(dataFile), args)
    dataKK <- do.call(extractMatrix, args = argsKK)

    if (is.null(data)) {
      naValue <- vector(storage.mode(dataKK), length=1);
      data <- matrix(naValue, nrow=nrow(dataKK), ncol=nbrOfFiles);
      colnames(data) <- getNames(this)
    }

    data[,kk] <- dataKK;
    # Not needed anymore
    dataKK <- NULL;
  }

  # Drop singelton dimensions?
  if (drop) {
    data <- drop(data);
  }

  data;
})


############################################################################
# HISTORY:
# 2008-05-12
# o Created.
############################################################################
