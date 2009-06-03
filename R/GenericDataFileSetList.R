setConstructorS3("GenericDataFileSetList", function(dsList=list(), ...) {
  # Argument 'dsList':
  if (is.list(dsList)) {
    className <- "GenericDataFileSet";
    for (kk in seq(along=dsList)) {
      ds <- dsList[[kk]];
      if (!inherits(ds, className)) {
        throw(sprintf("Element #%d of argument 'dsList' is not of class %s: ", kk, class(ds)[1]));
      }
    }
  } else {
    throw("Argument 'dsList' is not a list: ", dsList);
  }

  extend(dsList, "GenericDataFileSetList");
})


setMethodS3("getFileClass", "GenericDataFileSetList", function(this, ...) {
  className <- class(this)[1];
  className <- gsub("FileSet", "Set", className);
  className <- gsub("SetList$", "", className);
  className <- sprintf("%sFileList", className);

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



setMethodS3("getFileList", "GenericDataFileSetList", function(this, name, dropMissing=TRUE, ...) {
  # Argument 'name':
  name <- Arguments$getCharacter(name);

  dsList <- this;

  dfList <- list();
  names <- character(0);
  for (kk in seq(along=this)) {
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
  className <- getFileClass(this);
  clazz <- Class$forName(className);
  dfList <- newInstance(clazz, dfList);

  dfList;
})


###########################################################################
# HISTORY:
# 2009-05-12
# o Created.
###########################################################################
