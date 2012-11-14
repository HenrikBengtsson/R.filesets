# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# fromName() => byName()
#
# 2010-01-31
# o Deprecated static fromFiles() of GenericDataFileSet.  Use byPath() 
#   instead.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("fromFiles", "GenericDataFileSet", function(static, ...) {
  .Defunct("byName");
}, static=TRUE, protected=TRUE, deprecated=TRUE)


setMethodS3("getColumnNameTranslator", "GenericTabularFile", function(...) {
  .Deprecated("getColumnNamesTranslator");
  getColumnNamesTranslator(...);
}, deprecated=TRUE)

setMethodS3("setColumnNameTranslator", "GenericTabularFile", function(...) {
  .Deprecated("setColumnNamesTranslator");
  setColumnNamesTranslator(...);
}, deprecated=TRUE)


setMethodS3("getLabel", "GenericDataFile", function(this, ...) {
  .Deprecated("getLabel");
  label <- this$label;
  if (is.null(label))
    label <- getName(this, ...);
  label;
}, private=TRUE, deprecated=TRUE)

setMethodS3("setLabel", "GenericDataFile", function(this, label, ...) {
  .Deprecated("setLabel");
  this$label <- label;
  invisible(this);
}, private=TRUE, deprecated=TRUE)
 

############################################################################
# HISTORY:
# 2012-11-12
# o CLEANUP: Deprecated (get|set)Label() for GenericDataFile.
# 2012-11-01
# o CLEANUP: Deprecated (get|set)ColumnNameTranslator() in favor of
#   (get|set)ColumnNamesTranslator(); note the plural form.
# 2012-10-16
# o Created 999.DEPRECATED.R.
# 2011-02-18
# o DEPRECATION: Added a warning message reporting that fromFiles() of
#   GenericDataFileSet has been deprecated, if still called by someone.
# 2010-01-31
# o Deprecated static fromFiles() of GenericDataFileSet.  Use byPath() 
#   instead.
############################################################################
