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

 

############################################################################
# HISTORY:
# 2012-10-16
# o Created 999.DEPRECATED.R.
# 2011-02-18
# o DEPRECATION: Added a warning message reporting that fromFiles() of
#   GenericDataFileSet has been deprecated, if still called by someone.
# 2010-01-31
# o Deprecated static fromFiles() of GenericDataFileSet.  Use byPath() 
#   instead.
############################################################################
