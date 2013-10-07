###########################################################################/**
# @RdocDocumentation "Deprecated and defunct objects"
# @alias getColumnNameTranslator.GenericTabularFile
# @alias setColumnNameTranslator.GenericTabularFile
# @alias getLabel.GenericDataFile
# @alias setLabel.GenericDataFile
# @alias getAlias.GenericDataFile
# @alias setAlias.GenericDataFile
# @alias getAlias.GenericDataFileSet
# @alias setAlias.GenericDataFileSet
# @alias getFileListV0.GenericDataFileSetList
# @alias digest2
#
# \description{
#  The following objects are \emph{defunct}:
#  \itemize{
#   \item getAlias(), setAlias()
#   \item getLabel(), setLabel()
#   \item getColumnNameTranslator(), setColumnNameTranslator()
#   \item getFileListV0()
#  }
#
#  The following objects are \emph{deprecated}:
#  \itemize{
#   \item digest2()
#  }
# }
#
# @keyword internal
#*/###########################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DEFUNCT
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("getColumnNameTranslator", "GenericTabularFile", function(...) {
  .Defunct("getColumnNamesTranslator");
}, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("setColumnNameTranslator", "GenericTabularFile", function(...) {
  .Defunct("setColumnNamesTranslator");
}, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("getLabel", "GenericDataFile", function(this, ...) {
  .Defunct("getName");
}, private=TRUE, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("setLabel", "GenericDataFile", function(this, label, ...) {
  .Defunct("setName");
}, protected=TRUE, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("setAlias", "GenericDataFile", function(this, alias=NULL, ...) {
  .Defunct("setFullName");
}, protected=TRUE, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("getAlias", "GenericDataFileSet", function(this, ...) {
  .Defunct("getFullName");
}, protected=TRUE, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("setAlias", "GenericDataFileSet", function(this, alias=NULL, ...) {
  .Defunct("setFullName");
}, protected=TRUE, deprecated=TRUE, createGeneric=FALSE)

setMethodS3("getFileListV0", "GenericDataFileSetList", function(this, name, dropMissing=TRUE, ...) {
  .Defunct("getFileList");
}, protected=TRUE, deprecated=TRUE, createGeneric=FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# DEPRECATED
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
digest2 <- function(object, ..., skip="auto", ascii=FALSE) {
  .Deprecated("digest::digest");
  digest(object, ..., skip=skip, ascii=ascii);
} # digest2()


############################################################################
# HISTORY:
# 2013-10-05
# o CLEANUP: Now no generic functions are created for defunct methods.
# 2013-09-30
# o Deprecated digest2().
# o Defuncted (get|set)(Alias|Label)() [deprecated 2012-11-12].
# o Defuncted (get|set)ColumnNameTranslator() [deprecated 2012-11-01].
# o Removed fromFiles() [deprecated 2010-01-31 & defuncted 2012-10-16].
############################################################################
