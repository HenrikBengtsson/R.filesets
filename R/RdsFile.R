###########################################################################/**
# @RdocClass RdsFile
# @alias loadObject.RdsFile
# @alias loadObject
#
# @title "The RdsFile class"
#
# \description{
#  @classhierarchy
#
#  An RdsFile represents a binary file containing an R object
#  saved using the @see "base::saveRDS" function.
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @see "GenericDataFile".}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# @author
#
# \seealso{
#   An object of this class is typically part of an @see "RdsFileSet".
# }
#*/###########################################################################
setConstructorS3("RdsFile", function(...) {
  extend(GenericDataFile(...), "RdsFile");
})

setMethodS3("loadObject", "RdsFile", function(this, ...) {
  pathname <- getPathname(this);
  readRDS(pathname, ...);
})


############################################################################
# HISTORY:
# 2013-11-20
# o Created.
############################################################################
