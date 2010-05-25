###########################################################################/**
# @RdocClass TabularTextFileSet
#
# @title "The TabularTextFileSet class"
#
# \description{
#  @classhierarchy
#
#  An TabularTextFileSet object represents a set of @see "TabularTextFile"s.
# }
# 
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to @see "GenericTabularFileSet".}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
# 
# @examples "../incl/TabularTextFileSet.Rex"
# 
# @author
#*/###########################################################################
setConstructorS3("TabularTextFileSet", function(...) {
  extend(GenericTabularFileSet(...), "TabularTextFileSet");
}) 


###########################################################################
# HISTORY:
# 2008-05-16
# o Created.
############################################################################
