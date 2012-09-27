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





###########################################################################/**
# @RdocMethod readDataFrame
#
# @title "Reads the tabular data from all files as data frames"
#
# \description{
#  @get "title" and combines them into one data frame (by default).
# }
#
# @synopsis
#
# \arguments{
#   \item{...}{Arguments passed to 
#     \code{\link[R.filesets:readDataFrame.TabularTextFile]{readDataFrame}()}
#     as called for each @see "TabularTextFile" of the file set.}
#   \item{combineBy}{A @function that takes a @list of @data.frame:s
#     and combines them.  The default is to stack them into a single
#     @data.frame.  If @NULL, the @list is not combined.}
# }
#
# \value{
#   Returns what \code{combineBy} returns, which defaults to a @data.frame.
#   If \code{combineBy=NULL}, then a named @list of @data.frame:s is returned.
# }
#
# @examples "../incl/TabularTextFileSet.readDataFrame.Rex"
#
# @author
#
# \seealso{
#   @seeclass
# }
#
# @keyword IO
# @keyword programming
#*/###########################################################################
setMethodS3("readDataFrame", "TabularTextFileSet", function(this, ..., combineBy=function(x) Reduce(rbind, x)) {
  # Argument 'combineBy':
  if (!is.null(combineBy)) {
    if (!is.function(combineBy)) {
      throw("Argument 'combineBy' is not a function: ", mode(combineBy));
    }
  }

  # Read
  data <- lapply(this, readDataFrame, ...);
  if (is.function(combineBy)) {
    data <- combineBy(data);
  }

  data;
})


###########################################################################
# HISTORY:
# 2012-09-27
# o Added readDataFrame() for TabularTextFileSet.
# 2008-05-16
# o Created.
############################################################################
