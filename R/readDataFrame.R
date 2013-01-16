###########################################################################/**
# @RdocDefault readDataFrame
#
# @title "Reads data from a tabular file"
#
# \description{
#  @get "title".
# }
# 
# @synopsis
#
# \arguments{
#   \item{filename, path}{@character strings specifying the file to be read.}
#   \item{...}{Additional arguments passed to \code{\link[readDataFrame.TabularTextFile]{readDataFrame}}.}
#   \item{fileClass}{(optional) A @character string specifying the @see "TabularTextFile" class.}
# }
#
# \value{
#  Returns a @data.frame.
# }
#
# @examples "../incl/readDataFrame.Rex"
#
# @author
#
# \seealso{
#   @see "utils::read.table".
#   For further control, see the @see "TabularTextFile" class.
# }
#*/###########################################################################
setMethodS3("readDataFrame", "default", function(filename, path=NULL, ..., fileClass="TabularTextFile") {
  # Argument 'fileClass':
  clazz <- Class$forName(fileClass);

  db <- newInstance(clazz, filename, path=path);
  db <- Arguments$getInstanceOf(db, "TabularTextFile");

  readDataFrame(db, ...);
})



############################################################################
# HISTORY:
# 2013-01-16
# o Created.
############################################################################
