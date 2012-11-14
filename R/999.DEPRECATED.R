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
  .Deprecated("getName");
  label <- this$label;
  if (is.null(label))
    label <- getName(this, ...);
  label;
}, private=TRUE, deprecated=TRUE)

setMethodS3("setLabel", "GenericDataFile", function(this, label, ...) {
  .Deprecated("setName");
  this$label <- label;
  invisible(this);
}, private=TRUE, deprecated=TRUE)

setMethodS3("getAlias", "GenericDataFile", function(this, ...) {
  .Deprecated("getFullName");
  this$.alias;
}, protected=TRUE, deprecated=TRUE)

setMethodS3("setAlias", "GenericDataFile", function(this, alias=NULL, ...) {
  .Deprecated("setFullName");
  if (!is.null(alias)) {
    alias <- Arguments$getFilename(alias);
  }
  this$.alias <- alias;
  invisible(this);
}, protected=TRUE, deprecated=TRUE)
 


###########################################################################/**
# @set "class=GenericDataFile"
# @RdocMethod getAlias
#
# @title "Gets the alias of the file set"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character, or @NULL if no alias is set.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getAlias", "GenericDataFileSet", function(this, ...) {
  .Deprecated("getFullName");
  this$.alias;
}, protected=TRUE, deprecated=TRUE)



###########################################################################/**
# @set "class=GenericDataFile"
# @RdocMethod setAlias
#
# @title "Sets the alias of the file set"
#
# \description{
#   @get "title".
#   If specified, the alias overrides the name inferred from the pathname
#   of the file set.  This can be used in order to use another name of the
#   output data set than the input data set of many transforms and models.
# }
#
# @synopsis
#
# \arguments{
#  \item{alias}{A @character string for the new alias of the file set.
#   The alias must consists of valid filename characters, and must not
#   contain commas, which are used to separate tags.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# @author
#
# \seealso{
#   @seeclass
# }
#*/###########################################################################
setMethodS3("setAlias", "GenericDataFileSet", function(this, alias=NULL, ...) {
  .Deprecated("setFullName");
  # Argument 'alias':
  if (!is.null(alias)) {
    alias <- Arguments$getFilename(alias);  # Valid filename?

    # Assert that no commas are used.
    if (regexpr("[,]", alias) != -1) {
      throw("File-set aliases (names) must not contain commas: ", alias);
    }
  }

  this$.alias <- alias;
}, protected=TRUE, deprecated=TRUE)


############################################################################
# HISTORY:
# 2012-11-12
# o CLEANUP: Deprecated (get|set)Alias() for GenericData(File|FileSet).
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
