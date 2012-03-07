# Added '...' to some base functions. These will later be
# turned into default functions by setMethodS3().

append <- appendVarArgs(append);

# USED TO DO: readLines <- appendVarArgs(readLines)
readLines <- function(...) UseMethod("readLines");
setMethodS3("readLines", "default", function(...) {
  base::readLines(...);
})


############################################################################
# HISTORY:
# 2012-03-06 [HB]
# o CRAN POLICY: Removed all internal copies of 'base' functions that
#   have .Internal() calls. 
# 2007-02-27 [HB]
# o BUG FIX: Removed explicit reference to 'base' etc again. The reason is 
#   that if a previous package already modified, say, write(), to become a 
#   generic function, that was overwritten again when this package was 
#   loaded.
# 2007-02-23 [KS]
# o Make explicit reference to 'base' - this is safer, in case of colMeans()
#   defined by user or other packages.
# 2006-03-24
# o Created to please R CMD check.
############################################################################
