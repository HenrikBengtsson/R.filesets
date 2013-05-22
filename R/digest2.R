digest2 <- function(object, ..., skip="auto", ascii=FALSE) {
  names <- names(object);
  if (!is.null(names) && getRversion() <= "2.6.0") {
    # Patch for R v2.6.0, because it seems like "identical" 'names'
    # vectors can generate different hashcodes, which I suspect has
    # to do with the recent suffix-tree implementation of strings
    # added to R v2.6.0, but also not sure anymore ?!? /HB 2007-08-31
    rechar <- function(strs) {
      if (!is.null(strs))
        strs <- sapply(strs, FUN=function(s) rawToChar(charToRaw(s)));
      strs;
    }
    names <- rechar(names);
    names(object) <- names;
  }
  digest::digest(object, ..., skip=skip, ascii=ascii);
} # digest2()

############################################################################
# HISTORY:
# 2013-05-22
# o SPEEDUP: Now digest2() only applies the patch if the object has
#   names attributes and if running on R v2.6.0 or before.
# 2007-08-31
# o I'm still puzzled about the names() thing, cf. the Rd thread
#   "Consistency of serialize(): please enlighten me"
#   https://stat.ethz.ch/pipermail/r-devel/2007-August/046768.html.
# 2007-08-30
# o Created from .patchDigest() [2007-04-04].
############################################################################
