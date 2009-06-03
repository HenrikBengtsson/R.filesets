digest2 <- function(object, ..., skip="auto", ascii=FALSE) {
  rechar <- function(strs) {
    if (!is.null(strs))
      strs <- sapply(strs, FUN=function(s) rawToChar(charToRaw(s)));
    strs;
  }

  # Patch for R v2.6.0, because it seems like "identical" 'names'
  # vectors can generate different hashcodes, which I suspect has
  # to do with the recent suffix-tree implementation of strings
  # added to R v2.6.0, but also not sure anymore ?!? /HB 2007-08-31
  names(object) <- rechar(names(object));

  digest::digest(object, ..., skip=skip, ascii=ascii); 
} # digest2()

############################################################################
# HISTORY:
# 2007-08-31
# o I'm still puzzled about the names() thing, cf. the Rd thread 
#   "Consistency of serialize(): please enlighten me"
#   https://stat.ethz.ch/pipermail/r-devel/2007-August/046768.html.
# 2007-08-30
# o Created from .patchDigest() [2007-04-04].
############################################################################  
