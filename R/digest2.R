digest2 <- function(object, ..., skip="auto", ascii=FALSE) {
  digest::digest(object, ..., skip=skip, ascii=ascii);
} # digest2()

############################################################################
# HISTORY:
# 2013-09-30
# o CLEANUP: Dropped R (<= 2.6.0) patch from digest2(), because package
#   now requires R (>= 2.15.0).
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
