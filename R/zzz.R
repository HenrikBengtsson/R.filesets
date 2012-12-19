# Allows conflicts. For more information, see library() and
# conflicts() in [R] base.
.conflicts.OK <- TRUE


## .First.lib <- function(libname, pkgname) {
.onAttach <- function(libname, pkgname) {
  pkg <- Package(pkgname);
  assign(pkgname, pkg, pos=getPosition(pkg));
  startupMessage(pkg);
}


############################################################################
# HISTORY: 
# 2011-07-24
# o Added a namespace to the package.
############################################################################
