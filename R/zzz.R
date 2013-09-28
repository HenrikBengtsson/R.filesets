# Allows conflicts. For more information, see library() and
# conflicts() in [R] base.
.conflicts.OK <- TRUE


.onLoad <- function(libname, pkgname) {
  ns <- getNamespace(pkgname);
  pkg <- Package(pkgname);
  assign(pkgname, pkg, envir=ns);
}


.onAttach <- function(libname, pkgname) {
  startupMessage(get(pkgname, envir=getNamespace(pkgname)));
}


############################################################################
# HISTORY:
# 2011-07-24
# o Added a namespace to the package.
############################################################################
