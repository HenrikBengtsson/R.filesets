%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  999.package.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{R.filesets-package}
\alias{R.filesets-package}
\alias{R.filesets}
\docType{package}

\title{Package R.filesets}


\description{
  A file set refers to a set of files located in one or more directories on the file system.  This package provides classes and methods to locate, setup, subset, navigate and iterate such sets.  The API is designed such that these classes can be extended via inheritance to provide a richer API for special file formats.  Moreover, a specific name format is defined such that filenames and directories can be considered to have full names which consists of a name followed by comma-separated tags.  This adds additional flexibility to identify file sets and individual files.  NOTE: This package's API should be considered to be in an beta stage.  Its main purpose is currently to support the aroma.* packages, where it is one of the main core components; if you decide to build on top of this package, please contact the author first.

  This package should be considered to be in an alpha or beta phase.
  You should expect the API to be changing over time.
}

\section{Installation}{
  To install this package, call
  \code{install.packages("R.filesets")}.
}

\section{To get started}{
  To get started, see:
  \enumerate{
    \item \code{\link{GenericDataFileSet}}
    \item \code{\link{TabularTextFile}}
  }
}

\section{How to cite this package}{
  Please cite references [1] when using this package.
}

\author{Henrik Bengtsson}

\section{License}{
  The releases of this package is licensed under
  LGPL version 2.1 or newer.

  The development code of the packages is under a private licence
  (where applicable) and patches sent to the author fall under the
  latter license, but will be, if incorporated, released under the
  "release" license above.
}

\references{
 [1] H. Bengtsson, \emph{The R.oo package - Object-Oriented Programming with References Using Standard R Code}, In Kurt Hornik, Friedrich Leisch and Achim Zeileis, editors, Proceedings of the 3rd International Workshop on Distributed Statistical Computing (DSC 2003), March 20-22, Vienna, Austria. \url{http://www.ci.tuwien.ac.at/Conferences/DSC-2003/Proceedings/}
 \cr
}
\keyword{package}
