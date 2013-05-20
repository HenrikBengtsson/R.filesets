%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{GenericDataFileSet$byPath}
\alias{GenericDataFileSet$byPath}
\alias{byPath.GenericDataFileSet}
\alias{GenericDataFileSet.byPath}
\alias{byPath,GenericDataFileSet-method}

\title{Defines a GenericDataFileSet by searching for files in a directory}

\description{
  Defines a GenericDataFileSet by searching for files in a directory.
}

\usage{
## Static method (use this):
## GenericDataFileSet$byPath(path=NULL, pattern=NULL, recursive=FALSE, depth=0L,
##   fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE)

## Don't use the below:
\method{byPath}{GenericDataFileSet}(static, path=NULL, pattern=NULL, recursive=FALSE, depth=0L,
  fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE)
}

\arguments{
 \item{path}{The directory where to search for files.}
 \item{pattern}{The filename pattern for match files.
    If \code{\link[base]{NULL}}, filename extensions corresponding to known subclasses
    of the abstract \code{\link{GenericDataFile}} class are search for.}
 \item{recursive}{If \code{\link[base:logical]{TRUE}}, subdirectories are search recursively,
    otherwise not.}
 \item{...}{Optional arguments passed to the constructor of the
    static (calling) class.}
}

\value{
  Returns an \code{\link{GenericDataFileSet}} object.
}

\section{Reserved filenames}{
  Note that files with names starting with a period \code{.} are not
  searched for.  Such files are considered "private" and have to be
  included explicitly, if wanted.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
