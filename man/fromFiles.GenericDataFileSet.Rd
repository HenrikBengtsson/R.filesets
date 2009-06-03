%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{fromFiles.GenericDataFileSet}
\alias{fromFiles.GenericDataFileSet}
\alias{GenericDataFileSet.fromFiles}
\alias{fromFiles.GenericDataFileSet}
\alias{fromFiles,GenericDataFileSet-method}


\title{Defines an GenericDataFileSet object by searching for files}

\description{
  Defines an GenericDataFileSet object by searching for files.
}

\synopsis{fromFiles.GenericDataFileSet(static, path=NULL, pattern=NULL, recursive=FALSE, fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE)}
\usage{GenericDataFileSet$fromFiles(path=NULL, pattern=NULL, recursive=FALSE, fileClass=getFileClass(static), ..., .validate=FALSE, verbose=FALSE)}

\arguments{
 \item{path}{The path where to search for files.}
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

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
