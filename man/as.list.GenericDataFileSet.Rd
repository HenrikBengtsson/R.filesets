%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{as.list.GenericDataFileSet}
\alias{as.list.GenericDataFileSet}
\alias{GenericDataFileSet.as.list}
\alias{as.list,GenericDataFileSet-method}

\title{Returns the files of the file set}

\description{
 Returns the files of the file set.
}

\usage{
\method{as.list}{GenericDataFileSet}(x, useNames=TRUE, ...)
}

\arguments{
  \item{...}{Arguments passed to \code{\link[R.filesets:getFiles.GenericDataFileSet]{*getFiles}()}.}
  \item{useNames}{If \code{\link[base:logical]{TRUE}}, the list will be annotated with names.}
}

\value{
 Returns a \code{\link[base]{list}} of files, each of class \code{\link{getFileClass}}().
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
