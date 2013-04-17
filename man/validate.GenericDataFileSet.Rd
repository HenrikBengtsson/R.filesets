%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{validate.GenericDataFileSet}
\alias{validate.GenericDataFileSet}
\alias{GenericDataFileSet.validate}
\alias{validate,GenericDataFileSet-method}

\title{Validates all files in the data set}

\description{
  Validates all files in the data set.
}

\usage{
\method{validate}{GenericDataFileSet}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  If one of the files is invalid, then an error is thrown.
  If all of the files are valid, then \code{\link[base:logical]{TRUE}} is returned.
  Otherwise, \code{\link[base]{NA}} is returned.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
