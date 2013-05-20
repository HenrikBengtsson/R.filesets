%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{hasBeenModified.GenericDataFile}
\alias{hasBeenModified.GenericDataFile}
\alias{GenericDataFile.hasBeenModified}
\alias{hasBeenModified,GenericDataFile-method}

\title{Checks whether the file has been modified}

\description{
  Checks whether the file has been modified since last time checked.
}

\usage{
\method{hasBeenModified}{GenericDataFile}(this, ..., unknown=TRUE)
}

\arguments{
 \item{...}{Not used.}
 \item{unknown}{The \code{\link[base]{logical}} value returned if the timestamp for the
  previous modification, if any, is unknown.}
}

\value{
  Returns \code{\link[base:logical]{TRUE}}, \code{\link[base:logical]{FALSE}}, or the value of argument \code{unknown}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[base]{file.info}}().
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
