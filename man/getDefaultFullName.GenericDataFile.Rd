%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDefaultFullName.GenericDataFile}
\alias{getDefaultFullName.GenericDataFile}
\alias{GenericDataFile.getDefaultFullName}
\alias{getDefaultFullName,GenericDataFile-method}

\title{Gets the full name of the file}

\description{
  Gets the full name of the file, that is the filename without the filename extension.
}

\usage{
\method{getDefaultFullName}{GenericDataFile}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} string (\code{\link[base]{NA}} if "empty" file).
}

\details{
 The full name of a file is the filename excluding any
 extension (and period).
 For instance, the full name of \code{path/to/foo,a.2,b.ext} is
 \code{foo,a.2,b}.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
