%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getPathname.GenericDataFile}
\alias{getPathname.GenericDataFile}
\alias{GenericDataFile.getPathname}
\alias{getPathname.GenericDataFile}
\alias{getPathname,GenericDataFile-method}


\title{Gets the pathname of the file}

\description{
  Gets the pathname of the file.
}

\usage{
\method{getPathname}{GenericDataFile}(this, absolute=FALSE, ...)
}

\arguments{
 \item{absolute}{If \code{\link[base:logical]{TRUE}}, the absolute pathname is returned,
    otherwise the relative.}
 \item{...}{Not used.}
}

\value{
  Returns the pathname as \code{\link[base]{character}} string
  (or \code{\link[base]{NULL}} if an "empty" file).
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
