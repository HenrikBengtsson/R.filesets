%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{isFile.GenericDataFile}
\alias{isFile.GenericDataFile}
\alias{GenericDataFile.isFile}
\alias{isFile.GenericDataFile}
\alias{isFile,GenericDataFile-method}


\title{Checks if this is an existing file}

\description{
  Checks if this is an existing file and not a directory.
}

\usage{
\method{isFile}{GenericDataFile}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base:logical]{TRUE}} if an existing file (and not a directory),
  otherwise \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.utils]{isFile}}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
