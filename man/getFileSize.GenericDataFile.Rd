%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFileSize.GenericDataFile}
\alias{getFileSize.GenericDataFile}
\alias{GenericDataFile.getFileSize}
\alias{getFileSize.GenericDataFile}
\alias{getFileSize,GenericDataFile-method}


\title{Gets the size of a file}

\description{
  Gets the size of a file.
}

\usage{\method{getFileSize}{GenericDataFile}(this, what=c("numeric", "units"), sep="", ...)}

\arguments{
 \item{what}{A \code{\link[base]{character}} string specifying the data type returned.
  If \code{"numeric"}, then a \code{\link[base]{numeric}} value is returned.
  If \code{"units"}, then a human-readable \code{\link[base]{character}} string is returned.
 }
 \item{sep}{A \code{\link[base]{character}} string.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{numeric}} or a \code{\link[base]{character}} string.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[base]{file.info}}().
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
