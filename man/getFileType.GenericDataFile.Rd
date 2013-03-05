%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFileType.GenericDataFile}
\alias{getFileType.GenericDataFile}
\alias{GenericDataFile.getFileType}
\alias{getFileType.GenericDataFile}
\alias{getFileType,GenericDataFile-method}


\title{Gets the file type of a file}

\description{
  Gets the file type of a file.
}

\usage{
\method{getFileType}{GenericDataFile}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} in lower case letters
  (which is of length zero if "empty" file).
}

\details{
  By default, this methods returns the filename extension, but subclasses
  may override this.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
