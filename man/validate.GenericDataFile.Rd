%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{validate.GenericDataFile}
\alias{validate.GenericDataFile}
\alias{GenericDataFile.validate}
\alias{validate.GenericDataFile}
\alias{validate,GenericDataFile-method}


\title{Validates the content of a file}

\description{
  Validates the content of a file.
}

\usage{\method{validate}{GenericDataFile}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  If the file is invalid, then an error is thrown.
  If the files is valid, then \code{\link[base:logical]{TRUE}} is returned.
  Otherwise, \code{\link[base]{NA}} is returned, which happens if the file was not validated.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
