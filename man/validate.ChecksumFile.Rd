%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ChecksumFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{validate.ChecksumFile}
\alias{validate.ChecksumFile}
\alias{ChecksumFile.validate}
\alias{validate,ChecksumFile-method}

\title{Asserts that the checksum matches the checksum of file}

\description{
  Asserts that the checksum matches the checksum of file.
}

\usage{
\method{validate}{ChecksumFile}(this, ..., verbose=FALSE)
}

\arguments{
 \item{...}{Not used.}
 \item{verbose}{...}
}

\value{
  Returns \code{\link[base:logical]{TRUE}}.
  If checksum on record does not match the file, an exception is thrown.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{ChecksumFile}}.
}
\keyword{internal}
\keyword{methods}
