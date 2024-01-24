%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ChecksumFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readChecksum.ChecksumFile}
\alias{readChecksum.ChecksumFile}
\alias{ChecksumFile.readChecksum}
\alias{readChecksum,ChecksumFile-method}

\title{Reads the checksum value}

\description{
  Reads the checksum value.
}

\usage{
\method{readChecksum}{ChecksumFile}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a lower-case \code{\link[base]{character}} string.
}

\details{
  The content of the checksum file is trimmed from comment lines,
  whitespaces and then validated that the remaining part contains a
  hexadecimal value.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{ChecksumFile}}.
}
\keyword{internal}
\keyword{methods}
