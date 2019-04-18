%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readChecksum.GenericDataFile}
\alias{readChecksum.GenericDataFile}
\alias{GenericDataFile.readChecksum}
\alias{readChecksum,GenericDataFile-method}

\title{Reads the value of the corresponding checksum file}

\description{
  Reads the value of the corresponding checksum file, if existing.
}

\usage{
\method{readChecksum}{GenericDataFile}(this, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} string, which can be \code{\link[base]{NA}} if file is missing.
  An exception is thrown if the file does not exist, and hence not
  the checksum file.
}

\details{
  The content of the checksum file is trimmed from comment lines,
  whitespaces and then validated that the remaining part contains a
  hexadecimal value.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:validateChecksum.GenericDataFile]{*validateChecksum}()}.
  \code{\link[R.filesets:compareChecksum.GenericDataFile]{*compareChecksum}()}.
  \code{\link[R.filesets:writeChecksum.GenericDataFile]{*writeChecksum}()}.
  \code{\link[R.filesets:getChecksum.GenericDataFile]{*getChecksum}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
