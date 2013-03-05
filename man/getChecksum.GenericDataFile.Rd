%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getChecksum.GenericDataFile}
\alias{getChecksum.GenericDataFile}
\alias{GenericDataFile.getChecksum}
\alias{getChecksum,GenericDataFile-method}

\title{Gets the checksum of a file}

\description{
  Gets the checksum of a file.
}

\usage{
\method{getChecksum}{GenericDataFile}(this, ..., force=FALSE, verbose=FALSE)
}

\arguments{
 \item{force}{If \code{\link[base:logical]{FALSE}}, the file exists and has not be modified since,
   then the cached checksum is returned.}
 \item{verbose}{...}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} string, which can be \code{\link[base]{NA}} if file is missing.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  Internally \code{\link[digest]{digest}} is used.
  \code{\link[R.filesets:readChecksum.GenericDataFile]{*readChecksum}()}.
  \code{\link[R.filesets:writeChecksum.GenericDataFile]{*writeChecksum}()}.
  \code{\link[R.filesets:compareChecksum.GenericDataFile]{*compareChecksum}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
