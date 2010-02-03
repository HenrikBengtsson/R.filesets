%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{validateChecksum.GenericDataFile}
\alias{validateChecksum.GenericDataFile}
\alias{GenericDataFile.validateChecksum}
\alias{validateChecksum.GenericDataFile}
\alias{validateChecksum,GenericDataFile-method}


\title{Asserts that the file checksum matches the one of the checksum file}

\description{
  Asserts that the file checksum matches the one of the checksum file.
}

\usage{\method{validateChecksum}{GenericDataFile}(this, ..., verbose=FALSE)}

\arguments{
 \item{...}{Not used.}
 \item{verbose}{...}
}

\value{
  An exception is thrown if not, that is, if \code{\link[R.filesets:compareChecksum.GenericDataFile]{*compareChecksum}()}
  returns \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:validateChecksum.GenericDataFile]{*validateChecksum}()}.
  \code{\link[R.filesets:readChecksum.GenericDataFile]{*readChecksum}()}.
  \code{\link[R.filesets:writeChecksum.GenericDataFile]{*writeChecksum}()}.
  \code{\link[R.filesets:getChecksum.GenericDataFile]{*getChecksum}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
