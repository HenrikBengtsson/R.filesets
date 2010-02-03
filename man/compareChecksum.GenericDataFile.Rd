%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{compareChecksum.GenericDataFile}
\alias{compareChecksum.GenericDataFile}
\alias{GenericDataFile.compareChecksum}
\alias{compareChecksum.GenericDataFile}
\alias{compareChecksum,GenericDataFile-method}


\title{Compares the file checksum with the value of the checksum file}

\description{
  Compares the file checksum with the value of the checksum file.
}

\usage{\method{compareChecksum}{GenericDataFile}(this, ..., verbose=FALSE)}

\arguments{
 \item{...}{Not used.}
 \item{verbose}{...}
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the file checksum is identical to the stored value
  in the corresponding checksum file, otherwise \code{\link[base:logical]{FALSE}}.  \code{\link[base:logical]{FALSE}} is
  also returned if the checksum file does not exist.
  An exception is thrown if the file does not exist.
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
