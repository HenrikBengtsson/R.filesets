%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.getChecksum.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getChecksum.GenericDataFileSet}
\alias{getChecksum.GenericDataFileSet}
\alias{GenericDataFileSet.getChecksum}
\alias{getChecksum.GenericDataFileSet}
\alias{getChecksum,GenericDataFileSet-method}


\title{Generates a deterministic checksum for an R object}

\description{
 Generates a deterministic checksum for an R object that can be used to identify the data set.
}

\usage{
\method{getChecksum}{GenericDataFileSet}(this, ...)
}

\arguments{
  \item{...}{Arguments passed to \code{\link[R.filesets:getChecksumObjects.GenericDataFileSet]{*getChecksumObjects}()}.}
}

\value{
  Returns the checksum as a \code{\link[base]{character}} string.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  Internally, the checksum is calculated on the objects returned by
  \code{\link[R.filesets:getChecksumObjects.GenericDataFileSet]{*getChecksumObjects}()}.

  For more information see \code{\link{GenericDataFileSet}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
