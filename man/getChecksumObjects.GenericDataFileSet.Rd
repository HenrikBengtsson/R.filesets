%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.getChecksum.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{getChecksumObjects.GenericDataFileSet}
\alias{getChecksumObjects.GenericDataFileSet}
\alias{GenericDataFileSet.getChecksumObjects}
\alias{getChecksumObjects.GenericDataFileSet}
\alias{getChecksumObjects,GenericDataFileSet-method}


\title{Retrieves objects that uniquely identifying the data set}

\description{
 Retrieves objects that uniquely identifying the data set.
}

\usage{\method{getChecksumObjects}{GenericDataFileSet}(this, ..., verbose=FALSE)}

\arguments{
  \item{...}{Not used.}
  \item{verbose}{A \code{\link[base]{logical}} or \code{\link[R.utils]{Verbose}}.}
}

\value{
  Returns a named \code{\link[base]{list}} of objects that can be used to uniquely
  identifying the GenericDataFileSet.
}

\details{
  By default, a GenericDataFileSet is assumed to be uniquely
  identifiable by its files and their fullnames
  (possibly translated).
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getChecksum.GenericDataFileSet]{*getChecksum}()}

  For more information see \code{\link{GenericDataFileSet}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
