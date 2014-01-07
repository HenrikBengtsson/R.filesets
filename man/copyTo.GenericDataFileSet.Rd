%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{copyTo.GenericDataFileSet}
\alias{copyTo.GenericDataFileSet}
\alias{GenericDataFileSet.copyTo}
\alias{copyTo,GenericDataFileSet-method}

\title{Copies a data set to another directory}

\description{
  Copies a data set to another directory.
}

\usage{
\method{copyTo}{GenericDataFileSet}(this, path=NULL, ..., verbose=FALSE)
}

\arguments{
 \item{path}{The destination directory.  If missing, it is created.}
 \item{...}{Additional arguments passed to \code{copyTo()} used to copy
  the individual \code{\link{GenericDataFile}}:s in the set.}
 \item{verbose}{...}
}

\value{
  Returns a \code{\link{GenericDataFileSet}} consisting the new file copies.
}

\details{
  Each file is copied safely, but if this method is interrupted, it
  may results in a data set consisting of fewer than the original
  data set.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
