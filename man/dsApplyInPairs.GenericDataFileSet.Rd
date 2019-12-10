%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.PARALLEL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{dsApplyInPairs.GenericDataFileSet}
\alias{dsApplyInPairs.GenericDataFileSet}
\alias{GenericDataFileSet.dsApplyInPairs}
\alias{dsApplyInPairs,GenericDataFileSet-method}
\alias{dsApplyInPairs}

\title{Applies a function to each pair of file in two file sets}

\description{
  Applies a function to each pair of file in two file sets.
}

\usage{
\method{dsApplyInPairs}{GenericDataFileSet}(ds1, ds2, FUN, ..., args=list(), skip=FALSE, verbose=FALSE,
  .parallel=NULL, .control=NULL)
}

\arguments{
 \item{ds, ds1, ds2}{\code{\link{GenericDataFileSet}}:s.}
 \item{FUN}{A \code{\link[base]{function}}.}
 \item{...}{Arguments passed to \code{FUN}.}
 \item{args}{(optional) A named \code{\link[base]{list}} of additional arguments
   passed to \code{FUN}.}
 \item{skip}{If \code{\link[base:logical]{TRUE}}, already processed files are skipped.}
 \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
}

\value{
  Returns a named \code{\link[base]{list}} where the names are those of argument \code{IDXS}.
}

\examples{\dontrun{

# - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)


# - - - - - - - - - - - - - - - - - - - - - - - -
# Get the size of each file
# - - - - - - - - - - - - - - - - - - - - - - - -
sizes <- lapply(ds, FUN=getFileSize)
str(sizes)

}}

\details{
 \emph{
   WARNING: \code{dsApplyInPairs()} is deprecated; instead use
   \code{future.apply::future_mapply()}.
 }
}

\author{Henrik Bengtsson}


\keyword{internal}
\keyword{methods}
