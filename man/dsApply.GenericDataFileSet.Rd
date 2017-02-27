%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.PARALLEL.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{dsApply.GenericDataFileSet}
\alias{dsApply.GenericDataFileSet}
\alias{GenericDataFileSet.dsApply}
\alias{dsApply,GenericDataFileSet-method}
\alias{GenericDataFileSet.dsApplyInPairs}
\alias{dsApplyInPairs.GenericDataFileSet}
\alias{dsApplyInPairs,GenericDataFileSet-method}

\alias{dsApplyInPairs}

\title{Applies a function to each file in the file set}

\description{
  Applies a function to each file in the file set.

 \emph{
   WARNING: \code{dsApply(ds, FUN, ...)} will soon be deprecated.
   Instead, use \code{\link[future]{future_lapply}(ds, FUN, ...)}.
 }
}

\usage{
\method{dsApply}{GenericDataFileSet}(ds, IDXS=NULL, DROP=is.null(IDXS), AS=as.list, FUN, ..., args=list(), skip=FALSE,
  verbose=FALSE, .parallel=c("none", "future", "BatchJobs", "BiocParallel::BatchJobs"),
  .control=list(dW = 1))
}

\arguments{
 \item{ds, ds1, ds2}{\code{\link{GenericDataFileSet}}:s.}
 \item{IDXS}{A (named) \code{\link[base]{list}}, where each element contains a \code{\link[base]{vector}} data
   set indices, or an \code{\link[base]{integer}} \code{\link[base]{vector}} of individual elements.
   If \code{\link[base]{NULL}}, then ... with names as of the data set.}
 \item{DROP}{If \code{\link[base:logical]{FALSE}}, the first argument passed to \code{FUN} is always a \code{\link[base]{list}} of files.
   If \code{\link[base:logical]{TRUE}}, an single-index element is passed to \code{FUN} as a file instead of
   as a \code{\link[base]{list}} containing a single file.}
 \item{AS}{(optional) A \code{\link[base]{function}} coercing the first set/group object passed.}
 \item{FUN}{A \code{\link[base]{function}}.}
 \item{...}{Arguments passed to \code{FUN}.}
 \item{args}{(optional) A \code{\link[base]{list}} of additional arguments
   passed to \code{FUN}.}
 \item{skip}{If \code{\link[base:logical]{TRUE}}, already processed files are skipped.}
 \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
 \item{.parallel}{A \code{\link[base]{character}} string specifying what mechanism to use
   for performing parallel processing, if at all.}
 \item{.control}{(internal) A named \code{\link[base]{list}} structure controlling
       the processing.}
}

\value{
  Returns a named \code{\link[base]{list}} where the names are those of argument \code{IDXS}.
}

\examples{\dontrun{
 isPackageInstalled <- R.utils::isPackageInstalled

# - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)


# - - - - - - - - - - - - - - - - - - - - - - - -
# Get the size of each file
# - - - - - - - - - - - - - - - - - - - - - - - -
# Alt 1.
res1 <- lapply(ds, FUN=getFileSize)
print(res1)

# Alt 2. (according to current settings; see package startup message)
res2 <- dsApply(ds, FUN=getFileSize)
print(res2)
stopifnot(identical(res2, res1))

# Alt 3. (via an internal loop)
res2 <- dsApply(ds, FUN=getFileSize, .parallel="none")
print(res2)
stopifnot(identical(res2, res1))

# Alt 4. (via BiocParallel + BatchJobs)
if (isPackageInstalled("BiocParallel") && isPackageInstalled("BatchJobs")) {
  res3 <- dsApply(ds, FUN=getFileSize, .parallel="BiocParallel::BatchJobs")
  print(res3)
  stopifnot(identical(res3, res1))
}

}}

\seealso{
  The \pkg{future}, \pkg{BiocParallel} and \pkg{BatchJobs} packages
  are utilized for parallel/distributed processing, depending on settings.
}

\author{Henrik Bengtsson}


\keyword{internal}
\keyword{methods}
