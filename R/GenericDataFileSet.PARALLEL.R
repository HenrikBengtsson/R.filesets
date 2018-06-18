###########################################################################/**
# @set class=GenericDataFileSet
# @RdocMethod dsApplyInPairs
# @aliasmethod dsApply
# @alias dsApplyInPairs
#
# @title "Applies a function to each pair of file in two file sets"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{ds, ds1, ds2}{@see "GenericDataFileSet":s.}
#  \item{FUN}{A @function.}
#  \item{...}{Arguments passed to \code{FUN}.}
#  \item{args}{(optional) A named @list of additional arguments
#    passed to \code{FUN}.}
#  \item{skip}{If @TRUE, already processed files are skipped.}
#  \item{verbose}{See @see "R.utils::Verbose".}
# }
#
# \value{
#   Returns a named @list where the names are those of argument \code{IDXS}.
# }
#
# \examples{\dontrun{
#  @include "../incl/GenericDataFileSet.dsApply.Rex"
# }}
#
# \details{
#  \emph{
#    WARNING: \code{dsApplyInPairs()} is deprecated; use
#    \code{\link[future.apply]{future_mapply}()}.
#    WARNING: \code{dsApply()} is defunct; use
#    \code{\link[future.apply]{future_lapply}()}.
#  }
# }
#
# \seealso{
#  The \pkg{future} and \pkg{future.apply} packages are utilized for
#  parallel/distributed processing.
# }
#
# @author "HB"
#
# @keyword internal
#*/###########################################################################
setMethodS3("dsApplyInPairs", "GenericDataFileSet", function(ds1, ds2, FUN, ..., args = list(), skip = FALSE, verbose = FALSE, .parallel = NULL, .control = NULL) {
  ds2 <- Arguments$getInstanceOf(ds2, class(ds1)[1L])
  .stop_if_not(length(ds2) == length(ds1))
  skip <- Arguments$getLogical(skip)
  verbose <- Arguments$getVerbose(verbose)
  
  MoreArgs <- args
  MoreArgs$skip <- skip
  MoreArgs$verbose <- verbose
  
  names <- sprintf("Pair (%s,%s)", getFullNames(ds1), getFullNames(ds2))
  res <- future_mapply(FUN = FUN, ds1, ds2, ..., MoreArgs = MoreArgs,
                       SIMPLIFY = FALSE)
  names(res) <- names
  
  res
}, protected = TRUE)


setMethodS3("dsApply", "GenericDataFileSet", function(ds, ...) {
  .Defunct(msg = "R.filesets::dsApply(ds, FUN, ...) is defunct. Use lapply(ds, FUN, ...) or future.apply::future_lapply(ds, FUN, ...) instead.")
}, protected = TRUE)
