###########################################################################/**
# @set class=GenericDataFileSet
# @RdocMethod dsApplyInPairs
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
#  \item{ds1, ds2}{@see "GenericDataFileSet":s.}
#  \item{FUN}{A @function.}
#  \item{...}{Arguments passed to \code{FUN}.}
#  \item{args}{(optional) A named @list of additional arguments
#    passed to \code{FUN}.}
#  \item{skip}{If @TRUE, already processed files are skipped.}
#  \item{verbose}{See @see "R.utils::Verbose".}
# }
#
# \value{
#   Returns a @list.
# }
#
# \examples{\dontrun{
#  @include "../incl/GenericDataFileSet.dsApply.Rex"
# }}
#
# \details{
#  \emph{
#    WARNING: \code{dsApplyInPairs()} is defunct; instead use
#    \code{future.apply::future_mapply()}.
#  }
# }
#
# @author "HB"
#
# @keyword internal
#*/###########################################################################
setMethodS3("dsApplyInPairs", "GenericDataFileSet", function(ds1, ds2, FUN, ..., args = list(), skip = FALSE, verbose = FALSE, .parallel = NULL, .control = NULL) {
  .Defunct(msg = "R.filesets::dsApplyInPairs(ds1, ds2, FUN, ...) is defuct. Instead, use mapply(FUN, ds1, ds2, ..., SIMPLIFY = FALSE) or future.apply::future_mapply(FUN, ds1, ds2, ..., SIMPLIFY = FALSE).")
}, protected = TRUE)
