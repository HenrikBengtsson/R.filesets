###########################################################################/**
# @set class=GenericDataFileSet
# @RdocMethod dsApply
# @aliasmethod dsApplyInPairs
# @alias dsApplyInPairs
#
# @title "Applies a function to each file in the file set"
#
# \description{
#   @get "title".
#
#  \emph{
#    WARNING: \code{dsApply(ds, FUN, ...)} is deprecated.
#    Instead, use \code{\link[future.apply]{future_lapply}(ds, FUN, ...)}.
#  }
# }
#
# @synopsis
#
# \arguments{
#  \item{ds, ds1, ds2}{@see "GenericDataFileSet":s.}
#  \item{IDXS}{A (named) @list, where each element contains a @vector data
#    set indices, or an @integer @vector of individual elements.
#    If @NULL, then ... with names as of the data set.}
#  \item{DROP}{If @FALSE, the first argument passed to \code{FUN} is always a @list of files.
#    If @TRUE, an single-index element is passed to \code{FUN} as a file instead of
#    as a @list containing a single file.}
#  \item{AS}{(optional) A @function coercing the first set/group object passed.}
#  \item{FUN}{A @function.}
#  \item{...}{Arguments passed to \code{FUN}.}
#  \item{args}{(optional) A @list of additional arguments
#    passed to \code{FUN}.}
#  \item{skip}{If @TRUE, already processed files are skipped.}
#  \item{verbose}{See @see "R.utils::Verbose".}
#  \item{.parallel}{A @character string specifying what mechanism to use
#    for performing parallel processing, if at all.}
#  \item{.control}{(internal) A named @list structure controlling
#        the processing.}
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
# \seealso{
#  The \pkg{future} and \pkg{future.apply} packages are utilized for
#  parallel/distributed processing.
# }
#
# @author "HB"
#
# @keyword internal
#*/###########################################################################
setMethodS3("dsApply", "GenericDataFileSet", function(ds, IDXS=NULL, DROP=is.null(IDXS), AS=as.list, FUN, ..., args=list(), skip=FALSE, verbose=FALSE, .parallel=c("none", "future", "BatchJobs", "BiocParallel::BatchJobs"), .control=list(dW=1.0)) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Local functions
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  assertNoGlobalVariables <- function(FUN, ...) {
    # TO DO...
    ## globals <- findGlobals(FUN, merge=FALSE)
  } # assertNoGlobalVariables()


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'DROP':
  DROP <- Arguments$getLogical(DROP)

  # Argument 'IDXS':
  if (is.null(IDXS)) {
    IDXS <- seq_along(ds)
    names(IDXS) <- getFullNames(ds)
    IDXS <- as.list(IDXS)
  } else if (is.numeric(IDXS)) {
    max <- length(ds)
    IDXS <- Arguments$getIndices(IDXS, max=max)
    if (is.null(names(IDXS))) {
      names(IDXS) <- getFullNames(ds)
      IDXS <- as.list(IDXS)
    }
  } else if (is.list(IDXS)) {
    max <- length(ds)
    for (idxs in IDXS) {
      idxs <- Arguments$getIndices(idxs, max=max)
    }
  } else {
    throw("Invalid argument 'IDXS': ", class(IDXS)[1L])
  }

  # Argument 'FUN':
  stopifnot(is.function(FUN))
  assertNoGlobalVariables(FUN)


  # Arguments '...':
  vargs <- list(...)
  nvargs <- length(vargs)

  # Argument 'args':
  if (!is.list(args)) {
    throw("Argument 'args' must be a list: ", mode(args))
  }

  # Argument 'skip':
  skip <- Arguments$getLogical(skip)

  # Argument '.parallel':
  if (missing(.parallel)) {
    .parallel <- getOption("R.filesets/parallel", "none")
  }
  parallel <- match.arg(.parallel)

  # Argument 'verbose':
  verbose <- Arguments$getVerbose(verbose)
  if (verbose) {
    pushState(verbose)
    on.exit(popState(verbose))
  }


  verbose && enter(verbose, "Processing ", class(ds)[1L])
  verbose && cat(verbose, "Mechanism for parallel processing: ", parallel)
  verbose && print(verbose, ds)

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Sets of files to be processed
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  verbose && cat(verbose, "Number of subsets to be processed: ", length(IDXS))
  verbose && str(verbose, head(IDXS))

  # Drop only if all index groups have exactly one file
  if (DROP) {
    ns <- sapply(IDXS, FUN=length)
    DROP <- all(ns == 1L)
  }

  # Still drop?
  if (DROP) {
    sets <- lapply(IDXS, FUN=function(idx) ds[[idx]])
  } else {
    sets <- vector("list", length=length(IDXS))
    for (gg in seq_along(IDXS)) {
      idxs <- IDXS[[gg]]
      set <- ds[idxs]
      # FIXME/BACKWARD COMPATIBLE? /HB 2014-03-30
      if (is.function(AS)) {
        set <- AS(set)
        if (identical(AS, as.list)) {
          name <- names(IDXS)[gg]
          if (!is.null(name)) names(set)[1L] <- name
        }
      }
      sets[[gg]] <- set
    } # for (gg ...)
  }
  names(sets) <- names(IDXS)
  if (is.null(names(sets))) {
    names(sets) <- sprintf("<Group #%d>", seq_along(sets))
  }
  verbose && str(verbose, head(sets))
  IDXS <- NULL; # Not needed anymore

  # FIXME/AD HOC/BACKWARD COMPATIBLE /HB 2014-03-30
  # Set attribute 'groupName' for each element.  This is used to pass
  # the group name to FUN().
  for (gg in seq_along(sets)) {
    set <- sets[[gg]]
    name <- names(sets)[gg]
    attr(set, "name") <- name
    sets[[gg]] <- set
  }

  # The additional set of arguments passed in each function call
  vargs <- c(vargs, args)
  allArgs <- c(vargs, list(skip=skip, verbose=verbose))


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Alt 1: Sequentially using regular R
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  res <- NULL
  if (parallel == "none") {
    # Allocate result list
    res <- vector("list", length=length(sets))
    names(res) <- names(sets)

    for (gg in seq_along(sets)) {
      name <- names(sets)[gg]
      verbose && enter(verbose, sprintf("Group #%d ('%s') of %d", gg, name, length(sets)))
      set <- sets[[gg]]
      verbose && print(verbose, set)
      argsGG <- c(list(set), allArgs)
      verbose && cat(verbose, "Call arguments:")
      argsT <- argsGG; argsT$verbose <- as.character(argsT$verbose)
      verbose && str(verbose, argsT)
      argsT <- NULL; # Not needed anymore
      resGG <- do.call(FUN, args=argsGG)
      verbose && str(verbose, resGG)

      # Record
      res[[gg]] <- resGG

      # Not needed anymore
      idxs <- set <- argsGG <- resGG <- NULL

      verbose && exit(verbose)
    } # for (gg ...)
    
    .Deprecated(msg = "R.filesets::dsApply(ds, FUN, ..., .parallel = 'none') is deprecated. Instead, use lapply(ds, FUN, ...) or future.apply::future_lapply(ds, FUN, ...) with plan(sequential).")
  } # if (parallel == "none")


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Alt 2: Evaluation using futures
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (parallel == "future") {
    verbose && enter(verbose, "Processing using futures")

    call_args <- list(sets, FUN = FUN)
    call_args <- c(call_args, allArgs)
    res <- do.call(future_lapply, args = call_args)

    ## Not needed anymore
    rm(list = "call_args")

    .Deprecated(msg = "R.filesets::dsApply(ds, FUN, ..., .parallel = 'future') is deprecated. Instead, use future.apply::future_lapply(ds, FUN, ...).")
    
    verbose && exit(verbose)
  } # if (parallel == "future")


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # DEFUNCT
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (parallel == "BatchJobs") {
    .Defunct(msg = "dsApply(ds, FUN, ..., .parallel = 'BatchJobs') is defunct. Instead, use future.apply::future_lapply(ds, FUN, ...) after library('future.BatchJobs') with plan(batchjobs_custom).")
  } else if (parallel == "BiocParallel::BatchJobs") {
    .Defunct(msg = "dsApply(ds, FUN, ..., .parallel = 'BiocParallel::BatchJobs') is defunct. Instead, use future.apply::future_lapply(ds, FUN, ...) after library('future.BatchJobs') with plan(batchjobs_custom).")
  }

  verbose && exit(verbose)

  res
}, protected=TRUE) # dsApply()



setMethodS3("dsApplyInPairs", "GenericDataFileSet", function(ds1, ds2, ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'ds1' & 'ds2':
  ds2 <- Arguments$getInstanceOf(ds2, class(ds1)[1L])
  stopifnot(length(ds2) == length(ds1))

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # dsApply() in pairs
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  dsP <- c(ds1, ds2)
  idxs <- matrix(seq_along(dsP), nrow=2L, byrow=TRUE)
  IDXS <- as.list(as.data.frame(idxs))
  names <- getFullNames(dsP)
  names(IDXS) <- sapply(IDXS, FUN=function(idxs) {
    sprintf("Pair (%s,%s)", names[idxs[1]], names[idxs[2]])
  })
  idxs <- names <- ds1 <- ds2 <- NULL  # Not needed anymore

  dsApply(dsP, IDXS=IDXS, ...)
}, protected=TRUE) # dsApplyInPairs()
