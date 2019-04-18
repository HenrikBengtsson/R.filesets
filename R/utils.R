.stop_if_not <- function(...) {
  res <- list(...)
  n <- length(res)
  if (n == 0L) return()

  for (ii in 1L:n) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "...")
        stop(sQuote(call), " is not TRUE", call. = FALSE, domain = NA)
    }
  }
}

.validate_nextmethod_call <- function(call = sys.call(sys.parent())) {
  ast <- as.list(call)
  if (length(ast) <= 2L) return()
  names <- names(ast[-(1:2)])
  if (is.null(names) || any(names == "")) {
    stop(sprintf("Function %s() requires that all arguments beyond the first one are passed by name and not by position: %s", as.character(call[[1L]]), deparse(call, width.cutoff = 100L)))
  }
}
