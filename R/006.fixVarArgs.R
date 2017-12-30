append <- function(...) UseMethod("append");
setMethodS3("append", "default", function(...) {
  base::append(...);
})

readLines <- function(...) UseMethod("readLines");
setMethodS3("readLines", "default", function(...) {
  base::readLines(...);
})
