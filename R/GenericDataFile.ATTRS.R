setMethodS3("getAttributes", "GenericDataFile", function(this, ...) {
  attrs <- this$.attributes
  if (length(attrs) == 0) {
    attrs <- list()
  } else {
    # Always return attributes in lexicographic order by names
    names <- names(attrs)
    if (length(names) > 0) {
      o <- order(names)
      attrs <- attrs[o]
    }
  }
  attrs
}, protected=TRUE)



setMethodS3("setAttributes", "GenericDataFile", function(this, ...) {
  # Argument '...':
  args <- list(...)
  names <- names(args)
  if (is.null(names)) {
    throw("No named arguments specified.")
  }
  
  # Update the attributes.
  attrs <- this$.attributes
  attrs[names] <- args
  this$.attributes <- attrs

  invisible(args)
}, protected=TRUE)



setMethodS3("getAttribute", "GenericDataFile", function(this, name, defaultValue=NULL, ...) {
  attrs <- this$.attributes
  if (name %in% names(attrs)) {
    value <- attrs[[name]]
  } else {
    value <- defaultValue
  }
  value
}, protected=TRUE)



setMethodS3("setAttribute", "GenericDataFile", function(this, name, value, ...) {
  attrs <- this$.attributes
  attrs[[name]] <- value
  this$.attributes <- attrs

  invisible(attrs[name])
}, protected=TRUE)



setMethodS3("testAttributes", "GenericDataFile", function(this, select, ...) {
  # Get the attributes to be tested
  attrs <- getAttributes(this)
  expr <- substitute(select)
  res <- eval(expr, envir=attrs, enclos=parent.frame())
  res
}, protected=TRUE)



setMethodS3("setAttributesBy", "GenericDataFile", function(this, object, ...) {
  if (inherits(object, "character")) {
    setAttributesByTags(this, object, ...)
  } else {
    throw("Unknown type on argument 'object': ", class(object)[1])
  }
}, protected=TRUE)



setMethodS3("setAttributesByTags", "GenericDataFile", function(this, tags=getTags(this), ...) {
  # Split tags
  if (length(tags) > 0) {
    tags <- unlist(strsplit(tags, split=","), use.names=FALSE)
    tags <- trim(tags)
  }

  newAttrs <- list()

  # Get all <name>=<value> tags
  pattern <- "^([abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]+)=(.*)$"
  values <- grep(pattern, tags, value=TRUE)
  for (kk in seq_along(values)) {
    tag <- values[[kk]]
    key <- gsub(pattern, "\\1", tag)
    value <- gsub(pattern, "\\2", tag)

    # Try to coerce:
    suppressWarnings({
      value2 <- as.integer(value)
      if (!identical(value2 == value, TRUE)) {
        value2 <- as.double(value)
        if (!identical(value2 == value, TRUE)) {
          value2 <- as.character(value)
        }
      }
      value <- value2
    })

    newAttrs <- c(newAttrs, setAttribute(this, key, value))
  }

  # Return updated attributes
  invisible(newAttrs)
}, protected=TRUE)
