setMethodS3("getTags", "Arguments", function(static, ..., na.rm=TRUE, collapse=",") {
  # Generate tags
  tags <- paste(..., sep=",", collapse=",");
  tags <- Arguments$getCharacters(tags);
  tags <- strsplit(tags, split=",", fixed=TRUE);
  tags <- unlist(tags);
  tags <- trim(tags);

  # Drop missing tags?
  if (na.rm) {
    tags <- tags[!is.na(tags)];
  }

  # Drop empty tags
  tags <- tags[nchar(tags) > 0];

  # Nothing to do?
  if (length(tags) == 0) {
    return(NULL);
  }

  # Collapse?
  if (!is.null(collapse)) {
    tags <- paste(tags, collapse=collapse);
  }

  tags;
}, static=TRUE, protected=TRUE)


############################################################################
# HISTORY:
# 2011-02-18
# o Moved getTags() for Arguments from aroma.core to R.filesets.
# 2010-01-25
# o Added static getTags() to Arguments.
# ...
############################################################################
