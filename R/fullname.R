setMethodS3("fullname", "default", function(name, tags=NULL, ..., collapse=TRUE) {
  args <- list(name, tags, ...);
  parts <- unlist(args, use.names=FALSE);
  parts <- paste(parts, collapse=",");
  if (!collapse) {
    parts <- strsplit(parts, split=",", fixed=TRUE);
    parts <- unlist(parts, use.names=FALSE);
  }
  parts;
})


setMethodS3("name", "default", function(...) {
  fullname <- fullname(...);
  parts <- strsplit(fullname, split=",", fixed=TRUE);
  parts <- unlist(parts, use.names=FALSE);
  name <- parts[1];
  name;
})


setMethodS3("tags", "default", function(..., collapse=FALSE) {
  fullname <- fullname(...);
  parts <- strsplit(fullname, split=",", fixed=TRUE);
  parts <- unlist(parts, use.names=FALSE);
  tags <- parts[-1];
  if (collapse) {
    tags <- paste(tags, collapse=","); 
  }
  tags;
})


############################################################################
# HISTORY:
# 2011-03-08
# o Added fullname(), name(), and tags().  Amazing that I never thought
#   of having these very basic functions.  They will make some code
#   and examples much cleaner.  They are also great for illustrating
#   the definition of fullnames, names and tags.
# o Created.
############################################################################  
