#
# Valid filename characters:
# * The FTP RFCs require (7-bit) ASCII characters (and presumably not control
#   characters either). The 95 printable ASCII characters are (note initial 
#   space):
# 
#    !"#$%&'()*+,-./0123456789:;<=>?  (32)
#   @ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_  (32)
#   `abcdefghijklmnopqrstuvwxyz{|}~   (31)
# 
# * On Windows the following 9 characters aren't allowed: \ / : * ? " < > !.  
#   This leaves us with:
# 
#    #$%&'()+,-.0123456789;=          (24)
#   @ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_   (31)
#   `abcdefghijklmnopqrstuvwxyz{|}~   (31)
#
#
setMethodS3("getFilename", "Arguments", function(static, filename, nchar=c(1,64), class=c("safe"), ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument 'filename':
  filename <- getCharacter(static, filename, nchar=nchar);

  # Argument 'class':
  class <- match.arg(class);

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Filter out valid characters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  chars <- filename;

  # Always valid characters
  chars <- gsub("[abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0-9_.,]", "", chars);
  chars <- gsub("[-]", "", chars);
  chars <- gsub("[+]", "", chars);

  # Filter out according to classes.
  if ("safe" %in% class) {
    chars <- gsub("[ ]", "", chars);
    chars <- gsub("[\\[\\]]", "", chars);
    chars <- gsub("[#$%&'()`{|}~]", "", chars);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for remaining (=invalid) characters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (nchar(chars) > 0) {
    chars <- unlist(strsplit(chars, split=""));
    chars <- sort(unique(chars));
    chars <- paste(chars, collapse="");
    throw("Not a valid filename. Argument 'filename' contains non-valid filename characters (", chars, "): ", filename);
  }

  filename;
}, static=TRUE, private=TRUE)


############################################################################
# HISTORY:
# 2006-11-20
# o Added static getFilename() to Arguments to check if a string contains
#   valid filename characters.
############################################################################
