#########################################################################/**
# @set "class=Arguments"
# @RdocMethod getFilename
#
# @title "Gets and validates a filename"
#
# \description{
#  @get "title".
# }
#
# @synopsis
#
# \arguments{
#   \item{filename}{A @character string.}
#   \item{nchar}{An @integer @vector of length two specifying the range
#     of valid filename lengths.}
#   \item{class}{A @character string specifying the class of valid
#     filenames.}
#   \item{.name}{The name of the argument validated.}
#   \item{.type}{Not used.}
# }
#
# \value{
#  Returns a @character string if filename is valid, 
#  otherwise an exception is thrown.
# }
#
# \details{
#   When argument \code{class="safe"}, the following 86 ASCII characters
#   are allowed in filenames:
#   \preformatted{
#      #$%&'()+,-.0123456789;=         (24 including initial space)
#     @ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_  (31)
#     `abcdefghijklmnopqrstuvwxyz{|}~  (31)
#   }
#   This class of filenames has been extensively tested on for 
#   cross-platform support on Microsoft Windows, OSX and various 
#   Unix flavors.
# }
#
# \references{
#   [1] Microsoft, \emph{Naming Files, Paths, and Namespaces} (Section 'Windows Naming Conventions'), 2012. \url{http://msdn.microsoft.com/en-us/library/aa365247.aspx#naming_conventions}.
# }
#
# @author
#
# \seealso{
#   For more information see \code{\link[R.utils]{Arguments}}.
# }
#*/######################################################################### 
setMethodS3("getFilename", "Arguments", function(static, filename, nchar=c(1,128), class=c("safe"), .name=NULL, .type="filename", ...) {
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Validate arguments
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Argument '.name':
  if (is.null(.name)) {
    .name <- as.character(deparse(substitute(filename)));
  }

  # Argument 'filename':
  filename <- getCharacter(static, filename, nchar=nchar, .name=.name);

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
    chars <- gsub("[=]", "", chars);
  }

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Check for remaining (=invalid) characters
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (nchar(chars) > 0) {
    chars <- unlist(strsplit(chars, split=""));
    chars <- sort(unique(chars));
    chars <- sprintf("'%s'", chars);
    chars <- paste(chars, collapse=", ");
    throw(sprintf("Not a valid %s. Argument '%s' contains non-valid %s characters (%s): %s", .type, .name, .type, chars, filename));
  }

  filename;
}, static=TRUE, private=TRUE)


# OLD NOTES:
#   Valid filename characters:
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


############################################################################
# HISTORY:
# 2012-10-16
# o Added Rdoc comments.
# 2011-03-09
# o Added '=' to the list of safe characters for Arguments$getFilename().
# 2010-11-19
# o Now Arguments$getFilename() correctly reports the name of the argument.
# o Added argument private arguments .name and .type to getFilename().
# 2006-11-20
# o Added static getFilename() to Arguments to check if a string contains
#   valid filename characters.
############################################################################
