###########################################################################/**
# @RdocClass FullNameInterface
#
# @title "The FullNameInterface class interface"
#
# \description{
#  @classhierarchy
# }
# 
# @synopsis
#
# \arguments{
#   \item{...}{Not used.}
# }
#
# \section{Fields and Methods}{
#  @allmethods "public"
# }
#
# \details{
#  The full name consists of a name followed by optional comma-separated tags.
#  For instance, the full name of \code{foo,a.2,b} has name \code{foo} with
#  tags \code{a.2} and \code{b}.
# }
#
# @examples "../incl/FullNameInterface.Rex"
#
# @author
#*/########################################################################### 
setConstructorS3("FullNameInterface", function(...) {
  extend(Interface(), "FullNameInterface");
})





###########################################################################/**
# @RdocMethod getDefaultFullName
#
# @title "Gets the default full name"
#
# \description{
#   @get "title", that is, the fullname without translations.
# }
#
# @synopsis
#
# \arguments{
#  \item{aliased}{If @TRUE, and an alias has been set, the alias is 
#     returned, otherwise the default full name is returned.}
#  \item{translate}{If @TRUE, an a fullname translator is set, the fullname
#     is translated before returned.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# \details{
#  The full name of a file is the filename excluding any
#  extension (and period).
#  For instance, the full name of \code{path/to/foo,a.2,b.ext} is 
#  \code{foo,a.2,b}.
# }
#
# @author
#
# \seealso{
#   @seemethod "getName".
#   @seemethod "getTags".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getDefaultFullName", "FullNameInterface", abstract=TRUE, protected=TRUE);



###########################################################################/**
# @RdocMethod getFullName
#
# @title "Gets the full name"
#
# \description{
#   @get "title" consisting of a name and tags.
# }
#
# @synopsis
#
# \arguments{
#  \item{translate}{If @TRUE, an a fullname translator is set, the fullname
#     is translated before returned.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# @author
#
# \seealso{
#   @seemethod "getName".
#   @seemethod "getTags".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getFullName", "FullNameInterface", function(this, ..., translate=TRUE) {
  fullname <- getDefaultFullName(this, ...);

  # Translate?
  if (translate) {
    fullname <- translateFullName(this, fullname);
  } 

  fullname;
})



###########################################################################/**
# @RdocMethod getName
#
# @title "Gets the name"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character.
# }
#
# \details{
#  The name is the part of the fullname that preceeds any comma.
#  For instance, the name of \code{foo,a.2,b} is \code{foo}.
# }
#
# @author
#
# \seealso{
#   @seemethod "getFullName".
#   @seemethod "getTags".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getName", "FullNameInterface", function(this, ...) {
  name <- getFullName(this, ...);

  # Keep anything before the first comma
  name <- gsub("[,].*$", "", name);
  
  name;
})



###########################################################################/**
# @RdocMethod getTags
#
# @title "Gets the tags"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{pattern}{An optional regular expression used to filter out tags.
#     If @NULL, all tags are returned.}
#  \item{collapse}{A @character string used to concatenate the tags. 
#     If @NULL, the tags are not concatenated.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns a @character @vector or @NULL.
# }
#
# \details{
#  The \emph{tags} of a fullname are the comma separated parts of the
#  fullname that follows the the first comma, if any.
#  For instance, the tags of \code{foo,a.2,b} are \code{a.2} and \code{b}.
#
#  Any custom tag that equals \code{"*"} is replaced by the comma separated
#  tags from the fullname.
# }
#
# @author
#
# \seealso{
#   @seemethod "getFullName".
#   @seemethod "getName".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("getTags", "FullNameInterface", function(this, pattern=NULL, collapse=NULL, ...) {
  fullname <- getFullName(this, ...);

  # The name is anything before the first comma
  name <- gsub("[,].*$", "", fullname);

  # Keep anything after the name (and the separator).
  tags <- substring(fullname, nchar(name)+2);  
  tags <- unlist(strsplit(tags, split=","));

  # Are there custom tags?
  customTags <- this$.tags;
  if (length(customTags) > 0) {
    # Replace asterisk custom tags?
    if (is.element("*", customTags)) {
      pos <- whichVector("*" == customTags);
      customTags <- customTags[-pos];
      
      asteriskTags <- tags;
      if (length(asteriskTags) > 0) {
        if (length(customTags) == 0) {
          customTags <- asteriskTags;
        } else {
          customTags <- R.utils::insert.default(customTags, pos[1], asteriskTags); 
        }
      }
    }
    tags <- customTags;
  }

  # Keep only those matching a regular expression?
  if (!is.null(pattern)) {
    tags <- grep(pattern, tags, value=TRUE);
  }

  # Collapsed or split?
  if (!is.null(collapse)) {
    tags <- paste(tags, collapse=collapse);
  } else {
    tags <- unlist(strsplit(tags, split=","));
  }
 
  if (length(tags) == 0)
    tags <- NULL;

  tags;
})


setMethodS3("hasTags", "FullNameInterface", function(this, tags, ...) {
  tags <- strsplit(tags, split=",", fixed=TRUE);
  tags <- unlist(tags, use.names=FALSE);
  all(tags %in% getTags(this));
})

setMethodS3("hasTag", "FullNameInterface", function(this, tag, ...) {
  hasTags(this, tags=tag, ...);
})


###########################################################################/**
# @RdocMethod setTags
#
# @title "Sets the tags"
#
# \description{
#   @get "title".
# }
#
# @synopsis
#
# \arguments{
#  \item{tags}{A @character @vector of tags.}
#  \item{...}{Not used.}
# }
#
# \value{
#   Returns nothing.
# }
#
# \details{
#   See @seemethod "getTags" for so called \emph{special tags}.
# }
#
# @author
#
# \seealso{
#   @seemethod "getTags".
#   @seeclass
# }
#*/###########################################################################
setMethodS3("setTags", "FullNameInterface", function(this, tags="*", ...) {
  # Argument 'tags':
  if (!is.null(tags)) {
    tags <- Arguments$getCharacters(tags);
    tags <- trim(unlist(strsplit(tags, split=",")));
    tags <- tags[nchar(tags) > 0];
  }
  
  this$.tags <- tags;
})


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# TRANSLATOR FUNCTIONS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethodS3("clearListOfFullNameTranslators", "FullNameInterface", function(this, ...) {
  this$.listOfFullNameTranslators <- list();
}, protected=TRUE)

setMethodS3("clearFullNameTranslator", "FullNameInterface", function(this, ...) {
  clearListOfFullNameTranslators(this);
})


setMethodS3("getListOfFullNameTranslators", "FullNameInterface", function(this, ...) {
  res <- this$.listOfFullNameTranslators;
  if (is.null(res)) {
    res <- list();
  }
  res;
}, protected=TRUE)

setMethodS3("setListOfFullNameTranslators", "FullNameInterface", function(this, fnList, ...) {
  # Argument 'fnList':
  for (kk in seq(along=fnList)) {
    fcn <- fnList[[kk]];
    if (!is.function(fcn)) {
      throw("Element #", kk, " of argument 'fnList' is not a function: ", 
                                                           class(fcn)[1]);
    }
  }

  this$.listOfFullNameTranslators <- fnList;

  invisible(this);
}, protected=TRUE)


setMethodS3("getFullNameTranslator", "FullNameInterface", function(this, ...) {
  fnList <- getListOfFullNameTranslators(this, ...);

  # No fullnames translator?
  if (length(fnList) == 0) {
    return(NULL);
  }

  # Create fullnames translator function
  res <- function(names, ...) {
    for (kk in seq(along=fnList)) {
      fcn <- fnList[[kk]];
      names <- fcn(names, ...);
    }
    names;
  }
  res;
}, protected=TRUE)



setMethodS3("translateFullName", "FullNameInterface", function(this, names, ...) {
  nameTranslator <- getFullNameTranslator(this);

  if (!is.null(nameTranslator)) {
    names2 <- nameTranslator(names, file=this);

    # Sanity check
    if (any(is.na(names2))) {
      throw("Failed to translate names. Some names were translated to NA:s ", 
            paste(head(names[is.na(names2)]), collapse=", "));
    }
    if (length(names2) != length(names)) {
      throw(sprintf("Failed to translate full names. The translator is erroneous, because it drops/adds some names (passed %d names but got %d names).", length(names), length(names2)));
    }
    names <- names2;

    if (identical(attr(names, "isFinal"), TRUE))
      return(names);
  }

  # Do nothing
  names;
}, private=TRUE)


setMethodS3("appendFullNameTranslatorByNULL", "FullNameInterface", function(this, ...) {
  # Nothing to append
  invisible(this);
}, protected=TRUE)

setMethodS3("appendFullNameTranslatorBycharacter", "FullNameInterface", function(this, fullname, ...) {
  # Append a translator function that always returns a constant string
  appendFullNameTranslator(this, function(...) { fullname });
}, protected=TRUE)


setMethodS3("appendFullNameTranslatorByfunction", "FullNameInterface", function(this, fcn, ...) {
  # Arguments 'fcn':
  if (!is.function(fcn)) {
    throw("Argument 'fcn' is not a function: ", class(fcn)[1]);
  }

  # Sanity check
  names <- c("foo bar");
  names <- fcn(names, file=this);

  fnList <- getListOfFullNameTranslators(this);
  fnList <- c(fnList, fcn);
  setListOfFullNameTranslators(this, fnList);
}, protected=TRUE)



setMethodS3("appendFullNameTranslatorBylist", "FullNameInterface", function(this, list, ...) {
  # Arguments 'list':
  if (!inherits(list, "list")) {
    throw("Argument 'list' is not a list: ", class(list)[1]);
  }

  for (kk in seq(along=list)) {
    by <- list[[kk]];
    appendFullNameTranslator(this, by, ...);
  }
}, protected=TRUE)



setMethodS3("appendFullNameTranslator", "FullNameInterface", function(this, by, ...) {
  # Arguments 'by':
  classNames <- class(by);
  methodNames <- sprintf("appendFullNameTranslatorBy%s", classNames);

  keep <- sapply(methodNames, FUN=exists, mode="function");
  methodNames <- methodNames[keep];

  if (length(methodNames) == 0) {
    throw("Failed to set the fullname translator. Could not find an appendFullNameTranslatorBy<className>() function for this object: ", paste(classNames, collapse=", "));
  }

  methodName <- methodNames[1];
  fcn <- get(methodName, mode="function");
  res <- fcn(this, by, ...);

  # Allow the object to update itself according to these new rules.
  updateFullName(this);

  invisible(res);
}, protected=TRUE)


setMethodS3("setFullNameTranslator", "FullNameInterface", function(this, ...) {
  clearListOfFullNameTranslators(this);
  appendFullNameTranslator(this, ...);
})



setMethodS3("setFullName", "FullNameInterface", function(this, ...) {
 # Set a translator function that always returns a constant
 setFullNameTranslator(this, ...);
}, protected=TRUE)



# Sets the name part of the fullname, leaving the tags untouched.
setMethodS3("setName", "FullNameInterface", function(this, name=NULL, ...) {
  # Argument 'name':
  if (!is.null(name)) {
    name <- Arguments$getCharacter(name);
  }

  if (is.null(name)) {
    clearFullNameTranslator(this);
  } else {
    # Set a translator function that always returns the same name
    setFullNameTranslator(this, function(fullname, ...) {
      parts <- strsplit(fullname, split=",", fixed=TRUE)[[1]];
      parts[1] <- name;
      fullname <- paste(parts, collapse=",");
      fullname;
    });
  }
}, protected=TRUE)



setMethodS3("updateFullName", "FullNameInterface", function(this, ...) {
})




############################################################################
# HISTORY:
# 2009-10-30
# o ROBUSTIFICATION: Now translateFullName() of FullNameInterface throws
#   an exception if some fullnames were translated into NA.
# 2009-10-23
# o Added appendFullNameTranslatorBylist() which makes it possible to do
#   setup a sequence of fullnames translators fnt1, fnt2, fnt3 by calling
#   setFullNameTranslator(..., list(fnt1, fnt2, fnt3)).
# 2009-10-22
# o Added support for having a sequence of fullname translator functions.
#   These can be added using appendFullNameTranslator().
# o Added an example() to FullNameInterface.
# 2009-10-02
# o Now setFullName(...) applies to any FullNameInterface.
# o Now setFullNameTranslator(...) applies to any FullNameInterface and 
#   dispatches on the 'translator' argument so that the generic function
#   setFullNameTranslatorBy<class>() is called.
# o Created.
############################################################################
