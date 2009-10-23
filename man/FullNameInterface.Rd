%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{FullNameInterface}
\docType{class}
\alias{FullNameInterface}


 \title{The FullNameInterface class interface}

 \description{
  Package:  R.filesets \cr
\bold{Class FullNameInterface}\cr

\code{\link[R.oo]{Interface}}\cr
\code{~~|}\cr
\code{~~+--}\emph{\code{FullNameInterface}}\cr

\bold{Directly known subclasses:}\cr
\link[R.filesets]{GenericDataFile}, \link[R.filesets]{GenericDataFileSet}, \emph{\link[R.filesets]{GenericTabularFile}}, \link[R.filesets]{GenericTabularFileSet}, \link[R.filesets]{TabularTextFile}, \link[R.filesets]{TabularTextFileSet}\cr

public abstract class \bold{FullNameInterface}\cr
extends \link[R.oo]{Interface}\cr


 }

 \usage{FullNameInterface(...)}

 \arguments{
   \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{clearFullNameTranslator} \tab  -\cr
 \tab \code{\link[R.filesets:getFullName.FullNameInterface]{getFullName}} \tab Gets the full name.\cr
 \tab \code{\link[R.filesets:getName.FullNameInterface]{getName}} \tab Gets the name.\cr
 \tab \code{\link[R.filesets:getTags.FullNameInterface]{getTags}} \tab Gets the tags.\cr
 \tab \code{hasTag} \tab  -\cr
 \tab \code{hasTags} \tab  -\cr
 \tab \code{setFullNameTranslator} \tab  -\cr
 \tab \code{\link[R.filesets:setTags.FullNameInterface]{setTags}} \tab Sets the tags.\cr
 \tab \code{updateFullName} \tab  -\cr
}


 \bold{Methods inherited from Interface}:\cr
extend, print, uses


 }

 \details{
  The full name consists of a name followed by optional comma-separated tags.
  For instance, the full name of \code{foo,a.2,b} has name \code{foo} with
  tags \code{a.2} and \code{b}.
 }

 \examples{
# Setup a file set
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)

# List the fullnames of all files
cat("Default fullnames:\n")
print(getFullNames(ds))

# Translate fullnames to lower case
setFullNamesTranslator(ds, function(names, ...) tolower(names))
cat("Lower-case fullnames:\n")
print(getFullNames(ds))

# Append a translator that reverse the order of the letters
revStr <- function(names, ...) {
  names <- strsplit(names, split="", fixed=TRUE)
  names <- lapply(names, FUN=rev)
  names <- sapply(names, FUN=paste, collapse="")
  names
}
appendFullNamesTranslator(ds, revStr)
cat("Reversed lower-case fullnames:\n")
fn3 <- getFullNames(ds)
print(fn3)


# Alternative for setting up a sequence of translators
setFullNamesTranslator(ds, list(function(names, ...) tolower(names), revStr))
cat("Reversed lower-case fullnames:\n")
fn3b <- getFullNames(ds)
print(fn3b)
stopifnot(identical(fn3b, fn3))

# Reset
clearFullNamesTranslator(ds)
cat("Default fullnames (after resetting):\n")
print(getFullNames(ds))
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
