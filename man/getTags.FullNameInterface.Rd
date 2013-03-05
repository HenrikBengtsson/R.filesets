%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getTags.FullNameInterface}
\alias{getTags.FullNameInterface}
\alias{FullNameInterface.getTags}
\alias{getTags.FullNameInterface}
\alias{getTags,FullNameInterface-method}


\title{Gets the tags}

\description{
  Gets the tags.
}

\usage{
\method{getTags}{FullNameInterface}(this, pattern=NULL, collapse=NULL, ..., named=FALSE, na.rm=TRUE, useCustomTags=TRUE)
}

\arguments{
 \item{pattern}{An optional regular expression used to filter out tags.
    If \code{\link[base]{NULL}}, all tags are returned.}
 \item{collapse}{A \code{\link[base]{character}} string used to concatenate the tags.
    If \code{\link[base]{NULL}}, the tags are not concatenated.}
 \item{...}{Additional arguments passed to \code{\link[R.filesets:getFullName.FullNameInterface]{*getFullName}()}.}
 \item{named}{If \code{\link[base:logical]{TRUE}}, tags of format "<name>=<value>" will be parsed
    as \emph{named} "<value>" tags, e.g. \code{"foo,n=23,bar,n=42"} is
    parsed to \code{c("foo", "n"="23", "bar", "n"="42")}.
    Note that if \code{collapse} is \code{\link[base:logical]{FALSE}}, the names will be dropped.
 }
 \item{na.rm}{If \code{\link[base:logical]{TRUE}} and the fullname is \code{\link[base]{NA}}, then \code{\link[base]{NULL}} is returned,
    otherwise (character) \code{\link[base]{NA}} is returned.}
 \item{useCustomTags}{If \code{\link[base:logical]{TRUE}}, custom tags are used, otherwise not.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}} (possibly \code{\link[base]{NA}}) or \code{\link[base]{NULL}}.
}

\details{
 The \emph{tags} of a fullname are the comma separated parts of the
 fullname that follows the the first comma, if any.
 For instance, the tags of \code{foo,a.2,b} are \code{a.2} and \code{b}.

 Any custom tag that equals \code{"*"} is replaced by the comma separated
 tags from the fullname.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getFullName.FullNameInterface]{*getFullName}()}.
  \code{\link[R.filesets:getName.FullNameInterface]{*getName}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
