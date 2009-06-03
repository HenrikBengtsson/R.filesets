%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getTags.GenericDataFileSet}
\alias{getTags.GenericDataFileSet}
\alias{GenericDataFileSet.getTags}
\alias{getTags.GenericDataFileSet}
\alias{getTags,GenericDataFileSet-method}


\title{Gets the tags of the file set}

\description{
  Gets the tags of the file set.
  Any tag equals \code{"*"} is replaced by the comma separated tags part of
  the file-set pathname.
}

\usage{\method{getTags}{GenericDataFileSet}(this, collapse=NULL, ...)}

\arguments{
 \item{collapse}{A \code{\link[base]{character}} string used to concatenate the tags.
    If \code{\link[base]{NULL}}, the tags are not concatenated.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}} or \code{\link[base]{NULL}}.
}

\details{
 The \emph{tags} of a file set are the comma separated parts of the
 filename that follows the the first comma, if any, and that preceeds the
 last period (the filename extension).
 For instance, the tags of \code{path/to/foo,a.2,b.ext} are
 \code{a.2} and \code{b}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:setTags.GenericDataFileSet]{*setTags}()}.
  \code{\link[R.filesets:getName.GenericDataFileSet]{*getName}()}.
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
