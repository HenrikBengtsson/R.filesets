%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getTags.GenericDataFile}
\alias{getTags.GenericDataFile}
\alias{GenericDataFile.getTags}
\alias{getTags.GenericDataFile}
\alias{getTags,GenericDataFile-method}


\title{Gets the tags of the file}

\description{
  Gets the tags of the file.
}

\usage{\method{getTags}{GenericDataFile}(this, pattern=NULL, collapse=NULL, ...)}

\arguments{
 \item{pattern}{An optional regular expression used to filter out tags.
    If \code{\link[base]{NULL}}, all tags are returned.}
 \item{collapse}{A \code{\link[base]{character}} string used to concatenate the tags.
    If \code{\link[base]{NULL}}, the tags are not concatenated.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}} or \code{\link[base]{NULL}}.
}

\details{
 The \emph{tags} of a filename are the comma separated parts of the
 filename that follows the the first comma, if any, and that preceeds the
 last period (the filename extension).
 For instance, the tags of \code{path/to/foo,a.2,b.ext} are
 \code{a.2} and \code{b}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getName.GenericDataFile]{*getName}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
