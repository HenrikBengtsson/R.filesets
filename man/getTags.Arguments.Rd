%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  Arguments.EXTRAS.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{Arguments$getTags}
\alias{Arguments$getTags}
\alias{getTags.Arguments}
\alias{Arguments.getTags}
\alias{getTags,Arguments-method}

\title{Gets and validates tags}

\description{
 Gets and validates tags.
}

\usage{
## Static method (use this):
## Arguments$getTags(..., na.rm=TRUE, collapse=",")

## Don't use the below:
\method{getTags}{Arguments}(static, ..., na.rm=TRUE, collapse=",")
}

\arguments{
  \item{...}{\code{\link[base]{character}} \code{\link[base]{vector}} of tags.}
  \item{na.rm}{If \code{\link[base:logical]{TRUE}}, empty ("missing") tags are dropped.}
  \item{collapse}{A \code{\link[base]{character}} string specifying how the tags should
    be concatenated into a single string.
    If \code{\link[base]{NULL}}, they are not concattenated.}
}

\value{
 Returns a \code{\link[base]{character}} string or
 \code{\link[base]{character}} \code{\link[base]{vector}} (iff \code{collapse} is \code{\link[base]{NULL}}).
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link[R.utils]{Arguments}}.
}
\keyword{internal}
\keyword{methods}
