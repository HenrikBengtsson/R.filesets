%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFullName.FullNameInterface}
\alias{getFullName.FullNameInterface}
\alias{FullNameInterface.getFullName}
\alias{getFullName,FullNameInterface-method}

\title{Gets the full name}

\description{
  Gets the full name consisting of a name and tags.
}

\usage{
\method{getFullName}{FullNameInterface}(this, ..., translate=TRUE)
}

\arguments{
 \item{translate}{If \code{\link[base:logical]{TRUE}} and a fullname translator is set, the fullname
    is translated before returned.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getName.FullNameInterface]{*getName}()}.
  \code{\link[R.filesets:getTags.FullNameInterface]{*getTags}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
