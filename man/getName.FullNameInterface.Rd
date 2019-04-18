%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getName.FullNameInterface}
\alias{getName.FullNameInterface}
\alias{FullNameInterface.getName}
\alias{getName,FullNameInterface-method}
\alias{getName}

\title{Gets the name}

\description{
  Gets the name.
}

\usage{
\method{getName}{FullNameInterface}(this, ...)
}

\arguments{
 \item{...}{Additional arguments passed to \code{\link[R.filesets:getFullName.FullNameInterface]{*getFullName}()}.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 The name is the part of the fullname that precedes any comma.
 For instance, the name of \code{foo,a.2,b} is \code{foo}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getFullName.FullNameInterface]{*getFullName}()}.
  \code{\link[R.filesets:getTags.FullNameInterface]{*getTags}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
