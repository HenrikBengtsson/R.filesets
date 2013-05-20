%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setName.FullNameInterface}
\alias{setName.FullNameInterface}
\alias{FullNameInterface.setName}
\alias{setName,FullNameInterface-method}

\title{Sets the name part of the fullname}

\description{
  Sets the name part of the fullname, leaving the tags untouched.
}

\usage{
\method{setName}{FullNameInterface}(this, name=NULL, ...)
}

\arguments{
 \item{name}{The new name part of the fullname.
   If \code{\link[base]{NULL}}, then the fullname is reset to the default.}
 \item{...}{Not used.}
}

\value{
  Returns (invisibly) itself.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:setFullName.FullNameInterface]{*setFullName}()}.
  \code{\link[R.filesets:getName.FullNameInterface]{*getName}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
