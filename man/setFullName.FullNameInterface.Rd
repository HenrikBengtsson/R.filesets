%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setFullName.FullNameInterface}
\alias{setFullName.FullNameInterface}
\alias{FullNameInterface.setFullName}
\alias{setFullName,FullNameInterface-method}

\title{Sets the full name}

\description{
  Sets the full name.
  This is done using a fullname translator function that returns the
  specified fullname.
}

\usage{
\method{setFullName}{FullNameInterface}(this, ...)
}

\arguments{
 \item{...}{Arguments, typically a \code{\link[base]{character}} string, which are
    passed to the fullname translator generator.
 }
}

\value{
  Returns (invisibly) itself.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getFullName.FullNameInterface]{*getFullName}()}.
  \code{\link[R.filesets:setName.FullNameInterface]{*setName}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
