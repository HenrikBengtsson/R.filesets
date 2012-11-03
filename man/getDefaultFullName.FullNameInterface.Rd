%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDefaultFullName.FullNameInterface}
\alias{getDefaultFullName.FullNameInterface}
\alias{FullNameInterface.getDefaultFullName}
\alias{getDefaultFullName.FullNameInterface}
\alias{getDefaultFullName,FullNameInterface-method}


\title{Gets the default full name}

\description{
  Gets the default full name, that is, the fullname without translations.
}

\usage{\method{getDefaultFullName}{FullNameInterface}(...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 The full name of a file is the filename excluding any
 extension (and period).
 For instance, the full name of \code{path/to/foo,a.2,b.ext} is
 \code{foo,a.2,b}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getName.FullNameInterface]{*getName}()}.
  \code{\link[R.filesets:getTags.FullNameInterface]{*getTags}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
