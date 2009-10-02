%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getRawFullName.FullNameInterface}
\alias{getRawFullName.FullNameInterface}
\alias{FullNameInterface.getRawFullName}
\alias{getRawFullName.FullNameInterface}
\alias{getRawFullName,FullNameInterface-method}


\title{Gets the full name of the file}

\description{
  Gets the full name of the file, that is the filename without the extension.
}

\usage{\method{getRawFullName}{FullNameInterface}(...)}

\arguments{
 \item{aliased}{If \code{\link[base:logical]{TRUE}}, and an alias has been set, the alias is
    returned, otherwise the default full name is returned.}
 \item{translate}{If \code{\link[base:logical]{TRUE}}, an a fullname translator is set, the fullname
    is translated before returned.}
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
