%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDefaultFullName.GenericDataFile}
\alias{getDefaultFullName.GenericDataFile}
\alias{GenericDataFile.getDefaultFullName}
\alias{getDefaultFullName.GenericDataFile}
\alias{getDefaultFullName,GenericDataFile-method}


\title{Gets the full name of the file}

\description{
  Gets the full name of the file, that is the filename without the extension.
}

\usage{\method{getDefaultFullName}{GenericDataFile}(this, aliased=FALSE, ...)}

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
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
