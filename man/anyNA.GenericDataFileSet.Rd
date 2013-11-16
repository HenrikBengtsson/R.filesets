%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{anyNA.GenericDataFileSet}
\alias{anyNA.GenericDataFileSet}
\alias{GenericDataFileSet.anyNA}
\alias{anyNA,GenericDataFileSet-method}
\alias{anyNA}  %% To be removed when depending on R (>= 3.1.0)

\title{Checks whether any of the pathnames are missing}

\description{
  Checks whether any of the pathnames are missing.
  Note that this only tests the \emph{pathnames} of files,
  but it does not test whether the files exists or not.
}

\usage{
\method{anyNA}{GenericDataFileSet}(x, ...)
}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
