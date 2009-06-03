%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFilename.GenericDataFile}
\alias{getFilename.GenericDataFile}
\alias{GenericDataFile.getFilename}
\alias{getFilename.GenericDataFile}
\alias{getFilename,GenericDataFile-method}


\title{Gets the filename of the file}

\description{
  Gets the filename of the file.
}

\usage{\method{getFilename}{GenericDataFile}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 The filename of a file is the pathname excluding any path.
 For instance, the filename of \code{path/to/foo,a.2,b.ext} is
 \code{foo,a.2,b.ext}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
