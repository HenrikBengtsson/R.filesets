%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getName.GenericDataFile}
\alias{getName.GenericDataFile}
\alias{GenericDataFile.getName}
\alias{getName.GenericDataFile}
\alias{getName,GenericDataFile-method}


\title{Gets the name of the file}

\description{
  Gets the name of the file.
}

\usage{\method{getName}{GenericDataFile}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 The name of a file is the part of the filename without the extension and
 that preceeds any comma.
 For instance, the name of \code{path/to/foo,a.2,b.ext} is \code{foo}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getTags.GenericDataFile]{*getTags}()}.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
