%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getHeader.TabularTextFile}
\alias{getHeader.TabularTextFile}
\alias{TabularTextFile.getHeader}
\alias{getHeader,TabularTextFile-method}

\title{Gets the file header}

\description{
 Gets the file header.
}

\usage{
\method{getHeader}{TabularTextFile}(this, ..., header=TRUE, force=FALSE)
}

\arguments{
  \item{...}{Passed to internal \code{\link[R.filesets:readRawHeader.TabularTextFile]{*readRawHeader}()}.}
  \item{header}{A \code{\link[base]{logical}} specifying whether there are column
   headers or not.}
  \item{force}{If \code{\link[base:logical]{TRUE}}, an already retrieved header will be ignored.}
}

\value{
  Returns a named \code{\link[base]{list}}.
}
\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
