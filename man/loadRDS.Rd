%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RdsFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{loadRDS}
\alias{loadRDS}

\alias{loadRDS.default}
\alias{loadRDS.RdsFile}

\title{Reads data from a RDS file}

\description{
 Reads data from a RDS file.
}

\usage{
 \method{loadRDS}{default}(file, ...)
 \method{loadRDS}{RdsFile}(file, ...)
}

\arguments{
  \item{file}{A \code{\link[base]{character}} string, a \code{\link[base:connections]{connection}}, or an \code{\link{RdsFile}}
   specifying a RDS file/connection to be read.}
  \item{...}{Additional arguments passed to \code{\link[base]{readRDS}}().}
}

\value{
 Returns an R object.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[base]{readRDS}}().
}
\keyword{methods}
