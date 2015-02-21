%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{loadToEnv}
\alias{loadToEnv}

\alias{loadToEnv.RDataFile}

\title{Reads data from a RDS file}

\description{
 Reads data from a RDS file.
}

\usage{
 \method{loadToEnv}{RDataFile}(file, ...)
}

\arguments{
  \item{file}{A \code{\link[base]{character}} string, a \code{\link[base:connections]{connection}}, or an \code{\link{RDataFile}}
   specifying an RData file to be read.}
  \item{...}{Additional arguments passed to \code{\link[R.utils]{loadToEnv}}.}
}

\value{
 Returns an \code{\link[base]{environment}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.utils]{loadToEnv}}.
}
\keyword{methods}
