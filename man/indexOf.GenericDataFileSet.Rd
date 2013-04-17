%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{indexOf.GenericDataFileSet}
\alias{indexOf.GenericDataFileSet}
\alias{GenericDataFileSet.indexOf}
\alias{indexOf,GenericDataFileSet-method}

\title{Finds indices of a subset of files}

\description{
  Finds indices of a subset of files whose names match a given set of names or name patterns.
}

\usage{
\method{indexOf}{GenericDataFileSet}(this, patterns=NULL, ..., onMissing=c("NA", "error"))
}

\arguments{
 \item{patterns}{A \code{\link[base]{character}} \code{\link[base]{vector}} of length K of names and/or
  regular expressions to be matched.}
 \item{...}{Not used.}
}

\value{
  Returns an \code{\link[base]{integer}} \code{\link[base]{vector}} of length K with elements in
  [1,\code{\link[R.filesets:length.GenericDataFileSet]{*length}()}] or (integer) \code{\link[base]{NA}} (for non-matched names).
}

\details{
  If an element of \code{patterns} contains a comma, then that element
  is matched against the \code{\link[R.filesets:getFullNames.GenericDataFileSet]{*getFullNames}()}, otherwise it is
  matched against \code{\link[R.filesets:getNames.GenericDataFileSet]{*getNames}()}.
  First each pattern is matched by regular expression rules, and if
  there is not match, then by fixed strings.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getNames.GenericDataFileSet]{*getNames}()}
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
