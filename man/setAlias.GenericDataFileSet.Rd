%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setAlias.GenericDataFileSet}
\alias{setAlias.GenericDataFileSet}
\alias{GenericDataFileSet.setAlias}
\alias{setAlias.GenericDataFileSet}
\alias{setAlias,GenericDataFileSet-method}


\title{Sets the alias of the file set}

\description{
  Sets the alias of the file set.
  If specified, the alias overrides the name inferred from the pathname
  of the file set.  This can be used in order to use another name of the
  output data set than the input data set of many transforms and models.
}

\usage{\method{setAlias}{GenericDataFileSet}(this, alias=NULL, ...)}

\arguments{
 \item{alias}{A \code{\link[base]{character}} string for the new alias of the file set.
  The alias must consists of valid filename characters, and must not
  contain commas, which are used to separate tags.}
 \item{...}{Not used.}
}

\value{
  Returns nothing.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
