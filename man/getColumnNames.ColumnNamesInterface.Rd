%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ColumnNamesInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColumnNames.ColumnNamesInterface}
\alias{getColumnNames.ColumnNamesInterface}
\alias{ColumnNamesInterface.getColumnNames}
\alias{getColumnNames,ColumnNamesInterface-method}

\title{Gets the column names}

\description{
  Gets the column names.
}

\usage{
\method{getColumnNames}{ColumnNamesInterface}(this, ..., translate=TRUE)
}

\arguments{
 \item{translate}{If \code{\link[base:logical]{TRUE}} and a names translator is set, the
    column names are translated before returned.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getDefaultColumnNames.ColumnNamesInterface]{*getDefaultColumnNames}()}.
  For more information see \code{\link{ColumnNamesInterface}}.
}
\keyword{internal}
\keyword{methods}
