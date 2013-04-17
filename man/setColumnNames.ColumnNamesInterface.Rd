%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  ColumnNamesInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{setColumnNames.ColumnNamesInterface}
\alias{setColumnNames.ColumnNamesInterface}
\alias{ColumnNamesInterface.setColumnNames}
\alias{setColumnNames,ColumnNamesInterface-method}

\title{Sets the column names}

\description{
  Sets the column names.
  This is done using a names translator function that returns the
  specified names.
}

\usage{
\method{setColumnNames}{ColumnNamesInterface}(this, ...)
}

\arguments{
 \item{...}{Arguments, typically a \code{\link[base]{character}} string, which are
    passed to the names translator generator.
 }
}

\value{
  Returns (invisibly) itself.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getColumnNames.ColumnNamesInterface]{*getColumnNames}()}.
  For more information see \code{\link{ColumnNamesInterface}}.
}
\keyword{internal}
\keyword{methods}
