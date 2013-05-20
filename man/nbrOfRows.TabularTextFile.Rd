%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{nbrOfRows.TabularTextFile}
\alias{nbrOfRows.TabularTextFile}
\alias{TabularTextFile.nbrOfRows}
\alias{nbrOfRows,TabularTextFile-method}

\title{Counts the number of data rows}

\description{
 Counts the number of data rows.  The count does not include the header rows or comments.
}

\usage{
\method{nbrOfRows}{TabularTextFile}(this, fast=FALSE, ...)
}

\arguments{
  \item{fast}{Argument passed to \code{\link[R.filesets:nbrOfLines.TabularTextFile]{*nbrOfLines}()}.}
  \item{...}{Optional arguments passed to \code{\link[R.filesets:getHeader.TabularTextFile]{*getHeader}()}.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}}.
}
\author{Henrik Bengtsson}

\seealso{
   The number of data rows is always less or equal to the number of lines
   as returned by \code{\link[R.filesets:nbrOfLines.TabularTextFile]{*nbrOfLines}()}.
  Internally, \code{\link[R.utils]{countLines}} is used.
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
