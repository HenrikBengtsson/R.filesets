%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{nbrOfLines.TabularTextFile}
\alias{nbrOfLines.TabularTextFile}
\alias{TabularTextFile.nbrOfLines}
\alias{nbrOfLines.TabularTextFile}
\alias{nbrOfLines,TabularTextFile-method}


\title{Counts the number of lines in the data file}

\description{
 Counts the number of lines in the data file.  The count include header rows, comments and more.
}

\usage{\method{nbrOfLines}{TabularTextFile}(this, fast=FALSE, ...)}

\arguments{
  \item{fast}{If \code{\link[base:logical]{TRUE}}, \code{\link[base]{NA}} is returned for large data files (>1Mb),
    unless the number of lines has already been counted.}
  \item{...}{Optional arguments passed to \code{\link[R.utils]{countLines}}.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
   To count the number of data rows is the data table,
   use \code{\link[R.filesets:nbrOfRows.TabularTextFile]{*nbrOfRows}()}.
  Internally, \code{\link[R.utils]{countLines}} is used.
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
