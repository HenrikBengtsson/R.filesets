%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getColumnNames.TabularTextFile}
\alias{getColumnNames.TabularTextFile}
\alias{TabularTextFile.getColumnNames}
\alias{getColumnNames.TabularTextFile}
\alias{getColumnNames,TabularTextFile-method}


\title{Gets the column names}

\description{
 Gets the column names, either by inferring the from the file or using the
 preset column names.
}

\usage{\method{getColumnNames}{TabularTextFile}(this, ..., translate=TRUE)}

\arguments{
  \item{...}{Possibly passed \code{\link[R.filesets:getHeader.TabularTextFile]{*getHeader}()}.}
  \item{translate}{If \code{\link[base:logical]{TRUE}}, column names translators are applied,
   otherwise not.}
}

\value{
  Returns \code{\link[base]{character}} \code{\link[base]{vector}}.
}
\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
