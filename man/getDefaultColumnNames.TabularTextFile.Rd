%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDefaultColumnNames.TabularTextFile}
\alias{getDefaultColumnNames.TabularTextFile}
\alias{TabularTextFile.getDefaultColumnNames}
\alias{getDefaultColumnNames.TabularTextFile}
\alias{getDefaultColumnNames,TabularTextFile-method}


\title{Gets the default column names}

\description{
 Gets the default column names by inferring the from the file header.
}

\usage{\method{getDefaultColumnNames}{TabularTextFile}(this, ...)}

\arguments{
  \item{...}{Optional arguments passed \code{\link[R.filesets:getHeader.TabularTextFile]{*getHeader}()}.}
}

\value{
  Returns \code{\link[base]{character}} \code{\link[base]{vector}},
  or \code{\link[base]{NULL}} if there are no column names in the file header.
}
\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
