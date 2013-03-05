%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readDataFrame.TabularTextFile}
\alias{readDataFrame.TabularTextFile}
\alias{TabularTextFile.readDataFrame}
\alias{readDataFrame,TabularTextFile-method}

\title{Reads the tabular data as a data frame}

\description{
 Reads the tabular data as a data frame.
}

\usage{
\method{readDataFrame}{TabularTextFile}(this, con=NULL, rows=NULL, nrow=NULL, trimQuotes=FALSE, ..., debug=FALSE, verbose=FALSE)
}

\arguments{
  \item{con}{(Internal) If a \code{\link[base:connections]{connection}}, then it is used, otherwies
  a new file connection is temporarly opened and used.}
  \item{rows}{(Optional) An \code{\link[base]{integer}} \code{\link[base]{vector}} specifying which rows to
   be read.}
  \item{nrow}{(Optional) An \code{\link[base]{integer}} specifying how many rows to read.
   If specified, it corresponds to specifying \code{rows=seq_len(nrow)}.}
  \item{trimQuotes}{(Optional) If \code{\link[base:logical]{TRUE}}, quotes are trimmed from numeric
   columns before parsing them as numerics.  This makes it possible to
   read quoted numeric values.}
  \item{...}{Passed to internal \code{\link[R.filesets:getReadArguments.TabularTextFile]{*getReadArguments}()}.}
  \item{debug}{If \code{\link[base:logical]{TRUE}}, additional details on the file and how it was
   read is returned as part of the attributes.}
  \item{verbose}{A \code{\link[base]{logical}} or a \code{\link[R.utils]{Verbose}} object.}
}

\value{
  Returns a \code{\link[base]{data.frame}}.
}

\section{Reading quoted numerics}{
  If a specific data column is specified as being numeric in
  argument \code{colClasses} and that column contains quoted values
  it is necessary to use argument \code{trimQuotes=TRUE}, otherwise
  \code{\link[base]{scan}}() throws an exception similar to:
  \code{scan() expected 'a real', got '"1.0"'}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{TabularTextFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
