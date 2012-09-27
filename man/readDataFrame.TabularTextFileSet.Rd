%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{readDataFrame.TabularTextFileSet}
\alias{readDataFrame.TabularTextFileSet}
\alias{TabularTextFileSet.readDataFrame}
\alias{readDataFrame.TabularTextFileSet}
\alias{readDataFrame,TabularTextFileSet-method}


\title{Reads the tabular data from all files as data frames}

\description{
 Reads the tabular data from all files as data frames and combines them into one data frame (by default).
}

\usage{\method{readDataFrame}{TabularTextFileSet}(this, ..., combineBy=function(x) Reduce(rbind, x))}

\arguments{
  \item{...}{Arguments passed to
    \code{\link[R.filesets:readDataFrame.TabularTextFile]{readDataFrame}()}
    as called for each \code{\link{TabularTextFile}} of the file set.}
  \item{combineBy}{A \code{\link[base]{function}} that takes a \code{\link[base]{list}} of \code{\link[base]{data.frame}}:s
    and combines them.  The default is to stack them into a single
    \code{\link[base]{data.frame}}.  If \code{\link[base]{NULL}}, the \code{\link[base]{list}} is not combined.}
}

\value{
  Returns what \code{combineBy} returns, which defaults to a \code{\link[base]{data.frame}}.
  If \code{combineBy=NULL}, then a named \code{\link[base]{list}} of \code{\link[base]{data.frame}}:s is returned.
}

\examples{
# Setup a file set consisting of all *.dat tab-delimited files
# in a particular directory
path <- system.file("exData/dataSetA,original", package="R.filesets")
ds <- TabularTextFileSet$byPath(path, pattern="[.]dat$")
print(ds)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read data frames from each of the files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dataList <- lapply(ds, readDataFrame)
print(dataList)

rows <- c(3:5, 8, 2)
dataList <- lapply(ds, readDataFrame, rows=rows)
print(dataList)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read common columns and stack into one data frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colNames <- Reduce(intersect, lapply(ds, getColumnNames))
cat("Common column names:\n")
print(colNames)

# Read the *common* columns "as is" (hence 'NA')
colClassPatterns <- rep(NA, times=length(colNames))
names(colClassPatterns) <- colNames
cat("Column class patterns:\n")
print(colClassPatterns)

data <- readDataFrame(ds, colClassPatterns=colClassPatterns)
print(data)
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{TabularTextFileSet}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
