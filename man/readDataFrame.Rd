%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  readDataFrame.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{readDataFrame}
\alias{readDataFrame.default}
\alias{readDataFrame}

 \title{Reads data from a tabular file}

 \description{
  Reads data from a tabular file.
 }

 \usage{\method{readDataFrame}{default}(filename, path=NULL, ..., fileClass="TabularTextFile")}

 \arguments{
   \item{filename, path}{\code{\link[base]{character}} strings specifying the file to be read.}
   \item{...}{Additional arguments passed to \code{\link[readDataFrame.TabularTextFile]{readDataFrame}}.}
   \item{fileClass}{(optional) A \code{\link[base]{character}} string specifying the \code{\link{TabularTextFile}} class.}
 }

 \value{
  Returns a \code{\link[base]{data.frame}}.
 }

 \examples{
path <- system.file("exData/dataSetA,original", package="R.filesets")
pathname <- file.path(path, "fileA,20100112.dat")

# Read all data
df <- readDataFrame(pathname)
print(df)

# Read columns 'x', 'y', and 'char'
df <- readDataFrame(pathname, colClasses=c("(x|y)"="integer", "char"="character"))
print(df)
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[utils]{read.table}}.
   For further control, see the \code{\link{TabularTextFile}} class.
 }

