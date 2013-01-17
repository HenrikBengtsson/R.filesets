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
  Reads data from a tabular file or a set of such files.
 }

 \usage{\method{readDataFrame}{default}(filename, path=NULL, ...)}

 \arguments{
   \item{filename, path}{\code{\link[base]{character}} \code{\link[base]{vector}} specifying one or more files to
    be read.}
   \item{...}{Additional arguments passed to either
      (i) \code{\link[=readDataFrame.TabularTextFile]{readDataFrame}}
          for class \code{\link{TabularTextFile}}, or
      (ii) \code{\link[=readDataFrame.TabularTextFileSet]{readDataFrame}}
          for class \code{\link{TabularTextFileSet}},
     depending on whether one or multiple files are read.
   }
 }

 \value{
  Returns a \code{\link[base]{data.frame}}.
 }

 \details{
   When reading multiple files at once, first each file is read into
   a \code{\link[base]{data.frame}}, and then these \code{\link[base]{data.frame}}s are (by default) merged into
   one \code{\link[base]{data.frame}} using \code{\link[base]{rbind}}().  This requires that the
   same set of columns are read for each file.  Which columns to read
   can be controlled by specifying their names in
   argument \code{colClasses}.  To change how the \code{\link[base]{data.frame}}s are
   merged, use argument \code{combineBy}.
   For more information, follow the help on the above to
   \code{readDataFrame()} help links.
 }

 \examples{
path <- system.file("exData/dataSetA,original", package="R.filesets")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Example: Standard tab-delimited file with header comments
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pathname <- file.path(path, "fileA,20100112.dat")

# Read all data
df <- readDataFrame(pathname)
print(df)

# Read columns 'x', 'y', and 'char'
df <- readDataFrame(pathname, colClasses=c("(x|y)"="integer", "char"="character"))
print(df)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Example: Tab-delimited file with header comments but
#          also two garbage at the very beginning
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pathname <- file.path(path, "fileA,20130116.datx")

# Explicitly skip the two rows
df <- readDataFrame(pathname, skip=2)
print(df)


# Skip until the first data row
df <- readDataFrame(pathname, skip="^x")
print(df)
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[utils]{read.table}}.
   For further details, see classes \code{\link{TabularTextFile}} and
   \code{\link{TabularTextFileSet}}.
 }

