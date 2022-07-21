# Version (development version)

## Deprecated and Defunct

 * `dsApplyInPairs()` is defunct.  Use
   `future.apply::future_mapply()` instead.


# Version 2.14.0 [2020-12-07]

## Bug Fixes

 * Re-export S3 generic `extract()` from the **R.rsp** package.
 
## Deprecated and Defunct

 * Removed `dsApply()`, which has been defunct since version 2.13.0
   (April 2019).  Use `future.apply::future_lapply()` instead.


# Version 2.13.0 [2019-04-17]

## Deprecated and Defunct

 * `dsApplyInPairs()` is deprecated.  Use
   `future.apply::future_mapply()` instead.

 * `dsApply()` is defunct.  Use `future.apply::future_lapply()`
   instead.

 * Removed defunct and hidden argument `colClassPatterns` of
   `readDataFrame()` for `TabularTextFile`.  Use argument `colClasses`
   instead.

 * Removed defunct and hidden argument `files` of `extractMatrix()`
   for `GenericTabularFileSet`.  Use `extractMatrix(ds[files], ...)`
   instead.
   

# Version 2.12.1 [2018-04-08]

## Bug Fixes

 * `loadToEnv()` for `RDataFile` was not declared an S3 method.
 

# Version 2.12.0 [2018-02-11]

## Significant Changes
   
 * Package requires R (>= 3.2.0) released April 2015.

 * Package no longer requires Bioconductor.

## Cleanup

 * Package no longer suggests **BatchJobs** and **BiocParallel**.

## Deprecated and Defunct

 * `dsApply()` is now deprecated.  Instead, use
   `future.apply::future_lapply()`.
 
 * `dsApply()` with `.parallel = "BiocParallel::BatchJobs"` and
   `"BatchJobs"` are now defunct.  Instead, use
   `future.apply::future_lapply()` with one of the many backends that
   implements the Future API.
 

# Version 2.11.0 [2017-02-27]

## Significant Changes
   
 * Package requires R (>= 3.1.2) and Bioconductor (>= 3.0) both
   released in October 2014.

## New Features

 * Now `getChecksum()` for `ChecksumFile` defaults to not creating a
   checksum file (which is the default for other types of file), but
   instead always return the checksum of the file by only calculating
   and in memory.  This prevents for instance the `equals()` test on
   two different checksum files to generate another set of checksum
   files on themselves.
   
 * Now `findByName()` for `GenericDataFileSet` reports on the
   non-existing root paths in error messages.
   
 * `GenericDataFile` and `GenericDataFileSet` no longer report on
   memory (RAM) usage of objects.

## Cleanup

 * `dsApply(..., .parallel = "future")` now used `future_lapply()` of
   the **future** package internally.  `dsApply()` will soon be
   deprecated (see below).

## Installation

 * Package no longer needs to depend on **listenv**.
   
## Deprecated and Defunct

 * Argument `colClassPatterns` of `readDataFrame()` for
   `TabularTextFile` is now defunct.  Use `colClasses` instead.
 
 * Argument `files` of `extractMatrix()` for `GenericTabularFileSet`
   is defunct.
 
 * `dsApply()` with either `.parallel = "BiocParallel::BatchJobs"` or
   `"BatchJobs"` is deprecated.  Instead, use
   `future::future_lapply()` with whatever choice of `future::plan()`
   preferred.

 * Defunct argument `aliased` of `getDefaultFullName()` for
   `GenericDataFile` and defunct argument `alias` of
   `GenericDataFileSet()` have been removed.


# Version 2.10.0 [2016-01-04]

## Significant Changes
   
 * Package requires R (>= 3.1.2) released October 2014, because of its
   dependency on the **listenv** package.

## New Features

 * Now file sizes are reported using IEC binary prefixes, i.e.
   bytes, KiB, MiB, GiB, TiB, ..., YiB.
   
 * Added `hasChecksumFile()` for `GenericDataFile`.
 
 * `hasBeenModified()` for `GenericDataFile` gained argument `update`.

## Cleanup

 * Removed `na.omit()` for `GenericDataFileSet`; the default one in
   the **stats** package works equally well.
   
## Software Quality

 * Increased test coverage from 51% to 62%.

## Bug Fixes

 * `Arguments$getTags()` failed to drop missing values.
 
 * `equals(df, other)` for `GenericDataFile` would give an error if
   `other` was not a `GenericDataFile`.
   
 * `dropTags()` would drop name if a tag had the same name.
 
 * `getOneFile()` on a `GenericDataFileSet` with a single missing file
   would give an error, now it gives a file with an NA pathname.

## Deprecated and Defunct

 * Preparing to make the default pathname for `GenericDataFile()` to
   become `NA_character_`.  It is currently NULL, but the goal is to
   enforce `length(pathname)` to be one.
   
 * `extractMatrix(ds, files, ...)` for `GenericTabularFileSet` is
   deprecated.  Use `extractMatrix(ds[files], ...)` instead.
   
 
# Version 2.9.0 [2015-10-20]

## Significant Changes

 * Package now requires R (>= 3.1.1) released July 2014. This allows
   us to use Bioconductor (>= 3.0) (October 2014).

## New Features

 * Add support for `dsApply(..., .parallel = "future")`, which
   utilizes the **future** package.

## Deprecated and Defunct

 * Defunct methods dropped.
 
 
# Version 2.8.0 [2015-08-06]

## Significant Changes

 * Package requires R (>= 3.1.0) released March 2014.
 
## New Features

 * Added support for `sortBy(..., by = "mixedroman")` for
   `GenericDataFileSet`.
   
 * Now `commentChar = ""` and `commentChar = FALSE` also disables
   searching for comment characters (just as `commentChar = NULL`) for
   `TabularTextFile`.

## Software Quality

 * Explicit import of **utils** functions.

## Bug Fixes

 * `readDataFrame()` for `TabularTextFile` with column-names
   translators set, could give an error "Number of read data columns
   does not match the number of column headers: ...". This was due to
   an update in `utils::read.table()` as of R v3.2.1 svn rev 68831.
 
 
# Version 2.7.2 [2015-05-24]

## New Features

 * CONSISTENCY: Analogously to `lapply()`, `dsApply()` returns a list
   with names corresponds to the full names of the data set.

## Software Quality

 * ROBUSTNESS: Added `getFullNames(..., onRemapping)` to
   `GenericDataFileSet` to warn/err on full-name translations that
   generates inconsistent fullname-to-index maps before and after.
   
## Bug Fixes

 * `linkTo(..., skip = TRUE)` would give error "No permission to
   modify existing file: ..." also in the case when the proper link
   already exists and there is no need to create a new one.
   
 * Now `getReadArguments()` for `TabularTextFile` let duplicated named
   `colClasses` entries override earlier ones, e.g.  `colClasses=c("*"
   = NA, "*" = "NULL", a = "integer")` is effectively the same as
   `colClasses=c("*" = "NULL", a = "integer")`.  Added package test.
 
 
# Version 2.7.1 [2015-04-27]

## Software Quality

 * ROBUSTNESS: Now `nchar(..., type = "chars")` is used internally for
   all file and directory names (including tags).

## Installation

 * Bumped package dependencies.

## Bug Fixes

 * `as.character()` for `GenericDataFile` with a missing (NA) pathname
   on recent R-devel (>= 2015-04-23) related to an update on how
   `nchar()` handles missing values.
 
 
# Version 2.7.0 [2015-02-23]

## Significant Changes

 * Package requires R (>= 3.1.0) released March 2014. This allows us
   to use Bioconductor (>= 2.14) (April 2014).  These are in fact old; it's
   recommended to use a more recent version of R.

## New Features

 * Now `[[` for `GenericDataFileSet` returns a `GenericDataFile` not only
   if a numeric index is given but also if a character string is given.
   
 * Now argument `idx` for `getFile()` for `GenericDataFileSet` can
   also be a character string, in which case the file returned is
   identified using `indexOf(..., pattern = idx, by = "exact",
   onMissing = "error")`.
   
 * Added `RDataFile` and `RDataFileSet` classes for `*.RData` files.
 
## Software Quality

 * Package test coverage is 51%.
 
 
# Version 2.6.2 [2015-01-05]

## Cleanup

 * Using `requireNamespace()` instead of `require()` internally.
 
 
# Version 2.6.1 [2014-12-27]

## Software Quality

 * Added test for reading subset of a large tabular file.

## Bug Fixes

 * `as.character()` for `ChecksumFile` gave an error when the checksum
   files was missing.
 
 
# Version 2.6.0 [2014-08-26]

## New Features

 * Added support for `sortBy(..., by = "filesize")` and
   `sortBy(..., decreasing = TRUE)` for `GenericDataFileSet`.
   
 * Added `rep()` for `GenericDataFileSet`.

NOTES:

 * Submitted to CRAN.
 
 
# Version 2.5.10 [2014-08-25]

## Bug Fixes

 * `readDataFrame()` would ignore argument `colClasses` iff it had no
   names.  Added package system test for this case.
 
 
# Version 2.5.9 [2014-08-23]

## Bug Fixes

 * Using `commentChar = NULL` for `TabularTextFile`:s failed.
 
 
# Version 2.5.8 [2014-08-19]

## New Features

 * Added `readChecksums()` for `ChecksumFileSet`.
 
 
# Version 2.5.7 [2014-08-17]

## Bug Fixes

 * `byPath()` for `GenericDataFileSet` would output verbose message
   enumerating files loaded to stdout instead of stderr.
 
 
# Version 2.5.6 [2014-08-07]

## Bug Fixes

 * `dsApply()` for `GenericDataFileSet` would coerce argument `verbose` to
   logical before applying the function.
 
 
# Version 2.5.5 [2014-08-02]

## Installation

 * Bumped package dependencies.

## Bug Fixes

 * Argument `sep` for `readDataFrame()` would only work for `,` and
   `\t`; now it works for any separator.
 
 
# Version 2.5.4 [2014-06-11]

## New Features

 * Now `indexOf()` first searched by exact names, then as before, i.e.
   by regular expression and fixed pattern matching.  Added package
   system tests that contains particularly complicated cases for this.
   This was triggered by a rare but real use case causing an error in
   **aroma.affymetrix**.  Thanks Benilton Carvalho for reporting on
   this.
   
 * Added argument `by` to `indexOf()` for `GenericDataFileSet`|List.
 
 
# Version 2.5.3 [2014-06-07]

## Installation

 * Added `SuggestsNote` field to DESCRIPTION with list of packages
   that are recommended for the most common use cases.
   
 * Bumped package dependencies.
 
 
# Version 2.5.2 [2014-05-02]

## Cleanup

 * Now using `ds[[idx]]` instead of `getFile(ds, idx)` where possible.
 
 
# Version 2.5.1 [2014-04-19]

## New Features

 * `dsApply(..., .parallel = "none")` would lower the verbose
   threshold before applying the function resulting is less verbose
   output in the non-parallel case.
 
 
# Version 2.5.0 [2014-03-30]

## New Features

 * Starting to add an internal framework for generic
   parallel/distributed processing of file sets via **BiocParallel**,
   etc.  When stable, this will be made available part of the public
   API.
 
 
# Version 2.4.1 [2014-02-28]

## Bug Fixes

 * FIX / DOCUMENTATION: The system test for `GenericDataFile` would
   fail with `linkTo()` on Windows systems without necessary
   privileges.  Made the test less conservative.  Also, added an Rd
   section on privileges required on Windows for `linkTo()` to work.
   Thanks to Brian Ripley for reporting on this.
 
 
# Version 2.4.0 [2014-02-26]

NOTES:

 * No updates. Bumped the version for CRAN release.
 
 
# Version 2.3.15 [2014-02-26]

## Installation

 * Bumped up package dependencies.
 
 
# Version 2.3.14 [2014-01-24]

## New Features

 * Now `readColumns()` for `TabularTextFile` handles also header-less
   files.
 
 
# Version 2.3.13 [2014-01-18]

## Cleanup

 * Removed namespace patch for **R.utils** (< 1.27.5)
 
 
# Version 2.3.12 [2014-01-13]

## Bug Fixes

 * `copyTo()` for `GenericDataFileSet` no longer passes `...` to
   `byPath()` when constructing the return data set.
 
 
# Version 2.3.11 [2014-01-09]

## New Features

 * Now `renameTo()` passes `...` to `R.utils::renameFile()` making it
   possible to also overwrite existing files.
 
 
# Version 2.3.10 [2014-01-07]

## New Features

 * Added `is.na()` for `GenericDataFile` and `GenericDataFileSet` and
   `na.omit()` for the latter, which already supports `anyNA()`.
   
 * Added `linkTo()` for `GenericDataFile`, which create a symbolic
   link at a given destination pathname analogously to how `copyTo()`
   creates a file copy at a given destination pathname.
 
 
# Version 2.3.9 [2014-01-06]

## New Features

 * `copyTo()` for `GenericDataFile` passes `...` to
   `R.utils::copyFile()`.

## Bug Fixes

 * `copyTo()` and `renameTo()` for `GenericDataFile` had verbose
   output enabled by default.
   
## Deprecated and Defunct

 * `digest2()` is now defunct.
 
 
# Version 2.3.8 [2014-01-04]

## New Features

 * Added `duplicated()`, `anyDuplicated()`, and `unique()` for
   `GenericDataSet`, which all compare `GenericDataFile`:s using the
   `equals()` method.
   
 * Now `c()` for `GenericDataFileSet` also works to append
   `GenericDataFile`:s.  Added package system test for common use
   cases of `c()`.
 
 
# Version 2.3.7 [2013-12-18]

## New Features

 * Added `nbrOfColumns()` for `GenericTabularFile`, which, if the
   number of columns cannot be inferred from the column names, will
   fall back to read the first row of data and use that as the number
   of columns.
   
 * Now `nbrOfColumns()` for `ColumnNamesInterface` returns NA if
   column names cannot be inferred and hence not be counted.

## Bug Fixes

 * Now `readDataFrame(..., header = FALSE)` works as expected for
   tabular text files without headers.
   
 * Now `getReadArguments()` for `TabularTextFile` returns a
   `colClasses` vector of the correct length also in the case when
   there are no column names.
 
 
# Version 2.3.6 [2013-11-27]

## New Features

 * Added a generic `loadRDS()` available for plain files and
   `RdsFile`:s.
 
 
# Version 2.3.5 [2013-11-21]

## New Features

 * Added `RdsFile` and `RdsFileSet` objects for handling `*.rds` file
   sets.

## Cleanup

 * There's no longer a need to reset class for `GenericSummary`.
 
 
# Version 2.3.4 [2013-11-19]

## New Features

 * Added `ChecksumFile` and `ChecksumFileSet`.
 
 
# Version 2.3.3 [2013-11-15]

## New Features

 * Now `extract()` for `GenericDataFileSet` also handles when the data
   set to be extracted is empty, e.g. `extract(GenericDataFileSet(),
   NA_integer_)`.  Also, added support for argument `onMissing =
   "dropall"`, which drops all files if one or more missing files
   where requested.  Added package system tests for these case.
 
 
# Version 2.3.2 [2013-11-11]

## New Features

 * SPEEDUP: `GenericDataFileSet$byPath(..., recursive = TRUE)` would
   be very slow setting up the individual files, especially for large
   data sets.  Now it's only slow for the first file.
 
 
# Version 2.3.1 [2013-11-01]

## New Features

 * Added `"[["(x, i)` for `GenericDataFileSet`, which gets a
   `GenericDataFile` by index `i` in `[1,length(x)]`.  When `i` is
   non-numeric, the next `"[["(x, i)` method in the class hierarchy is
   used, e.g. the one for `Object`:s.
   
 * Added `gzip()`/`gunzip()` for `GenericDataFileSet`.
 
 * Added `anyNA()` to `GenericDataFileSet` to test whether any of the
   pathnames are NA, or not.
 
 
# Version 2.3.0 [2013-10-16]

## Installation

 * Bumped up package dependencies.

## Cleanup

 * Minor cleanups.


# Version 2.2.5 [2013-10-09]

## Software Quality

 * Added system tests validating `getChecksum()` on
   `GenericDataFile`:s and `GenericDataFileSet`:s.
 
 
# Version 2.2.4 [2013-10-07]

## New Features

 * ROBUSTNESS: The overriding of `append()` to become a generic
   function does now call `base::append()` in the default, instead of
   copy the latter.  All this will eventually be removed, when proper
   support for `c`, `[`, `[[`, etc. has been added everywhere.

## Installation

 * Bumped up package dependencies.

## Cleanup

 * Now explicitly importing only what is needed in NAMESPACE.
 
 
# Version 2.2.3 [2013-10-05]

## Bug Fixes

 * Package now re-exports `getChecksum()` from **R.cache** instead of
   creating its own.  This solves the problem of the default
   `getChecksum()` of **R.cache** not being found.
 

# Version 2.2.2 [2013-09-30]

## Significant Changes

 * Package requires R (>= 3.1.0) released March 2014. This allows us
   to use Bioconductor (>= 2.14) (April 2014).  These are in fact old;
   it's recommended to use a more recent version of R.

## New Features

 * SPEEDUP: Now `readDataFrame()` for `TabularTextFile` subsets by row,
   before reparsing numerical columns that were quoted.

## Cleanup

 * Dropped obsolete `autoload()`:s used internally.

## Deprecated and Defunct

 * Deprecated `digest2()` and deprecated -> defunct -> dropped.
 
 * Now `GenericDataFileSet()` gives an error informing that argument
   `alias` is defunct.
   
 * Now no generic functions are created for defunct methods.

 
# Version 2.2.1 [2013-09-28]

## New Features

 * Now the `R.filesets` `Package` object is also available when the
   package is only loaded (but not attached).

## Bug Fixes

 * Forgot to import `cat()` from **R.utils**.
 
 
# Version 2.2.0 [2013-09-23]

## Significant Changes

 * Package no longer attaches **R.utils** - only imports it.

## Cleanup

 * SPEEDUP: Package no longer uses `R.utils::whichVector()`, which use
   to be 10x faster, but since R 2.11.0 `which()` is 3x times again.

 * Package no longer utilizes `import()`, only `importFrom()`:s.
 
## Installation

 * Bumped up package dependencies.

## Software Quality

 * Added package system tests.
 
## Bug Fixes

 * WORKAROUND: For now, package attaches the **R.oo** package.  This
   is needed due to what appears to be a bug in how **R.oo** finalizes
   `Object`:s assuming **R.oo** is/can be attached.  Until that is
   resolved, we make sure **R.oo** is attached.

 * Forgot to import `R.methodsS3::appendVarArgs()`.
 

 
# Version 2.1.1 [2013-08-31]

## New Features

 * Added `[()` and `c()` for `GenericDataFileSet`.
 
 
# Version 2.1.0 [2013-08-01]

## New Features

 * Added argument `private = FALSE` to `byPath()` of
   `GenericDataFileSet`.

## Installation

 * Bumped up package dependencies.
 
 
# Version 2.0.6 [2013-07-20]

## Installation

 * Removed **R.oo** from Depends and bumped up package dependencies.
 
 
# Version 2.0.5 [2013-06-26]

## New Features

 * Now `isGzipped()` ignores the case of the filename extension when
   testing whether the file is gzipped or not.
 
 
# Version 2.0.4 [2013-05-25]

## Cleanup

 * Minor speedup by replacing all `rm()` calls with NULL assignments.
 
 
# Version 2.0.3 [2013-05-22]

## Deprecated and Defunct

 * Package no longer utilizes `digest2()`, which soon will be
   deprecated.
 
 
# Version 2.0.2 [2013-05-20]

## Software Quality

 * CRAN POLICY: Now all Rd `\usage{}` lines are at most 90 characters
   long.

## Installation

 * Bumped up package dependencies.
 
 
# Version 2.0.1 [2013-03-04]

## Software Quality

 * DOCUMENTATION: Updated the help usage section for all static methods.

## Installation

 * Bumped up package dependencies.
 
 
# Version 2.0.0 [2013-01-17]

## New Features

 * In addition to a fixed integer, argument `skip` for `readDataFrame()`
   (default and for `TabularTextFile`) may also specify a regular
   expression matching the first row of the data section.
   
 * Now argument `skip` to `TabularTextFile` and `readDataFrame()` for
   that class causes the parser to skip that many lines including
   commented lines, whereas before it did not count commented lines.
   
 * Added a default `readDataFrame()` for reading data from one or more
   tabular text files via the `TabularTextFile`/`TabularTextFileSet`
   classes.
 
 
# Version 1.9.0 [2012-12-20]

## Deprecated and Defunct

 * Argument `colClassPatterns` of `readDataFrame()` for
   `TabularTextFile` has been renamed to `colClasses`.
 
 
# Version 1.8.2 [2012-12-19]

## New Features

 * Utilizing new `startupMessage()` of **R.oo**.
 
## Software Quality

 * ROBUSTNESS: Now `indexOf()` for `GenericDataFileSet` throws an
   exception if user tries to pass an argument `names`.

## Installation

 * Bumped up package dependencies.

 
# Version 1.8.1 [2012-12-09]

## New Features

 * Added `head()` and `tail()` for `GenericTabularFile`.

 * Added subsetting via `[()` to `GenericTabularFile`.

## Bug Fixes

 * `nbrOfRows()` for `TabularTextFile` forgot to exclude comment rows
   in the file header.
   
 * `readColumns()` for GenericTabularFile would not preserve the order
   of the requested `columns`.
 
 
# Version 1.8.0 [2012-12-06]

## New Features

 * Added `getOneFile()` for `GenericDataFileSet`, which returns the
   first `GenericDataFile` with a non-missing pathname.
   
 * Added argument `absolute = FALSE` to `getPathname()` for
   `GenericDataFile`.

## Software Quality

 * ROBUSTNESS: Now `GenericDataFile` stores the absolute pathname of
   the file, even if a relative pathname is given.  This makes sure
   that the file is found also when the working directory is changed.

## Bug Fixes

 * `equals()` for `GenericDataFileSet` would only compare the first
   `GenericDataFile` in each set.
 
 
# Version 1.7.3 [2012-12-03]

## New Features

 * Added `isGzipped()` to `GenericDataFile`.
 
 
# Version 1.7.2 [2012-12-03]

## New Features

 * Generalized `writeColumnsToFiles()` to `GenericTabularFile`.  Used
   to be available only for `TabularTextFile`.
  
## Software Quality

 * Now package declares S3 methods in the namespace.

## Installation

 * Now package only imports **digest** and **R.methodsS3**.  Added
   **stats** to list of imported packages.
 
 
# Version 1.7.1 [2012-12-02]

## Bug Fixes

 * `getDefaultColumnNames()` for `TabularTextFile` did not use
   `columnNames` if it was set when creating the `TabularTextFile`
   object.
   
 * Now `getReadArguments()` for `TabularTextFile` drops arguments that
   are NULL, because they could cause errors downstreams,
   e.g. `readDataFrame()` calling `read.table(..., colClasses = NULL)`
   => `rep_len(NULL, x)` => "Error in rep_len(colClasses, cols) :
   cannot replicate NULL to a non-zero length".
 
 
# Version 1.7.0 [2012-11-30]

## New Features

 * Updated `as.list()` for `GenericDataSet` to return a _named_ list
   of `GenericDataFile`:s (previously it had no names).  The names are
   the (translated) full names of the `GenericDataFile`:s.

## Cleanup

 * Removed `lapply()` and `sapply()` for `GenericDataSet`, because the
   corresponding functions in the **base** package utilizes
   `as.list()`.
 
 
# Version 1.6.1 [2012-11-28]

## New Features

 * Now `GenericDataFile()` retrieves the file time stamps such that
   `hasBeenModified()` returns a correct value also when first called,
   and not only TRUE just in case.  This has the effect that
   `getChecksum()` will detected cached results already at the second
   call as long as the file has to been modified.  Previously it took
   two calls to `getChecksum()` for it to be properly cached.
   
 * Now declaring more internal and temporary `Object` fields as
   "cached", which means they will be cleared if `clearCache()` or
   `gc()` is called on the corresponding object.
   
 * Added further verbose output to `TabularTextFileSet`.
 
 * DOCUMENTATION: Minor corrections to help pages.
 
 
# Version 1.6.0 [2012-11-15]

NOTES:

 * No updates. Bumped the version for CRAN release.
 
 
# Version 1.5.4 [2012-11-15]

## New Features

 * BACKWARD COMPATIBILITY: Made it possible for `TabularTextFile` to ignore
   header comment arguments when inferring column names and classes.
 
 
# Version 1.5.3 [2012-11-13]

## Cleanup

 * Now `clearCache()` for `GenericDataFileSet` relies on ditto of
   `Object` to clear all cached fields (= with field modifier
   `"cached"`).

## Deprecated and Defunct

 * Deprecated `{get,set}Label()` for `GenericDataFile` and
   `{get,set}Alias()` for `GenericData{File,FileSet}`.  Related
   arguments such at `alias` to `GenericDataFileSet` and `aliased` to
   `getDefaultFullName()` for `GenericDataFile` are also deprecated.
 
 
# Version 1.5.2 [2012-11-12]

## Cleanup

 * Using `seq_along(x)` instead of `seq(along = x)` everywhere.
   Similarly, `seq(ds)` where `ds` is `GenericDataFileSet` is now
   replaced by `seq_along(ds)`.  Likewise, `seq_len(x)` replaces
   `seq(length = x)`, and `length(ds)` replaces `nbrOfFiles(ds)`.
 
 
# Version 1.5.1 [2012-11-08]

## New Features

 * Now `TabularTextFile()` tries to infer whether the data section
   contains column names or not.  This is done by comparing to
   the optional `columnNames` header argument.  If that is not
   available, it will (as before) assume there are column names.
   
 * Now `readDataFrame()` acknowledge header comment arguments
   `columnNames` and `columnClasses` if specified in the file.
   
 * Now `getDefaultColumnNames()` for `TabularTextFile` falls back to
   header comment argument `columnNames`, if there are no column names
   in the actual data table.
   
 * Now `readRawHeader()` for `TabularTextFile` also parses and returns
   header comment arguments.
 
 
# Version 1.5.0 [2012-11-02]

## New Features

 * Added `ColumnNamesInterface` which `GenericTabularFile` now
   implements.  Classes inheriting from `GenericTabularFile` should
   rename any `getColumnNames()` method to `getDefaultColumnNames()`.

## Cleanup

 * Replaced all `whichVector()` with `which()`, because the latter is
   now the fastest again.
 
 
# Version 1.4.2 [2012-11-01]

## New Features

 * Added `setColumnNames()` for `GenericTabularFile`, which utilizes
   `setColumnNamesTranslator()`.

## Deprecated and Defunct

 * Deprecated `{get,set}ColumnNameTranslator()` in favor of
   `{get,set}ColumnNamesTranslator()`; note the plural form.
 
 
# Version 1.4.1 [2012-10-31]

## Cleanup

 * `readDataFrame()` for `TabularTextFile` no longer returns attribute
   `fileHeader`, unless argument `debug` is TRUE.
 
 
# Version 1.4.0 [2012-10-30]

## New Features

 * Added `validate()` to `GenericDataFileSet`, which iteratively calls
   `validate()` on all the `GenericDataFile`:s in the set.  The
   default is to return NA, indicating that no validation was done.
 
 
# Version 1.3.3 [2012-10-29]

## Cleanup

 * Now using `Arguments$getReadablePath()` instead of
   `filePath(..., expandLinks = "any")`.
 
 
# Version 1.3.2 [2012-10-17]

## Cleanup

 * Forgot to drop `Arguments$getFilename()` below.
 
 
# Version 1.3.1 [2012-10-16]

## Bug Fixes

 * ROBUSTNESS: No longer passing `...` to `NextMethod()`, cf. R-devel
   thread 'Do *not* pass '...' to NextMethod() - it'll do it for you;
   missing documentation, a bug or just me?' on Oct 16, 2012.
 
 
# Version 1.3.0 [2012-10-16]

## Cleanup

 * Moved `Arguments$getFilename()` from this package to **R.utils**
   v1.17.0.

## Installation

 * ROBUSTNESS: Bumped up package dependencies.
 
 
# Version 1.2.2 [2012-10-16]

## New Features

 * DOCUMENTATION: Added Rd help for a few more methods.
 
## Deprecated and Defunct

 * Static `fromFiles()` for `GenericDataFileSet` is now defunct in
   place for `byName()`, which has been recommended since January
   2010.
 
 
# Version 1.2.1 [2012-09-27]

## New Features

 * Now `readDataFrame()` for `TabularTextFile` defaults to read
   strings as characters rather than as factors.  To read strings as
   factors, just pass argument `stringsAsFactors = TRUE`.
   
 * Added `readDataFrame()` for `TabularTextFileSet`.
 
 * ROBUSTNESS: Now `getHeader()` for `TabularTextFile` checks if the
   file has been modified before returned cached results.
 
 
# Version 1.2.0 [2012-09-25]

## Installation

 * Now **R.filesets** imports **R.methodsS3**, **R.oo** and
   **R.utils**. This solves issues such as `trim()` being overridden
   by ditto from the **IRanges** package, iff loaded.
 
 
# Version 1.1.6 [2012-09-01]

## New Features

 * CONSISTENCY: Now `extractMatrix()` for `GenericTabularFile` adds
   column names just as ditto for `GenericTabularFileSet` does.
 
 
# Version 1.1.5 [2012-03-06]

## Cleanup

 * CRAN POLICY: Removed all internal copies of **base** functions that
   have `.Internal()` calls.
 
 
# Version 1.1.4 [2011-11-19]

## New Features

 * Now more methods for `GenericDataFile` and `GenericDataFileSet`
   handle so called "empty" files, which are files with NULL
   pathnames.
 
 
# Version 1.1.3 [2011-09-26]

## New Features

 * Added methods set- and `getCommentChar()` to `TabularTextFile` and
   argument `commentChar` to its constructor.  This allows to use
   custom comment characters other than just `"#"`.
 
 
# Version 1.1.2 [2011-09-11]

## Bug Fixes

 * `GenericDataFileSet$byName(..., subdirs)` would throw `Error in
   strsplit(subdirs, split = "/\\")` iff `subdirs != NULL`.
   
 * Improved the handling of the newly introduced `depth` parameter, e.g.
   by making it optional/backward compatible.
 
 
# Version 1.1.1 [2011-07-25]

## New Features

 * Added "depth" to `GenericDataFileSet`, such that one can correctly
   infer fullname and subdirs from the path.
 
 
# Version 1.1.0 [2011-07-24]

## Installation

 * Added a namespace to the package, which will be more or less
   a requirement in the next major release of R.
 
 
# Version 1.0.4 [2011-07-15]

## New Features

 * Added argument `named` to `getTags()` for `FullNameInterface`.  If
   TRUE, tags of format `"<name>=<value>"` will be parsed and returned
   as a named `"<value>"`, e.g. `"foo,n=23,bar,n=42"` is returned as
   `c("foo", "n"="23", "bar", "n"="42")`.
 
 
# Version 1.0.3 [2011-07-13]

## New Features

 * GENERALIZATION: Now `readDataFrame(..., colClasses = ...,
   trimQuotes = TRUE)` of `TabularTextFile` will read numeric columns
   that are quoted.  This is done by first reading them as quoted
   character strings, dropping the quotes, and then rereading them as
   numeric values.
 
 
# Version 1.0.2 [2011-05-23]

## New Features

 * Added argument `.fileClass` to `appendFiles()` for
   `GenericDataFileSet`.
 
 
# Version 1.0.1 [2011-05-16]

## New Features

 * ROBUSTNESS: Now `appendFiles()` for `GenericDataFileSet` asserts
   that all files to be appended are instances of the file class of
   this set as given by the static `getFileClass()`.
   
 * ROBUSTNESS: Added argument `.assertSameClass` to `appendFiles()`
   for `GenericDataFileSet`, which if TRUE asserts that the files to
   be appended inherits from the same class as the existing files.
   Before this test was mandatory.
 
 
# Version 1.0.0 [2011-04-06]

## New Features

 * Added `getChecksum()` to `GenericDataFileSet`, which calculates the
   checksum of the object returned by the protected
   `getChecksumData()`.  Use with care, because what objects should be
   the basis of the checksum is not clear, e.g. should it be only the
   file system checksum, or should things such as translated fullnames
   be included as well?
 
 
# Version 0.9.9 [2011-04-04]

## Bug Fixes

 * `equals()` for `GenericDataFile` would consider two files not to be
   equal only if their checksums was equal, and vice verse.  Also,
   when creating the message string explaining why they differ an
   error would have been thrown.
 
 
# Version 0.9.8 [2011-04-03]

## Cleanup

 * Utilizing `hpaste()` internally wherever applicable.
 
 
# Version 0.9.7 [2011-03-11]

## New Features

 * ROBUSTNESS: Now `appendFullNameTranslatorBy<what>()` for
   `<character>` and `<function>` assert that the translator correctly
   returns exactly one string.  This has the effect that
   `setFullName()` and friends are also tested.
 
 
# Version 0.9.6 [2011-03-09]

## New Features

 * Added `=` to the list of safe characters for
   `Arguments$getFilename()`.
 
 * Added `fullname()`, `name()`, `tags()`, and `dropTags()`.
 
 
# Version 0.9.5 [2011-02-27]

## Bug Fixes

 * After the recent generalization of `findByName()` for
   `GenericDataFileSet` it would throw "<simpleError in
   paths[sapply(rootPaths, FUN = isDirectory)]: invalid subscript type
   'list'>" in case no matching root path directories existed.
 
 
# Version 0.9.4 [2011-02-24]

## New Features

 * Added `dropRootPathTags()`.
 
 * GENERALIZATION: Added support to `findByName()` for
   `GenericDataFileSet` such that root paths also can be specified by
   simple regular expression (still via argument `paths`).  Currently
   it is only the last subdirectory that can be expanded,
   e.g. `foo/bar/data(,.*)/`.
 
 
# Version 0.9.3 [2011-02-18]

## New Features

 * GENERALIZATION: Now `byName()` for `GenericDataFileSet` will try
   all possible data set directories located when trying to setup a
   data set.  Before it only tried the first one located.  This new
   approach is equally fast for the first data set directory as
   before.  The advantage is that it adds further flexibilities,
   e.g. the first directory may not be what we want but the second,
   which can be further tested by the `byPath()` and downstream
   methods such as the constructor.
   
 * ROBUSTNESS: Now `writeColumnsToFiles()` for `TabularTextFile`
   writes files atomically, which should minimize the risk for
   generating incomplete files.

## Cleanup

 * Copied static `getTags()` for `Arguments` from **aroma.core**
   package.

## Deprecated and Defunct

 * Added a warning message reporting that `fromFiles()` of
   `GenericDataFileSet` has been deprecated, if still called by
   someone.
 
 
# Version 0.9.2 [2011-02-14]

## New Features

 * GENERALIZATION: Now `append()` for `GenericDataFileSet` tries to
   also append non-`GenericDataFileSet` object by passing them down to
   `appendFiles()` assuming they are `GenericDataFile`:s.
   
 * GENERALIZATION: Now `appendFiles()` for `GenericDataFileSet` also
   accepts a single item.  Thus, there is no longer a need to wrap up
   single items in a list.
 
 
# Version 0.9.1 [2010-11-19]

## New Features

 * ROBUSTNESS: Now `GenericDataFileSet$byName()` asserts that
   arguments `name` and `tags` contain only valid characters.  This
   will for instance prevent passing paths or pathnames by mistake.
   
 * Now `appendFullNameTranslator(..., df)` for `FullNameInterface`
   takes either `pattern` or `fixed` translations in data.frame.
 
 
# Version 0.9.0 [2010-08-19]

## New Features

 * Added `sortBy()` to `GenericDataFileSet`, which sorts files either
   in a lexicographic or a mixedsort order.
   
 * DOCUMENTATION: Added more Rd help pages.
 
 * DOCUMENTATION: Removed any duplicated `\usage{}` statements from
   the Rd documentation.
 
 
# Version 0.8.3 [2010-07-06]

## Bug Fixes

 * `indexOf()` for `GenericDataFileSet`/List would return NA if the
   search pattern/string contained parentheses.  The reason is that
   such have a special meaning in regular expression.  Now `indexOf()`
   first search by regular expression patterns, then by fixed strings.
   Thanks Johan Staaf at Lund University and Larry(?) for reporting on
   this issue.
 
 
# Version 0.8.2 [2010-05-26]

## New Features

 * Now `GenericDataFileSet$findByName(..., mustExist = FALSE)` do no
   longer throw an exception even if there is no existing root path.
   
 * Added argument `firstOnly = TRUE` to `findByName()` for
   `GenericDataFileSet`.
 
 * Added `appendFullNameTranslatorBy...()` methods to the
   `FullNameInterface` class for data frames, `TabularTextFile`:s, and
   `TabularTextFileSet`:s.
 
 
# Version 0.8.1 [2010-04-22]

## New Features

 * Added `"NA"` to the default `na.strings` returned by
   `getReadArguments()` for `TabularTextFile`.
 
 
# Version 0.8.0 [2010-02-22]

NOTES:

 * Submitted to CRAN.  No changes since v0.7.6.
 
 
# Version 0.7.6 [2010-02-13]

## New Features

 * Added argument `.onUnknownArgs` to `GenericDataFile()` and
   `GenericDataFileSet()`.  As before, the default is to throw an
   exception if there are unknown arguments.  However, in certain
   case it is useful to allow (and ignore) "stray" arguments.
 
 
# Version 0.7.5 [2010-02-07]

## Bug Fixes

 * `indexOf()` of `GenericDataFileSet` and `GenericDataFileSetList`
   did not handle names with regular expression symbols `+` and
   `*`. Thanks to Randy Gobbel for the initial error report.
 
 
# Version 0.7.4 [2010-01-31]

## New Features

 * DOCUMENTATION: Added Rd help paged to more methods for classes
   `GenericDataFile` and `GenericDataFileSet`.

## Deprecated and Defunct

 * Deprecated static `fromFiles()` of `GenericDataSet`. Use `byPath()`
   instead.
 
 
# Version 0.7.3 [2010-01-24]

## New Features

 * ROBUSTNESS: If argument `files` is logical, then `extract()` of
   `GenericDataFileSet` and `GenericDataFileSetList` now asserts that
   the length of `files` matches the number of available files.
 
 
# Version 0.7.2 [2010-01-12]

## New Features

 * Added some example data files under `exData/`.

## Bug Fixes

 * `readColumns(..., column=<string>)` on a `TabularTextFile` would
   give "Error ... object 'columnNames' not found".
 
 
# Version 0.7.1 [2010-01-02]

## New Features

 * Added argument `default = "\\.([^.]+)$"` to `getExtensionPattern()`
   of `GenericDataFile`.  Before the default value was hard coded
   inside this function.

## Bug Fixes

 * Now `setExtensionPattern(..., pattern = NULL)` of `GenericDataFile`
   works.
 
 
# Version 0.7.0 [2010-01-02]

## New Features

 * Added protected `as.data.frame()` to `GenericDataFileSet`List.
 
 * Now `GenericDataFile(NA, mustExist = FALSE`) is a valid object.
   Made all methods aware of such missing files.
   
 * Now `extract(ds, c(1, 2, NA, 4), onMissing = "NA")` returns a valid
   `GenericDataFileSet` where missing files are returned as missing
   `GenericDataFile`:s.
   
 * Added `na.rm = TRUE` to all `getTags()` so that it returns NULL in
   case the file is missing.
   
 * `copyTo()` of `GenericDataFileSet` quietly ignores missing files.
 
 * Added Rd help for `indexOf()` of `GenericDataFileSet`.

 * ROBUSTNESS: Using new `Arguments$getInstanceOf()` were possible.

## Bug Fixes

 * Now all index arguments are validated correctly using the new `max`
   argument of `Arguments$getIndices()`.  Before the case where `max
   == 0` was not handled correctly.

 * Changed the default to `parent = 0` for `getDefaultFullName()` of
   `GenericDataFileSet` to be consistent with the documentation.
   
 * Now `GenericDataFile(pathname)` throws an error if `pathname` is
   referring to a directory.
   
 * `getPath()` and `getDefaultFullName()` of `GenericDataFileSet`
   would return a *logical* instead of *character* value.
   
 * `indexOf(ds, names)` of `GenericDataFileSet` would return a
   *logical* instead of an *integer* vector of NA:s if none of the
   names existed.
 
 
# Version 0.6.5 [2009-10-30]

## New Features

 * ROBUSTIFICATION: Now `translateFullName()` of `FullNameInterface`
   and `translateColumnNames()` of `GenericTabularFile` throw an
   exception if some fullnames were translated into NA.  They also
   assert that no names were dropped or added in the process.

## Bug Fixes

 * After doing `append()` to a `GenericDataFileSet`, the total file
   size reported would remain the same.
   
 * Appending empty data sets using `append()` of `GenericDataFileSet`
   would give error 'Error in this$files[[1]] : subscript out of
   bounds'.
 
 
# Version 0.6.4 [2009-10-25]

## New Features

 * Added `{get,set}ExtensionPattern`() to `FullNameInterface`.
 
 * Added `getExtension()` to `GenericDataFile`.
 
 
# Version 0.6.3 [2009-10-23]

## New Features

 * Added `appendFullNameTranslatorBylist()` which makes it possible to
   do setup a sequence of fullnames translators `fnt1`, `fnt2`, `fnt3`
   by calling `setFullNameTranslator(..., list(fnt1, fnt2, fnt3))`.
 
 
# Version 0.6.2 [2009-10-22]

## New Features

 * Added support for having a sequence of fullname translator
   functions.  These can be added using `appendFullNameTranslator()`.
   
 * Added an `example()` to `FullNameInterface`.
 
 
# Version 0.6.1 [2009-10-06]

## New Features

 * Added subsetting via `[()` to `TabularTextFile`.
 
 
# Version 0.6.0 [2009-10-02]

## New Features

 * Added the `FullNameInterface`, which is the interface class that
   defines what fullnames, names, tags etc are.
   
 * Now `setFullName*s*Translator()` for `GenericDataFileSet`
   dispatches on the `by` argument.  If that is not possible, it call
   `setFullNameTranslator()` for each file in the set (as before).

## Cleanup

 * `GenericDataFile` and `GenericDataFileSet` implements the
   `FullNameInterface`, which mean less redundant code.

## Deprecated and Defunct

 * CLEANUP: Renamed `fromFiles()` to `byPath()`.  For backward
   compatibility the former calls the latter.
   
 
# Version 0.5.3 [2009-08-12]

## New Features

 * Now `findByName()` of `GenericDataFileSet` follows Windows Shortcut
   links also for subdirectories.
 
 
# Version 0.5.2 [2009-06-08]

## New Features

 * Analogously to the method for a `GenericDataFile`, the
   `setFullNameTranslator()` method for `GenericDataFileSet` now
   assumes that the fullname translator function accepts also argument
   `set`.
   
 * Added argument `.fileSetClass` to `GenericDataFileSet()`.
 
 
# Version 0.5.1 [2009-05-19]

## New Features

 * A fullname translator function for a `GenericDataFile` should
   accept any number of arguments.  The first argument will always be
   (an unnamed) argument containing the name (or names) to be
   translated.  If the translator is for a `GenericDataFile`, an
   additional argument `file` will also be passed.  This allows the
   translator function to for instance read the file header and infer
   the name that way.
 
 
# Version 0.5.0 [2009-05-17]

## New Features

 * Extracted several classes and methods from the **aroma.core**
   package.
 
 * Created package.
