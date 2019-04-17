# R.filesets: Easy Handling of and Access to Files Organized in Structured Directories


## Installation
R package R.filesets is available on [CRAN](https://cran.r-project.org/package=R.filesets) and can be installed in R as:
```r
install.packages("R.filesets")
```

### Pre-release version

To install the pre-release version that is available in Git branch `develop` on GitHub, use:
```r
remotes::install_github("HenrikBengtsson/R.filesets@develop")
```
This will install the package from source.  



## Contributions

This Git repository uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model (the [`git flow`](https://github.com/petervanderdoes/gitflow-avh) extension is useful for this).  The [`develop`](https://github.com/HenrikBengtsson/R.filesets/tree/develop) branch contains the latest contributions and other code that will appear in the next release, and the [`master`](https://github.com/HenrikBengtsson/R.filesets) branch contains the code of the latest release, which is exactly what is currently on [CRAN](https://cran.r-project.org/package=R.filesets).

Contributing to this package is easy.  Just send a [pull request](https://help.github.com/articles/using-pull-requests/).  When you send your PR, make sure `develop` is the destination branch on the [R.filesets repository](https://github.com/HenrikBengtsson/R.filesets).  Your PR should pass `R CMD check --as-cran`, which will also be checked by <a href="https://travis-ci.org/HenrikBengtsson/R.filesets">Travis CI</a> and <a href="https://ci.appveyor.com/project/HenrikBengtsson/r-filesets">AppVeyor CI</a> when the PR is submitted.


## Software status

| Resource:     | CRAN        | Travis CI       | AppVeyor         |
| ------------- | ------------------- | --------------- | ---------------- |
| _Platforms:_  | _Multiple_          | _Linux & macOS_ | _Windows_        |
| R CMD check   | <a href="https://cran.r-project.org/web/checks/check_results_R.filesets.html"><img border="0" src="http://www.r-pkg.org/badges/version/R.filesets" alt="CRAN version"></a> | <a href="https://travis-ci.org/HenrikBengtsson/R.filesets"><img src="https://travis-ci.org/HenrikBengtsson/R.filesets.svg" alt="Build status"></a>   | <a href="https://ci.appveyor.com/project/HenrikBengtsson/r-filesets"><img src="https://ci.appveyor.com/api/projects/status/github/HenrikBengtsson/R.filesets?svg=true" alt="Build status"></a> |
| Test coverage |                     | <a href="https://codecov.io/gh/HenrikBengtsson/R.filesets"><img src="https://codecov.io/gh/HenrikBengtsson/R.filesets/branch/develop/graph/badge.svg" alt="Coverage Status"/></a>     |                  |
