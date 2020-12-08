# CRAN submission R.filesets 2.14.0

on 2020-12-07

I've verified that this submission causes no issues for any of the 5 reverse package dependencies on CRAN.

Thanks in advance


## Notes not sent to CRAN

### R CMD check validation

The package has been verified using `R CMD check --as-cran` on:

| R version   | GitHub Actions | Travis CI | AppVeyor CI | Rhub      | Win-builder | Other  |
| ----------- | -------------- | --------- | ----------- | --------- | ----------- | ------ |
| 3.3.*       | L              |           |             |           |             |        |
| 3.4.*       | L              |           |             |           |             |        |
| 3.5.*       | L              |           |             |           |             |        |
| 3.6.*       | L, W           | L,        |             |    S (32) |             |        |
| 4.0.*       | L, W           | L, M      | W           |           | W           |        |
| devel       |    W           | L, M      | W (32 & 64) | W         | W           |        |

*Legend: OS: L = Linux, S = Solaris, M = macOS, W = Windows.  Architecture: 32 = 32-bit, 64 = 64-bit*
