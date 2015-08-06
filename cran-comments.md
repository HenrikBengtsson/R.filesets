# CRAN submission R.filesets 2.8.0
on 2015-08-06

This release address two R/CRAN related issues:

* Explicit import of 'utils' functions.
* readDataFrame() adjusted to updates in utils::read.table() as of svn rev 68831.

Thanks in advance


## Notes not sent to CRAN
R.filesets 2.8.0 has been verified using `R CMD build` and `R CMD check --as-cran` on

* R version 3.1.3 (2015-03-09) [Platform: x86_64-unknown-linux-gnu (64-bit)]
* R version 3.1.3 (2015-03-09) [Platform: x86_64-w64-mingw32/x64 (64-bit)]
* R version 3.2.2 beta (2015-08-05 r68859) [Platform: x86_64-unknown-linux-gnu (64-bit)]
* R version 3.2.2 beta (2015-08-04 r68843) [Platform: x86_64-w64-mingw32/x64 (64-bit)]
* R version 3.3.0 Under development (unstable) (2015-08-05 r68859) [Platform: x86_64-w64-mingw32/x64 (64-bit)]
  
It has also been verified by the <http://win-builder.r-project.org/> service.

Further more, its 6 reverse-dependent packages(*) have been verified with `R CMD check --as-cran` on:

* R version 3.1.3 (2015-03-09) [Platform: x86_64-unknown-linux-gnu (64-bit)]
* R version 3.2.2 beta (2015-08-05 r68859) [Platform: x86_64-unknown-linux-gnu (64-bit)]


(*) The submitted updates cause no issues for any of the following 6 reverse dependencies on CRAN and Bioconductor ACNE 0.8.0, aroma.affymetrix 2.13.2, aroma.cn 1.6.0, aroma.core 2.13.1, calmate 0.12.0 and MPAgenomics 1.1.2.
