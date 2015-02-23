# CRAN submission R.filesets 2.7.0
on 2015-02-23

Changes related to R/CRAN updates:

* Using Title Case.
* Registering S3 methods.
* requireNamespace() instead of require().


## Notes not sent to CRAN
R.filesets 2.7.0 and its 6 reverse-dependent packages(*) have been verified using `R CMD build` and `R CMD check --as-cran` on

* R version 3.0.3 (2014-03-06) [Platform: x86_64-unknown-linux-gnu(64-bit)].
* R version 3.1.2 Patched (2015-02-19 r67842) [Platform: x86_64-unknown-linux-gnu (64-bit)].
* R Under development (unstable) (2015-02-21 r67865) [Platform: x86_64-unknown-linux-gnu (64-bit)].

It has also been verified by the <http://win-builder.r-project.org/> service.

(*) The submitted updates cause no issues for any of the following 6 reverse dependencies on CRAN and Bioconductor: ACNE 0.7.0, aroma.affymetrix 2.13.0, aroma.cn 1.5.0, aroma.core 2.13.0, calmate 0.11.0 and MPAgenomics 1.1.2.
