## Resubmission

## Test environments
* ubuntu 14.04 (on travis-ci), (release)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Richard White <hello@rwhite.no>’

Resubmission

* checking re-building of vignette outputs ... [509s] OK

Can this be halved? e.g. by using few iterations, toy data, or providing
pre computed resuls of lengthy parts? Otherwise we cannot check the
vignette on CRAN.

This is now solved by the argument "n_sim" that is set to 20 simulations (instead of the previous default of 500).


Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:.....>?

This is now added.

## Downstream dependencies

none
