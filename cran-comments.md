## Resubmission

This is a resubmission. In this version I have:

* Added references for the method in the 'Description' field of the DESCRIPTION file and in the documentation of the simulate_kde function.
* Unwrapped the 1st example which can be executed in < 5 sec. For 2nd example created an additional small example which is wrapped in \dontshow{}. And a version of the example for users is wrapped in \donttest{}.
* Added CITATION file.
* Added some tetst by using testthat and changed some lines of R code.
* Rechecked with R CMD check.

## Test environments

* local ubuntu 18.04, R 3.5.0
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs when we checked with the function devtools::check().

But there was only one NOTE at Maintainer in DESCRIPTION file when we used the function devtools::release(). However such line of DESCRIPTION file is looking properly.

## Downstream dependencies

There are currently no downstream dependencies for this package.
