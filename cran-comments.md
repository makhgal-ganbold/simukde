
## Test environments

* local ubuntu 18.04, R 3.5.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs, WARNINGs or NOTEs when we checked with the function devtools::check().

But there was TWO NOTE at Maintainer in DESCRIPTION file and at testing the package when we used the function devtools::release(). However such line of DESCRIPTION file is looking properly. For second note, the command `R CMD check` was executed with parameters `--run-donttest --as-cran`. So the example in the enviroment `donttest` was run and it consumed some time which is approximately 90 seconds. In the first submission of the package, such example was wrapped by `dontrun`. But a CRAN member, who reviewed the submission, wrote "don't wrap by `dontrun`". So we wrapped it by `donttest` and it was accepted by CRAN. For this submission, we didn't change the example.

## Downstream dependencies

There are currently no downstream dependencies for this package.
Reverse dependencies were checked by the function devtools::use_revdep() and no errors or warnings found.
