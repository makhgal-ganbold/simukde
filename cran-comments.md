
## Resubmission

This is resubmission. The reviewer tell us to change a URL in the file README.md which is redirected to the other address with SSL and www.
Now the URL http://galaa.mn/ is changed to https://www.galaa.mn/.

## Test environments

* Ubuntu 20.04, R 3.6.3 (local machine)
* r-hub
* win-builder (devel, oldrelease and release)

## R CMD check results

There were no ERRORs or WARNINGs. The only one NOTE was detected for "checking for future file timestamps" on r-hub and its message was "unable to verify current time". However, there was no NOTE for R-devel on Fedora.

For win-builder, one NOTE was detected for the maintainer address on R-devel only. However, it seems to be fine.

Also, the example in the enviroment `donttest` consumes long time which is approximately 90 seconds. In the first submission of the package, such example was wrapped by `dontrun`. But a CRAN member, who reviewed the submission, wrote "don't wrap by `dontrun`". So we wrapped it by `donttest` and it was accepted by CRAN. For second submission, we didn't change the example.

## Downstream dependencies

There are currently no downstream dependencies for this package. Reverse dependencies were checked by the function `tools::package_dependencies()`.
