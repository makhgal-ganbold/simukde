
## Test environments

* Ubuntu 20.04, R 3.6.3 (local machine)
* r-hub
* win-builder (devel, oldrelease and release)

## R CMD check results

There were no ERRORs or WARNINGs or NOTEs.

In addition, the example in the enviroment `donttest` consumes long time which is approximately 90 seconds. In the first submission of the package, such example was wrapped by `dontrun`. But a CRAN member, who reviewed the submission, wrote "don't wrap by `dontrun`". So we wrapped it by `donttest` and it was accepted by CRAN. For second submission, we didn't change the example.

## Downstream dependencies

There are currently no downstream dependencies for this package. Reverse dependencies were checked by the following form `tools::package_dependencies(packages = "simukde", reverse = TRUE)`.
