
# Incidents R package
An R package for predicting failures in machine. The prediction can be made with i.e. one day (or more) in adavnaced, and it works for any type of failure. The procedure is based on flexible regression models, particularly, generalized additive models. 

### Index
* [Requirements](README.md#requirements)
* [Build](README.md#build)
* [Installation](README.md#installation)

### Requirements
To build and test the package you need an R environment and the R's devtools package.

To have an R environment, you can use the [guides of Digital Ocean to install R on Ubuntu | https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-16-04-2] or [launch a docker container | https://github.com/rocker-org/rocker/tree/master/r-base].

With your R environment running, you can install devtools from R console using:
``
install.packages('devtools')
``
Also, you can install the package from the command line using:
``
R CMD install devtools
``
NOTE: In you are working on MacOS, perhaps you have problems with dependencies (openssl) installing devtools. Please, check [this link | https://github.com/ropensci/git2r/issues/204]

### Build
The build process will generate a tar.gz file you can use to install the package in an R environment.
``
library(devtools)
devtools::build()
``

### Installation
With your R environment running, you can install devtools from R console using:
``
install.packages(<path_to_your_tar.gz_package>)
``
Also, you can install the package from the command line using:
``
R CMD install <path_to_your_tar.gz_package>
``

