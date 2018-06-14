
# Incidents R package
An R package for predicting failures in machines. Particuarly, for HVAC systems (Heating, ventilation, and air conditioning) which are the technology of indoor and vehicular environmental comfort. 

This project was developed to provide a solution, based on bigdata and on machine learning techniques, to a company focused on energy efficiency. The company needs are to know, in adavanced, when a particularly HVAC system failed (no matter the reason). The prediction can be made with i.e. one day (or more) in advanaced, and it works for any type of failure (blind of the cause). 

"Incidents" collects, analyze, visualize and predicts failures in real scenario. The procedure is based on flexible regression models, particularly, generalized additive models. The data set (not shown) were collected from a big company that have many shops in spain and arround the world with these type of HVAC systems. 


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

