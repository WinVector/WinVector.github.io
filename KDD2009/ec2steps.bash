#!/bin/bash

sudo apt-get -y update
# install some common tools
sudo apt-get -y install git emacs default-jre default-jdk gcc gfortran

# from https://cran.r-project.org
printf 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/\n'| sudo tee -a /etc/apt/sources.list
sudo apt-get update
sudo apt-get -y --force-yes install r-base r-base-dev
# may need to turn on backports
# From: http://stackoverflow.com/questions/20923209/problems-installing-the-devtools-package
sudo apt-get -y build-dep libcurl4-gnutls-dev
sudo apt-get -y install libcurl4-gnutls-dev
# from: http://stackoverflow.com/questions/30794035/install-packagesdevtools-on-r-3-0-2-fails-in-ubuntu-14-04
sudo apt-get -y install libssl-dev
sudo apt-get -y install xml2
# from: http://stackoverflow.com/questions/30837741/failed-to-install-devtools-in-r-3-1-2
sudo apt-get -y install libxml2-dev

# install the packages we need
# not sure https mirrors are quite ready for prime time
 sudo Rscript ec2steps.R



