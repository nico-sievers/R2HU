####################################################
#                                                  #
#                       R2HU                       #
#                                                  #
####################################################
#                                                  #
# An R package containing Nico's code snippets for #
#                     sharing                      #
#                                                  #
#                  by Nico Sievers                 #
#                                                  #
#            first released 14/05/2024             #
#                                                  #
####################################################


### Intro and Disclaimer
# This is a collection of very Beta functions that
# I use in my own scripts and might share
# occasionally. They are not qualified for general
# use.


### Installation

# - Update R-Studio (help -> check for updates)
#   and R to the current versions. For R, this is
#   4.3.3, which you need to download directly from
#   www.r-project.org.
#
# - Also, update all of your currently installed
#   packages via
update.packages()
#
# - If possible on your computer start R-Studio
#   with administrator privileges and in a fresh
#   session with empty workspace.

# First, the package "devtools" is needed as it
# contains the function to download packages from
# GitHub:
install.packages("devtools")

# Next, install my package!
devtools::install_github("nico-sievers/R2HU")
library(R2HU)
help(package="R2HU")

# Before continuing with scripting - one more
# installation! GitHub packages don't update with
# other packages downloaded from CRAN. The following
# setup will ensure that updates I release for my
# package are installed on your computer:
devtools::install_github("hrbrmstr/dtupdate")

# During installation, you might be asked to update
# certain other packages if you didn't do so before.
# Select "update all" by typing the respective number
# into the console and hitting enter. Again, check
# for errors before testing:
dtupdate::github_update()

# This will list all packages installed from GitHub
# with their current and latest version numbers.
# Check that "KOSMOSplotR" is on the list. If any
# package is not up-to-date, run
dtupdate::github_update(T,F)
# to update them. Should that cause an error try
# alternatively
dtupdate::github_update(T)
# and enter the number of the package to update.

# Important: To ensure that you stay up-to-date with
# my patches (which there might be quite a few of
# especially in the beginning), include this line of
# code in the top section of every script that uses
# my package and run it before you plot ahead!
dtupdate::github_update(T,F)


### USAGE

# R2HUconcatenateFlowdata() function, check
?R2HUconcatenateFlowdata()

