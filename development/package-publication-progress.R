devtools::document()
#devtools::test()
devtools::check()

# libary(goodpractice)
# goodpractice::gp()
#
# the inteRgrate package
# check_pkg() installs package dependencies, builds, and installs the package, before running package check (by default this check is rather strict and any note or warning raises an error by default)
# check_lintr() runs lintr on the package, README, and the vignette. lintr checks whether your code adheres to certain standards and that you avoid syntax errors and semantic issues.
# check_tidy_description() makes sure that your DESCRIPTION file is tidy. If not, you can use usethis::use_tidy_description() to follows the tidyverse conventions for formatting.
# check_r_filenames() checks that all file extensions are .R and all names are lower case.
# check_gitignore() checks whether .gitignore contains standard files.
# check_version() ensures that you update your package version (might be good to run as the last step)
#
#
# evtools::check_win_devel()
# usethis:::use_github_action_check_standard()
#
#
#
# devtools::build_manual
#
#
# #die seite ist noch nicht zu finden, sondern glaub ich irgendwie nur lokal
#
# usethis::use_pkgdown(config_file = "./_pkgdown.yml")
# pkgdown::build_site()
# usethis::use_github_action("pkgdown")
#
# # https://nico-sievers.github.io/KOSMOSplotR
#
#
# # das zeug lässt sich nicht setzen
# globalVariables(c("KOSMOScurrentControlcol"))
#
#
#
#
# # install.packages("githubinstall")
# # githubinstall::githubinstall("nico-sievers/KOSMOSplotR")
#
