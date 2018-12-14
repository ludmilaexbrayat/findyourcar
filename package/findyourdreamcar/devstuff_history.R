# the goal of this file is to keep track of all devtools/usethis
# call you make for yout project

# Feel free to cherry pick what you need and add elements

# install.packages("desc")
# install.packages("devtools")
# install.packages("usethis")

# Hide this file from build
usethis::use_build_ignore("devstuff_history.R")

# DESCRIPTION

library(desc)
# Create and clean desc
my_desc <- description$new("DESCRIPTION")
# Set your package name
my_desc$set("Package", "findyourdreamcar")

#Set your name
my_desc$set("Authors@R", "c(person('Skander', 'Kamoun', email = 'skander.kamoun@polytechnique.edu', role = c('cre', 'aut')), person('Ines', 'VanAgt', email = 'ines.vanagt@polytechnique.edu', role = c('cre', 'aut')), person('Arnaud', 'Fournier', email = 'arnaud.fournier@polytechnique.edu', role = c('cre', 'aut')), person('Ludmila', 'Exbrayat', email = 'ludmila.exbrayat@polytechnique.edu', role = c('cre', 'aut')), person('Gaelle', 'Guillou', email = 'gaelle.guillou@polytechnique.edu', role = c('cre', 'aut')))")

# Remove some author fields
my_desc$del("Maintainer")

# Set the version
my_desc$set_version("0.0.0.9000")

# The title of your package
my_desc$set(Title = "Find Your Dream Car")
# The description of your package
my_desc$set(Description = "Find your dream car with our production-ready app based on scrapped Paru Vendu listings from oct-nov 2018.")

# The urls
my_desc$set("URL", "https://github.com/ThinkR-open/findyourdreamcar")
my_desc$set("BugReports", "https://github.com/ThinkR-open/findyourdreamcar/issues")
# Save everyting
my_desc$write(file = "DESCRIPTION")

# If you want to use the MIT licence, code of conduct, lifecycle badge, and README
usethis::use_mit_license(name = "findyourdreamcar")
usethis::use_readme_rmd()
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge("Experimental")
usethis::use_news_md()

# For data
#library(readr)
#Sys.setlocale(locale="en_US.UTF-8")
#cardata <- data.frame(read_csv("data-raw/cardata.csv"))
#usethis::use_data(cardata, overwrite = T)

# For tests
usethis::use_testthat()
usethis::use_test("app")

# Dependencies
usethis::use_package("shiny")
usethis::use_package("DT")
usethis::use_package("stats")
usethis::use_package("graphics")
usethis::use_package("glue")
usethis::use_package("dplyr")
usethis::use_package("magrittr")
usethis::use_package("RColorBrewer")
usethis::use_package("readr")
usethis::use_package("sf")
usethis::use_package("ggplot2")
usethis::use_package("raster")
usethis::use_package("rasterVis")
usethis::use_package("units")
usethis::use_package("scales")
usethis::use_package("tibble")
usethis::use_package("purrr")

# R : set version with a patchlevel (z in x.y.z) = 0

# Reorder your DESC

usethis::use_tidy_description()

# Vignette
usethis::use_vignette("findyourdreamcar")
devtools::build_vignettes()

# Codecov
usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage()

# Test with rhub
rhub::check_for_cran()

