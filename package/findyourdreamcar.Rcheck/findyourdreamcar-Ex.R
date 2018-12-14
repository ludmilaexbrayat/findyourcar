pkgname <- "findyourdreamcar"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "findyourdreamcar-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('findyourdreamcar')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("mod_advanced_filteringUI")
### * mod_advanced_filteringUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_advanced_filteringUI
### Title: mod_advanced_fileringUI and mod_advanced_filtering
### Aliases: mod_advanced_filteringUI mod_advanced_filtering

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_advanced_filteringUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_area_statsUI")
### * mod_area_statsUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_area_statsUI
### Title: mod_area_statsUI and mod_area_stats
### Aliases: mod_area_statsUI mod_area_stats

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_area_statsUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_basic_filteringUI")
### * mod_basic_filteringUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_basic_filteringUI
### Title: mod_basic_fileringUI and mod_basic_filtering
### Aliases: mod_basic_filteringUI mod_basic_filtering

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_basic_filteringUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_country_statsUI")
### * mod_country_statsUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_country_statsUI
### Title: mod_country_statsUI and mod_country_stats
### Aliases: mod_country_statsUI mod_country_stats

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_country_statsUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_csv_fileInput")
### * mod_csv_fileInput

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_csv_fileInput
### Title: mod_csv_fileInput and mod_csv_file
### Aliases: mod_csv_fileInput mod_csv_file

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_csv_fileInput", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_map_areaUI")
### * mod_map_areaUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_map_areaUI
### Title: mod_map_areaUI and mod_map_area
### Aliases: mod_map_areaUI mod_map_area

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_map_areaUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_map_countryUI")
### * mod_map_countryUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_map_countryUI
### Title: mod_map_countryUI and mod_map_country
### Aliases: mod_map_countryUI mod_map_country

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_map_countryUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_regressionUI")
### * mod_regressionUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_regressionUI
### Title: mod_regressionUI and mod_regression
### Aliases: mod_regressionUI mod_regression

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_regressionUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_table_areaUI")
### * mod_table_areaUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_table_areaUI
### Title: mod_table_areaUI and mod_table_area
### Aliases: mod_table_areaUI mod_table_area

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_table_areaUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("mod_table_countryUI")
### * mod_table_countryUI

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: mod_table_countryUI
### Title: mod_table_countryUI and mod_table_country
### Aliases: mod_table_countryUI mod_table_country

### ** Examples

library(shiny)
library(DT)
if (interactive()){
ui <- fluidPage(
  mod_csv_fileInput("fichier"),
DTOutput("tableau")
)

server <- function(input, output, session) {
  data <- callModule(mod_csv_file,"fichier")
  output$tableau <- renderDT({data()})
}

shinyApp(ui, server)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("mod_table_countryUI", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_app")
### * run_app

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_app
### Title: run the Shiny Application
### Aliases: run_app

### ** Examples


if (interactive()) {

  run_app()

}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_app", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
