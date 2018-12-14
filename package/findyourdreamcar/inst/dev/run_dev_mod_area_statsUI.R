.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE))
rm(list=ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod=FALSE) # TRUE = production mode, FALSE = development mode

# example("mod_area_stats",package = "findyourdreamcar") # PR welcome
library(shiny)
library(dplyr)
library(magrittr)
library(readr)
library(units)
library(utils)
if (interactive()){
  ui <- fluidPage(
    mod_area_statsUI("fichier")
  )

  server <- function(input, output, session) {
    callModule(mod_area_stats, "fichier", cardata, "Montpellier", "Berline")
  }

  shinyApp(ui, server)
}

