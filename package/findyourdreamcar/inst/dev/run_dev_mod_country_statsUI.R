.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE))
rm(list=ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod=FALSE) # TRUE = production mode, FALSE = development mode

# example("mod_country_stats",package = "findyourdreamcar") # PR welcome
library(shiny)
library(dplyr)
library(magrittr)
if (interactive()){
  ui <- fluidPage(
    mod_country_statsUI("fichier")
  )

  server <- function(input, output, session) {
    callModule(mod_country_stats, "fichier", cardata)
  }

  shinyApp(ui, server)
}

