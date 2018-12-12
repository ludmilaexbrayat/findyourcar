# This script allow you to quick clean your R session
# update documentation and NAMESPACE, localy install the package
# and run the example used to show how mod_csv_fileInput work
.rs.api.documentSaveAll() # close and save all open file
suppressWarnings(lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE))
rm(list=ls(all.names = TRUE))
devtools::document('.')
devtools::load_all('.')
options(app.prod=FALSE) # TRUE = production mode, FALSE = development mode

# example("mod_csv_fileInput",package = "findyourdreamcar") # PR welcome
library(shiny)
library(dplyr)
library(magrittr)
if (interactive()){
  ui <- fluidPage(
      sidebarLayout(
        sidebarPanel(
          mod_basic_filteringUI("fichier")
      )
    )
  )

  server <- function(input, output, session) {
    fichier <- callModule(mod_basic_filtering, "fichier")
  }

  shinyApp(ui, server)
}

