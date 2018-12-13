# Naming convention :
# all Shinymodule have to begin with `mod_`, in lowercase Except for `UI` , `Input` and `Output`
# use `Input` as sufix if your module is an Input
# use `Output` as sufix if your module is an Output
# use `UI` as sufix if your module is both Input and Output
#
# examples :
# ui side : mod_truc_bidulUI
# server side : mod_truc_bidul
#
# ui side : mod_machin_chouetteInput
# server side : mod_machin_chouette

# all shinyModule must have a documentation page
# one unique page for both ui and server side ( you can use `#' @rdname` to link both function)

# A minimalist example is mandatory

#' @title   mod_basic_fileringUI and mod_basic_filtering
#' @description  A shiny Module that allows the user to select basic filters
#'
#' @import dplyr
#' @import magrittr
#' @export
#' @examples
#' library(shiny)
#' library(DT)
#' if (interactive()){
#' ui <- fluidPage(
#'   mod_csv_fileInput("fichier"),
#' DTOutput("tableau")
#' )
#'
#' server <- function(input, output, session) {
#'   data <- callModule(mod_csv_file,"fichier")
#'   output$tableau <- renderDT({data()})
#' }
#'
#' shinyApp(ui, server)
#' }
#'
mod_basic_filteringUI <- function(id) {

  ns <- NS(id)

    tagList(
      # Select Input for the Postal Code:
      selectInput(ns("city"), "Select your city:", choices = cardata$nom_commune %>% unique %>% sort()),

      # Select type of car: for the time being, choices are linked to factors taken by Carosserie in the first raw dataset
      selectInput(ns("carrosserie"), "Choose the type of car:", choices = cardata$carrosserie %>% unique()),

      # Action Button (to be automatized)
      actionButton(ns("go"), "Go")
    )

}


#' mod_basic_filtering server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom utils data
#' @export
#' @rdname mod_basic_filteringUI
mod_basic_filtering <- function(input, output, session) {

  #data("cardata")

}
