#' @title   mod_basic_fileringUI and mod_basic_filtering
#' @description  A shiny module that allows the user to select basic filters
#'
#' @param id shiny id
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @export
#' @examples
#' library(shiny)
#' library(dplyr)
#' library(magrittr)
#' library(utils)
#' if (interactive()){
#'   ui <- fluidPage(
#'     mod_basic_filteringUI("fichier")
#'   )
#'
#'   server <- function(input, output, session) {
#'   }
#'
#'   shinyApp(ui, server)
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
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @export
#' @rdname mod_basic_filteringUI
mod_basic_filtering <- function(input, output, session) {
}
