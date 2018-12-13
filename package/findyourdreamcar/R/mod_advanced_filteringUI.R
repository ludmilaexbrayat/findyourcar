#' @title   mod_advanced_fileringUI and mod_advanced_filtering
#' @description  A shiny Module that allows the user to select the advanced filters
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
mod_advanced_filteringUI <- function(id) {

  ns <- NS(id)

  tagList(
    selectInput(ns("transmission"), "Choose the type of transmission:", choices = c("No Preference", cardata$transmission[!is.na(cardata$transmission)] %>% unique())),
    selectInput(ns("brand"), "Brand:", choices = c("No Preference", cardata$brand[!is.na(cardata$brand)] %>% unique() %>% sort())),
    sliderInput(ns("year_built"), "Year:", min = 1950, max = 2017, value = c(1950,2017)),
    sliderInput(ns("mileage"), "Mileage:", min = 10000, max = 100000, value = c(10000, 100000)),
    selectInput(ns("fuel"), "Type of fuel:", choices = c("No Preference", cardata$energie[!is.na(cardata$energie)] %>% unique() %>% sort())),
    selectInput(ns("nb_seats"), "Number of seats:", choices = c("No Preference", cardata$nb_places[!is.na(cardata$nb_places)] %>% unique() %>% sort())),
    selectInput(ns("nb_doors"), "Number of doors:", choices = c("No Preference", cardata$nb_portes[!is.na(cardata$nb_portes)] %>% unique() %>% sort()))
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
#' @rdname mod_advanced_filteringUI
mod_advanced_filtering <- function(input, output, session) {

}
