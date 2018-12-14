#' @title   mod_advanced_fileringUI and mod_advanced_filtering
#' @description  A shiny Module that allows the user to select the advanced filters
#'
#' @param id id for proper interaction with shiny
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
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
#' @export
#' @rdname mod_advanced_filteringUI
mod_advanced_filtering <- function(input, output, session) {

  # data_filtered_advanced_country <- reactive({
  #   dataframe %>%
  #     filter(
  #       !is.na(prix_euros),
  #       carrosserie == input$carrosserie
  #     ) %>%
  #     mutate(
  #       year = substr(date, 0, 4)
  #     ) %>%
  #     filter(
  #       (transmission %in% input$transmission) | (input$transmission == "No Preference"),
  #       (brand %in% input$brand) | (input$brand == "No Preference"),
  #       #year >= input$year_built[1] & year <= input$year_built[2],
  #       kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
  #       (energie %in% input$fuel) | (input$fuel == "No Preference"),
  #       (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
  #       (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
  #     )
  # })
  #
  # return(data_filtered_advanced_country)

}
