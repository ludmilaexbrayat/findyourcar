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

#' @title   mod_table_countryUI and mod_table_country
#' @description  A shiny Module that shows principal statistics for the selected basic and advanced filters of the user in a 100km area
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
mod_table_countryUI <- function(id) {

  ns <- NS(id)

  DT::DTOutput(ns("table"))

}


#' mod_table_country server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with at one column named "prix-euros" containing the price
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @importFrom utils data
#' @export
#' @rdname mod_table_countryUI
mod_table_country <- function(input, output, session, dataframe) {

  # Creating an eventreactive for the basic filters on the condition of the "Go" button
  data_filtered_basic_country <- eventReactive(input$go, {
    dataframe %>%
      filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })

  # Creating a reactive for the advanced filters at the country level
  data_filtered_advanced_country <- reactive({
    data_filtered_basic_country() %>%
      mutate(
        year = substr(date, 0, 4)
      ) %>%
      filter(
        (transmission %in% input$transmission) | (input$transmission == "No Preference"),
        (brand %in% input$brand) | (input$brand == "No Preference"),
        year >= input$year_built[1] & year <= input$year_built[2],
        kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
        (energie %in% input$fuel) | (input$fuel == "No Preference"),
        (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
        (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
      )
  })

  result_table <- function(data) {
    table <- as.data.frame(data) %>%
      dplyr::select(prix_euros, brand, modele, nom_commune, kilometrage_km, transmission) %>%
      filter(!is.na(brand) & !is.na(nom_commune))

    colnames(table) <- c("Price", "Brand", "Modele", "City", "Mileage", "Transmission")

    return(table)
  }

  output$table <- DT::renderDT({
    result_table(data_filtered_advanced_country())
  })

}
