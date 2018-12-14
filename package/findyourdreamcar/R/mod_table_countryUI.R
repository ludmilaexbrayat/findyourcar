#' @title   mod_table_countryUI and mod_table_country
#' @description  A shiny module that displays a table of the results in the whole country
#'
#' @param id shiny id
#'
#' @importFrom DT DTOutput
#' @import shiny
#' @export
#' @examples
#' "No example to display"
#'
mod_table_countryUI <- function(id) {

  ns <- NS(id)

  # Displaying the result table
  DT::DTOutput(ns("table"))

}


#' mod_table_country server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with columns named "prix-euros", "nom_commune", "carrosserie", "transmission", "brand", "date", "energie", "nb_places", "kilometrage_km" and "nb_portes"
#'
#' @import dplyr
#' @import magrittr
#' @importFrom DT renderDT
#' @import shiny
#' @importFrom utils data
#' @export
#' @rdname mod_table_countryUI
mod_table_country <- function(input, output, session, dataframe) {

  # Creating an eventReactive for the results corresponding to the basic filtering
  # of the user at the country level
  data_filtered_basic_country <- eventReactive(input$go, {
    dataframe %>%
      filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })

  # Creating a reactive for the results corresponding to the advanced filters
  # of the user at the country level
  data_filtered_advanced_country <- reactive({
    data_filtered_basic_country() %>%
      dplyr::mutate(
        year = substr(date, 0, 4) # Extracting year from date column
      ) %>%
      dplyr::filter(
        (transmission %in% input$transmission) | (input$transmission == "No Preference"),
        (brand %in% input$brand) | (input$brand == "No Preference"),
        year >= input$year_built[1] & year <= input$year_built[2],
        kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
        (energie %in% input$fuel) | (input$fuel == "No Preference"),
        (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
        (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
      )
  })

  # Creating a function to output the dataframe with appropriate colnames
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
