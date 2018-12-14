#' @title   mod_table_areaUI and mod_table_area
#' @description  A shiny module that displays a table of the results for a 100km radius around the user's chosen city
#'
#' @param id shiny id
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @export
#' @examples
#' "No example to display"
#'
mod_table_areaUI <- function(id) {

  ns <- NS(id)

  # Displaying the result table
  DT::DTOutput(ns("table_area"))

}


#' mod_table_area server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with columns named "prix-euros", "latitude", "longitude", "nom_commune", "carrosserie", "transmission", "brand", "date", "energie", "nb_places", "kilometrage_km" and "nb_portes"
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @importFrom DT renderDT
#' @import sf
#' @importFrom utils data
#' @importFrom utils head
#' @export
#' @rdname mod_table_areaUI
mod_table_area <- function(input, output, session, dataframe) {

  # Assigning the crs to a specific variable to align all sf objects on the same crs
  crs_lambert <- 2154

  # Transforming the dataset into an sf object from latitude and longitude columns
  dataset_map <- dataframe %>%
    dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>%
    sf::st_transform(crs = crs_lambert)

  # Creating an eventReactive for the user input location
  point_user <- eventReactive(input$go, {dataset_map %>%
      dplyr::filter(nom_commune == input$city) %>%
      head(n = 1)
  })

  # Creating an eventReactive for the results corresponding to the basic filtering
  # of the user at the country level
  data_filtered_basic_country <- eventReactive(input$go, {
    dataset_map %>%
      dplyr::filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })

  # Creating a reactive for the results corresponding to the advanced filters
  # of the user at his area level
  data_filtered_advanced_100km <- reactive({
    data_filtered_basic_country() %>%
      dplyr::mutate(
        year = substr(date, 0, 4), # Extracting year from date column
        distance = as.vector(sf::st_distance(geometry, point_user())) # Calculating the distance between the user and each point
      ) %>%
      dplyr::filter(
        distance <= 100000,
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

  output$table_area <- DT::renderDT({
    result_table(data_filtered_advanced_100km())
  })

}
