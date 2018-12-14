#' @title   mod_area_statsUI and mod_area_stats
#' @description  A shiny module that displays principal statistics for the basic and advanced filters selected by the user in a 100km radius
#'
#' @param id shiny id
#'
#' @import shiny
#' @export
#' @examples
#' "No example to display"
#'
mod_area_statsUI <- function(id) {

  ns <- NS(id)

  tagList(
    h4("In a 100km radius area:"),
    h4(" "),
    h5("Mean price: "),
    textOutput(outputId = ns("mean2")),
    h5("Minimum price:"),
    textOutput(outputId = ns("min2")),
    h5("Maximum price:"),
    textOutput(outputId = ns("max2"))
  )

}


#' mod_area_stats server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with columns named "prix-euros", "latitude", "longitude", "nom_commune", "carrosserie", "transmission", "brand", "date", "energie", "nb_places", "kilometrage_km" and "nb_portes"
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @import sf
#' @import readr
#' @import units
#' @importFrom utils data
#' @importFrom utils head
#' @export
#' @rdname mod_area_statsUI
mod_area_stats <- function(input, output, session, dataframe) {

  crs_lambert <- 2154

  dataset_map <- dataframe %>%
    dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>%
    sf::st_transform(crs = crs_lambert)

  point_user <- eventReactive(input$go, {dataset_map %>%
      dplyr::filter(nom_commune == input$city) %>%
      head(n = 1)
  })

  data_filtered_basic_country <- eventReactive(input$go, {
    dataset_map %>%
      dplyr::filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })

  # Creating a reactive for the advanced filters at the country level
  data_filtered_advanced_100km <- reactive({
    data_filtered_basic_country() %>%
      dplyr::mutate(
        year = substr(date, 0, 4),
        distance = as.vector(sf::st_distance(geometry, point_user()))
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

  result_price <- reactive({
    c(
      mean(data_filtered_advanced_100km()$prix_euros),
      max(data_filtered_advanced_100km()$prix_euros),
      min(data_filtered_advanced_100km()$prix_euros)
    )
  })

  output$mean2 <- renderText({
    round(result_price()[1])
  })

  output$max2 <- renderText({
    round(result_price()[2])
  })

  output$min2 <- renderText({
    round(result_price()[3])
  })

}
