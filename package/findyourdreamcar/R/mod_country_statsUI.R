#' @title   mod_country_statsUI and mod_country_stats
#' @description  A shiny module that displays principal statistics for the basic and advanced filters selected by the user in the whole country
#'
#' @param id shiny id
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @export
#' @examples
#' "No example to display"
#'
mod_country_statsUI <- function(id) {

  ns <- NS(id)

  tagList(
    h4("In the whole country"),
    h4(" "),
    h5("Mean price: "),
    textOutput(outputId = ns("mean")),
    h5("Minimum price:"),
    textOutput(outputId = ns("min")),
    h5("Maximum price:"),
    textOutput(outputId = ns("max"))
    )

}


#' mod_country_stats server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with columns named "prix-euros", "nom_commune", "carrosserie", "transmission", "brand", "date", "energie", "nb_places", "kilometrage_km" and "nb_portes"
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @importFrom utils data
#' @export
#' @rdname mod_country_statsUI
mod_country_stats <- function(input, output, session, dataframe) {

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

  # Creating a vector with the 3 stats
  result_price <- reactive({
    c(
      mean(data_filtered_advanced_country()$prix_euros),
      max(data_filtered_advanced_country()$prix_euros),
      min(data_filtered_advanced_country()$prix_euros)
    )
  })

  output$mean <- renderText({
    round(result_price()[1])
  })

  output$max <- renderText({
    round(result_price()[2])
  })

  output$min <- renderText({
    round(result_price()[3])
  })

}
