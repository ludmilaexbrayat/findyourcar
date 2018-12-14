#' @title   mod_map_areaUI and mod_map_area
#' @description  A shiny module that displays a map of the results for a 100km radius around the user's chosen city
#'
#' @param id shiny id
#'
#' @import shiny
#' @export
#' @examples
#' "No example to display"
#'
mod_map_areaUI <- function(id) {

  ns <- NS(id)

  # Plotting the map of the results around the user
  plotOutput(ns("drive_map"))

}


#' mod_map_area server function
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
#' @importFrom scales seq_gradient_pal
#' @import ggplot2
#' @import units
#' @import readr
#' @importFrom utils data
#' @importFrom utils head
#' @export
#' @rdname mod_map_areaUI
mod_map_area <- function(input, output, session, dataframe) {

  # Assigning the crs to a specific variable to align all sf objects on the same crs
  crs_lambert <- 2154

  # Laoding department map shapes
  dept_193 <- sf::st_read(system.file("extdata", "DEPARTEMENT.shp", package = "findyourdreamcar"))

  # Aligning crs of department map
  dpt_map <- dept_193 %>%
    sf::st_transform(crs = crs_lambert)

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

  # Creating a function returning the polygon of the area around the argument point
  area_around <- function(geometry) {
    sf::st_buffer(geometry, dist = units::set_units(100, km)) %>%
      sf::st_transform(crs = sf::st_crs(dpt_map))
  }

  # Creating a polygon of the intersection of the department map and the area around the user
  intersection <- reactive({
    sf::st_intersection(dpt_map, area_around(point_user()))
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

  # Creating extended palettes for the sake of designing the maps
  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  cc_dark <- scales::seq_gradient_pal("orange","black")(seq(0,1,length.out=15))

  # Creating a function that takes in arguments the location of the user, the intersection with the map
  # and the area around the user, and the results in this area and returning the map of results
  plot_drive_map <- function(intersection, point_user, points_around) {
    ggplot2::ggplot(intersection) +
      geom_sf(aes(fill = CODE_REG)) +
      geom_sf(data = points_around, color = "black", size = 1) +
      geom_sf(data = point_user, color = "orange", size = 5) + # Plotting a point at the position of the user
      coord_sf(crs = sf::st_crs(dpt_map)) +
      scale_fill_manual(values = cc) + # Applying the palette we have built further up
      # All the lines below have as sole purpose to erase all the grids, axis, etc. of the plot
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank(),
            panel.grid.major = element_line(colour = "white"))
  }

  output$drive_map <- renderPlot({
    plot_drive_map(intersection = intersection(), point_user = point_user(), points_around = data_filtered_advanced_100km())
  })

}
