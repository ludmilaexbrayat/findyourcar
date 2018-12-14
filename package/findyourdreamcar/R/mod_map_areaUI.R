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

#' @title   mod_map_areaUI and mod_map_area
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
mod_map_areaUI <- function(id) {

  ns <- NS(id)

  plotOutput(ns("drive_map"))

}


#' mod_table_area server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with at one column named "prix-euros" containing the price
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @import sf
#' @import ggplot2
#' @import units
#' @import readr
#' @importFrom utils data
#' @importFrom utils head
#' @export
#' @rdname mod_map_areaUI
mod_map_area <- function(input, output, session, dataframe) {

  crs_lambert <- 2154

  dept_193 <- sf::st_read(system.file("extdata", "DEPARTEMENT.shp", package = "findyourdreamcar"))

  dpt_map <- dept_193 %>%
    sf::st_transform(crs = crs_lambert)

  dataset_map <- dataframe %>%
    dplyr::filter(!is.na(longitude) & !is.na(latitude)) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>%
    sf::st_transform(crs = crs_lambert)

  point_user <- eventReactive(input$go, {dataset_map %>%
      dplyr::filter(nom_commune == input$city) %>%
      head(n = 1)
  })

  area_around <- function(geometry) {
    sf::st_buffer(geometry, dist = units::set_units(100, km)) %>%
      sf::st_transform(crs = sf::st_crs(dpt_map))
  }

  intersection <- reactive({
    sf::st_intersection(dpt_map, area_around(point_user()))
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

  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  cc_dark <- scales::seq_gradient_pal("orange","black")(seq(0,1,length.out=15))

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
