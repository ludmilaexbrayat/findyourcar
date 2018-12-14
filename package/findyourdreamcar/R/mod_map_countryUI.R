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

#' @title   mod_map_countryUI and mod_map_country
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
mod_map_countryUI <- function(id) {

  ns <- NS(id)

  plotOutput(ns("map"))

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
#' @import units
#' @import readr
#' @import scales
#' @import ggplot2
#' @importFrom utils data
#' @importFrom utils head
#' @export
#' @rdname mod_map_countryUI
mod_map_country <- function(input, output, session, dataframe) {

  crs_lambert <- 2154

  dept_193 <- sf::st_read(system.file("extdata", "DEPARTEMENT.shp", package = "findyourdreamcar"))

  dpt_map <- dept_193 %>%
    sf::st_transform(crs = crs_lambert)

  region_map <- dpt_map %>%
    dplyr::group_by(NOM_REG, CODE_REG) %>%
    summarize()

  # Adding the coordinates of the centroids of each region
  coord <- region_map %>%
    sf::st_centroid() %>%
    dplyr::arrange(CODE_REG) %>%
    sf::st_coordinates() %>%
    as.data.frame()

  list_regions <- region_map %>% dplyr::arrange(CODE_REG)

  coord$CODE_REG <- list_regions$CODE_REG

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

  # Computing the average price per region
  prices_per_region <- reactive({dataset_map %>%
      sf::st_join(dpt_map) %>%
      dplyr::mutate(
        year = substr(date, 0, 4)
      ) %>%
      dplyr::filter(carrosserie == input$carrosserie,
             (transmission %in% input$transmission) | (input$transmission == "No Preference"),
             (brand %in% input$brand) | (input$brand == "No Preference"),
             year >= input$year_built[1] & year <= input$year_built[2],
             kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
             (energie %in% input$fuel) | (input$fuel == "No Preference"),
             (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
             (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")) %>%
      dplyr::group_by(CODE_REG) %>%
      dplyr::summarize(mean_price = as.integer(mean(prix_euros))) %>%
      merge(coord, by = c("CODE_REG" = "CODE_REG"))})

  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  cc_dark <- scales::seq_gradient_pal("orange","black")(seq(0,1,length.out=15))

  # Creating a function to plot the map of the whole country
  plot_map <- function(map, point_user) {
    ggplot(region_map) +
      geom_sf(fill = "white") +
      geom_sf(data = point_user, color = "orange", size = 5) + # Plotting a point at the position of the user
      geom_text(data = map, aes(x = X, y = Y, label = mean_price, colour = as.factor(mean_price))) + # Adding label with prices
      scale_color_manual(values = cc_dark) + # Applying the palette we have built further up
      coord_sf(crs = st_crs(region_map)) +
      #All the lines below have as sole purpose to erase all the grids, axis, etc. of the plot
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_line(colour = "white"),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  }

  output$map <- renderPlot({
    plot_map(map = prices_per_region(), point_user = point_user())
  })

}
