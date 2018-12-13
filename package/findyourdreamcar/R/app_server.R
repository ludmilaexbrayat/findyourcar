#' @import shiny
#' @importFrom graphics hist
#' @importFrom stats rnorm
#'
app_server <- function(input, output,session) {

  if ( app_prod() ){message("prod mode")}else{message("dev mode")}
  output$distPlot <- renderPlot({
    x    <- rnorm(1000)
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })

  # basic_filtering <- callModule(mod_basic_filtering, "fichier1", cardata)
  #
  # advanced_filtering <- callModule(mod_advanced_filtering, "fichier2", cardata)
  #
  # callModule(mod_country_stats, "fichier3", advanced_filtering)

  callModule(mod_area_stats, "fichier", cardata)

  callModule(mod_country_stats, "fichier", cardata)

  callModule(mod_table_country, "fichier", cardata)

  callModule(mod_table_area, "fichier", cardata)

  callModule(mod_map_area, "fichier", cardata)

  callModule(mod_map_country, "fichier", cardata)

}
