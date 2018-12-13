
#' @import shiny
app_ui <- function() {
  fluidPage(
    titlePanel("Find Your Dream Car"),
    sidebarLayout(
      sidebarPanel(
        mod_basic_filteringUI("fichier")
      ),
      mainPanel(
        tabsetPanel(type = "tabs",
          tabPanel("Complementary Adjustments",
                   # Left column with adjustment inputs
                   column(6,
                          mod_advanced_filteringUI("fichier")
                          ),
                   column(6,
                          h3("Adjusted Price"),
                          h3(" "),
                          mod_area_statsUI("fichier"),
                          mod_country_statsUI("fichier")
                          )),

          tabPanel("At 30 min drive",
                   # Map Title
                   div(h3("Available cars around you")),
                   # Plotting the Map
                   mod_map_areaUI("fichier"),

                   # Table Title
                   div(h3("Summary Results")),
                   # Drawing the Table
                   mod_table_areaUI("fichier")),
          tabPanel("In the whole country",
                   # Map Title
                   div(h3("Available cars")),
                   # Plotting the Map

                   # Table Title
                   div(h3("Summary Results")),
                   # Drawing the Table
                   mod_table_countryUI("fichier"))
        )
      )
    )
  )
}
