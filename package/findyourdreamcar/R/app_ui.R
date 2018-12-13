
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
                          )),
          tabPanel("At 30 min drive", h5("")),
          tabPanel("In the whole country", h5(""))
        )
      )
    )
  )
}
