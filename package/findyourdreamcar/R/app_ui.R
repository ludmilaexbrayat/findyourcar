
#' @import shiny
app_ui <- function() {
  fluidPage(

    # Title of the App
    titlePanel("Find Your Car"),

    sidebarLayout(
      sidebarPanel(
        # Select Input for the Postal Code:
        selectInput("city", "Select your city:", choices = dataset1$nom_commune %>% unique %>% sort()),

        # Select type of car: for the time being, choices are linked to factors taken by Carosserie in the first raw dataset
        selectInput("carrosserie", "Choose the type of car:", choices = dataset1$carrosserie %>% unique()),

        # Action Button (to be automatized)
        actionButton("go", "Go")
      ),


      mainPanel(

        # Creating the tabs
        tabsetPanel(type = "tabs",

                    # First tab
                    tabPanel("Complementary Adjustments",
                             # Left column with adjustment inputs
                             column(6,
                                    selectInput("transmission", "Choose the type of transmission:", choices = c("No Preference", dataset1$transmission[!is.na(dataset1$transmission)] %>% unique())),
                                    selectInput("brand", "Brand:", choices = c("No Preference", dataset1$brand[!is.na(dataset1$brand)] %>% unique() %>% sort())),
                                    sliderInput("year_built", "Year:", min = 1950, max = 2017, value = c(1950,2017)),
                                    sliderInput("mileage", "Mileage:", min = 10000, max = 100000, value = c(10000, 100000)),
                                    selectInput("fuel", "Type of fuel:", choices = c("No Preference", dataset1$energie[!is.na(dataset1$energie)] %>% unique() %>% sort())),
                                    selectInput("nb_seats", "Number of seats:", choices = c("No Preference", dataset1$nb_places[!is.na(dataset1$nb_places)] %>% unique() %>% sort())),
                                    selectInput("nb_doors", "Number of doors:", choices = c("No Preference", dataset1$nb_portes[!is.na(dataset1$nb_portes)] %>% unique() %>% sort()))
                             ),
                             # Right column with impact on the price
                             column(6,
                                    h3("Adjusted Price"),
                                    h3(" "),
                                    h4("In a 100km radius"),
                                    h4(" "),
                                    h5("Mean price:"),
                                    textOutput(outputId = "mean_area"),
                                    h5("Minimum price:"),
                                    textOutput(outputId = "min_area"),
                                    h5("Maximum price:"),
                                    textOutput(outputId = "max_area"),
                                    h3(" "),
                                    h4("In the whole country"),
                                    h4(" "),
                                    h5("Mean price: "),
                                    textOutput(outputId = "mean"),
                                    h5("Minimum price:"),
                                    textOutput(outputId = "min"),
                                    h5("Maximum price:"),
                                    textOutput(outputId = "max")),
                             # Bottom right column with additional informations
                             column(6,
                                    h3("For your information"),
                                    h5("Importance of main parameters in the price"),
                                    plotOutput("fyi"))
                    ),

                    # Second Tab
                    tabPanel("At 30 min drive",
                             # Map Title
                             div(h3("Available cars around you")),
                             # Plotting the Map
                             plotOutput("drive_map"),

                             # Table Title
                             div(h3("Summary Results")),
                             # Drawing the Table
                             DT::DTOutput("table_area")),

                    # Third Tab
                    tabPanel("In the whole country",
                             # Map Title
                             div(h3("Available cars")),
                             # Plotting the Map
                             plotOutput("map"),

                             # Table Title
                             div(h3("Summary Results")),
                             # Drawing the Table
                             DT::DTOutput("table"))
        )
      )
    )
  )
}
