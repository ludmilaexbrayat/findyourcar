
# Please note that this is a first version whose only aim is to show what the interface could look like. 
# No automation has been made yet regarding the input made by the user
# We have chosen to work with the first raw dataset as a first approach, and then extend it to the entire dataset when ready
# Name of variables as well as other choices will have to be fitted to the "clean" dataset

library(shiny)

# Define UI for application
ui <- fluidPage(
   
  # Title of the App
   titlePanel("Find Your Car"),
   
   sidebarLayout(
      sidebarPanel(
        # Slider Input for the Price
        sliderInput("Price", "Choose a price range (in euros):", min = 1000, max = 20000, value = c(2000, 10000)),
        
        # Select Input for the Postal Code: for the time being, choices are linked to the "Mapping_villes" csv file
        selectInput("postalcode", "Select your postal code:", choices = mapping_villes$codes_postaux %>% sort()),
        
        # Select type of car: for the time being, choices are linked to factors taken by Carosserie in the first raw dataset
        selectInput("carosserie", "Choose the type of car:", choices = dataset1$Carrosserie %>% unique()),
        actionButton("go", "Go")
        ),
      
      
      mainPanel(
        
        # Creating the tabs
        tabsetPanel(type = "tabs",
        
        # First Tab
         tabPanel("Results Overview",
                  # Map Title
                  div(h3("Available cars around you")),
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

server <- function(input, output) {

  ### PACKAGES AND DATASETS AREA
  
  library(readr)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(raster)
  library(rasterVis)
  
  # For the time being, we work with dataset1
  dataset1 <- read_csv("datasets/scrapped_listings_1_1_500.csv")
  
  # Loading "Mapping_villes" csv file for Postal Codes matches
  mapping_villes <- read_csv2("datasets/mapping_villes.csv")
  
  # Loading the department shapes
  dept_193 <- st_read("cartography/DEPARTEMENT.shp")
  
  
  ### FUNCTION AREA
  
  # Creating a function to plot the maps
  plot_map <- function() {
    ggplot(dept_193) +
      geom_sf() +
      coord_sf(crs = st_crs(dept_193)) +
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
             panel.grid.major=element_blank(),
             panel.grid.minor=element_blank(),
             plot.background=element_blank())
  }
  
  # Creating a function to draw the appropriate table (this is just an example)
  result_table <- function() {
    head(
      dataset1 %>% 
      dplyr::select(Prix, brand, modele, address_pro, Kilométrage, Transmission) %>% 
      filter(!is.na(brand) & !is.na(address_pro) & !is.na(brand))
    )
  }
  
  
  ### OUTPUT AREA
  
   output$map <- renderPlot({
     plot_map()
   })
   
   output$table <- DT::renderDT({
     result_table()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

