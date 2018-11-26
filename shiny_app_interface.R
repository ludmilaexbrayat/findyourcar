
# Please note that this is a first version whose only aim is to show what the interface could look like. 
# No automation has been made yet regarding the input made by the user
# We have chosen to work with the first raw dataset as a first approach, and then extend it to the entire dataset when ready
# Name of variables as well as other choices will have to be fitted to the "clean" dataset

library(shiny)
library(dplyr)
library(RColorBrewer)

# Define UI for application
ui <- fluidPage(
   
  # Title of the App
   titlePanel("Find Your Car"),
   
   sidebarLayout(
      sidebarPanel(
        # Slider Input for the Price
        sliderInput("Price", "Choose a price range (in euros):", min = 1000, max = 40000, value = c(2000, 10000)),
        
        # Select Input for the Postal Code: for the time being, choices are linked to the "Mapping_villes" csv file
        selectInput("postalcode", "Select your postal code:", choices = mapping_villes$codes_postaux %>% sort()),
        
        # Select type of car: for the time being, choices are linked to factors taken by Carosserie in the first raw dataset
        selectInput("carosserie", "Choose the type of car:", choices = dataset1$Carrosserie %>% unique()),
        
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
                        selectInput("transmission", "Choose the type of transmission:", choices = c("Manual", "Automatic", "No Preference")),
                        selectInput("brand", "Brand:", choices = c("No Preference", dataset1$brand %>% unique())),
                        sliderInput("year_built", "Year:", min = 1950, max = 2017, value = c(2000,2014)),
                        sliderInput("mileage", "Mileage:", min = 10000, max = 100000, value = c(50000, 80000)),
                        selectInput("fuel", "Type of fuel:", choices = dataset1$Energie %>% unique()),
                        selectInput("nb_seats", "Number of seats:", choices = dataset1$`Nombre de places` %>% unique()),
                        selectInput("nb_doors", "Number of doors:", choices = dataset1$`Nb de portes` %>% unique())
                        ),
                 # Right column with impact on the price
                 column(6, 
                        h3("Adjusted Price"),
                        h3(" "),
                          h4("At 30 min drive"),
                          h4(" "),
                            h5("Mean price:"),
                            h5("Minimum price:"),
                            h5("Maximum price:"),
                          h3(" "),
                          h4("In the whole country"),
                          h4(" "),
                            h5("Mean price:"),
                            h5("Minimum price:"),
                            h5("Maximum price:"))),    
        
        # Second Tab
        tabPanel("At 30 min drive",
                  # Map Title
                  div(h3("Available cars around you")),
                  # Plotting the Map
                 plotOutput("drive_map"),
                  
                  # Table Title
                  div(h3("Summary Results")),
                  # Drawing the Table
                 DT::DTOutput("table_drive")),
                  
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

server <- function(input, output) {

  ### PACKAGES AND DATASETS AREA
  
  library(readr)
  library(sf)
  library(ggplot2)
  library(raster)
  library(rasterVis)
  
  # For the time being, we work with dataset1
  dataset1 <- read_csv("datasets/scrapped_listings_1_1_500.csv")
  
  # Loading "Mapping_villes" csv file for Postal Codes matches
  mapping_villes <- read_csv2("datasets/mapping_villes.csv")
  
  # Loading the department shapes
  dept_193 <- st_read("cartography/DEPARTEMENT.shp")
  
  # Transforming the department map into region map
  region_map <- dept_193 %>% 
    group_by(CODE_REG, NOM_REG) %>%
    summarize()
  
  # Creating centroids of each region to plot the labels at the center of each region
  centroids <- region_map %>% st_centroid()
  
  # Transforming st format into coordinates format
  coord <- centroids %>% st_coordinates() 
  centroids <- cbind(centroids, coord) %>% 
    mutate(price_ex = "3,338€") # This is just a dummy value for the moment
  
  # Defining location (to be replaced by automated location)
  point <- region_map %>% 
    filter(CODE_REG == 11) %>% # Example of Ile de France is taken here as the location of the user
    st_centroid()
    
  # Creating an appropriate palette for the plot
  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  
  ### FUNCTION AREA
  
  # Creating a polygon of the area around the location of the user
  area_around <- function(geometry) {
    st_buffer(geometry, dist = units::set_units(200, km)) # For the moment we have put 200 km as an example
  }
  
  # Creating the polygon of the intersection of our national map and of the area around the location of the user
  intersection <- st_intersection(region_map, area_around(point)) 
  
  # Creating a dummy dataset to have an example of plotting the results (this is just the centroid of each department)
  example_points <- dept_193 %>% 
    st_centroid() %>% 
    st_intersection(intersection)
  
  
  # Creating a function to plot the map of the 30 min drive area
  plot_drive_map <- function() {
    ggplot(intersection) +
      geom_sf(aes(fill = CODE_REG)) +
      scale_fill_manual(values = cc) + # Applying the palette we have built further up
      geom_sf(data = point, color = "orange", size = 5) + # Plotting a point at the position of the user (to be automatized)
      geom_sf(data = example_points, color = "black", size = 1) + # Plotting the cars around
      coord_sf(crs = st_crs(region_map)) +
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
  
  # Creating a function to plot the map of the whole country
  plot_map <- function() {
    ggplot(region_map) +
      geom_sf(aes(fill = CODE_REG)) +
      geom_text(data = centroids, aes(x=X, y=Y, label= price_ex)) + # Adding label with prices
      scale_fill_manual(values = cc) + # Applying the palette we have built further up
      geom_sf(data = point, color = "orange", size = 5) + # Plotting a point at the position of the user (to be automatized)
      coord_sf(crs = st_crs(region_map)) +
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
   
   output$drive_map <- renderPlot({
     plot_drive_map()
   })
   
   output$table <- DT::renderDT({
     result_table()
   })
   
   output$table_drive <- DT::renderDT({
     result_table()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

