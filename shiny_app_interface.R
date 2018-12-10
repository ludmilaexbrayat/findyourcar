
# Still to do:
# - Affiner les sliders input en ??liminant les NAs et valeurs incongrues
# - Missing value dans les coordonne??s
# - "No Preference" ne marche pas vriament encore
# - On a supprim??e l'ann??e de fabrication du dataset ?

# Please note that this is a first version whose only aim is to show what the interface could look like. 
# No automation has been made yet regarding the input made by the user
# We have chosen to work with the first raw dataset as a first approach, and then extend it to the entire dataset when ready
# Name of variables as well as other choices will have to be fitted to the "clean" dataset

### PACKAGES AND DATASETS AREA

library(shiny)
library(dplyr)
library(RColorBrewer)
library(readr)
library(sf)
library(ggplot2)
library(raster)
library(rasterVis)

# For the time being, we work with dataset1
dataset1 <- read_csv("datasets/first_dataset_full_coordOK.csv")

# Loading "Mapping_villes" csv file for Postal Codes matches
mapping_villes <- read_csv2("datasets/mapping_villes.csv")

# Loading the department shapes
dept_193 <- st_read("cartography/DEPARTEMENT.shp")

# Transforming the department map into region map
crs_lambert <- 2154

region_map <- dept_193 %>% 
  st_transform(crs = crs_lambert) %>%                     # With the good CRS
  st_simplify(preserveTopology = TRUE, dTolerance = 2000) # Simplifying the raster so that the map loads faster


# Define UI for application
ui <- fluidPage(
   
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
                        selectInput("transmission", "Choose the type of transmission:", choices = c("No Preference", dataset1$transmission)),
                        selectInput("brand", "Brand:", choices = c("No Preference", dataset1$brand %>% unique())),
                        sliderInput("year_built", "Year:", min = 1950, max = 2017, value = c(1950,2017)),
                        sliderInput("mileage", "Mileage:", min = 10000, max = 100000, value = c(10000, 100000)),
                        selectInput("fuel", "Type of fuel:", choices = c("No Preference", dataset1$energie %>% unique())),
                        selectInput("nb_seats", "Number of seats:", choices = c("No Preference", dataset1$nb_places %>% unique())),
                        selectInput("nb_doors", "Number of doors:", choices = c("No Preference", dataset1$nb_portes %>% unique()))
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
  
  # Creating centroids of each region to plot the labels at the center of each region
  centroids <- region_map %>% st_centroid()
  
  # Transforming st format into coordinates format
  coord <- centroids %>% st_coordinates() 
  centroids <- cbind(centroids, coord) %>% 
    mutate(price_ex = "3,338???") # This is just a dummy value for the moment
  
  # Defining location (to be replaced by automated location)
  point <- region_map %>% 
    filter(CODE_REG == 53) %>% # Example of Ile de France is taken here as the location of the user
    st_centroid()
  
  dataset1_sf <- st_as_sf(dataset1 %>% filter(!is.na(longitude) & !is.na(latitude)), 
                          coords = c("longitude", "latitude"), crs = 4326)
  
  mapping_villes_sf <- st_as_sf(mapping_villes %>% filter(!is.na(longitude) & !is.na(latitude)), 
                                 coords = c("latitude", "longitude"), crs = 4326)
  
  # Creating an appropriate palette for the plot
  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  
  ### DATA INPUT AREA
  
  # Creating an eventreactive for the basic filters on the condition of the "Go" button
  data_filtered_basic_country <- eventReactive(input$go, {
    dataset1 %>%
      filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })
  
  # Creating a reactive for the advanced filters
  data_filtered_advanced_country <- reactive({
    data_filtered_basic_country() %>%
      filter(
        (transmission %in% input$transmission) | (input$transmission == "No Preference"),
        (brand %in% input$brand) | (input$brand == "No Preference"),
        kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
        (energie %in% input$fuel) | (input$fuel == "No Preference"),
        (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
        (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
        )
  })
  
  ### FUNCTION AREA
  # Preparing the objects we need for the plot
  dataset_map <- dataset1 %>% 
    filter(!is.na(longitude) & !is.na(latitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>% 
    st_transform(crs = crs_lambert)
  
  
  # Finding the localization of the user
  #point_user <- eventReactive(input$go, {dataset_map %>% 
  #  filter(nom_commune == input$city) %>%
  #  head(n = 1) %>% 
  #  st_simplify(preserveTopology = TRUE, dTolerance = 2000)})  # Simplifying the raster so that the map loads faster
  
  point_user <-dataset_map %>% 
      filter(nom_commune == "Montpellier") %>%
      head(n = 1) %>% 
      st_simplify(preserveTopology = TRUE, dTolerance = 2000)  # Simplifying the raster so that the map loads faster

  # Cropping the map of France to focus on the region
  area_around <- function(geometry) {
    st_buffer(geometry, dist = units::set_units(200, km)) %>% 
      st_transform(crs = st_crs(region_map)) # For the moment we have put 200 km as an example
  }
  
  intersection <- st_intersection(region_map, area_around(point_user))
  
  # Creating a function to plot the map of the 200km area
  plot_drive_map <- function() {
    ggplot(intersection) +
      geom_sf(aes(fill = CODE_REG)) +
      geom_sf(data = point_user, color = "orange", size = 5) + # Plotting a point at the position of the user (to be automatized)
      coord_sf(crs = st_crs(region_map)) +
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
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())
  }
  
  # Creating a function to plot the map of the whole country
  #plot_map <- function() {
  #  ggplot(region_map) +
  #    geom_sf(aes(fill = CODE_REG)) +
  #    geom_text(data = centroids, aes(x=X, y=Y, label= price_ex)) + # Adding label with prices
  #    scale_fill_manual(values = cc) + # Applying the palette we have built further up
  #    geom_sf(data = point, color = "orange", size = 5) + # Plotting a point at the position of the user (to be automatized)
  #    coord_sf(crs = st_crs(region_map)) +
      # All the lines below have as sole purpose to erase all the grids, axis, etc. of the plot
  #    theme(axis.line=element_blank(),
  #           axis.text.x=element_blank(),
  #           axis.text.y=element_blank(),
  #           axis.ticks=element_blank(),
  #           axis.title.x=element_blank(),
  #           axis.title.y=element_blank(),
  #           legend.position="none",
  #           panel.background=element_blank(),
  #           panel.border=element_blank(),
  #           panel.grid.major=element_blank(),
  #           panel.grid.minor=element_blank(),
  #           plot.background=element_blank())
  #}
  
  # Creating a dummy dataframe just for plotting the importance of parameters
  df <- data.frame(c("Number of seats", "Number of seats",
          "Brand", "Brand", "Brand", "Brand", "Brand",
          "Type of Fuel",
          "Year"))
  
  # Creating a function to plot the graph of importance of each parameter
  plot_fyi <- function() {
    ggplot(data = df) +
      geom_bar(aes(df[,1]), fill = "orange") +
      labs(x = "Parameter", y = "Relative Importance")
  }
  
  # Creating a function to draw the appropriate table (this is just an example)
  result_table <- function() {
    head(
      dataset1 %>% 
      dplyr::select(prix_euros, brand, modele, nom_commune, kilometrage_km, transmission) %>% 
      filter(!is.na(brand) & !is.na(nom_commune))
    )
  }
  
  # Creating a function to give the mean, minimum and maximum price depending on the filters
  result_price <- function() {
          a <- c(
            mean(data_filtered_advanced_country()$prix_euros),
            max(data_filtered_advanced_country()$prix_euros),
            min(data_filtered_advanced_country()$prix_euros)
          )
    return(a)
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
   
   output$mean <- renderText({
     round(result_price()[1])
  })
   
   output$max <- renderText({
     round(result_price()[2])
   })
   
   output$min <- renderText({
     round(result_price()[3])
   })
   
   output$fyi <- renderPlot({
     plot_fyi()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

