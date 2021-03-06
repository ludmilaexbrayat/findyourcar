
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
dataset1 <- read_csv("archive/datasets/final_dataset_full.csv")

# Loading "Mapping_villes" csv file for Postal Codes matches
mapping_villes <- read_csv2("archive/datasets/mapping_villes.csv")

# Loading the department shapes
dept_193 <- st_read("cartography/DEPARTEMENT.shp")

# Transforming the department map into region map
crs_lambert <- 2154

dpt_map <- dept_193 %>% 
  st_transform(crs = crs_lambert)

region_map <- dpt_map %>% 
  group_by(NOM_REG, CODE_REG) %>% 
  summarize()

# Adding the coordinates of the centroids of each region
coord <- region_map %>% 
  st_centroid() %>% 
  arrange(CODE_REG) %>% 
  st_coordinates() %>% 
  as.data.frame()

list_regions <- region_map %>% arrange(CODE_REG)

coord$CODE_REG <- list_regions$CODE_REG


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

server <- function(input, output) {
  
  ### BASE OBJECTS
  
  # Preparing the dataset we'll need for the plots
  dataset_map <- dataset1 %>% 
    filter(!is.na(longitude) & !is.na(latitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude"),
             crs = 4326) %>% 
    st_transform(crs = crs_lambert)
  
  # Creating appropriate palettes for the plot
  cc <- scales::seq_gradient_pal("white","orange")(seq(0,1,length.out=15))
  cc_dark <- scales::seq_gradient_pal("orange","black")(seq(0,1,length.out=15))
  
  
  ### DATA INPUT AREA
  
  # Creating an eventreactive for the basic filters on the condition of the "Go" button
  data_filtered_basic_country <- eventReactive(input$go, {
    dataset_map %>%
      filter(
        !is.na(prix_euros),
        carrosserie == input$carrosserie
      )
  })
  
  # Creating a reactive for the advanced filters at the country level
  data_filtered_advanced_country <- reactive({
    data_filtered_basic_country() %>%
      mutate(
        year = substr(date, 0, 4)
      ) %>% 
      filter(
        (transmission %in% input$transmission) | (input$transmission == "No Preference"),
        (brand %in% input$brand) | (input$brand == "No Preference"),
        #year >= input$year_built[1] & year <= input$year_built[2],
        kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
        (energie %in% input$fuel) | (input$fuel == "No Preference"),
        (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
        (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
      )
  })
  
  
  # Creating a reactive for the advanced filters at the 100km area level
  data_filtered_advanced_100km <- reactive({
    data_filtered_basic_country() %>%
      mutate(
        year = substr(date, 0, 4),
        distance = st_distance(data_filtered_basic_country(), point_user())
      ) %>% 
      filter(
        distance <= units::set_units(100, km),
        (transmission %in% input$transmission) | (input$transmission == "No Preference"),
        (brand %in% input$brand) | (input$brand == "No Preference"),
        year >= input$year_built[1] & year <= input$year_built[2],
        kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
        (energie %in% input$fuel) | (input$fuel == "No Preference"),
        (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
        (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")
      )
  })
  
  
  # Finding the localization of the user
  point_user <- eventReactive(input$go, {dataset_map %>%
      filter(nom_commune == input$city) %>%
      head(n = 1)
    })
  
  
  # Finding the cars around the localization of the user
  points_around <- reactive({
    data_filtered_advanced_100km() %>% 
      filter(st_distance(data_filtered_advanced_100km(), point_user()) <= units::set_units(100, km))
    })
  
  
  # Cropping the map of France to focus on the region
  area_around <- function(geometry) {
    st_buffer(geometry, dist = units::set_units(100, km)) %>% 
      st_transform(crs = st_crs(dpt_map))
    }
  
  intersection <- reactive({
    st_intersection(dpt_map, area_around(point_user()))
  }) 
  
  
  # Creating a function to plot the map of the 100km area
  plot_drive_map <- function(intersection, point_user, points_around) {
    ggplot(intersection) +
      geom_sf(aes(fill = CODE_REG)) +
      geom_sf(data = points_around, color = "black", size = 1) +
      geom_sf(data = point_user, color = "orange", size = 5) + # Plotting a point at the position of the user
      coord_sf(crs = st_crs(dpt_map)) +
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
  
  
  # Computing the average price per region
  prices_per_region <- eventReactive(input$go, {dataset_map %>% 
    st_join(dpt_map) %>%
    filter(carrosserie == input$carrosserie,
           (transmission %in% input$transmission) | (input$transmission == "No Preference"),
           (brand %in% input$brand) | (input$brand == "No Preference"),
           year >= input$year_built[1] & year <= input$year_built[2],
           kilometrage_km >= input$mileage[1] & kilometrage_km <= input$mileage[2],
           (energie %in% input$fuel) | (input$fuel == "No Preference"),
           (nb_places %in% input$nb_seats) | (input$nb_seats == "No Preference"),
           (nb_portes %in% input$nb_doors) | (input$nb_doors == "No Preference")) %>%
    group_by(CODE_REG) %>% 
    summarize(mean_price = as.integer(mean(prix_euros))) %>%
    merge(coord, by = c("CODE_REG" = "CODE_REG"))})
  
  
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
  
  
  # Creating a function to draw the appropriate table 
  result_table <- function() {
    table <- as.data.frame(data_filtered_advanced_country()) %>% 
      dplyr::select(prix_euros, brand, modele, nom_commune, kilometrage_km, transmission) %>% 
      filter(!is.na(brand) & !is.na(nom_commune))
    
    colnames(table) <- c("Price", "Brand", "Modele", "City", "Mileage", "Transmission")
    
    return(table)
  }
  
  
  # Creating a function to draw the appropriate table within the 100km area
  result_table_area <- function() {
    table <- as.data.frame(data_filtered_advanced_100km()) %>% 
      dplyr::select(prix_euros, brand, modele, nom_commune, kilometrage_km, transmission) %>% 
      filter(!is.na(brand) & !is.na(nom_commune))
    
    colnames(table) <- c("Price", "Brand", "Modele", "City", "Mileage", "Transmission")
    
    return(table)
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
  
  result_price_area <- function() {
    a <- a <- c(
      mean(data_filtered_advanced_100km()$prix_euros),
      max(data_filtered_advanced_100km()$prix_euros),
      min(data_filtered_advanced_100km()$prix_euros)
    )
    return(a)
  }
  
  
  ### OUTPUT AREA
  
  output$map <- renderPlot({
    plot_map(map = prices_per_region(), point_user = point_user())
  })
  
  output$drive_map <- renderPlot({
    plot_drive_map(intersection = intersection(), point_user = point_user(), points_around = points_around())
  })
  
  output$table <- DT::renderDT({
    result_table()
  })
  
  output$table_area <- DT::renderDT({
    result_table_area()
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
  
  output$mean_area <- renderText({
    round(result_price_area()[1])
  })
  
  output$max_area <- renderText({
    round(result_price_area()[2])
  })
  
  output$min_area <- renderText({
    round(result_price_area()[3])
  })
  
  output$fyi <- renderPlot({
    plot_fyi()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

