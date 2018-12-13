#' run the Shiny Application
#'
#'
#' @export
#' @importFrom shiny shinyApp
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   run_app()
#'
#' }

library(shiny)
library(dplyr)
library(RColorBrewer)
library(readr)
library(sf)
library(ggplot2)
library(raster)
library(rasterVis)


# For the time being, we work with dataset1
data("cardata")
dataset1 <- cardata

# Loading "Mapping_villes" csv file for Postal Codes matches
mapping_villes <- read_csv2("datasets/mapping_villes.csv")

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

run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
