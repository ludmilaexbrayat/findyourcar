#' run the Shiny Application
#'
#'
#' @export
#' @import shiny
#' @import dplyr
#' @import RColorBrewer
#' @import readr
#' @import sf
#' @import ggplot2
#' @import raster
#' @import rasterVis
#' @examples
#'
#' if (interactive()) {
#'
#'   run_app()
#'
#' }

#library(shiny)
#library(dplyr)
#library(RColorBrewer)
#library(readr)
#library(sf)
#library(ggplot2)
#library(raster)
#library(rasterVis)

run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
