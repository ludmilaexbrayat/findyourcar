# This script have to be regularly update to contains all
# dependances needed to launch the app ()

# this instruction create the vector you need
# shinytemplate:::get_dependencies()


to_install <- c("DT",
                "glue",
                "graphics",
                "shiny",
                "sf",
                "dplyr",
                "magrittr",
                "RColorBrewer",
                "readr",
                "ggplot2",
                "raster",
                "rasterVis",
                "stats",
                "glue",
                "units")
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }

}
