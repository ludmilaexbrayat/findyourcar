# Naming convention :
# all Shinymodule have to begin with `mod_`, in lowercase Except for `UI` , `Input` and `Output`
# use `Input` as sufix if your module is an Input
# use `Output` as sufix if your module is an Output
# use `UI` as sufix if your module is both Input and Output
#
# examples :
# ui side : mod_truc_bidulUI
# server side : mod_truc_bidul
#
# ui side : mod_machin_chouetteInput
# server side : mod_machin_chouette

# all shinyModule must have a documentation page
# one unique page for both ui and server side ( you can use `#' @rdname` to link both function)

# A minimalist example is mandatory

#' @title   mod_regressionUI and mod_regression
#' @description  A shiny Module that shows principal statistics for the selected basic and advanced filters of the user in a 100km area
#'
#' @param id id for proper interaction with shiny
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @import DT
#' @export
#' @examples
#' library(shiny)
#' library(DT)
#' if (interactive()){
#' ui <- fluidPage(
#'   mod_csv_fileInput("fichier"),
#' DTOutput("tableau")
#' )
#'
#' server <- function(input, output, session) {
#'   data <- callModule(mod_csv_file,"fichier")
#'   output$tableau <- renderDT({data()})
#' }
#'
#' shinyApp(ui, server)
#' }
#'
mod_regressionUI <- function(id) {

  ns <- NS(id)

  tableOutput(ns("regression_table"))

}


#' mod_table_area server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with at one column named "prix-euros" containing the price
#'
#' @import dplyr
#' @import magrittr
#' @import shiny
#' @import stats
#' @import tibble
#' @import purrr
#' @importFrom utils data
#' @export
#' @rdname mod_regressionUI
mod_regression <- function(input, output, session, dataframe) {


  regression_function <- function(data_reg, choix){

    data_reg <- data_reg %>% filter(carrosserie == choix) #Filter the dataset for this type of car

    reg_sol <- lm(prix_euros  ~ energie + transmission+is_pro+kilometrage_km+nb_portes, data_reg)
    return(summary(reg_sol))
  }

  reg_shiny_function <- function(df, choice){

    #Lets create the table we'll use later
    df_significant_coef <- tibble::rownames_to_column(as.data.frame(regression_function(df, choice)$coef), "coef")#Create a data frame with no row naes as it is easier to work without


    df_significant_coef <- df_significant_coef %>%
      dplyr::filter(coef!="(Intercept)") # Take out intercept as it doesnt matter for our study


    #Long step, we change individually the name of oef for interpretation (couldn't do it in 1 mutatet as regression don't all have the same coefficients)
    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "energieElectrique") %>%
      dplyr::mutate(coef="electric car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "energieElectrique")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "kilometrage_km") %>%
      dplyr::mutate(Estimate=Estimate*10000, coef="car with + 10'000 km") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "kilometrage_km")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "energieEssence") %>%
      dplyr::mutate(coef="gasoline car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "energieEssence")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "energieHybride") %>%
      dplyr::mutate(coef="hybrid car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "energieHybride")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "energieGPL ou GNL") %>%
      dplyr::mutate(coef="GPL car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "energieGPL ou GNL")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "transmissionAutres") %>%
      dplyr::mutate(coef="unvonventional transmission type car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "transmissionAutres")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "transmissionManuelle") %>%
      dplyr::mutate(coef= "manual transmission car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "transmissionManuelle")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "transmissionSemi automatique") %>%
      dplyr::mutate(coef= "semi-automatic transmission car") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "transmissionSemi automatique")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "is_pro") %>%
      dplyr::mutate(coef= "car from a profresional seller") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "is_pro")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "nb_portes4") %>%
      dplyr::mutate(coef= "car with 4 doors") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "nb_portes4")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "nb_portes5") %>%
      dplyr::mutate(coef= "car with 5 doors") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "nb_portes5")

    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(coef== "nb_portes") %>%
      dplyr::mutate(coef= "car with one more door") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "nb_portes")


    df_significant_coef  <- df_significant_coef %>%
      dplyr::filter(`Pr(>|t|)`<0.05) %>% #Filter for significant features only
      dplyr::select(coef, Estimate)



    #Split the significant coef into 2 separate table, the positive and the negative
    #So that we can have different interpretation
    positiv_coef <- df_significant_coef %>% dplyr::filter(Estimate>0)
    negativ_coef <- df_significant_coef %>% dplyr::filter(Estimate<0)


    negativ_coef <- negativ_coef %>% purrr::pmap_chr(.f= function(Estimate,coef){paste("If you switch to a",coef,"you could save about", -signif(Estimate,2), "euros")})

    positiv_coef <- positiv_coef %>% purrr::pmap_chr(.f= function(Estimate,coef){paste("If you switch to a", coef, "you would pay an extra", signif(Estimate,2), "euros")})

    return(c(negativ_coef,positiv_coef)) #Returns the two

  }

  carro <- eventReactive(input$go, {
    reg_shiny_function(dataframe, input$carrosserie)
  })

  output$regression_table <- renderTable({
    carro()
  }, colnames = FALSE)

}
