#' @title   mod_regressionUI and mod_regression
#' @description  A shiny Module that performs a regression of the price with respect to other parameters and displays the results for a specific type of car
#'
#' @param id shiny id
#'
#' @import shiny
#' @export
#' @examples
#' "No example to display"
#'
mod_regressionUI <- function(id) {

  ns <- NS(id)

  tagList(
    tableOutput(ns("regression_table")),
    plotOutput(ns("regression_plot"))
  )

}


#' mod_regression server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataframe dataframe with columns named "prix-euros", "nom_commune", "carrosserie", "transmission", "brand", "date", "energie", "nb_places", "kilometrage_km" and "nb_portes"
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @import shiny
#' @importFrom stats lm
#' @importFrom stats coef
#' @import tibble
#' @importFrom purrr pmap_chr
#' @importFrom utils data
#' @export
#' @rdname mod_regressionUI
mod_regression <- function(input, output, session, dataframe) {


  regression_function <- function(data_reg, choix){

    data_reg <- data_reg %>% dplyr::filter(carrosserie == choix) #Filter the dataset for this type of car

    reg_sol <- lm(prix_euros  ~ energie + transmission+is_pro+kilometrage_km+nb_portes, data_reg)
    return(summary(reg_sol))
  }

  reg_shiny_function <- function(df, choice){

    #Lets create the table we'll use later
    df_significant_coef <- tibble::rownames_to_column(as.data.frame(regression_function(df, choice)$coef), "coef")#Create a data frame with no row naes as it is easier to work without

    keys <- data.frame(coef=c("energieDiesel","energieElectrique","kilometrage","energieEssence","energieHybride","energieGPL ou GNL","transmissionAutres","transmissionManuelle","transmissionSemi automatique","is_pro","nb_portes4","nb_portes5","nb_portes"), name_i=c("diesel car","electric car","car with + 10'000 km","gasoline car","hybrid car","GPL car","unvonventional transmission type car","manual transmission car","semi-atomatic transmission car","car from a profresional seller","car with 4 doors","car with 5 doors","nb_portes"))

    # Take out intercept as it doesnt matter for our study
    df_significant_coef <- df_significant_coef %>%
      dplyr::filter(coef== "kilometrage_km") %>%  #Need to change kilometrage to have compute change for 10k
      dplyr::mutate(Estimate=Estimate*10000, coef="kilometrage") %>%
      dplyr::bind_rows(df_significant_coef) %>%
      dplyr::filter(coef != "kilometrage_km") %>%

      dplyr::filter(coef!="(Intercept)") %>% # Take out intercept for interpretability
      dplyr::left_join(keys, by="coef") %>% #join on coef to have interpretable name
      dplyr::mutate(coef=as.character(name_i)) %>%
      dplyr::filter(`Pr(>|t|)`<0.05) %>% #Filter for significant features only
      dplyr::select(coef, Estimate)


    #Long step, we change individually the name of oef for interpretation (couldn't do it in 1 mutatet as regression don't all have the same coefficients)


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
