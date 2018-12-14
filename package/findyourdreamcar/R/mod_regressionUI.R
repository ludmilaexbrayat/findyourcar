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
#' @importFrom stringr str_to_lower
#' @importFrom stats lm
#' @importFrom stats coef
#' @importFrom stats reorder
#' @import tibble
#' @importFrom purrr pmap_chr
#' @importFrom utils data
#' @importFrom kableExtra kable
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra column_spec
#' @export
#' @rdname mod_regressionUI
mod_regression <- function(input, output, session, dataframe) {


  regression_function <- function(data_reg, choix){

    data_reg <- data_reg %>% dplyr::filter(carrosserie == choix) #Filter the dataset for this type of car

    reg_sol <- lm(prix_euros  ~ energie + transmission+is_pro+kilometrage_km+nb_portes, data_reg)
    return(summary(reg_sol))
  }

  reg_shiny_function <- function(df, choice){

    # Creating the basis table
    df_significant_coef <- tibble::rownames_to_column(as.data.frame(regression_function(df, choice)$coef), "coef")#Create a data frame with no row naes as it is easier to work without

    keys <- data.frame(coef=c("energieDiesel","energieElectrique","kilometrage","energieEssence","energieHybride","energieGPL ou GNL","transmissionAutres","transmissionManuelle","transmissionSemi automatique","is_pro","nb_portes4","nb_portes5","nb_portes"), name_i=c("Diesel car","Electric car","Car with + 10'000 km","Gasoline car","Hybrid car","GPL car","Unvonventional transmission type car","Manual transmission car","Semi-atomatic transmission car","Car from a profresional seller","Car with 4 doors","Car with 5 doors","Car with one more door"))

    # Taking out the intercept (as it doesnt matter for our study)
    df_significant_coef <- df_significant_coef %>%
      filter(coef== "kilometrage_km") %>%  #Need to change kilometrage to have compute change for 10k
      mutate(Estimate=Estimate*10000, coef="kilometrage") %>%
      bind_rows(df_significant_coef) %>%
      filter(coef != "kilometrage_km") %>%

      filter(coef!="(Intercept)") %>% # Take out intercept for interpretability
      left_join(keys, by="coef") %>% #join on coef to have interpretable name
      mutate(coef=as.character(name_i)) %>%
      filter(`Pr(>|t|)`<0.05) %>% #Filter for significant features only
      dplyr::select(coef, Estimate)


    # Building the object that will display the results as sentences
    table_regression <- df_significant_coef %>%
        mutate(if_you = "If you switch to a",
               coef = paste0(str_to_lower(coef), ","),
               you_could = case_when(Estimate > 0 ~ "you would pay an extra",
                                     Estimate <= 0 ~ "you could save about"),
               Estimate_formated = paste0(abs(round(Estimate,0)), " euros")) %>%
        dplyr::select(if_you, coef, you_could, Estimate_formated) %>%
        kableExtra::kable(col.names = c()) %>%
        kableExtra::kable_styling() %>%
        kableExtra::column_spec(c(2,4), bold = T)


   # Building the object that will display the results as a plot
    plot <- ggplot(df_significant_coef %>%
                     mutate(causes_an_increase = as.factor( case_when(Estimate > 0 ~ "red",
                                                                      TRUE ~ "green")))) +
      aes(x = reorder(coef, Estimate),
          y = Estimate,
          fill = causes_an_increase,
          title = "Which features can make you to save money?") +
      geom_col() +
      geom_text(aes(label = case_when(Estimate > 0 ~ paste0("+",round(Estimate,0)),
                                      Estimate <= 0 ~ as.character(round(Estimate,0)))),
                position = position_dodge(0.9), vjust = 0) +
      labs(x = "Feature", y="Change in the price (in euros)") +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank()) +
      scale_fill_manual("legend", values = c("green" = "#007f00", "red" = "#cc0000"))

    return(list(
      table_regression,
      plot))
  }

  carro <- eventReactive(input$go, {
    reg_shiny_function(dataframe, input$carrosserie)
  })


  output$regression_table <- renderText({
    carro()[[1]]
  })

  output$regression_plot <- renderPlot({
    carro()[[2]]
  })

}
