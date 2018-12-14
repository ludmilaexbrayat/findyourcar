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
    # Displaying the default case text
    textOutput(ns("regression_default_case")),
    h3(""),

    # Displaying the output table of the regression
    tableOutput(ns("regression_table")),

    # Displaying the plot output of the regression
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


  # Creating a function that takes a dataset and a value for the type of "carrosserie"
  # and returning the summary of the regression and the base case
  regression_function <- function(data_reg, choix){

    data_reg <- data_reg %>% filter(carrosserie == choix) #Filter the dataset for this type of car

    # List min configuration
    min_config_table <- data_reg %>% dplyr::select(energie, transmission, is_pro, kilometrage_km, nb_portes) %>%
      dplyr::mutate_all(get_first_col) %>% utils::head(1)

    reg_sol <- lm(prix_euros  ~ energie + transmission + is_pro + kilometrage_km + nb_portes, data_reg)

    return(c(summary(reg_sol), min_config_table))
  }

  # Creating a function taking a dataset and a value for the type of "carrosserie"
  # and returning a list with the resulting dataframe, the plot and the text with the
  # default case
  reg_shiny_function <- function(df, choice){

    # Creating the basis table
    df_significant_coef <- tibble::rownames_to_column(as.data.frame(regression_function(df, choice)$coef), "coef")#Create a data frame with no row naes as it is easier to work without

    # Translating coefficients into real terms
    keys <- data.frame(coef=c("energieDiesel",
                              "energieElectrique",
                              "kilometrage",
                              "energieEssence",
                              "energieHybride",
                              "energieGPL ou GNL",
                              "transmissionAutres",
                              "transmissionManuelle",
                              "transmissionSemi automatique",
                              "is_pro",
                              "nb_portes4",
                              "nb_portes5",
                              "nb_portes"),
                       name_i=c("Diesel car",
                                "Electric car",
                                "Car with + 10,000 km",
                                "Gasoline car",
                                "Hybrid car",
                                "GPL car",
                                "Unconventional transmission type car",
                                "Manual transmission car",
                                "Semi-automatic transmission car",
                                "Car from a professional seller",
                                "Car with 4 doors",
                                "Car with 5 doors",
                                "Car with one more door")) %>%
      dplyr::mutate(coef = as.character(coef))

    # Taking out the intercept (as it doesnt matter for our study)
    df_significant_coef <- df_significant_coef %>%
      filter(coef== "kilometrage_km") %>%  #N eed to change kilometrage to have compute change for 10k
      mutate(Estimate=Estimate*10000, coef="kilometrage") %>%
      bind_rows(df_significant_coef) %>%
      filter(coef != "kilometrage_km") %>%
      filter(coef!="(Intercept)") %>% # Take out intercept for interpretability
      left_join(keys, by="coef") %>% # Join on coef to have interpretable name
      mutate(coef=as.character(name_i)) %>%
      filter(`Pr(>|t|)`<0.05) %>% # Filter for significant features only
      dplyr::select(coef, Estimate)

    # Creating the text with the default configuration
    min_config <- paste("Default configuration for this regression is a car with: power regime",
                        str_to_lower(regression_function(df, choice)$energie),
                        ", transmission", str_to_lower(regression_function(df, choice)$transmission),
                        ", ", regression_function(df, choice)$nb_portes, "doors and sold by an individual seller.")


    # Building the object that will display the results as a table
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
          title = "Which features can make you save money?") +
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
      plot,
      min_config))
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

  output$regression_default_case <- renderText({
    carro()[[3]]
  })

}
