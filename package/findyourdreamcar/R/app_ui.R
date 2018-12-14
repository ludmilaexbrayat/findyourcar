
#' @import shiny
app_ui <- function() {
  fluidPage(

    # Displaying the logo
    titlePanel(title=div(img(src="www/logo.png"))),

    sidebarLayout(

      sidebarPanel(
        # Displaying basic filtering panel on the left
        mod_basic_filteringUI("fichier")
      ),

      mainPanel(
        tabsetPanel(type = "tabs",

                    tabPanel("Welcome",
                             h3(" "),
                             h3(" "),
                             HTML('<h3>We are here to help you find your dream car</h3>
                                  <br>
                                  <b><p style="color:#e59400">The purpose of this app is to help you:</p></b>
                                  <ul>
                                  <li>Choose the best set of options that fits your budget</li>
                                  <li>Find the localization of your dream car</li>
                                  </ul>
                                  <br>
                                  <b><p style="color:#e59400">To find your dream car, you need to:</p></b>
                                  <ol type="1">
                                  <li>Select, in the left panel, your city and the type of car you are looking for</li>
                                  <li>Discover which feature(s) would be the most expensive in the tab <i>1. Learn where to save money</i></li>
                                  <li>Based on your learnings and whishes, select your features in the tab <i>2. Choose your options</i></li>
                                  <li>Find the localization of the cars that meet your criteria in the tabs <i>3. Find your dream car near your ...</i> and <i>4. ... or in France</i></li>
                                  </ol>
                                  Dont forget to press "Go" before reading the other tabs.
                                  <br>
                                  <br>
                                  <h3>Meet the team!</h3>
                                  <br>
                                  <b><p style="color:#e59400">Who are we?</p></b>
                                  We are five housemates studying at Ecole Polytechnique and HEC Paris (MSc Data Science for Business). This project is part of our class <a href="https://moodle.polytechnique.fr/course/view.php?id=6125">R for Data Science</a>.
                                  <br>
                                  <br>
                                  <b><p style="color:#e59400">The story behind this app</p></b>
                                  In June 2017, one of us started looking for a motorbike but had very few knowledge about the different brands and models. Finding the dream motorbike quickly became impossible, and she gave up. One year later, the team decided to create the tool she wished she had had back then, but focusing first on cars instead of motorbikes.
                                  <br>
                                  <br>
                                  <b><p style="color:#e59400">Where does the data come from?</p></b>
                                  We scrapped the car listings on ParuVendu that were active between October and November 2018.
                                  ')
                             ),

                    tabPanel("1. Learn where to save money",
                             h3("Prices of the main features"),
                             HTML('<b><p style="color:#e59400">How much could you save if you purchased a car with a greater mileage? Or with a manual transmission?</p></b>
                                  You can find the answers to these questions here. We performed a regression on the cars around you that meet your "type of car" criteria, and shared the findings below.'),
                             h3(" "),
                             # Displaying the three regression output: text of default case, result table and result plot
                             mod_regressionUI("fichier")
                             ),

                    tabPanel("2. Choose your options",

                             # Left column with adjustment inputs
                             HTML('<br>
                                  <br>
                                  <b><p style="color:#e59400">Now that you know how each feature impacts the final price, you can pick your favorite set of options.</p></b>
                                  <br>'),
                             column(5,
                                    HTML("<h3>Pick your favorite features...</h3>
                                         <br>"),
                                    # Displaying advanced filtering panel
                                    mod_advanced_filteringUI("fichier")
                                    ),
                             # Spacing both columns
                             column(1.5),

                             # Right column with result outputs
                             column(5,
                                    HTML("<h3>... and get a price estimate</h3>
                                       <br>"),
                                    h3(" "),
                                    h3(" "),
                                    # Area output
                                    mod_area_statsUI("fichier"),
                                    h3(" "),
                                    h3(" "),
                                    # Country output
                                    mod_country_statsUI("fichier")
                                    )),

                    tabPanel("3. Find your dream car near you...",
                             # Map Title
                             div(h3("Available cars around you")),
                             HTML("This map displays the cars around you that are meeting all the criteria that you've defined in the tab <i>2. Choose your options</i>."),
                             # Plotting the Map
                             mod_map_areaUI("fichier"),

                             # Table Title
                             div(h3("Summary Results")),
                             # Drawing the Table
                             mod_table_areaUI("fichier")),

                    tabPanel("4. ... or in France",
                             # Map Title
                             div(h3("Available cars")),
                             HTML("This map displays the average price per region of the cars that are meeting all the criteria that you've defined in the tab <i>2. Choose your options</i>."),
                             # Plotting the Map
                             mod_map_countryUI("fichier"),

                             # Table Title
                             div(h3("Summary Results")),
                             # Drawing the Table
                             mod_table_countryUI("fichier"))
                    )
                             )
    )
  )
}
