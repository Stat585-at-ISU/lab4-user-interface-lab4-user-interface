#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shiny_start <- function() {
  countries <- get0("countries", envir = asNamespace("worldle"))

  today <- lubridate::today()
  set.seed(as.numeric(today))
  todays_country_id <- sample(nrow(countries), 1)

  iso_rda <- countries$iso_rda[todays_country_id]
  if (!file.exists(file.path("data", iso_rda))) {
    sf <- get_shapes(file.path("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/",countries$links[todays_country_id]))
    sf <- thin(sf, 0.001)
  }

  gg <- sf %>% ggplot() + geom_sf() + theme_void()

  print(gg)
  print("Which country is it? ")
}


# move this somewhere else
shiny_guess(sf, countries$country.name.en[todays_country_id], attempt=1, guess="", countries$unicode.symbol[todays_country_id])


shiny_guess <- function(sf, name, attempt=1, guess="", unicode) {
  cat(sprintf("Attempt %d", attempt))

  guess <- scan(what=character(), n=1)
  if (str_equal(tolower(name), tolower(guess))) {
    cat(sprintf("\nYou got it in %d attempts, congratulations!", attempt))
    return()
  }
}


shiny_quit <- function() {
  cat(sprintf("\nSo close! It is %s!", name))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Let's Play Shiny Wordle!"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

      # Action button
      actionButton("shiny_start", label = "Start"),


        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application
shinyApp(ui = ui, server = server)

