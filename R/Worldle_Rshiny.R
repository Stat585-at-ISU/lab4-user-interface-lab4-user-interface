library(shiny)
library(ggplot2)
library(stringr)
library(dplyr)
source("get_shapes.R")
source("thin.r")

# Define the country for today
countries <- get0("countries", envir = asNamespace("worldle"))
today <- lubridate::today()
set.seed(as.numeric(today))
todays_country_id <- sample(nrow(countries), 1)
iso_rda <- countries$iso_rda[todays_country_id]

name<-countries$country.name.en[todays_country_id]
sf <- get_shapes(file.path("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/",countries$links[todays_country_id]))
sf <- thin(sf, 0.001)


gg <- sf %>% ggplot() + geom_sf() + theme_void()

# Define the Shiny app UI
ui <- fluidPage(
  titlePanel("Guess the Country!"),
  sidebarLayout(
    sidebarPanel(
      selectInput("guess", "Guess the Country:", choices = c(na.omit(countries$country.name.en))),
      actionButton("submit", "Submit Guess"),
      verbatimTextOutput("result"),
      actionButton("quit", "Give Up"),
      verbatimTextOutput("quit_result"),
    ),
    mainPanel(
      plotOutput("gg")
    )
  )
)

# Define the Shiny app server
server <- function(input, output, session) {
  # Initialize the number of guesses
  guesses_left <- 5

  # Show the photo in the app
  output$gg <- renderPlot({
    gg
  })

  # Define the guess button action
  observeEvent(input$submit, {
    # Decrease the number of guesses left
    guesses_left <<- guesses_left - 1

    # Check if the guess is correct
    if (tolower(input$guess) == tolower(name)) {
      output$result <- renderText("Congratulations, you identified the Country!")
    } else if (guesses_left > 0) {
      output$result <- renderText(paste0("Wrong guess. \n You have ", guesses_left, " tries left."))
    } else {
      output$result <- renderText(paste0("Sorry, you ran out of tries \n The correct Country was \n '", name, "'."))
    }
  })
  observeEvent(input$quit, {
    if(input$quit >= 1){
      output$quit_result <- renderText(paste0("The correct country was \n '", name, "'."))
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)

