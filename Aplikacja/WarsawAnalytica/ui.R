
library(shiny)
library(leaflet)

shinyUI(
    navbarPage(
        "WarsawAnalytica",
        tabPanel(
            "Application",
            sidebarLayout(
                sidebarPanel(
                    h3("Tell us something about yourself!"),
                    textInput("address", label = "Where do you live?"),
                    selectInput("sex", label = "Sex", choices = c("Male", "Female", "Prefer not to say")),
                    sliderInput("age", label = "Age", min = 18, max = 100, value = 25, step = 1),
                    numericInput("earnings", label = "Earnings in PLN", value = 5000, min = 0, max = 1000000),
                    checkboxGroupInput("amenities", label = "Which amenities are you interested in?",
                                       c("Restaurants", "Cafes", "Fastfoods", "Public transport", 
                                         "Service point", "Chain shop", "Education", "Culture & entertainment",
                                         "Shopping mall", "Health", "Place of worship")),
                    checkboxGroupInput("place", label = "Choose a place to display", c("MIM", "WNE")),
                    sliderInput("lattice_dim", label = "Lattice dimension in meters", 
                                min = 100, max = 500, value = 200, step = 100),
                    submitButton()
                ),
                mainPanel(
                    h2("Map of the faculties you have chosen."),
                    leafletOutput("map"),
                    textOutput("text"),
                    textOutput("address_clean")
                )
            )
            
        ),
        tabPanel("Documentation")
    )
)
