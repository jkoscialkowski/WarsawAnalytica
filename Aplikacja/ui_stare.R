
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
                    numericInput("income", label = "Monthly income in PLN", value = 5000, min = 0, max = 1000000),
                    checkboxInput("buildings", label = "Do you prefer lower buildings"),
                    sliderInput("children", label = "Your attitude towards children?", 
                                min = 0, max = 10, value = 5, step = 1),
                    sliderInput("teenagers", label = "Your attitude towards teenagers?", 
                                min = 0, max = 10, value = 5, step = 1),
                    sliderInput("students", label = "Your attitude towards students?", 
                                min = 0, max = 10, value = 5, step = 1),
                    checkboxGroupInput("amenities", label = "Which amenities are you interested in?",
                                       c("Food", "Cafes", "Public transport", 
                                         "Service points", "Chain shops", "Education", "Culture & entertainment",
                                         "Shopping malls", "Health", "Places of worship")),
                    submitButton()
                ),
                mainPanel(
                    h2("Search for a flat THERE!"),
                    leafletOutput("map"),
                    textOutput("text"),
                    textOutput("address_clean")
                )
            )
            
        ),
        tabPanel("Documentation",
                 p(paste(rep("WIEDZA EKSPERCKA", 100000), sep = ",")))
    )
)
