
library(shiny)
library(leaflet)

shinyUI(
    navbarPage(
        "WarsawAnalytica",
        tabPanel(
            "Application",
            sidebarLayout(
                sidebarPanel(
                    h3("Feel free to play around"),
                    checkboxGroupInput("place", label = "Choose a place to display", c("MIM", "WNE")),
                    sliderInput("lattice_dim", label = "Lattice dimension in meters", 
                                min = 100, max = 500, value = 200, step = 100),
                    submitButton()
                ),
                mainPanel(
                    h2("Map of the faculties you have chosen."),
                    leafletOutput("map"),
                    textOutput("text")
                )
            )
            
        ),
        tabPanel("Documentation")
    )
)
