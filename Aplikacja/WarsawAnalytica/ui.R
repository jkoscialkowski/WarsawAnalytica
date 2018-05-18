#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    navbarPage(
        "WarsawAnalytica",
        tabPanel(
            "Application",
            sidebarLayout(
                sidebarPanel(
                    h3("Feel free to play around"),
                    checkboxGroupInput("place", label = "Choose a place to display", c("MIM", "WNE")),
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
