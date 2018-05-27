library(shiny)
library(leaflet)
library(shinysky)
#install.packages('shinyWidgets')
library(shinyWidgets)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
#install.packages('shinythemes')
library(shinythemes)
streets <- read.csv("D:/WarsawAnalytica/Aplikacja/WarsawAnalytica/PoznanStreets.csv", encoding = "UTF-8", stringsAsFactors = F)
colnames(streets) <- "streets"
adress_autocomplete <- streets$streets

fluidPage(
    theme = shinytheme("yeti"),
    tags$head(
        tags$style("label[for=earnings] {width: 100%}
                   h3 {margin-top: 5px}
                   button[data-id=city] {padding: 7px;}
                   button[type = submit] {background-color: #222222}
                   .noUi-connect {background: #222222}
                   .control-label {width: 100%}
                   #earnings, #age {width: 92%}
                   .btn-default{background-color: white;}
                   hr {margin-top: 3px; margin-bottom: 3px; border-top: 1px solid darkgrey; border-bottom: 2px solid white}")
        ),
    navbarPage(
        "WarsawAnalytica",
        tabPanel(
            "Application",
            sidebarLayout(
                sidebarPanel(width = 3,
                             h3("Tell us something about yourself!"),
                             h5('Pick adress and type of city'),
                             div(style="display: inline-block;vertical-align:top; width: 50%;", 
                                 selectizeInput("address",
                                                label="Where do you live?",
                                                choices = adress_autocomplete,
                                                options = list(
                                                    placeholder = 'Select street',
                                                    onInitialize = I('function() { this.setValue(""); }')
                                                )
                                                #local=data.frame(name=c(adress_autocomplete)),
                                                #valueKey = "name",
                                                #tokens=c(1:length(adress_autocomplete)),
                                                #template = HTML("<p class='repo-language'>{{info}}</p> <p class='repo-name'>{{name}}</p>")
                                 )),
                             div(style="display: inline-block;vertical-align:bottom; width: 38%; margin-left:10%", 
                                 pickerInput("city", label = 'Or:',
                                             c("village", "city under 19k", "city 19-99k", "city 100-499k", "city over 500k"), options = list('actions-box' = TRUE),
                                             selected = "city over 500k")),
                             hr(),
                             checkboxInput("buildings", "Do you prefer lower buildings?"),
                             hr(),
                             pickerInput("sex", label = "Sex", choices = c("Prefer not to say", "Male", "Female")),
                             hr(),
                             noUiSliderInput("age", label = "Age", min = 18, max = 100, value = 25, step = 1),
                             hr(),
                             noUiSliderInput("income", 
                                             label = div(
                                                 div('Income in PLN (net)'), 
                                                 div(style='float:right; font-weight:100;', 'or more')),
                                             value = 5000, min = 0, max = 10000, step = 500),
                             hr(),
                             noUiSliderInput("children", label = "Attitude towards children?", 
                                             min = 1, max = 10, value = 5, step = 1),
                             hr(),
                             noUiSliderInput("teenagers", label = "Attitude towards teenagers?",
                                             min = 1, max = 10, value = 5, step = 1),
                             hr(),
                             noUiSliderInput("students", label = "Attitude towards students?",
                                             min = 1, max = 10, value = 5, step = 1),
                             hr(),
                             pickerInput(inputId = "amenities", 
                                         label = "Which amenities are you interested in?", 
                                         choices = c("Prefer not to say", "Cafes", "Food", "Public transport", 
                                                     "Service point", "Chain shops", "Education", "Culture & entertainment",
                                                     "Shopping malls", "Health", "Places of worship"), options = list('actions-box' = TRUE), 
                                         multiple = T, selected = "Prefer not to say"),
                             hr(),
                             pickerInput("education", label = "What is your education level?",
                                         c("Prefer not to say", "Primary", "Vocational", "Secondary", "Higher"), options = list('actions-box' = TRUE),
                                         selected = NULL),
                             submitButton()
                ),
                mainPanel(
                    h2("You want to move THERE!"),
                    leafletOutput("map"),
                    textOutput("text")
                )
            )
            
        ),
        tabPanel("Documentation")
    )
        )
