library(shiny)
library(leaflet)
library(shinysky)
#install.packages('shinyWidgets')
library(shinyWidgets)
#devtools::install_github("AnalytixWare/ShinySky")
library(shinysky)
#install.packages('shinythemes')
library(shinythemes)
streets <- read.csv("PoznanStreets.csv", encoding = "UTF-8", stringsAsFactors = F)
colnames(streets) <- "streets"
adress_autocomplete <- streets$streets

fluidPage(
    theme = shinytheme("yeti"),
    tags$head(tags$style("label[for=income] {width: 100%}
                label[for=buildings] {background-color: lightblue; top: 16px;left: 35px;}
               h3 {margin-top: 5px}
               #building {padding: 7px;height: 80%;}
               button[type = submit] {background-color: #222222}
               .noUi-connect {background: #222222}
               .control-label {width: 100%}
               #earnings, #age {width: 92%}
               div.material-switch{font-size: 80%;margin-top: 4%;}
               .btn-default{background-color: white;}
               hr {margin-top: 1px; margin-bottom: 1px; border-top: 1px solid darkgrey; border-bottom: 2px solid white}")
              
        ),
    navbarPage(
        "WarsawAnalytica",
        tabPanel(
            "Application",
            sidebarLayout(
                sidebarPanel(width = 3,
                             h3("Tell us something about yourself!"),
                             h5('Pick adress and type of city'),
                             div(style="display: inline-block;vertical-align:top; width: 67%;", 
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
                             div(style="display: inline-block;vertical-align:bottom; width: 20%; margin-left:10%", 
                                 numericInput("building", label = 'Building nr:',
                                              value = 1, min = 0, max = 1000, step = 1)),
                             hr(),
                             div(style="display: inline-block;vertical-align:bottom; width: 50%;", 
                                 pickerInput("city", label = 'City type:',
                                             c("village", "city under 19k", "city 19-99k", "city 100-499k", "city over 500k"), options = list('actions-box' = TRUE),
                                             selected = "city over 500k")),
                             div(style="display: inline-block;vertical-align:top; width: 38%; margin-left:10%", 
                                 materialSwitch("buildings", "Love lower buildings!"), status = "primary", 
                                 right = TRUE),
                             hr(),
                             pickerInput("sex", label = "Sex", choices = c("Prefer not to say", "Male", "Female")),
                             hr(),
                             noUiSliderInput("age", label = "Age", min = 18, max = 100, value = 25, step = 1),
                             hr(),
                             noUiSliderInput("income", 
                                             label = div(
                                                 div('Income in PLN (net)'), 
                                                 div(style='float:right; font-weight:100;', 'or more')),
                                             value = 5000, min = 1500, max = 10000, step = 500),
                             hr(),
                             div('Attitude towards:'),
                             div(style="display: inline-block;vertical-align:bottom; width: 20%; margin-left:10%", 
                                 numericInput("children", label = 'Children',
                                              value = 1, min = 1, max = 10, step = 1)),
                             div(style="display: inline-block;vertical-align:bottom; width: 20%; margin-left:10%", 
                                 numericInput("teenagers", label = 'Teenagers',
                                              value = 1, min = 1, max = 10, step = 1)),
                             div(style="display: inline-block;vertical-align:bottom; width: 20%; margin-left:10%", 
                                 numericInput("students", label = 'Students',
                                              value = 1, min = 1, max = 10, step = 1)),
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
                    h2("You want to move to the darkest areas!"),
                    leafletOutput("map", height = 600)
                )
            )
            
        ),
        tabPanel("Documentation")
    )
        )
