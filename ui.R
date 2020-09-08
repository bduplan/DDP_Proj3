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
shinyUI(fluidPage(

    # Application title
    titlePanel("Predicting Fuel Mileage"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("Select Predictors"),
            checkboxGroupInput("predictors", "Predictors", 
                               c("Number of Cylinders" = "cyl",
                                 "Engine Displacement" = "disp",
                                 "Engine Horsepower" = "hp",
                                 "Final Drive Ratio" = "drat",
                                 "Vehicle Weight (1000's of lbs)" = "wt",
                                 "1/4-Mile Elapsed Time" = "qsec",
                                 "Engine Cylinder Arrangment (0 = V)" = "vs",
                                 "Auto/Manual Transmission (0 = auto)" = "am",
                                 "Number of Forward Gears" = "gear",
                                 "Number of Carburetors" = "carb"),
                               selected = "cyl"),
            h3("Predictors:"),
            tableOutput("predTable")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # actionButton("browser", "browser"),
            tabsetPanel(type = "tabs",
                        tabPanel("Summary", br(), 
                                 h3("Coefficients and Accuracy"),
                                 verbatimTextOutput("smry")),
                        tabPanel("Diagnostics", br(),
                                 plotOutput("plot1")),
                        tabPanel("Predictions", br(),
                                 "Expected Value w/ 95% Confidence Interval: ",
                                 tableOutput("predMPG"),
                                 uiOutput("predSliders")
                        )
            )
        )
    )
))













