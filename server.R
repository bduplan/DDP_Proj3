#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    observeEvent(input$browser,{
        browser()
    })
    
    # output$predVals <- c("Number of Cylinders" = "cyl",
    #     "Engine Displacement" = "disp",
    #     "Engine Horsepower" = "hp",
    #     "Final Drive Ratio" = "drat",
    #     "Vehicle Weight" = "wt",
    #     "1/4-Mile Elapsed Time" = "qsec",
    #     "Engine Cylinder Arrangment" = "vs",
    #     "Auto/Manual Transmission" = "am",
    #     "Number of Forward Gears" = "gear",
    #     "Number of Carburetors" = "carb")
    
    predLookup <- c("cyl" = "Number of Cylinders",
                    "disp" = "Engine Displacement",
                    "hp" = "Engine Horsepower",
                    "drat" = "Final Drive Ratio",
                    "wt" = "Vehicle Weight",
                    "qsec" = "1/4-Mile Elapsed Time",
                    "vs" = "Engine Cylinder Arrangment",
                    "am" = "Auto/Manual Transmission",
                    "gear" = "Number of Forward Gears",
                    "carb" = "Number of Carburetors")
    
    output$predTable <- renderTable({predLookup[input$predictors]}, 
                                    colnames = FALSE, rownames = TRUE)
    
    model1 <- reactive({
        # form <- paste("mpg ~ ",
        #                  paste(names(grep(paste(input$predictors, collapse="|"), 
        #                                   predLookup, value=TRUE)), collapse = " + "))
        predString <- paste(input$predictors, collapse=" + ")
        if(predString == ""){predString <- "."}
        form <- paste(c("mpg", predString), 
                      collapse = " ~ ")
        lmcall <- paste("lm(", form, ", data = mtcars)", collapse = "")
        lmf <- function(funText) {eval(parse(text=funText))}
        lmf(lmcall)
    })
    
    output$smry <- renderPrint({summary(model1())})
    
    output$plot1 <- renderPlot({
        par(mfrow = c(2,3))
        plot(model1(), which = 1)
        plot(model1(), which = 2)
        plot(model1(), which = 3)
        plot(model1(), which = 4)
        plot(model1(), which = 5)
        plot(model1(), which = 6)
    })

})
