#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # observeEvent(input$browser,{
    #     browser()
    # })
    
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
    
    ## Add Sliders for Each Checked Box
    output$predSliders <- renderUI({
        out <- vector(mode = "list", length = 0)
        for(p in names(predLookup)) {
            if (any(input$predictors == p)) {
                if(p == "cyl") {step <- 2} 
                else if (p == "vs" || p == "am" || p == "gear" || p == "carb") {
                    step <- 1
                }
                else {step <- NULL}
                out[[p]] <- sliderInput(
                        paste0("slider_", p),
                        predLookup[p],
                        min = min(mtcars[[p]]),
                        max = max(mtcars[[p]]),
                        value = min(mtcars[[p]]),
                        step = step
                )
            }
        }
        if(is.null(out)){
            out <- renderText("Select Predictors to Select Their Values")
        }
        
        return(out)
    })
    
    ## Predict MPG based on the given inputs
    pred1 <- reactive({
        sliderVals <- lapply(input$predictors, 
                             function(x){input[[paste0("slider_", x)]]})
        sliderVals <- as.data.frame(sliderVals)
        names(sliderVals) <- input$predictors

        predict(model1(), sliderVals, interval = "confidence")
    })
    
    output$predMPG <- renderTable(pred1())
})
    
