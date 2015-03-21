shinyServer(function(input, output, session){

  output$orderNum <- renderText(input$orders)
  
  # Progress value
  output$progress <- renderUI({
    tagList(input$progress, tags$sup(style="font-size: 20px", "%"))
  })
  
  # Icon to show with progress
  output$progressIcon <- renderUI({
    iconName <- switch(input$progress,
                       "100" = "ok",
                       "0" = "remove",
                       "road"
    )
    icon(iconName, lib = "glyphicon")
  })
  
  output$approvalBox <- renderUI({
    # Take a dependence on the button
    button <- input$approvalButton
    
    isolate({
      if (is.null(button) ||
            is.null(input$approvalText) ||
            input$approvalText == "") {
        return(NULL)
      }
      valueBox(
        tagList(input$approvalText, tags$sup(style="font-size: 20px", "%")),
        "Approval Rating", icon = icon("line-chart"), color = "green"
      )
    })
  })
  
  # Histogram
  output$plot <- renderPlot({
    hist(rnorm(input$orders))
  })
  
  # Predicted values for dygraph
  predicted <- reactive({
    hw <- HoltWinters(ldeaths)
    predict(hw, n.ahead = input$months,
            prediction.interval = TRUE,
            level = as.numeric(input$interval))
  })
  
  output$dygraph <- renderDygraph({
    dygraph(predicted(), main = "Predicted Deaths/Month") %>%
      dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  # Status text
  output$status <- renderText({
    paste0("The number of months is ", input$months,
           ", and the interval is ", input$interval, ".")
  })
  
  # Status with uiOutput
  output$status2 <- renderUI({
    total <- round(sum(predicted()[,"fit"]))
    if(total < 75000)
      iconClass <- "smile-o"
    else if (total < 150000)
      iconClass <- "meh-o"
    else
      iconClass <- "frown-o"
    
    div(
      "Total predicted deaths in range: ",
      div(total, style = "font-size: 30px"),
      div(icon(iconClass), style = "font-size: 50px; text-align: right;")
    )
  })
}



)
