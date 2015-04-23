library("shiny")

shinyServer(function(input, output, session){
  
  output$explanatoryVar <- renderUI({
    regressors <- list("End.Av.Weight", "Start.Av.Weight", "Days", "Period.Feed.Qty",
                       "Suggested.Feed.Qty", "Econ.FCR.Period", "SFR.Period", "SGR.Period",
                       "LTD.Mortality", "Avg.Temperature", "Age")
    
    checkboxGroupInput(inputId='explanatory.Variables', label=h3('Explanatory Variable(s):'), 
                       choices=regressors, selected=regressors[1])
  })
  
  
  output$numpreds <- renderPrint({
    if (input$goSetValues == 0){
        return() }
    else{ 
        list.predictors <- input$explanatory.Variables
        num.preds <- length(list.predictors)
        print(num.preds)
    }
  })  
  
  
output$dyn_input <- renderUI({
  if (input$goSetValues == 0){
    return() }
  else{ 
  list.predictors <- input$explanatory.Variables
  num.preds <- length(list.predictors)
  
  set.inputs.list <- lapply(1:num.preds, function(i) {
      input_name <- paste0("input", i, sep="")
     # wellPanel(      
      numericInput( input_name, label = h4( as.character(list.predictors[[i]]) ), value = NA)
    #  )
  })
  
 } # end else
 do.call(tagList, set.inputs.list)
}) 

#-------------------------------- 
output$value <- renderPrint({
  if (input$goSetValues == 0){
    return() }
  else{ 
    list.predictors <- input$explanatory.Variables
    num.preds <- length(list.predictors)
    
    newdata <- as.data.frame(matrix(0, nrow = 1, ncol=num.preds))
    #newdata <- data.frame()
    newdata <- lapply(1:num.preds, function(i) {
        input_name <- paste0("input", i, sep="")
        input[[ input_name ]]
      print( input[[ input_name ]] )
       
      } # end function
      
    )# end lapply
    
    names(newdata) <- list.predictors
   View( newdata )
    
    print( names(newdata) )
  } # end if...else
  
})







})