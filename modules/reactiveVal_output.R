reactiveVal_output <- function(name, value, content, output){

    name[[value]] <- content
    
    output[['perfMetrics']] <- reactive({
      return(name[[value]])
    })
    outputOptions(output, value, suspendWhenHidden=F)
}


