## for each process one indicator of reactiveValue is created (eg. perfMetrics) with different
## values when is non proessessed or complete to change text in each section before and after process
reactiveVal_output <- function(name, value, content, output){
    # Change the values of each reactiveVal indicator
    name[[value]] <- content
    output[[value]] <- reactive({
      return(name[[value]])
    })
    outputOptions(output, value, suspendWhenHidden=F)
}