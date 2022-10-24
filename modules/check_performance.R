check_perf <- function (data){
  
    if ("Total_MACE" %in% names(data())){
      
      check_performance <- TRUE
      
    } else {
      
      check_performance <- FALSE
      
    } 
    check_performance
}