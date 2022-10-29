check_perf <- function (data){
    # Check if uploaded file has known target variable or not
    # This is because if target variable is available, then performance would be assessed
    
    if ("Total_MACE" %in% names(data())){
      check_performance <- TRUE
    } else {
      check_performance <- FALSE
    } 
    check_performance
}