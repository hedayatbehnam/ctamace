check_perf <- function (data){
    # Check if uploaded file has known target variable or not
    # This is because if target variable is available, then performance would be assessed
    if (!data$target){
      check_performance <- TRUE
    } else {
      check_performance <- FALSE
    } 
    print(data$target)
    print(check_performance)
    return (check_performance)
}