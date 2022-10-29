load_dependencies <- function(){
    # Loading packages and sources
    # Not Run in shiny cloud.
    libraries <- c("shiny", 
                   "shinydashboard", 
                   "foreign", 
                   "readxl",
                   "h2o", 
                   "recipes", 
                   "tools",
                   "pROC",
                   "ggplot2")
    invisible(lapply(libraries, library, character.only = TRUE))
    source('init_h2o.R')
    modulesPath <- 'modules/'
    modules <- c('load_data.R', 'loading_function.R',
                 'upload_file.R', 'check_performance.R', 'reactiveVal_output.R')
    invisible(lapply(modules, function(x) source(paste0(modulesPath, x))))
}


