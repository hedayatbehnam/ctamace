## function to upload data files by user
upload_data <- function(input){
    dataset <- NULL
    noTarget <- FALSE
    loadedFile <- input$loadFile  # read uploaded file in fileInput section
    
    if (is.null(loadedFile$datapath)){
      return ('empty')
    }

    ext <- tools::file_ext(loadedFile$datapath) # detect file extension
    req(loadedFile)
    
    if (!(tolower(ext) %in% c("csv", "rds", "xlsx", "sav"))) {
      return ('nonValid')
    }
    
    if (tolower(ext) == "rds"){
      dataset <- readRDS(loadedFile$datapath)
    } else if (tolower(ext) == "csv"){
      dataset <- read.csv(loadedFile$datapath)
    } else if (tolower(ext) == "sav") {
      dataset <- read.spss(loadedFile$datapath, to.data.frame = TRUE)
    } else if (tolower(ext) == "xlsx") {
      dataset <- read_excel(loadedFile$datapath)
    } 
    
    if (!any((names(dataset) %in% varnames[,'Variable']))){
      return("mismatch")
    }

    if (!is.null(loadedFile)){
      if (!"Total_MACE" %in% names(dataset)){   
        # creating a random non-used target variable for uploaded data file with no target variable
        # to make preprocessing by `recipe` package `prep` function possible. (a bug in `recipes` package)
        noTarget <- TRUE
        f_col <- sample(c("No", "Yes"), nrow(dataset), replace = T)
        dataset$Total_MACE <- as.factor(f_col)
      }
      tryCatch({
        # defining preprocessing rules in main study to uploaded data
        test_data <- prep(blueprint, training = dataset)
        # applying preprocessing rules to uploaded data
        test_data_j <- test_data %>% juice() 
        test_data_h2o <- test_data_j %>% as.h2o() #changing format of data.frame to h2o.Frame
        return(list(h2oData = test_data_h2o, target = noTarget))
      },
      error=function(e){
        return ("mismatch")
      })
    }
}