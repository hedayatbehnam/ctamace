## function to upload data files by user
upload_data <- function(input){
    dataset <- NULL
    noTarget <- FALSE
    if (is.null(input$loadFile$datapath)){
      dataset <- loadedFile <- studyTest # if no file is uploaded, sample test is used by default
    } else {
      loadedFile <- input$loadFile  # read uploaded file in fileInput section
      ext <- tools::file_ext(loadedFile$datapath) # detect file extension
      req(loadedFile)
      validate(
        # validating allowed file extensions
        need(tolower(ext) %in% c("csv", "rds", "xlsx", "sav"), 
              "Uploaded file shoud be in .csv, .rds, .xlsx or .sav format"))
      if (tolower(ext) == "rds"){
        dataset <- readRDS(loadedFile$datapath)
      } else if (tolower(ext) == "csv"){
        dataset <- read.csv(loadedFile$datapath)
      } else if (tolower(ext) == "sav") {
        dataset <- read.spss(loadedFile$datapath, to.data.frame = TRUE)
      } else if (tolower(ext) == "xlsx") {
        dataset <- read_excel(loadedFile$datapath)
      } 
      validate(
        # validating if names of variables are the same with original study
        need(names(dataset) %in% names(studyTest), "At least one variable name
          in uploaded dataset is not the same with original study train set variable names")
      )
    }
    if (!"Total_MACE" %in% names(dataset)){   
      # creating a random non-used target variable for uploaded data file with no target variable
      # to make preprocessing by `recipe` package `prep` function possible. (a bug in `recipes` package)
      noTarget <- TRUE
      f_col <- sample(c("No", "Yes"), nrow(dataset), replace = T)
      dataset$Total_MACE <- as.factor(f_col)
    }
    test_data <- prep(blueprint, training = dataset) #defining preprocessing rules in main study to uploaded data  
    test_data_j <- test_data %>% juice() #applying preprocessing rules to uploaded data
    if (noTarget){
      # making target variable NULL again after preprocessing conducted,
      test_data_j$Total_MACE <- NULL
    }
    test_data_h2o <- test_data_j %>% as.h2o() #changing format of data.frame to h2o.Frame
    test_data_h2o
}