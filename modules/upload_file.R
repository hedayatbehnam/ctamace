upload_data <- function(input){

    dataset <- NULL
    noTarget <- FALSE
    
    if (is.null(input$loadFile$datapath)){
      
      dataset <- loadedFile <- studyTest
      
    } else {
      
      loadedFile <- input$loadFile
      
      ext <- tools::file_ext(loadedFile$datapath)
      
      req(loadedFile)
      
      validate(need(tolower(ext) %in% c("csv", "rds", "xlsx", "sav"), 
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
        
        need(names(dataset) %in% names(studyTest), "At least one variable name
          in uploaded dataset is not the same with original study train set variable names")
      )
      
    }
    
    if (!"Total_MACE" %in% names(dataset)){
      
      noTarget <- TRUE
      f_col <- sample(c("No", "Yes"), nrow(dataset), replace = T)
      dataset$Total_MACE <- as.factor(f_col)
    }
    
    test_data <- prep(blueprint, training = dataset)
    
    test_data_j <- test_data %>% juice()
    
    if (noTarget){
      test_data_j$Total_MACE <- NULL
    }
    
    test_data_h2o <- test_data_j %>% as.h2o()
    
    test_data_h2o
}