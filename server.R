library(shiny)
library(shinydashboard)
library(foreign)
library(readxl)
library(h2o)
library(recipes)
library(tools)


server <- function(input, output) {

  blueprint <- readRDS("www/blueprint_mod.RDS")
  varnames <- readRDS("www/varnames.RDS")
  studyTest <- readRDS("www/df_test.RDS")

  h2o.init(max_mem_size = "1G", nthreads = 2)
  h2o.no_progress()
 
  rf_model <- h2o.loadModel("www/rf_grid1_model_20")
  ensGlm_model <- h2o.loadModel("www/ensembledModel_glm")
  ensNb_model <- h2o.loadModel("www/ensembledModel_nb")
  gbm_model <- h2o.loadModel("www/gbm_grid1_model_33")
  glm_model <- h2o.loadModel("www/glm_grid1_model_2")
  fnn_model <- h2o.loadModel("www/fnn_grid1_model_17")
  xgboost_model <- h2o.loadModel("www/xgb_grid1_model_38")



  output$tableVarNames <- renderDataTable({varnames}, options = list(
                                                              pageLength=10))
  
  model <- eventReactive(input$predict_btn,{

    if (input$models == "RF"){
      selected_model <- rf_model
    } else if (input$models == "Ensemble GLM"){
      selected_model <- ensGlm_model
    } else if (input$models == "Ensemble NB"){
      selected_model <- ensNb_model
    } else if (input$models == "GBM"){
      selected_model <- gbm_model
    } else if (input$models == "GLM LR Ridge"){
      selected_model <- glm_model
    } else if (input$models == "FNN"){
      selected_model <- fnn_model
    } else if (input$models == "Xgboost"){
      selected_model <- xgboost_model
    } 
    selected_model
  })
  
  

  data <- reactive({
    
    
    dataset <- NULL
    
    if (is.null(input$loadFile$datapath)){
      
      loadedFile <- studyTest
      
    } else {
      
      loadedFile <- input$loadFile
      
      
    ext <- tools::file_ext(loadedFile$datapath)
    
    req(loadedFile)
  
    validate(need(tolower(ext) %in% c("csv", "rds", "xlsx", "sav"), "Uploaded file shoud be in .csv, .rds, .xlsx or .sav format"))
      
    if (tolower(ext) == "rds"){
      
      dataset <- readRDS(input$loadFile$datapath)
      
    } else if (tolower(ext) == "csv"){
      
      dataset <- read.csv(input$loadFile$datapath)
      
    } else if (tolower(ext) == "sav") {
      
      dataset <- read.spss(input$loadFile$datapath, to.data.frame = TRUE)
      
    } else if (tolower(ext) == "xlsx") {
      
      dataset <- read_excel(input$loadFile$datapath)
      
    }
      
    
    validate(
      need(names(dataset) %in% names(studyTest), "At least one variable name
      in uploaded dataset is not the same with original study train set variable names")
    )
    
    }
    
    test_data <- prep(blueprint, training = dataset)

    test_data_j <- test_data %>% juice()

    test_data_h2o <- test_data_j %>% as.h2o()
    
    test_data_h2o

    
  })
  
  
  
  check_performance <- reactive({ 
    
    if ("Total_MACE" %in% names(data())){
    
      check_performance <- TRUE
      
    } else {
      
      check_performance <- FALSE
      
    } 
    
    check_performance
    
  })
  

    
  output$performance <- renderPrint({

    
    if (check_performance()){

      h2o.performance(model(), newdata = data())
      
    }
  })
  

  
  # output$prediction <- renderDataTable({
  #   
  #   if (check_performance()){
  #     
  #     h2o.predict(model(), newdata = data())
  #   }
  # })
  

  
}
