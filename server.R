library(shiny)
library(shinydashboard)
library(foreign)
library(readxl)
library(h2o)
library(recipes)
library(tools)
library(pROC)
library(ggplot2)

server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$varnameComplete <- rv$predictTableComplete <- rv$perfComplete <- rv$perfPlot <- FALSE
    
  loadingFunc <- function(message='Loading ...') { 
    
      withProgress(min=1, max=15, expr={
    
      for(i in 1:15) {
        setProgress(message,
                    detail = 'This may take a while...',
                    value=i)
        Sys.sleep(0.05)
      }
    })
  }
    
  observe({

    rv$varnameComplete <- TRUE
    
    output$tableVarNames <- renderDataTable({ 
      
      loadingFunc(message = "Initializing variables loading...")
      varnames <- readRDS("www/varnames.RDS")
      varnames}, options = list(pageLength=10)
    )
  
    output$varnameComplete <- reactive({
      return(rv$varnameComplete)
    })
    
    outputOptions(output, 'varnameComplete', suspendWhenHidden=FALSE)
  })
  
  blueprint <- readRDS("www/blueprint_mod.RDS")
  studyTest <- readRDS("www/df_test.RDS")

  h2o.init()
  h2o.no_progress()

  model <- eventReactive(input$predict_btn,{

    if (input$models == "RF"){
      selected_model <- h2o.loadModel("www/rf_grid1_model_20")
    } else if (input$models == "Ensemble GLM"){
      selected_model <- h2o.loadModel("www/ensembledModel_glm")
    } else if (input$models == "Ensemble NB"){
      selected_model <- h2o.loadModel("www/ensembledModel_nb")
    } else if (input$models == "GBM"){
      selected_model <- h2o.loadModel("www/gbm_grid1_model_33")
    } else if (input$models == "GLM LR Ridge"){
      selected_model <- h2o.loadModel("www/glm_grid1_model_2")
    } else if (input$models == "FNN"){
      selected_model <- h2o.loadModel("www/fnn_grid1_model_17")
    } else if (input$models == "Xgboost"){
      selected_model <- h2o.loadModel("www/xgb_grid1_model_38")
    } 
    selected_model
  })
  
  data <- reactive({
    
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
  
  })
  
  check_performance <- reactive({ 
    
    if ("Total_MACE" %in% names(data())){
    
      check_performance <- TRUE
      
    } else {
      
      check_performance <- FALSE
      
    } 
    check_performance
  })
  

  observe({
    
    if (input$predict_btn){
      
      rv$perfComplete <- TRUE
    
      output$performance <- renderPrint({
      
        if (check_performance()){
          
          loadingFunc(message = "Initializing performance summary...")
          
          h2o.performance(model(), newdata = data())
          
        } else {
          
          cat(
            "No performance assessment would be conducted due to unknown target variable
    in uploaded data file.
    please click on 'Table' tab to see prediction result."
             )
          }
        }) 
        output$perfComplete <- reactive({
          return(rv$perfComplete)
        })
        
        outputOptions(output, 'perfComplete', suspendWhenHidden=FALSE)
  }
  })
  
  predict_metrics <- reactive({
    
    if (check_performance()){
      
      h2o.predict(model(), newdata = data())
      
    } else {
      
      f_col <- as.h2o(data.frame(Total_MACE = as.factor(
        sample(c("No", "Yes"),nrow(data()), replace = T)),
                                    stringsAsFactors = F))
      
      dataset_mod <- h2o.cbind(data(), f_col)
      
      h2o.predict(model(), newdata = dataset_mod)
    }
  })
  
  observe({
    
    if (input$predict_btn){
    
      rv$predictTableComplete <- TRUE
      
      output$predict_tbl <- renderDataTable({
      
        loadingFunc("Loading predictions table...")
        
        predict_metrics()
        
      })
      
      output$predictTableComplete <- reactive({
        return(rv$predictTableComplete)
      })
      
      outputOptions(output, 'predictTableComplete', suspendWhenHidden=FALSE)
      
    } 
  
  })

  observe({
  
      if (input$predict_btn){
        rv$perfPlot <- TRUE
        
        output$predict_plot <- renderPlot({
          if (check_performance()){
          
              loadingFunc(message = "initiating smoothing ROC plot...")
              pred <- as.data.frame(predict_metrics())
              
              df <- as.data.frame(data())
        
              ggroc(roc(df$Total_MACE, pred$Yes,
                  ci=T, smooth=T, smooth.n=10,
                  auc=T), legacy.axes = T, color="red") + 
                  xlab("1-Specificity") + ylab("Sensitivity")+
                  geom_segment(aes(x=0,y=0,xend = 1, yend = 1),
                               linetype = 2,col='black',
                               lwd=0.05) 
          }
        }, width="auto", height = "auto", res = 96)
        
      output$perfPlot <- reactive({
        return(rv$perfPlot)
      })
      
      outputOptions(output, 'perfPlot', suspendWhenHidden=FALSE)
  }
  })
  }
