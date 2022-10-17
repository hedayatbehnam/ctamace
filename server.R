library(shiny)
library(shinydashboard)

library(h2o)
library(recipes)


server <- function(input, output) {
  
  blueprint <- readRDS("www/blueprintSurv.RDS")
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
  
  
  model <- reactive({

    if (input$models == "RF"){
      selected_model <- rf_model
    } else if (input$models == "Ensemble GLM"){
      selected_model <- ensGlm_model
    }else if (input$models == "Ensemble NB"){
      selected_model <- ensNb_model
    } else if (input$models == "GBM"){
      selected_model <- gbm_model
    }else if (input$models == "GLM LR Ridge"){
      selected_model <- glm_model
    }else if (input$models == "FNN"){
      selected_model <- fnn_model
    }else if (input$models == "Xgboost"){
      selected_model <- xgboost_model
    }

    selected_model
  })

  
  data <- reactive({

    if(is.null(input$file_input))
    {
      final <- studyTest
    }
    else
    {
      req(input$rdsFile)
      final <- readRDS(input$rdsFile$datapath)
    }
      
    test_data <- prep(blueprint, training = final)

    test_data_j <- test_data %>% juice()

    test_data_h2o <- test_data_j %>% as.h2o()


  })
  
  output$performance <- renderPrint({
    h2o.performance(model(), newdata = data())
  })


  output$prediction <- renderTable({

    h2o.predict(rf_model,newdata = data() )
  })
  
  
  
  observeEvent(input$titleId, {
    js$collapse("varnames")
  })
  
}
