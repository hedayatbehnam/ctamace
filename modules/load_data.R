load_data <- function(input){
  
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
}

## upload blueprint of preprocessing methods during training main dataset
blueprint <- readRDS("www/blueprint_mod.RDS")

## upload a sample test set by default when no file data uploaded
studyTest <- readRDS("www/df_test.RDS")