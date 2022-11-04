options(shiny.maxRequestSize=30*1024^2) 
library(shiny)
library(shinydashboard)
library(foreign)
library(readxl)
library(h2o)
library(recipes)
library(tools)
library(pROC)
library(ggplot2)
source('./init_h2o.R', local = T)
source('modules/load_model.R', local = T)
source('modules/loading_function.R', local=T)
source('modules/reactiveVal_output.R', local=T)
source('modules/upload_file.R', local= T)

server <- function(input, output, session) {
  rv <- reactiveValues()
  rv$varnameComplete <- rv$perfMetrics <- rv$predMetrics <- FALSE
  reactiveVal_output(rv, 'perfMetrics', 'empty', output)
  reactiveVal_output(rv, 'predMetrics', 'empty', output)
  
  observe({
    rv$varnameComplete <- TRUE
    output$tableVarNames <- renderDataTable({ 
      loadingFunc(message = "Initializing variables loading...")
      varnames <<- readRDS("www/varnames.RDS")
      varnames
    }, options = list(pageLength=10, scrollX=TRUE) 
    )
    output$varnameComplete <- reactive({
      return(rv$varnameComplete)
    })
    outputOptions(output, 'varnameComplete', suspendWhenHidden=FALSE)
  })
  
  init_h2o()
  vars <- reactiveValues()
  
  observeEvent(input$predict_btn,{
    vars$model <- load_model(input)
    vars$data <- upload_data(input)
    if (is.character(vars$data)){
      if (vars$data == "empty"){
        reactiveVal_output(rv, 'predMetrics', 'empty', output)
        reactiveVal_output(rv, 'perfMetrics', 'empty', output)
      } else if (vars$data == 'nonValid') {
        reactiveVal_output(rv, 'predMetrics', 'nonValid', output)
        reactiveVal_output(rv, 'perfMetrics', 'nonValid', output)
      } else if (vars$data == 'mismatch'){
        reactiveVal_output(rv, 'predMetrics', 'mismatch', output)
        reactiveVal_output(rv, 'perfMetrics', 'mismatch', output)
      }
    } else {
      reactiveVal_output(rv, 'predMetrics', 'complete', output)
      vars$predict_metrics <- h2o.predict(vars$model, newdata = vars$data$h2oData)
      if (!vars$data$target){
        vars$performance_result <- suppressWarnings(h2o.performance(vars$model, newdata = vars$data$h2oData))
        vars$max_scores <- as.data.frame(vars$performance_result@metrics$max_criteria_and_metric_scores)
        vars$max_scores <- vars$max_scores[which(names(vars$max_scores) != "idx")]
        loadingFunc(message = "initializing ROC plot...")
        vars$pred <- as.data.frame(vars$predict_metrics)
        vars$df <- as.data.frame(vars$data$h2oData)
        reactiveVal_output(rv, 'perfMetrics', 'complete', output)
      } else {
        reactiveVal_output(rv, 'perfMetrics', 'noTarget', output)
      }
    }
  })

  output$predict_tbl <- renderDataTable({
    as.data.frame(vars$predict_metrics)
  })

  output$performance <- renderDataTable({
    vars$max_scores
  })
  
  output$predict_plot <- renderPlot({
      ggroc(roc(vars$df$Total_MACE, vars$pred$Yes,
                ci=T, smooth=T, smooth.n=10,
                auc=T), legacy.axes = T, color="red") + 
        xlab("1-Specificity") + ylab("Sensitivity")+
        geom_segment(aes(x=0,y=0,xend = 1, yend = 1),
                     linetype = 2,col='black',
                     lwd=0.05)
  }, width="auto", height = 500, res = 96)
}