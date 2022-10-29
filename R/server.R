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
source('modules/load_model.R', local = T)
source('modules/loading_function.R', local=T)
source('modules/check_performance.R', local=T)
source('modules/reactiveVal_output.R', local=T)
source('./init_h2o.R', local = T)
source('modules/upload_file.R', local= T)
server <- function(input, output) {
  
  rv <- reactiveValues()
  rv$varnameComplete <- rv$predictTableComplete <- rv$perfPlot <- FALSE
  reactiveVal_output(rv, 'perfMetrics', 'empty', output)
  
  observe({
    rv$varnameComplete <- TRUE
    output$tableVarNames <- renderDataTable({ 
      loadingFunc(message = "Initializing variables loading...")
      varnames <- readRDS("www/varnames.RDS")
      varnames
      }, options = list(pageLength=10, scrollX=TRUE) 
    )
    output$varnameComplete <- reactive({
      return(rv$varnameComplete)
    })
    outputOptions(output, 'varnameComplete', suspendWhenHidden=FALSE)
  })
  
  init_h2o()

  model <- eventReactive(input$predict_btn,{
      load_model(input)
  })
  
  data <- reactive({
      upload_data(input)
  })
  
  check_performance <- reactive({ 
      check_perf(data)
  })
  
  performance_result <- eventReactive(input$predict_btn, {
      # reactiveVal_output(rv, 'perfMetrics', 'loading', output)
      if (check_performance()){
        h2o.performance(model(), newdata = data())
      }
  })

  observe({
    if (!is.null(performance_result())){
      loadingFunc(message = "Initializing performance summary...")
      output$performance <- renderDataTable({
            max_scores <- as.data.frame(performance_result()@metrics$max_criteria_and_metric_scores)
            max_scores[which(names(max_scores) != "idx")]
      },options = list(scrollX = TRUE))
      reactiveVal_output(rv, 'perfMetrics', 'complete', output)
    } else {
      reactiveVal_output(rv, 'perfMetrics', 'noTarget', output)
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
        as.data.frame(predict_metrics())
      },options = list(scrollX = TRUE))
      output$predictTableComplete <- reactive({
        return(rv$predictTableComplete)
      })
      outputOptions(output, 'predictTableComplete', suspendWhenHidden=FALSE)
    } 
  })
  
  output$no_target <- renderText({"Because your data file does not contains target variable called 
                      Total_MACE, no performance assessment was conducted."})
  
  output$loading <- renderText({'Loading performance metrics'})
  
  observe({
    output[["performanceState"]] <- renderUI({
      perfStat <- NULL
      if (rv$perfMetrics == 'empty'){
        perfStat <- 'No data available yet.'
      } else if (rv$perfMetrics == 'loading'){
        perfStat <- textOutput('loading')
      } else if (rv$perfMetrics == 'complete'){
        perfStat <- dataTableOutput('performance')
      } else if (rv$perfMetrics == 'noTarget'){
        perfStat <- textOutput("no_target")
      }
      box(id="perfmet", title=strong("Performance Metrics"),  
                   width=12,
                   status="primary", 
                   collapsible = T, 
                   collapsed = F,
                   perfStat)
    })
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
        }, width="auto", height = 500, res = 96)
      output$perfPlot <- reactive({
        return(rv$perfPlot)
      })
      outputOptions(output, 'perfPlot', suspendWhenHidden=FALSE)
      }
  })
  }
