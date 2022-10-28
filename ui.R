library(shiny)
library(shinydashboard)
library(shinybusy)
source("styles/styles.R", local = T)

sidebar <- dashboardSidebar(

  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Authors", tabName = "authors", icon = icon("pencil-alt")),
    menuItem("Abstract", tabName = "abstract", icon = icon("file")),
    menuItem("Prediction Tool", tabName = "prediction", icon = icon("wrench"),
             badgeLabel = "API", badgeColor = "purple"),
    menuItem("Contact", tabName = "contact", icon = icon("phone")),
    menuItem("About", tabName = "about", icon = icon("info"))
  )
)

body <- dashboardBody(
  
  shinybusy::add_busy_spinner(spin = "fading-circle", position = "bottom-right",
                              margins = c("60%", "42%"), width = "60px",
                              height = "60px"),
  style_ref(tags, "style"),
  tags$script(HTML("$('body').addClass('fixed');")),
  
  tabItems(
    
    tabItem(tabName = "home", 
            
            fluidRow(column(12,box(width=12, status = "primary", solidHeader = F,
                column(12, align="center",tags$div("Machine Learning 
                Scoring Models for Prediction of MACE Using Coronary 
                Multidetector Computed Tomography Anatomical Findings 
                and Clinical Features", class="home-title"))))),
            
            fluidRow(width=12, solidHeader = F,
                column(12, align="center",tags$div(img(src='logothc.png', 
                       align = "center", width=200),br(),
                       p("Tehran Heart Center", class="thc-logo-text"),
                       p("Artificial Intelligence Division", 
                       class="thc-logo-subtext")))),
           
            fluidRow(column(6,box(width=12 , background = "light-blue",
                              tags$div("Using Seven Machine Learning Models
                              to Predict Outcome in file format of .RDS, 
                              .csv, .sav, .xlsx ",class="home-box-title"))),
                     
                column(6,box(width=12, background = "teal",
                          column(12, align="center",tags$div("An Online 
                          Machine Learning Tool to Predict Two 
                          Years Major Adverse Cardiovascular Events  
                          Following Coronary MDCT", class="home-box-title"))
                          ))
        ),
    ),
    
    tabItem(tabName = "authors",
            
            fluidRow(box( width = 12, status = "primary",
            
              div(p(span("Authors and Affiliations", style="font-weight:bold; 
                    font-size:24px;")),hr(),
                    "Seyyed Mojtaba Ghorashi"," MD-MPH", tags$sup("1"),br(),
                    "Amir Fazeli, MD", tags$sup("1"), br(),
                    "Behnam Hedayat, MD", tags$sup("1"), br(),
                    "Hamid Mokhtari, PhD", tags$sup("1"),br(),
                    "Arash Jalali, PhD", tags$sup("1"),br(),
                    "Pooria Ahmadi, MD", tags$sup("1"),br(),
                    "Hamid Chalian, MD", tags$sup("3"),br(),
                    "Nicola Luigi Bragazzi, MD, PhD",  tags$sup("4"),br(),
                    "Shapour Shirani, MD", tags$sup("5"), br(),
                    "Negar Omidi, MD", tags$sup("5*"), br(),
                    hr(),
  
                    "1. Tehran Heart Center, Tehran University of Medical Science, 
                        Tehran, Iran.", br(),
                    "2. Shahid Beheshti University of Medical Science, Tehran, 
                        Iran.", br(),
                    "3. Division of Cardiothoracic Imaging, Department of Radiology, 
                        University of Washington, Seattle, Washington, USA.", br(),
                    "4. Laboratory for industrial and applied mathematics (LIAM), 
                        Department of mathematics and statistics, York university, 
                        Toronto, Canada.",br(),
                    "5. Department of Cardiovascular Imaging, Tehran Heart Center,
                        Tehran University of Medical Sciences, Tehran, Iran.",
                    br(),hr(),
                    span("Correspondence:", style="font-weight:bold"),br(), 
                         "Negar Omidi, ", br(), "Associate Professor Department of 
                          Cardiovascular Imaging", br(),
                          "Tehran Heart Center, ", br(), 
                          "Tehran University of Medical Sciences,", br(),
                          "Kargar St. Jalal al-Ahmad Cross, zip code: 1411713138, 
                          Tehran, Iran.", 
                    class="abstract-text")))),
    
    tabItem(tabName = "abstract", 
            
            fluidRow(box(width=12, status = "primary",
                  div(p(span("Abstract", style="font-weight:bold; font-size:24px;")), hr(),
            
                  p(strong("Background:")),"The study aims to compare the 
                  prognostic performance of conventional scoring systems to a 
                  machine learning (ML) model on coronary computed tomography 
                  angiography (CCTA) to discriminate between the patients with 
                  and without major adverse cardiovascular events (MACE) and 
                  to find the most important contributing factor of MACE.", hr(),

                  p(strong("Materials and methods:")),"From November to December 
                  2019, 500 of 1586 CCTA scans were included and analyzed, then 
                  six conventional scores were calculated for each participant, 
                  and seven ML models were designed. Our study endpoints were 
                  all-cause mortality, non-fatal myocardial infarction, late 
                  coronary revascularization, and hospitalization for unstable 
                  angina or heart failure. Score performance was assessed by 
                  area under the curve (AUC) analysis.", hr(),

                  p(strong("Results:")),"Of 500 patients (mean age: 60±10; 
                  53.8% male) referred for CCTA, 416 patients have met inclusion 
                  criteria, 46 patients with early (<90 days) cardiac evaluation
                  (due to the inability to clarify the reason for assessment,
                  deterioration of the symptoms vs. the CCTA result), and 38 
                  patients because of missed follow-up were not enrolled in the 
                  final analysis. Forty-six patients (11.0%) developed MACE 
                  within 20.5±7.9 months of follow-up. Compared to conventional 
                  scores,ML models showed better performance, only one model 
                  which is eXtreme Gradient Boosting had lower performance than 
                  conventional scoring systems (AUC:0.824, 95% confidence 
                  interval (CI): 0.701-0.947). Between ML models, Random Forest, 
                  Ensemble With Generalized Linear, and Ensemble With Naive-Bayes
                  were shown to have higher prognostic performance (AUC: 0.92, 
                  95% CI: 0.85-0.99, AUC: 0.90, 95% CI: 0.81-0.98, and AUC: 0.89, 
                  95% CI: 0.82-0.97), respectively. Coronary artery calcium score 
                  (CACS) had the highest correlation with MACE.", hr(),

                  p(strong("Conclusion:")), "Compared to the conventional scoring 
                  system, ML models using CCTA scans show improved prognostic 
                  prediction for MACE. Anatomical features were more important 
                  than clinical characteristics.", class="abstract-text"))
    )),
  
    tabItem(tabName = "prediction",

        tabsetPanel(type="tabs", id = "predictTabs",
                        
            tabPanel("Introduction", 
            
                fluidRow(box(width=12,status="primary",
                        div(p(span("Prediction Tool", style="font-weight:bold; 
                          font-size:24px;")), hr(),
                          p("Here a prediction tool is provided based on seven 
                          machine learning models trained on patients which 
                          underwent elective coronary MDCT, to predict major 
                          cardiovascular event (MACE) with using coronary MDCT 
                          anatomical features combined with clinical features.
                          ", class="predict-text"), 
                          
                          p("You can upload your custom file from file input 
                          box bellow. At the moment Allowed format is *.rds, 
                          *.csv, *.sav and *.xlsx formats.", class="predict-text"),
                          p("Because the models have been trained with specific names 
                          of features, your dataset features names should be 
                          transformed to the names provided in 'Variables Names' 
                          box bellow to enable prediction.", class="predict-text"),
                  
                ))), br(),

                conditionalPanel(condition = "output.varnameComplete",
                       fluidRow(box(title=strong("Variables Names"),
                                    id="varnames",width=12,
                                    status="primary", collapsible = T, collapsed = F,
                                    dataTableOutput("tableVarNames")))
                ),
                conditionalPanel(condition = "!output.varnameComplete",
                                 fluidRow(box(title=strong("Variables Names"),
                                              id="varnames",width=12,
                                              status="primary", collapsible = T, collapsed = F,
                                              "Loading variables, It
                                              may take a while..."))
                ),
            ),
          
            tabPanel("Prediction",
                     
                fluidRow(box(title=strong("Upload File"),width=12, 
                         status = "primary",
                         collapsible = T, collapsed = F,
                         p("You can upload your data with *.rds, *.csv, *.sav or 
                          *.xlsx formats. 
                          If you do not upload a file, a new sample test set
                          with known target variable
                          would be used for prediction as default.", 
                          class="predict-text"),
                         
                         fileInput("loadFile", label = "Please Upload Your Data:",
                                   width="300px"),
                         
                         selectInput("models", "Please Select a Model", 
                                     choices = c("RF", "Ensemble GLM", 
                                                 "Ensemble NB", 
                                                 "GBM", "GLM LR Ridge", "FNN", 
                                                 "Xgboost"), 
                                     selected = "RF", width = '200px'),
                      
                         p(strong("Click predict... button bellow to initiate 
                                  prediction")),
                         
                         actionButton("predict_btn", label = "Predict...", 
                                      width = "100px"))),

                fluidRow(uiOutput("performanceState")),
            ),
            
            tabPanel("Table",
                
                fluidRow(box(title=strong("Prediction Ouput Table"), width=12, 
                             status="primary", collapsible = T, collapsed = F,
                             p("Here, Prediction table with probability of 
                             'No' event and 'Yes' event are provided. 
                             Final prediction of Total_MACE is provided in first
                             column.
                             If your data file contains target variable named 
                             Total_MACE, the cutoff for discriminating MACE-No vs
                             MACE-Yes is calculated by maximum F1 score in training set 
                             by default.
                             If your data file does not contain target variable, 
                             The cutoff for defining MACE vs No-MACE is calcluated
                             by our study cutoff maximum F1 score of each model in 
                             original training set.",
                             "You can define which metrics should be used for 
                             threshold assignment.", class="predict-text"))),
                
                fluidRow(box(title = strong("Metrics"), 
                             "Please Select a Metric for Classification Threshold",
                             width=12, status = "primary",
                             collapsible = T, collapsed = F, 
                             selectInput("metrics_input", label = " ",
                                         choices = c("F1 score", "F2 score", "f0point5",
                                                     "Accuracy","Precision",
                                                     "recall", "Specificity",
                                                     "Absoulute_mcc",
                                                     "TNR", "FNR", "FPR",
                                                     "TPR"), selected = "F1 score", 
                                                     width = '200px'))
                ),

                conditionalPanel(condition = "output.predictTableComplete",
                                 fluidRow(box(title=strong("Table"),  
                                              id="predictTable",width=12,
                                              status="primary", collapsible = T, 
                                              collapsed = F,
                                              dataTableOutput("predict_tbl")))
                ),
                conditionalPanel(condition = "!output.predictTableComplete",
                                 fluidRow(box(title=strong("Table"),
                                              id="predictTable",width=12,
                                              status="primary", collapsible = T, 
                                              collapsed = F,
                                              "Waiting for predictions table, It
                                              may take a while..."))
                ),
            ),
              
            tabPanel("Plots", 
                
                fluidRow(box(title=strong("Performance Plots"), width=12,
                             status="primary", collapsible = T, collapsed = F,
                             "If your dataset contains target variable named
                             Total_MACE, ROC curve would be provided.",
                             class="predict-text")),
                
                conditionalPanel(condition = "output.perfPlot",
                                 fluidRow(box(title=strong("Plot"),  
                                              id="predictPlot",width=12,
                                              height = 650,
                                              status="primary", collapsible = T, 
                                              collapsed = F,
                                              column(12, align="center", 
                                              plotOutput("predict_plot"))))
                ),
                conditionalPanel(condition = "!output.perfPlot",
                                 fluidRow(box(title=strong("Plot"),
                                              id="predictPlot",width=12,
                                              status="primary", collapsible = T, 
                                              collapsed = F,
                                              "Waiting for Performance Plot, It
                                              may take a while..."))
                ),
            ),
        ),
    ),
    
    tabItem(tabName = "contact",
            fluidRow(box(width=12, status = "primary",
                         div(p(span("Contact", style="font-weight:bold; 
                                    font-size:24px;")), hr(),
                          strong("Phone: "), "+98-21-88029600", br(),
                          strong("Email: "),"email@thc.tums.ac.ir, 
                          dr.hedayatb@gmail.com", br(),
                          strong("Address: "), "Tehran Heart Center, 
                          Kargar St. Jalal al-Ahmad Cross, Tehran, Iran",br(),
                          strong("Zip Code: "), "1411713138", class="abstract-text"))
    )),
    
    tabItem(tabName = "about",
            fluidRow(box(width=12, status="primary",
            div(p(span("About", style="font-weight:bold; font-size:24px;")),
                style="width:80%; margin-top:0px; font-size:16px;"))
    ))
  )
)
  
dashboardPage(
  skin = "purple",
  dashboardHeader(title = "CTAMACE"),
  sidebar,
  body
)
