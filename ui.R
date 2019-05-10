library(shiny)
library(ggplot2)

shinyUI(pageWithSidebar(
  
  ## headerPanel ==================================================================
  headerPanel("Spatio-temporal data analysis"),
  
  ## sidebarPanel ==================================================================
  sidebarPanel(
    
    ## upload data -----------------------------------------------------------------
    h3("1. Upload Data"),
    # helpText("Input data must contain four columns (in order): time (0, 1, 2, ...),
    #          spatial coordinates (X, Y) and group (1, 2, 3, ...)"), 
    fileInput('file1', 'Choose csv or txt File',
              multiple = FALSE,
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    checkboxInput('header', 'Header', TRUE), 
    actionButton(inputId = "ExDat", "or use Example Data", 
                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    hr(), 
    h3("2. Set Tunning Parameters"),
    uiOutput("sum"), 
    br(), 
    uiOutput("growth_t"), 
    br(), 
    uiOutput("dist_n"), 
    br(), 
    uiOutput("dist_t"), 
    br(),
    uiOutput("est_buttom"),
    hr(), 
    uiOutput("sel_buttom")
  ),
  
  
  ## mainPanel ==================================================================
  mainPanel(
    htmlOutput("summary"),
    br(),
    tabsetPanel(tabPanel("Data", dataTableOutput("indata")), 
                tabPanel("Summary", 
                         downloadButton("downloadSum", "Download"),
                         plotOutput("growth", width = "100%")
                         ), 
                tabPanel("Estimation", 
                         downloadButton("downloadEst", "Download"),
                         plotOutput("beta_plot", width = "100%")), 
                tabPanel("Selection", 
                         downloadButton("downloadSel", "Download"),
                         h5("This is going to take a few minutes. Patience ..."), 
                         plotOutput("sel_plot", width = "100%"))) 
              
    )
  )
)