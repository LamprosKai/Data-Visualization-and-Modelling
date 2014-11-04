 library(shiny)
 
 dataset <- list('Upload a file'=c(1))
 
shinyUI(pageWithSidebar(
 
  headerPanel("Data Visualization and Regression"),
 
  sidebarPanel(
    
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 selected = NULL),
    
    numericInput("obs", "Number of observations to view:", 100),
    
    checkboxInput("plotting", "Plotting",
                  value = FALSE),  
    
    conditionalPanel(  
      condition = "input.plotting == true" ,
      
      
      selectInput("plot",label="Plot Type",choices=list('None'='none',"Histogram"="hist",'Density Chart'='dens',
                                                        "Linechart"="line","Pointchart"="point")),
      
      conditionalPanel(
        condition = "input.plot == 'none'"
        
      ) ,
      
      conditionalPanel(
        condition = "input.plot == 'hist'",
        
        selectInput(inputId = "attributes",
                    label = "ATTRIBUTES",
                    choices=names(dataset)),
        
        
        sliderInput("bin",label="Binwidth",
                    min=0,max=200,value=1,step=0.1),
        
        actionButton("goButton1", strong("Update View", style = "color:blue")),
        
        
        selectInput("fil", "Fill",choices=
                      c('None'='.',names(dataset))),
        
        selectInput('facet_row', 'Facet Row', c('None'='.',names(dataset))),
        selectInput('facet_col', 'Facet Column', c('None'='.',names(dataset)))
        
      ),
      
      conditionalPanel(
        condition = "input.plot == 'dens'",
        
        selectInput(inputId = "attributes.dens",
                    label = "ATTRIBUTES",
                    choices=names(dataset)),
        
        
        actionButton("goButton2", strong("Update View", style = "color:blue")),
        
        
        selectInput("fil.dens", "Fill",choices=
                      c('None'='.',names(dataset))),
        
        selectInput('facet_row.dens', 'Facet Row', c('None'='.',names(dataset))),
        selectInput('facet_col.dens', 'Facet Column', c('None'='.',names(dataset)))
        
      ),
      
      
      conditionalPanel(
        condition = "input.plot == 'line'",
        selectInput(
          inputId = "attributx",
          label = "X",
          choices = names(dataset),
        ),
        selectInput(
          inputId = "attributy",
          label = "Y",
          choices = names(dataset),
        ) ,
        actionButton("goButton4", strong("Update View", style = "color:blue")),
        
        selectInput("filline", "Fill",choices=
                      c('None'='.',names(dataset))),
        
        selectInput("sizeline", "Size",choices=
                      c('None'='.',names(dataset))),
        
        selectInput('facet_rowline', 'Facet Row', c('None'='.',names(dataset))),
        selectInput('facet_colline', 'Facet Column', c('None'='.',names(dataset)))
        
      ),
      
      
      conditionalPanel(
        condition = "input.plot == 'point'" ,
        selectInput(
          inputId = "attrx",
          label = "X",
          choices = names(dataset),
        ),
        selectInput(
          inputId = "attry",
          label = "Y",
          choices = names(dataset),
        ),
        actionButton("goButton5", strong("Update View", style = "color:blue")),
        
        selectInput("filpoint", "Fill",choices=
                      c('None'='.',names(dataset))),
        
        selectInput("sizepoint", "Size",choices=
                      c('None'='.',names(dataset))),
        
        selectInput('facet_rowpoint', 'Facet Row', c('None'='.',names(dataset))),
        selectInput('facet_colpoint', 'Facet Column', c('None'='.',names(dataset)))
        
      )                
      
      
    ),
    
    checkboxInput("reg", "Modelling",
                  value = FALSE),  
    
    tags$hr(),
    
    conditionalPanel(
      condition = "input.reg == true" ,
      
       
      conditionalPanel(
        condition = "input.reg == 'none'"
        
      ) ,
      
 
      checkboxInput(inputId = "simple", label = strong("Simple Regression")),
      conditionalPanel(
        condition = "input.simple == true",
   
  selectInput(inputId = "reg_y",
                  label = strong("Response variable"),
                  choices = names(dataset)
                 ),
  
  selectInput(inputId='reg_x',
                     label=strong('Predictor Variable'),
                    choices=names(dataset)),
  

    
 
 
  p(strong("Model predictions")),
  
  
  checkboxInput(inputId = "mod_linear",    label = "Linear (two-dash)"),
  conditionalPanel(
    condition = "input.mod_linear == true",
    checkboxInput(inputId ="stder",label = "Std Error")
  ), 
  
  checkboxInput(inputId = "mod_rlm",    label = "Robust Linear (F1)"),
  conditionalPanel(
    condition = "input.mod_rlm == true",
    checkboxInput(inputId ="stder.rlm",label = "Std Error")
  ), 
  
  checkboxInput(inputId = "mod_quadratic", label = "Quadratic (dashed)"),
  conditionalPanel(
    condition = "input.mod_quadratic == true",
    checkboxInput(inputId ="stder.quad",label = "Std Error")
  ),
  
  checkboxInput(inputId = "mod_loess",     label = "Locally weighted LOESS (solid)"),
  conditionalPanel(
    condition = "input.mod_loess == true",
    checkboxInput(inputId ="stder.loess",label = "Std Error"),
 
    sliderInput(inputId = "mod_loess_span", label = "Smoothing (alpha)",
                min = 0.15, max = 1, step = 0.05, value = 0.75)
  )
      ),
  tags$hr(),
  
  checkboxInput(inputId = "mult", label = strong("Multiple Regression")),
  conditionalPanel(
    condition = "input.mult == true",
  selectInput(inputId = "mult_reg_y",
              label = strong("Response variable"),
              choices = names(dataset)
  ),
  radioButtons(
    inputId="radio",
    label=strong("Predictor variable Selection Type:"),
    choices=list(
      "All",
      "Manual Select"
    ),
    selected="Manual Select"),
  
  conditionalPanel(
    condition = "input.radio != 'All'",
    checkboxGroupInput(
      "mult_reg_x", 
      "Choose Predictor variables",
      choices=names(dataset) 
      
    )
  ),
 actionButton("goButtonmod", strong("Update Model", style = "color:blue"))
  
  )
  )),

 
    mainPanel(
      
  
      # Show a summary of the dataset and an HTML table with the requested
      # number of observations
      
      tabsetPanel(
        tabPanel("Data", tableOutput("contents")),
        tabPanel("Plot", plotOutput("Plot", width = "1000px", height = "800px")),
        tabPanel("Summary",verbatimTextOutput("summary")),
        tabPanel("Simple Regresion",plotOutput("Regresion", width = "1000px", height = "800px"),
                 
                 conditionalPanel("input.mod_linear == true",p(strong("Linear model")), 
                                  verbatimTextOutput(outputId = "mod_linear_text")),
                 conditionalPanel("input.mod_rlm == true",p(strong("Robust Linear model")), 
                                  verbatimTextOutput(outputId = "mod_rlm_text")),
                 conditionalPanel("input.mod_quadratic == true",p(strong("Quadratic model")),
                                  verbatimTextOutput(outputId = "mod_quadratic_text")),
                 conditionalPanel("input.mod_loess == true",p(strong("LOESS model")),
                                  verbatimTextOutput(outputId = "mod_loess_text"))
                  ),
         tabPanel("Multiple Regression",verbatimTextOutput(outputId = "mult_reg_text"))
        
        
        )
      
    )
 
  
  ))
