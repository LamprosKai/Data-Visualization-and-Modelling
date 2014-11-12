 library(shiny)
 
 dataset <- list('Upload a file'=c(1))
 
shinyUI(pageWithSidebar(
 
  headerPanel("Data Visualization and Modelling"),
 
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
    
    numericInput(inputId ="obs",label = "Number of observations to view:", 100),
    
    checkboxInput(inputId ="plotting", label ="Plotting",
                  value = FALSE),  
    
    conditionalPanel(  
      condition = "input.plotting == true" ,
      
      
      selectInput(inputId ="plot",label=strong("Plot Type"),
                  choices=list("None"="none","Histogram"="hist", "Barplot"="bar", "Density Chart"="dens",
                                                        "Linechart"="line","Pointchart"="point")),
      
      conditionalPanel(
        condition = "input.plot == 'none'"
        
      ) ,
      
      conditionalPanel(
        condition = "input.plot == 'hist'",
        
        selectInput(inputId = "attributes",
                    label = "ATTRIBUTES",
                    choices=names(dataset)),
        
        
        sliderInput(inputId ="bin",label="Binwidth",
                    min=0,max=200,value=1,step=0.1),
        
        actionButton(inputId ="goButton1", label =strong("Update View", style = "color:blue")),
        
        
        selectInput(inputId ="fil", label ="Fill",
                    choices=c('None'='.',names(dataset))),
        
        selectInput(inputId ='facet_row', label ='Facet Row', 
                    choices= c('None'='.',names(dataset))),
        selectInput(inputId ='facet_col', label ='Facet Column', 
                    choices= c('None'='.',names(dataset)))
        
      ),
      
      conditionalPanel(
        condition = "input.plot == 'bar'",
        
        selectInput(inputId = "attributes.bar",
                    label = "ATTRIBUTES",
                    choices=names(dataset)),
        
        
        checkboxInput(inputId ="identity", label ="Stat.Identity",
                      value = FALSE),  
        
        conditionalPanel(  
          condition = "input.identity == true" ,
        
          selectInput(inputId = "attributes.ident",
                      label = "ATTRIBUTES",
                      choices=names(dataset))
          
          ),
       
        
        actionButton(inputId ="goButton3", label =strong("Update View", style = "color:blue")),
        
        
        selectInput(inputId ="filbar", label ="Fill",
                    choices=c('None'='.',names(dataset))),
        
        selectInput(inputId ='facet_row.bar', label ='Facet Row', 
                    choices= c('None'='.',names(dataset))),
        selectInput(inputId ='facet_col.bar', label ='Facet Column', 
                    choices= c('None'='.',names(dataset)))
        
      ),
      
      conditionalPanel(
        condition = "input.plot == 'dens'",
        
        selectInput(inputId = "attributes.dens",
                    label = "ATTRIBUTES",
                    choices= names(dataset)),
        
        
        actionButton(inputId ="goButton2", label =strong("Update View", style = "color:blue")),
        
        
        selectInput(inputId ="fil.dens", label ="Fill",
                    choices= c('None'='.',names(dataset))),
        
        selectInput(inputId ='facet_row.dens', label ='Facet Row', 
                    choices= c('None'='.',names(dataset))),
        selectInput(inputId ='facet_col.dens', label ='Facet Column', 
                    choices= c('None'='.',names(dataset)))
        
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
        actionButton(inputId ="goButton4", label =strong("Update View", style = "color:blue")),
        
        selectInput(inputId ="filline", label ="Fill",
                    choices= c('None'='.',names(dataset))),
        
        selectInput(inputId ="sizeline", label ="Size",
                    choices= c('None'='.',names(dataset))),
        
        selectInput(inputId ='facet_rowline', label ='Facet Row', 
                    choices=c('None'='.',names(dataset))),
        selectInput(inputId ='facet_colline', label ='Facet Column', 
                    choices=c('None'='.',names(dataset)))
        
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
        actionButton(inputId ="goButton5", label =strong("Update View", style = "color:blue")),
        
        selectInput(inputId ="filpoint", label ="Fill",
                    choices=c('None'='.',names(dataset))),
        
        selectInput(inputId ="sizepoint", label ="Size",
                    choices=c('None'='.',names(dataset))),
        
        selectInput(inputId ='facet_rowpoint', label ='Facet Row', 
                    choices= c('None'='.',names(dataset))),
        selectInput(inputId ='facet_colpoint', label ='Facet Column', 
                    choices= c('None'='.',names(dataset)))
        
      )
     
    ),
    
    checkboxInput(inputId ="reg", label ="Modelling",
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
  
  
  checkboxInput(inputId = "mod_linear", label = "Linear (two-dash)"),
  conditionalPanel(
    condition = "input.mod_linear == true",
    checkboxInput(inputId ="stder",label = "Std Error")
  ), 
  
  checkboxInput(inputId = "mod_rlm", label = "Robust Linear (F1)"),
  conditionalPanel(
    condition = "input.mod_rlm == true",
    checkboxInput(inputId ="stder.rlm", label = "Std Error")
  ), 
  
  checkboxInput(inputId = "mod_quadratic", label = "Quadratic (dashed)"),
  conditionalPanel(
    condition = "input.mod_quadratic == true",
    checkboxInput(inputId ="stder.quad", label = "Std Error")
  ),
  
  checkboxInput(inputId = "mod_loess",  label = "Locally weighted LOESS (solid)"),
  conditionalPanel(
    condition = "input.mod_loess == true",
    checkboxInput(inputId ="stder.loess", label = "Std Error"),
 
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
      inputId ="mult_reg_x", 
      label = "Choose Predictor variables",
      choices=names(dataset) 
      
    )
  ),
  selectInput(
    inputId="radio.plots",
    label=strong("Diagnostic Plots"),
    choices=list(
      "avplot"="Added_Variable_Plot",
      "resplot"="Residual_Plot",
      "margplot"="Marginal_Plot",
      "crplot"="Partial_Residual_Plot"
    )),
  
 actionButton(inputId ="goButtonmod", label =strong("Update Model", style = "color:blue"))
  
  ),
 
 tags$hr(),
 
 checkboxInput(inputId = "glm", label = strong("GLM")),
 conditionalPanel(
   condition = "input.glm == true",
   selectInput(inputId = "glm_y",
               label = strong("Response variable"),
               choices = names(dataset)
   ),
   radioButtons(
     inputId="radioglm",
     label=strong("Predictor variable Selection Type:"),
     choices=list(
       "All",
       "Manual Select"
     ),
     selected="Manual Select"),
   
   conditionalPanel(
     condition = "input.radioglm != 'All'",
     checkboxGroupInput(
       inputId = "glm_x", 
       label="Choose Predictor variables",
       choices=names(dataset) 
       
     )
   ),
   radioButtons(
     inputId="family", 
     label= strong("Choose Family"),
     choices=c('Gaussian'='gauss',
               'Poisson'='pois',
               'Binomial'='binom',
               'Negative Binomial'='neg.bin',
               'Gamma'='gamma'),
     selected="gauss"
     
   ),
   selectInput(
     inputId="radio.plots.glm",
     label=strong("Diagnostic Plots"),
     choices=list(
       "avplot"="Added_Variable_Plot",
       "resplot"="Residual_Plot",
       "margplot"="Marginal_Plot",
       "crplot"="Partial_Residual_Plot"
     )),
   
   actionButton(inputId ="goButtonglm", label =strong("Update Model", style = "color:blue"))
   
 )
 
 
  )),

 
    mainPanel(
     
  
      # Show a summary of the dataset and an HTML table with the requested
      # number of observations
      
      tabsetPanel(
        tabPanel("Data", tableOutput("contents")),
        tabPanel("Plot", plotOutput("Plot", width = "1000px", height = "800px"),
                 downloadButton(outputId='downloadPlot', label= 'Download Plot')),
        tabPanel("Summary",verbatimTextOutput("summary")),
        tabPanel("Simple Regresion",plotOutput("Regresion", width = "1000px", height = "1000px"),
                 
                 conditionalPanel("input.mod_linear == true",p(strong("Linear model")), 
                                  verbatimTextOutput(outputId = "mod_linear_text")),
                 conditionalPanel("input.mod_rlm == true",p(strong("Robust Linear model")), 
                                  verbatimTextOutput(outputId = "mod_rlm_text")),
                 conditionalPanel("input.mod_quadratic == true",p(strong("Quadratic model")),
                                  verbatimTextOutput(outputId = "mod_quadratic_text")),
                 conditionalPanel("input.mod_loess == true",p(strong("LOESS model")),
                                  verbatimTextOutput(outputId = "mod_loess_text"))
                  ),
         tabPanel("Multiple Regression",plotOutput("Mult_Regresion", width = "1000px", height = "600px"),
                  verbatimTextOutput(outputId = "mult_reg_text")
                  ),
         tabPanel("GLM",plotOutput("GLM_Regresion", width = "1000px", height = "600px"),
                  verbatimTextOutput(outputId = "glm_text")),
         tabPanel("Help",verbatimTextOutput(outputId = "help_text"))
        
        
        )
      
      
    )
 
  
  ))
