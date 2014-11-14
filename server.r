
library(ggplot2)
library(MASS)
library(car)
library(coefplot)


shinyServer(function(input, output,session) {
  
  data <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    d <- read.csv(input$file1$datapath, header=input$header, sep=input$sep)    # read the file
    return(d)     
    
  })   
  
  output$contents <- renderTable({
  inFile <- input$file1
  
  if (is.null(inFile))
    return(NULL)
  
  read.csv(inFile$datapath, header=input$header, sep=input$sep)       #, quote=input$quote
 
  })  
  
 observe({
 df <- data()

 if (!is.null(df)) {
   updateSelectInput(session, 'attributes', choices = names(df))
   updateSelectInput(session, 'attributes.dens', choices = names(df))
   updateSelectInput(session, 'attributes.bar', choices = names(df))
   updateSelectInput(session, 'attributes.ident', choices = names(df))
   updateSelectInput(session, 'attributesx.box', choices =c(None='.', names(df)))
   updateSelectInput(session, 'attributesy.box', choices =c(None='.', names(df)))
   
   updateSelectInput(session, 'attributx', choices = names(df))
   updateSelectInput(session, 'attributy', choices = names(df))
   
   updateSelectInput(session, 'attrx', choices = names(df))
   updateSelectInput(session, 'attry', choices = names(df))
   
   updateSelectInput(session, 'sizeline', choices = c(None='.', names(df)))
   updateSelectInput(session, 'sizepoint', choices = c(None='.', names(df)))
   
   updateSelectInput(session, 'fil', choices = c(None='.', names(df)))
   updateSelectInput(session, 'fil.dens', choices = c(None='.', names(df)))
   updateSelectInput(session, 'filline', choices = c(None='.', names(df)))
   updateSelectInput(session, 'filpoint', choices = c(None='.', names(df)))
   updateSelectInput(session, 'filbar', choices = c(None='.', names(df)))
   updateSelectInput(session, 'filbox', choices = c(None='.', names(df)))
   
   updateSelectInput(session, 'facet_col', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_row', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_colline', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_rowline', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_rowpoint', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_colpoint', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_col.dens', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_row.dens', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_col.bar', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_row.bar', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_col.box', choices = c(None='.', names(df)))
   updateSelectInput(session, 'facet_row.box', choices = c(None='.', names(df)))
   
   updateSelectInput(session, 'reg_y', choices = c(None='.', names(df)))
   updateSelectInput(session, 'reg_x', choices = c(None='.', names(df)))
   updateCheckboxGroupInput(session, 'mult_reg_x', choices = c(names(df)))
   updateSelectInput(session, 'mult_reg_y', choices = c(None='.', names(df)))
   updateSelectInput(session, 'glm_y', choices = c(None='.', names(df)))
   updateCheckboxGroupInput(session, 'glm_x', choices = c(names(df)))
   updateRadioButtons(session, 'family', choices=c(Gaussian='gauss',
                                                         Poisson='pois',
                                                         Binomial='binom',
                                                   Negative.Binomial='neg.bin',
                                                   Gamma='gamma'))
   updateSelectInput(session, 'radio.plots', choices = c(None='.', Added_Variable_Plot="avplot",
                                                         Residual_Plot="resplot",
                                                         Marginal_Plot="margplot",
                                                         Partial_Residual_Plot="crplot"))
   updateSelectInput(session, 'radio.plots.glm', choices = c(None='.', Added_Variable_Plot="avplot",
                                                         Residual_Plot="resplot",
                                                         Marginal_Plot="margplot",
                                                         Partial_Residual_Plot="crplot",
                                                         Coefplot='coefplot'))
   updateSelectInput(session, 'help_page', choices = c(None='.', General_Help="gen.help",
                                                             Plotting_Help="plot.help",
                                                             Modelling_Help="mod.help"))
   
 }
}) 
 


output$Plot <- renderPlot({
  if (is.null(data()))
    return(NULL) 
  input$goButton
  input$goButton1
  input$goButton2
  input$goButton3
  input$goButton4
  input$goButton5
  input$goButton6
  
  if ( input$plot == "none" ) {
    return(NULL)
  }
  
  #plotting parameters
  isolate({
    
    if ( input$plot == "hist" ) {
      
      if(input$fil!="." ){
        aes_mapping <- aes_string(x = input$attributes,group=input$fil,fill=input$fil) }
      
     
      else{
        
        aes_mapping <- aes_string(x =input$attributes)  }
      
 
      #plot
      p <- ggplot(data(),mapping=aes_mapping ) +
        geom_histogram(binwidth=input$bin)
      
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
      
    }
    
  }) 
  
  isolate({
    
    if ( input$plot == "box" ) {
      
      if(input$filbox!="." ){
        aes_mapping <- aes_string(x = input$attributesx.box,y = input$attributesy.box,group=input$filbox,fill=input$filbox) }
      
      
      else{
        
        aes_mapping <- aes_string(x =input$attributesx.box,y = input$attributesy.box)  }
      
      
      #plot
      p <- ggplot(data(),mapping=aes_mapping ) +
        geom_boxplot()
      
      facets <- paste(input$facet_row.box, '~', input$facet_col.box)
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
      
    }
    
  })
  
  isolate({
    
    if ( input$plot == "bar" ) {
      
      if(input$filbar!="." ){
        if (input$identity == TRUE ) {
          aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident ,group=input$filbar,fill=input$filbar)}
          else {
          aes_mapping <- aes_string(x = input$attributes.bar,group=input$filbar,fill=input$filbar) }
      }
      else if(input$identity == TRUE){
        aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident)
      }
          else {
            
            aes_mapping <- aes_string(x =input$attributes.bar)  }
        
        
    
      
      
      #plot
      p <- ggplot(data(),mapping=aes_mapping )
     
     if (input$identity ==TRUE ){
       p<- p+geom_bar(stat='identity') }
      
      else{
        p<- p+ geom_bar()
      }
      
      facets <- paste(input$facet_row.bar, '~', input$facet_col.bar)
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
      
    }
    
  }) 
  
  
  isolate({
    
    if ( input$plot == "dens" ) {
      
      if(input$fil.dens!="." ){
        aes_mapping <- aes_string(x = input$attributes.dens,group=input$fil.dens,fill=input$fil.dens) } 
      
      
      else{
        
        aes_mapping <- aes_string(x =input$attributes.dens)  }
      
      
      #plot
      p <- ggplot(data(),mapping=aes_mapping ) +
        geom_density(alpha=.3)
      
      facets <- paste(input$facet_row.dens, '~', input$facet_col.dens)
      if (facets != '. ~ .')
        p <- p + facet_grid(facets,scales='free')  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
      
    }
    
  }) 
  
  isolate({
  
  if ( input$plot == "line" ) {
    
    if(input$filline!='.' ){
      if(input$sizeline!='.'){
        aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline,size=input$sizeline) }       
      else{
        aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline) }
    }
    
    else{
      aes_mapping <- aes_string(x =input$attributx,y=input$attributy)   }
    
    #plot
    p <- ggplot(data(),mapping=aes_mapping ) +
      geom_line(group=input$filline)              
    
    facets <- paste(input$facet_rowline, '~', input$facet_colline)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
    
  }
  })
  
  isolate({
  if ( input$plot == "point" ) {
    
    if(input$filpoint!='.' ){
      if(input$sizepoint!='.'){
        aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint,size=input$sizepoint) }     
      else{
        aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint) }
    }
    
    else{
      aes_mapping <- aes_string(x =input$attrx,y=input$attry)  }
    
    #plot
    p <- ggplot(data(),mapping=aes_mapping ) +
      geom_point()
    
    facets <- paste(input$facet_rowpoint, '~', input$facet_colpoint)
    if (facets != '. ~ .')
      p <- p + facet_grid(facets)  +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
    
  }   
  })
  
  print(p)
})

output$downloadPlot <- downloadHandler(
  filename = function() { paste(input$file1, '.png', sep='') },
  content = function(file) {
    device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
    ggsave(file,  device = device)
  }
)
  
  output$Regresion <- renderPlot({
   
    if (is.null(data()))
      return()
    
    # ------------------------------------------------------------------
    # Make the base plot
    
    # If any models are drawn, make the points less prominent
    if (input$mod_linear || input$mod_quadratic || input$mod_loess || input$mod_rlm)
      point_alpha <- 0.5
    else
      point_alpha <- 1
     
  
    # Base plot
    p <- ggplot(data(), mapping = aes_string(x =input$reg_x,y=input$reg_y)) + 
      geom_point(shape = 21, alpha = point_alpha)+
      scale_colour_hue(l = 40) +
      scale_shape(solid = FALSE) 
   
    # Add model lines
    
    if ( (input$mod_linear) & (input$stder) ){
      p <- p + geom_smooth(method = lm, se = TRUE, size = 0.75,
                           linetype = "twodash")
    }
   else if((input$mod_linear)){
     p <- p + geom_smooth(method = lm, se = FALSE, size = 0.75,
                          linetype = "twodash")
   }
   
   if ( (input$mod_rlm) & (input$stder.rlm) ){
     p <- p + geom_smooth(method = rlm, se = TRUE, size = 0.75,
                          linetype = "F1")
   }
   else if((input$mod_rlm)){
     p <- p + geom_smooth(method = rlm, se = FALSE, size = 0.75,
                          linetype = "F1")
   }
   
   
    if( (input$mod_quadratic) & (input$stder.quad) ) {
      p <- p + geom_smooth(method = lm, se = TRUE, formula = y ~ x + I(x^2),
                           size = .75, linetype = "dashed")
    }
   else if (input$mod_quadratic) {
     p <- p + geom_smooth(method = lm, se = FALSE, formula = y ~ x + I(x^2),
                          size = .75, linetype = "dashed")
   }
   
   if( (input$mod_loess) & (input$stder.loess) ) {
     p <- p +  geom_smooth(method = loess, se = TRUE, linetype = "solid",
                           span = input$mod_loess_span)
   }
   else if (input$mod_loess) {
      p <- p + geom_smooth(method = loess, se = FALSE, linetype = "solid",
                           span = input$mod_loess_span)
    }
    
    print(p)
  })

output$Mult_Regresion <- renderPlot({
  
  
  input$goButtonmod
  if(input$goButtonmod > 0){
    
    isolate({
      if(input$radio == "All"){
        formula <- paste(input$mult_reg_y, "~.")
        
      }
      
      else{
        
        formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
      }
    })
    
  }
  
  if(input$radio.plots == "avplot"){
    avPlots(lm(formula,data=data()))
  }
  if(input$radio.plots == "resplot"){
    residualPlots(lm(formula,data=data()))
  }
  if(input$radio.plots == "margplot"){
    marginalModelPlots(lm(formula,data=data()))
  }
  if(input$radio.plots == "crplot"){
    crPlots(lm(formula,data=data()))
  }
  
})

output$GLM_Regresion <- renderPlot({
  
  
  input$goButtonglm
  if(input$goButtonglm > 0){
    
    isolate({
      if(input$radioglm == "All"){
        formula <- paste(input$glm_y, "~.")
        
      }
      
      else{
        
        formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))
      }
    })
    
  }
  
  if(input$radio.plots.glm == "avplot"){
    avPlots(lm(formula,data=data()))
  }
  if(input$radio.plots.glm == "resplot"){
    residualPlots(lm(formula,data=data()))
  }
  if(input$radio.plots.glm == "margplot"){
    marginalModelPlots(lm(formula,data=data()))
  }
  if(input$radio.plots.glm == "crplot"){
    crPlots(lm(formula,data=data()))
  }
  if(input$radio.plots.glm == "coefplot"){
    coefplot(lm(formula,data=data()))
  }
  
})

  # ------------------------------------------------------------------
 # Generate a summary of the dataset
   output$summary <- renderPrint({
    summary(data())
})

# Show the first "n" observations
   output$contents <- renderTable({
    head(data(), n = input$obs)
})
  
  # ------------------------------------------------------------------
  # Functions for creating models and printing summaries
  
  make_model <- function(model_type, formula, family=NULL,...) {
    
    # In order to get the output to print the formula in a nice way, we'll
    # use do.call here with some quoting.
   if(!input$glm){
     do.call(model_type, args = list(formula = formula,data = quote(data()),  ...)) }
    
    else if(input$family=='pois'){
      do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(poisson),  ...))
    }
    else if(input$family=='binom'){
      do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(binomial),  ...))
    }
    else if ( (input$family=='gamma') ){
      do.call(model_type, args = list(formula = formula,data = quote(data()[,input$glm_y >0]),family=quote(Gamma),subset=input$glm_y >0,  ...))
    }

    else{
      do.call(model_type, args = list(formula = formula,data = quote(data()),  ...))
    }
    
  }


  output$mod_linear_text <- renderPrint({
   
    formula <- paste(input$reg_y, "~", input$reg_x)

    summary(make_model("lm", formula))
   
  })

  output$mod_rlm_text <- renderPrint({
    
    formula<-formula(paste(input$reg_y, '~',paste(input$reg_x)))
  
    summary(make_model("rlm", formula))
})
  
  output$mod_quadratic_text <- renderPrint({
    
    formula <- paste(input$reg_y, " ~ ", "I(", input$reg_x, "^2) + ",
                     input$reg_x, sep = "")
    
    summary(make_model("lm", formula))
  })
  
  output$mod_loess_text <- renderPrint({
    
    formula <- paste(input$reg_y, "~", input$reg_x)
    
    summary(make_model("loess", formula, span = input$mod_loess_span))
  })

  output$mult_reg_text <- renderPrint({ 
 
  input$goButtonmod
  
  if(input$goButtonmod > 0){
    
    isolate({
      
      if(input$radio == "All"){
      
      if( !input$interac_mult ){
        formula <- paste(input$mult_reg_y, "~.")
        summary(make_model("lm", formula)) }
      
      else if( input$interac_mult ){
        formula <- paste(input$mult_reg_y, "~.*.")
        summary(make_model("lm", formula))
      }
      
    } 
    else{
      
      if( input$radio != "All"){
        
        if( !input$interac_mult ){
          formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
          summary(make_model("lm", formula)) }
        
        else if( input$interac_mult ){
      formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='*'))
      summary(make_model("lm", formula))
        }
      }
    }
    
    
    })
  
} 
else{
  return(cat('Must choose one or more predictor variables and hit Update Model \n'))
}


})

  output$glm_text <- renderPrint({ 
  
   input$goButtonglm
   
   if(input$goButtonglm > 0){
    
    isolate({
      
        if( input$radioglm != "All"){
        
        if( (input$family != 'neg.bin') & (!input$interac) ){
          formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+')) 
          summary(make_model("glm", formula)) }
        else if( (input$family == 'neg.bin') & (!input$interac) ){
          formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+')) 
          summary(make_model("glm.nb", formula))
        }
        else if( (input$family == 'neg.bin') & (input$interac) ){
          formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='*')) 
          summary(make_model("glm.nb", formula))
        }
        else if( (input$family != 'neg.bin') & (input$interac) ){
          formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='*')) 
          summary(make_model("glm", formula)) }
        
} 
else{
   
      if( input$radioglm == "All"){
  
       if( (input$family != 'neg.bin') & (!input$interac) ){
         formula <- paste(input$glm_y, "~.")
         summary(make_model("glm", formula)) }
         
       else if( (input$family == 'neg.bin') & (!input$interac) ){
         formula <- paste(input$glm_y, "~.")
         summary(make_model("glm.nb", formula))
         }
      
       else if( (input$family == 'neg.bin') & (input$interac) ){
         formula <- paste(input$glm_y, "~.*.") 
         summary(make_model("glm.nb", formula))
        }
       else  if( (input$family != 'neg.bin') & (input$interac) ){
         formula <- paste(input$glm_y, "~.*.")
         summary(make_model("glm", formula)) }
  
   }
}

    })
    
  } else{
    return(cat('Must choose one or more predictor variables and hit Update Model \n'))
  }
  
  
})

output$help_text <- renderText({ 
  
if(input$help_page=='gen.help'){
  
'Data Visualization and Modelling application using an uploaded csv.

 Part of the data and data summary is shown immediately after the uploaded data set(by clicking on the specified tabs).
 The user is able to choose the number of rows to appear as well as the csv format and if header will appear or not.
 The user is able to choose between plotting and modelling by "clicking" the boxes.' 
}

 else if(input$help_page=='plot.help'){

 
 'Plotting consists of histogram,boxplot, barplot, density plot, line plot and point plot with the associated colour, size and faceting capabilities.
 For example when histogram is chosen , x axis is requested and then there are options for filling and faceting. The user chooses the 
 appropriate data columns.
 Barplot has one more option that of "identity", when the user "click" the box,the heights of the bars represent the specific y-value in the data, 
 and map a value to the y aesthetic
 The same exist with line and point plots with size option(the size of point or line) as an addition.'
}

 else if(input$help_page=='mod.help'){

 'Modelling consists of Simple regression, Multiple regression and Generalized Linear Model(GLM). By "clicking" the respective box
 more options are available. 

 Simple Regression consists of one response and one dependent variable in the form "y~x" and the user chooses the appropriate variables.
 A plot of the selected raw columns appeared. By choosing one or more of the following boxes(Linear, Robust Linear etc.) the app presents the 
 respective model summary and the smooth model line on the plot. Std.Error depicts the 95% confidence interval.
 
 Multiple regression builds a linear model of the form y~x1+x2+...+xk by choosing manual the number of predictor variables or choosing all variables.
 In Addition there are options of plotting some diagnostic plots. These are:
 Added variable plot: Attempts to show the effect of adding another variable to a model already having one or more independent variables.
 Residual plot: Plots the residuals versus each term in a mean function and versus fitted values. Also computes a curvature test for each of the plots 
 by adding a quadratic term and testing the quadratic to be zero.
 Marginal model plot:  plots the response on the vertical axis versus a linear combination of terms in the mean function on the horizontal axis. 
 Added to the plot are a loess smooth for the graph, along with a loess smooth from the plot of the fitted values.
 Partial residual plot: Attempts to show the relationship between the given independent variable and the response variables given that the other 
 independent variables are also in the model.
 
 GLM builds a generalized linear model, again by choosing manual the number of predictor variables or choosing all variables, in addition to 
 family selection of the error term. 
 There are the same diagnostic plots as in multiple regression plus a coefplot, that is the coefficients and standard errors from a fitted model.
 
 For Multiple regression and GLMs there is an option(called Predictor"s Interactions) to add interactions on the chosen predictors.
 ' 
}           
  
})




  
})
