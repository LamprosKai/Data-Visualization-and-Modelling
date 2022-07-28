#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

options(shiny.maxRequestSize = 100000*1024^2)
library(shiny)
library(shinythemes)
library(ggplot2)
library(MASS)
library(car)
library(magrittr)
library(randomForest)
library(e1071)
library(data.table)
library(party)
library(Cubist)
library(plyr)
library(nnet)
library(factoextra)
library(shinyjs)
library(NeuralNetTools)
library(shinyTree)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(shinyWidgets)
library(DataExplorer)
library(gridExtra)
library(promises)
library(future)
library(Amelia)
library(parallel)
library(reshape)
library(dplyr)
library(missForest)
library(doParallel)
library(fields)
library(caret)
library(GGally)
library(FactoMineR)


coef.plot <- function (df.coef) {
  library(ggplot2)
  
  old <- as.numeric(substr(packageVersion('ggplot2'),1,3))<2
  df.coef.temp <- df.coef
  df.coef.temp$SE <- ifelse(df.coef.temp$SE<=5,df.coef.temp$SE,ifelse(df.coef.temp$P <= 0.1, 5,NA))
  
  p  <- ggplot(df.coef.temp, aes(ESTIMATE, PREDICTOR, color = DESCRIPTION)) + geom_point(size = 2, alpha = 0.90)
  p1 <- p + facet_wrap(~DESCRIPTION, scales = 'free') + geom_errorbarh(aes(xmin = ESTIMATE - 1.96*SE, xmax = ESTIMATE + 1.96*SE)) +
    theme(legend.position = "none", strip.text.x = element_text(angle = 0)) 
  
  if (old) {
    p1 <- p1+ geom_vline(v = 0, linetype = 2)
  }else{
    p1 <- p1 + geom_vline(xintercept = 0, linetype = 2)
  }
  
  if(length(levels(df.coef.temp$PREDICTOR)) > 45) p1 <- p1 + theme(axis.text.y = element_text(size = 7))	
  return(print(p1))
}


model.coef <- function (model.in, tag = '', run.id = 666) {
  
  df.results <- as.data.frame(summary(model.in)$coefficients)
  
  df.results <- cbind(RUNID = run.id, DESCRIPTION = tag, PREDICTOR = rownames(df.results), df.results)
  
  names(df.results) <- c("RUNID", "DESCRIPTION", "PREDICTOR", "ESTIMATE", "SE", "Z", "P" )
  
  return(df.results)
  
}

glm.failure.distr <- function(model.obj, logscale = TRUE, newdata = NULL, response = NULL, ...) {
  # Logarithmic (base 10) plot of fitted and observed pipe failures
  # Currently only Poisson and negative binomial families
  # P. Jonkergouw 20 March 2008
  
 library(scales)
  m.family <- family(model.obj)$family
  if (!is.null(newdata) & !is.null(response)){
    y <- newdata[, response]
    y.fit <- predict(model.obj, newdata = newdata, type = "response")
  } else {
    y <- model.obj$y
    y.fit <- fitted(model.obj)
  }
  
  
  max.y <- max(y)
  p <- as.numeric(names(table(y)))
  factor.y <- factor(y, levels = p)
  table.y <- summary(factor.y, maxsum = max.y + 1)
  
  #r <- array(0, length(table.y))
  r <- 1:length(table(y))
  if (m.family == "poisson") { # Poisson
    for(i in 1:length(table(y))) {
      r[i] <- round(sum(dpois(p[i], y.fit)))
    }
  } else { # Negative binomial
    for(i in 1:length(table(y))) {
      r[i] <- round(sum(dnbinom(p[i], mu = y.fit, size = model.obj$theta)))
    }
  }
  
  # Deal with the zeros - add 1 to all values
  
  dat1 <- data.frame(count = p, value = r, src = 'Fitted')
  dat2 <- data.frame(count = p, value = table.y, src = 'Observed')
  
  dat <- rbind(dat1, dat2)
  dat <- within (dat, count <- factor(count))
  
  p1 <- ggplot(dat, aes(count, y = value + 1, fill = factor(src, levels = levels(src)[2:1]))) + geom_bar(position = 'dodge', stat = 'identity')
  p1 <- p1 + ylab('Frequency') + xlab('Failures')
  p1 <- p1 + theme(legend.position = c(1,1), legend.justification = c(1,1)) + scale_fill_discrete(guide = guide_legend(title = NULL))
  
  
  x.length.levels <- length(levels(dat[['count']]))
  if (x.length.levels > 25) { 
    leng <- seq(1, x.length.levels, by = 5)
    brk <- levels(dat[['count']])[c(leng, x.length.levels)]
    p1 <- p1 + scale_x_discrete(breaks = brk)	
  }
  
  if(logscale) p1 <- p1 + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x)))#scale_y_log10(10^unique(table.y)) #breaks = 10^p
  
  return(p1)
}

glm.scatter.plot <- function(m, newdata = NULL, response = NULL, ylab = "Fitted", xlab = "Observations", pred.type = 'response', ...) {
  
  if(!any(class(m) %in% 'glm')) stop('Bad input, please supply a \'glm\' model')
  if (!is.null(newdata) & !is.null(response)){
    y <- newdata[, response]
    m.fit <- predict(m, newdata = newdata, type = pred.type)
  } else {
    y <- m[['y']]
    m.fit <- predict(m, type = pred.type)
  }
  k.fit <- log(length(m.fit))
  
  limits <- c(min(y)*1.1, max(m.fit,y)*1.1)
  
  dat <- data.frame(y = y, m.fit = m.fit)
  p <- ggplot(data = dat, aes(y, m.fit)) + geom_jitter(alpha = 0.25) + ylim(limits[1], limits[2]) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.75, col = 'red') 
  p <- p + xlab(xlab) + ylab(ylab) + xlim(limits[1], limits[2]) #+ scale_x_continuous(breaks=0:limits[2])
  
  adj.r.sq <- round(1 - (m$deviance / (length(m[['y']]) - length(m$coeff)) / (m$null / m$df.null)),2)
  
  t.size <- 3
  p <- p + annotate('text', label = paste("Adj-R^2=", round(adj.r.sq, 2)), x = 0.85 * limits[2], y = limits[1], size = t.size) 
  p <- p + annotate('text', label = paste("AIC=", round(AIC(m), 0)), x = 0.85 * limits[2], y = limits[2], size = t.size)
  p <- p + annotate('text', label = paste("BIC=", round(AIC(m, k = k.fit), 0)), x = 0.85 *limits[2], y = 0.90 *limits[2], size = t.size)
  sum.obs <- sum(y)
  sum.pred <- sum(m.fit[m.fit < 2 * limits[2]]) # Ignores extreme outliers!
  sum.pred <- sum(m.fit)
  p <- p + annotate('text', label=paste("obs=", round(sum.obs)), x = 0.85 * limits[2], y = limits[2]/2, size = t.size)
  p <- p + annotate('text', label=paste("fit=", round(sum.pred)), x = 0.85 * limits[2], y = limits[2]/3, size = t.size)
  
  return(p)
}

glm.diag.plot <- function(m, newdata = NULL, response = NULL, logscale = TRUE, title = NULL) {
  #browser()
  
   library(ggplot2)
   library(gridExtra)
   library(grid)
  
  old <- as.numeric(substr(packageVersion('gridExtra'),1,3))<2
  options(digits = 5)
  m.family <- family(m)$family
  if (!is.null(newdata) & !is.null(response)) {
    m.fit <- predict(m, newdata = newdata, type = "response")
    y <- newdata[, response]
  } else {
    m.fit <- fitted(m)
    y <- m[['y']]
  }
  
  k.fit <- log(length(m.fit))
  adj.r.sq <- round(1 - (m$deviance / (length(m[['y']]) - length(m$coeff)) / (m$null/m$df.null)), 2)
  
  # if(family(m)$link == "log" & m.family == "gaussian") p1 <- ggplot(dat = data.frame(resp = log(y)), aes(resp)) + geom_histogram(binwidth = 0.2) + xlab('log(y)') + ggtitle("Histogram of Residuals")
  if(family(m)$link == "log" & m.family == "gaussian"){ 
    dat = data.frame(resp = log(y))
    bw <- 2*IQR(dat$resp)*nrow(dat)^(-1/3)
    #bw <- ifelse(bw < 1e-03, 0.01,bw)
    if(bw < 1e-03) bw <- NULL
    p1 <- ggplot(dat, aes(resp)) + geom_histogram(binwidth = bw) + xlab('log(y)') + ggtitle("Histogram of Residuals")
    }
  # Plot the number of observed and fitted failures on a log10 scale ...
  #if(substring(m.family, 0, 3) == "Neg" || m.family == "poisson") p1 <- ggplot(dat = data.frame(resp = y-m.fit), aes(resp)) + geom_histogram(binwidth = 0.2) + xlab('Residuals') + ggtitle("Histogram of Residuals") + scale_y_log10()  #+ xlim(-min(10, min(100, length(y)/20)),min(10, min(100, length(y)/20)))
  if(substring(m.family, 0, 3) == "Neg" || m.family == "poisson"){
    dat <- data.frame(resp = y-m.fit)
    bw <- 2*IQR(dat$resp)*nrow(dat)^(-1/3)
    #bw <- ifelse(bw < 1e-03, 0.01, bw)
    if(bw < 1e-03) bw <- NULL
    p1 <- ggplot(dat, aes(resp)) + geom_histogram(binwidth = bw) + xlab('Residuals') + ggtitle("Histogram of Residuals") + scale_y_log10()  #+ xlim(-min(10, min(100, length(y)/20)),min(10, min(100, length(y)/20)))
    p2 <- glm.failure.distr(m, newdata = newdata, logscale = logscale, response = response) + ggtitle('Distribution Fit')
  }
  # if(substring(m.family, 0, 3) == "Neg" || m.fami
  #ly == "poisson") p2 <- glm.failure.distr(m, newdata = newdata, logscale = logscale, response = response) + ggtitle('Distribution Fit')
  # if(family(m)$family == "gaussian") print(anova(m, test = "F"))
  #else print(anova(m, test="Chisq"))
  
  #  if(substring(m.family,0,3) == "poi") print(summary(m)) else print(summary(m, correlation = FALSE))
  #  print(paste("Standard deviation of residuals=", sd(y - m.fit)))
  #  print(paste("BIC=", AIC(m, k = k.fit)))
  #  print(paste("Adjusted R-squared ", adj.r.sq, sep = ""))
  
  # Plot the modelled versus observed data with some stats ...
  
  p3 <- glm.scatter.plot(m, newdata = newdata, response = response) + ggtitle('Fitted vs Observed')
  
  # Plot the diagnostic graphs
  dat <- data.frame(y = y, m.fit = m.fit)
  dat <- within (dat, y <- factor(y))
  p4 <- ggplot(dat, aes(y, m.fit)) + geom_boxplot(aes(group = y)) + xlab('Observations') + ylab('Fitted') + ggtitle('Boxplot of Fitted vs Observed')
  x.length.levels <- length(levels(dat[['y']]))
  if (x.length.levels > 25) { 
    leng <- seq(1, x.length.levels, by = 5)
    brk  <- levels(dat[['y']])[c(leng, x.length.levels)]
    p4    <- p4 + scale_x_discrete(breaks=brk)						
  }
  #boxplot(m.fit ~ y, xlab="Number of failures", ylab="Fitted failure rate")
  if(old) {
    if(family(m)$link == "log" & m.family == "gaussian"){
      grid.arrange(p1,p3,p4, nrow = 2, ncol=2) 
    }else{
      grid.arrange(p1,p2,p3,p4, nrow = 2, ncol=2) 
    }
  }
  if(!old) {
    if(family(m)$link == "log" & m.family == "gaussian"){
      grid.arrange(p1,p3,p4, nrow = 2, ncol=2) #p2,p3
    }else{
      grid.arrange(p1,p2,p3,p4, nrow = 2, ncol=2) #p2,p3, , top = textGrob(title, gp = gpar(fontsize = 18, cex = 1.5))
    }
   
  }
  
}

adms.f.mode <- function(vect){
  
  library(modeest)
  if(!is.factor(vect)) vect <- factor(vect) # convert data to factor if not already one
  num <- as.numeric(vect)
  l <- round(mlv(num, method = "discrete")$M) # estimate mode using numeric levels
  vect <- levels(vect)[l] # identify the mode
  
  return(vect)
}
###Imputation function
adms.stat.infill <- function(df.data, id.vars = NULL, noms.vars = NULL, sqrts.vars = NULL,
                             ts.vars = NULL, cs.vars = NULL, polytime.vars = NULL, splinetime.vars = NULL,
                             intercs.vars = FALSE, bounds.vars = NULL, from = NULL, to = NULL, lags.vars = NULL,
                             leads.vars = NULL, logs.vars = NULL, lgstc.vars = NULL, prior=NULL,plot = FALSE,widths=NULL,heights=NULL,
                             num_rows=1,method="Amelia",n_tree=5,max_iter=5,data_full,make_stats=FALSE,...) {
  
  
  #browser()
  # libs <- c("Amelia", "plyr")
  # adms.requiredPackages(libs = libs)
  # 
  
  list.of.packages <- c("Amelia", "parallel","gridExtra","reshape","plyr","data.table","ggplot2","missForest","doParallel","fields") #
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')
  lapply(list.of.packages,suppressPackageStartupMessages(require),character.only=TRUE)
  
  x <- sapply(df.data, class)
  x.f <- x == "factor"  #find factors
  d <- any(grep(paste0(c("POS","Date"),collapse = "|"), x))
  colClasses <- which(x %in% c("POSIXct" , "POSIXlt" , "Date"))
  if (d)
    df.data[, colClasses] <- (sapply(df.data[, colClasses], function(x) as.Date(x,
                                                                                format = "%Y-%m-%d")))/365 + 1970
  if (method=='Amelia') {
    options(warn=1)
    if (is.null(id.vars)) warning("All of the variables in the dataset will be included in the imputation algorithm!")
    if (identical(intersect(noms.vars,id.vars),character(0))) warning("You have not defined any factor/character variable.If exists in the dataset please consider if it has to be set as 'id.vars' or 'noms.vars'!")
    if (any(ts.vars == cs.vars))
      stop("Time series and cross-sectional variables cannot be the same")
    if (length(ts.vars) > 1 | length(cs.vars) > 1)
      stop("'ts.vars' and 'cs.vars' option cannot be longer than one integer/variable")
    if (any(polytime.vars > 3, polytime.vars < 0))
      stop("The number of polynomial terms to include must be between 1 and 3")
    if (!identical(polytime.vars, NULL)) {
      if (identical(ts.vars, NULL))
        stop("You have set polynomials of time without setting the 'ts.vars'")
    }
    if (any(splinetime.vars > 6, splinetime.vars < 0))
      stop("The number of spline degrees of freedom to include must be between 0 and 6")
    if (!identical(splinetime.vars, NULL)) {
      if (identical(ts.vars, NULL))
        stop("You have set splines of time without setting the 'ts.vars'")
    }
    if (identical(intercs.vars, TRUE)) {
      if (identical(cs.vars, NULL))
        stop("You have indicated an interaction with the cross section without setting the cross section variable")
      if (sum(is.na(df.data[, cs.vars])) > 0)
        stop("There are missing values in the 'cs.vars'")
    }
    if (!is.null(lags.vars)) {
      if (is.null(ts.vars))
        stop("You need to specify the 'ts.vars' in order to create lags")
    }
    if (!is.null(leads.vars)) {
      if (is.null(ts.vars))
        stop("You need to specify the 'ts.vars' in order to create leads")
    }
    if (length(bounds.vars) != 0 & (length(from) == 0 | length(to) == 0))
      stop("Please provide 'bounds.vars' and 'from' and 'to' ")
    options(warn=-1)
    
    if (any(length(bounds.vars) >= 1)) {
      bd <- matrix(NA, nrow = length(bounds.vars), ncol = 3)
      for (i in 1:length(bounds.vars)) {
        bd[i, ] <- c(which(names(df.data) == bounds.vars[i]), from[i], to[i])
      }
      bounds.vars <- bd
    }
    
    am.out <- amelia(df.data, m = 5, p2s = 0, idvars = id.vars, ts = ts.vars, cs = cs.vars,
                     polytime = polytime.vars, splinetime = splinetime.vars, intercs = intercs.vars,
                     lags = lags.vars, leads = leads.vars, logs = logs.vars, lgstc = lgstc.vars,
                     noms = noms.vars, sqrts = sqrts.vars, priors = prior, empri = 0.01 * nrow(df.data), bounds = bounds.vars,
                     parallel = "multicore", ncpus = parallel::detectCores())
    
    # various stop
    if (any(grep("One or more of the imputations", am.out$message)) | any(is.na(match(id.vars,colnames(df.data)))) | 
        any(is.na(match(noms.vars, colnames(df.data)))) |
        any(grep("You have a variable in your dataset that does not vary", am.out$message)) |
        any(grep("The time series and cross sectional variables cannot be transformed",am.out$message)) | 
        any(grep("The square root transformation", am.out$message)) |
        any(grep("One of the variable names in the options list", am.out$message)) |
        any(grep("The following variable", am.out$message)))
      stop()
    
    
    # extract numeric columns from each element of am.out$imputations
    num <- lapply(am.out$imputations, function(df) df[, sapply(df, is.numeric)])
    # sum them up and divide by length to get mean
    mean.find = Reduce("+", num)/length(num)
    n.f <- x.f[x.f==TRUE] # identify factors
    df.fact <- data.frame(id=1:nrow(df.data))
    #find factors that have NA's and exclude them from finding mode. Those factors have not been infilled by choice. keep them to be included in the final output
    n.f <- n.f[!names(n.f) %in% names(am.out$imputations$imp1[sapply(am.out$imputations$imp1, function(yy) any(is.na(yy)))])] 
    no_imp_fact <- df.data[which(sapply(am.out$imputations$imp1, function(yy) any(is.na(yy))) & sapply(am.out$imputations$imp1,is.factor))]
    use_mode <- intersect(names(n.f[!names(n.f) %in% id.vars]) , names(which(sapply(df.data, function(yy) any(is.na(yy))))))
    remain <- setdiff(names(n.f),use_mode)
    
    cl = makeCluster(detectCores())
    clusterExport(cl, c("adms.f.mode"),envir=environment())
    # for(i in names(n.f)) 
    
    rr <- lapply(use_mode, function(i) {
      fact <- lapply(am.out$imputations, function(df) df[i])  #llply(am.out$imputations, function(df) df[i]) # imputed data for each factor
      fact <- do.call(data.table,fact) #as.data.frame(fact)
      fact <- fact[,v1 := unlist(parApply(cl, .SD, 1, adms.f.mode))]
      fact <- fact[,v1]
      
    })
    stopCluster(cl)
    df.fact$id <- NULL
    names(rr) <- use_mode
    df.fact <- data.frame(rr,df.data[remain])
    #put factors that have not been imputed back to DF
    if (ncol(no_imp_fact)!=0) for(yy in 1:ncol(no_imp_fact)) {df.fact[names(no_imp_fact[yy])] <- no_imp_fact[yy]}
    f.d <- cbind(df.fact, mean.find)
    if (ncol(df.data) != ncol(f.d)) {
      diff_name <- setdiff(names(df.data),names(f.d))
      f.d$diff_name <- df.data[diff_name]
    }
    
  }else{ ##missForest imputation
    
    if (is.null(n_tree) | is.null(max_iter)) {
      stop("Please provide both 'n_tree' and 'max_iter' to continue with imputation")
    }
    
    registerDoParallel(cores=detectCores()-1)
    
    #find columns with more than 52 levels that missforest can't handle
    ind_52 <- which(sapply(df.data, function(x) length(levels(x)))>52 )
    #exclude from imputation all the id.vars that are not playing any role
    not_included <- union(ind_52,which(colnames(df.data) %in% id.vars))
    
    if(length(not_included)==0){
      miss_out <- missForest(df.data, maxiter = max_iter, ntree = n_tree ,parallelize =  'forests')
    }else{
      miss_out <- missForest(df.data[,-c(not_included)], maxiter = max_iter, ntree = n_tree ,parallelize =  'forests')
    }
    #final imputed data frame(with all variables)
    if(length(not_included)==0){
      f.d <- miss_out$ximp
    }else{
      f.d <- cbind(miss_out$ximp,df.data[not_included])
    }
    error <- miss_out$OOBerror
  }
  
  
  if (plot) {
    if (is.null(widths) | is.null(heights)) {
      stop("Please provide 'widths' and 'heights' to continue with plotting ")
    }
    
    # find variables to be plotted. only those that have been imputed.
    vars_plot <- intersect(names(f.d[!names(f.d) %in% id.vars]) , names(which(sapply(df.data, function(yy) any(is.na(yy))))))
    #keep the factors as they require other type of plot!
    vars_plot_f <- names(which(sapply(f.d[vars_plot],is.factor)))
    
    dd <- df.data[vars_plot]
    aa <- f.d[vars_plot]
    names(aa) <- paste0(names(aa),"_i")
    df_pl <- cbind(dd,aa)
    
    list_plot_f <- list()
    list_plot_n <- list()
    list_plot_freq <- list()
    
    for (n.names in vars_plot) {
      if (is.factor(f.d[[n.names]])) {
        
        n.names_AM <- paste0(n.names,"_FINAL_AM")
        df_pl[n.names]<-df.data[n.names]
        df_pl[n.names_AM] <- ifelse(is.na(df_pl[[n.names]]),as.character(df_pl[[paste0(n.names,"_i")]]),as.character(df_pl[[n.names]]))
        df_pl[n.names_AM] <- as.factor(df_pl[[n.names_AM]])
        
        #hist imp obs
        p<-ggplot(df_pl,aes_string(x=n.names_AM,fill=paste0("is.na(",n.names,")")))+geom_bar(aes(y = (..count..)/sum(..count..)),position = 'dodge')+ylab("density")+
          guides(fill = guide_legend(paste0(n.names,"_Infill")))+xlab(n.names)
        list_plot_f[[n.names]]<-p+theme(strip.text.y=element_text(size=8),strip.text.x=element_text(size=8),legend.text=element_text(size=8),legend.title=element_text(size=8),
                                        axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))
        
        
      }else{
        
        n.names_AM <- paste0(n.names,"_FINAL_AM")
        df_pl[n.names]<-df.data[n.names]
        df_pl[n.names_AM] <- ifelse(is.na(df_pl[[n.names]]),df_pl[[paste0(n.names,"_i")]],df_pl[[n.names]])
        
        p<-ggplot(df_pl,aes_string(x=n.names_AM,fill=paste0("is.na(",n.names,")")))+ geom_histogram(aes(y=..density..),position = 'dodge')+guides(fill = guide_legend(paste0(n.names,"_Infill")))
        list_plot_n[[n.names]]<-p+theme(strip.text.y=element_text(size=8),strip.text.x=element_text(size=8),axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.4),
                                        legend.text=element_text(size=8),legend.title=element_text(size=8),axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))+xlab(n.names)
        
        
        p_all <- ggplot(df_pl,aes_string(x=n.names_AM,colour=paste0("is.na(",n.names,")"),"stat(density)"))+geom_freqpoly(size=0.5)+guides(colour = guide_legend(paste0(n.names,"_Infill")))
        list_plot_freq[[n.names]]<-p_all+theme(strip.text.y=element_text(size=8),strip.text.x=element_text(size=8),legend.text=element_text(size=6),legend.title=element_text(size=6),
                                               axis.title.x=element_text(size=8),axis.title.y=element_text(size=8))+xlab(n.names)
        
        
      }
    }
    if (length(list_plot_f) !=0) {
      png(filename = "Diagnostic_Plots_fact.png", width = widths, height = heights, res = 150, units = "cm")
      do.call(grid.arrange, c(list_plot_f, list(nrow=num_rows)))
      dev.off()
    }
    if (length(list_plot_n) !=0) {
      png(filename = "Diagnostic_Plots_num.png", width = widths, height = heights, res = 150, units = "cm")
      do.call(grid.arrange, c(list_plot_n, list(nrow=num_rows)))
      dev.off()
      png(filename = "Diagnostic_Plots_freq.png", width = widths, height = heights, res = 150, units = "cm")
      do.call(grid.arrange, c(list_plot_freq, list(nrow=num_rows)))
      dev.off()
      
    }
    
  }
  
  if (make_stats) {
    if(missing(data_full)) stop("Please provide the full dataset 'data_full' to calculate summary statistics")
    
    f.d <- f.d[,names(df.data)]
    #variables that have been imputed
    not_included <- which(colnames(df.data) %in% id.vars)
    find_fact <- which(colnames(df.data) %in% noms.vars)
    #if ind_na is integer(0) means that there are no numerical variables with any's to be part of the summary stats so, the full summary will be returned
    ind_na <- unique(which(apply(df.data[,-c(not_included,find_fact)], 1, function(x){any(is.na(x))})))
    not_na <- which(apply(df.data[,], 2, function(x){!any(is.na(x))}))
    
    #check if there are missing rows/columns and do the summary stats accordingly. Also try to to have only the imputed columns in the summary stats
    if (length(ind_na) == 0) {
      if (length(not_na) == 0) {
        summary_na <- data.frame(t(stats(data_full[,])))[,-c(1,5,7,9)]
        summary_full <- data.frame(t(stats(f.d[,])))[,-c(1,5,7,9)]
        
      }else{
        summary_na <- data.frame(t(stats(data_full[,-which(names(data_full) %in% unique(c(names(data_full[colnames(data_full) %in% id.vars]),names(data_full[colnames(data_full) %in% noms.vars]),names(not_na)))),drop=FALSE])))[,-c(1,5,7,9)]
        summary_full <- data.frame(t(stats(f.d[,-which(names(f.d) %in% unique(c(names(f.d[colnames(f.d) %in% id.vars]),names(f.d[colnames(f.d) %in% noms.vars]),names(not_na)))),drop=FALSE])))[,-c(1,5,7,9)]
      }
      
    }else if (length(not_na) == 0) {
      summary_na <- data.frame(t(stats(data_full[ind_na,])))[,-c(1,5,7,9)]
      summary_full <- data.frame(t(stats(f.d[ind_na,])))[,-c(1,5,7,9)]
    }
    else{
      summary_na <- data.frame(t(stats(data_full[ind_na,-which(names(data_full) %in% unique(c(names(data_full[colnames(data_full) %in% id.vars]),names(data_full[colnames(data_full) %in% noms.vars]),names(not_na)))),drop=FALSE])))[,-c(1,5,7,9)]
      summary_full <- data.frame(t(stats(f.d[ind_na,-which(names(f.d) %in% unique(c(names(f.d[colnames(f.d) %in% id.vars]),names(f.d[colnames(f.d) %in% noms.vars]),names(not_na)))),drop=FALSE])))[,-c(1,5,7,9)]
    }
    
    
    if (method=="Amelia") {return(f.d)}
    return(f.d)  
    
  }else{
    if (method=="Amelia") {return(f.d)}
    return(f.d)
  }
}



dataset <- list('Upload a file'=c(1))
ui <- fluidPage(pageWithSidebar( #theme = shinytheme("journal"),
 
  
  headerPanel("Data Visualization and Modelling"),
  
  sidebarPanel(
    useShinyjs(),
    shinythemes::themeSelector(),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    
    numericInput(inputId ="obs",label = "Number of observations to view:", 100),
    
    checkboxInput(inputId ="plotting", label ="Plotting",
                  value = FALSE),  
    
    conditionalPanel(
      condition = "input.plotting == true" ,


      selectInput(inputId ="plot",label=strong("Plot Type"),
                  choices=list("None"="none","Histogram"="hist", "Boxplot"="box", "Barplot"="bar", "Density Plot"="dens",
                               "Linechart"="line","Pointchart"="point",
                               "Correlation"="correlation",
                               "Pairs Plot"="pairs",
                               "Missing Values"="missingness")),

      conditionalPanel(
        condition = "input.plot == 'none'"

      ) ,

      conditionalPanel(
        condition = "input.plot == 'hist'",

        selectInput(inputId = "attributes",
                    label = "ATTRIBUTES",
                    choices=names(dataset)),
        
        conditionalPanel(
          condition = "input.attributes == '.'",
          textInput("vars_trans", h5("Specify the transformed variable of x axis"), "")
     
        ),
        conditionalPanel(
          condition = "input.vars_trans != ''",
          checkboxInput(
            inputId ="vars_trans_x_log_trans",
            label = "Log transformation of x axis")
          
        ),
        
        
        conditionalPanel(
          condition = "input.attributes != '.'",
          checkboxInput(
            inputId ="density_hist",
            label = "Plot density"),
          
          checkboxInput(
            inputId ="x_log_trans",
            label = "Log transformation of x axis"),
          
          uiOutput("log_label_hist")
        ),
   
        sliderInput(inputId ="bin",label="Binwidth",
                    min=0,max=200,value=1,step=0.1),


        selectInput(inputId ="fil", label ="Fill",
                    choices=c('None'='.',names(dataset))),
        
        checkboxInput(
          inputId ="dodge_hist",
          label = "Position Dodge"),

        selectInput(inputId ='facet_row', label ='Facet Row',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_row != '.'",
          uiOutput("facet_strip_y_hist")
        ),
        selectInput(inputId ='facet_col', label ='Facet Column',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_col != '.'",
          uiOutput("facet_strip_x_hist")
        ),
        
        actionButton(inputId ="goButton1", label =strong("Update View", style = "color:red"))

      ),

      conditionalPanel(
        condition = "input.plot == 'box'",

        selectInput(inputId = "attributesx_box",
                    label = "X",
                    choices=names(dataset),selected='None'),

        selectInput(inputId = "attributesy_box",
                    label = "Y",
                    choices = names(dataset),selected='None'),
        
        conditionalPanel(
          condition = "input.attributesy_box != '.'",
          checkboxInput(
            inputId ="y_log_trans_box",
            label = "Log transformation of y axis"),
          
          uiOutput("log_label_box")
        ),
   
        selectInput(inputId ="filbox", label ="Fill",
                    choices=c('None'='.',names(dataset))),

        selectInput(inputId ='facet_row_box', label ='Facet Row',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_row_box != '.'",
          uiOutput("facet_strip_y_box")
        ),
        
        selectInput(inputId ='facet_col_box', label ='Facet Column',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_col_box != '.'",
          uiOutput("facet_strip_x_box")
        ),
        
        actionButton(inputId ="goButton6", label =strong("Update View", style = "color:red"))

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

        selectInput(inputId ="filbar", label ="Fill",
                    choices=c('None'='.',names(dataset))),
        
        checkboxInput(
          inputId ="dodge_bar",
          label = "Position Dodge"),

        selectInput(inputId ='facet_row_bar', label ='Facet Row',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_row_bar != '.'",
          uiOutput("facet_strip_y_bar")
        ),
        
        selectInput(inputId ='facet_col_bar', label ='Facet Column',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_col_bar != '.'",
          uiOutput("facet_strip_x_bar")
        ),
        
        actionButton(inputId ="goButton3", label =strong("Update View", style = "color:red"))

      ),

      conditionalPanel(
        condition = "input.plot == 'dens'",

        selectInput(inputId = "attributes_dens",
                    label = "ATTRIBUTES",
                    choices= names(dataset)),
        
        conditionalPanel(
          condition = "input.attributes_dens != '.'",
          checkboxInput(
            inputId ="x_log_trans_dens",
            label = "Log transformation of x axis"),
          
          uiOutput("log_label_dens")
        ),
        

        selectInput(inputId ="fil.dens", label ="Fill",
                    choices= c('None'='.',names(dataset))),

        selectInput(inputId ='facet_row_dens', label ='Facet Row',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_row_dens != '.'",
          uiOutput("facet_strip_y_dens")
        ),
        
        selectInput(inputId ='facet_col_dens', label ='Facet Column',
                    choices= c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_col_dens != '.'",
          uiOutput("facet_strip_x_dens")
        ),
        
        actionButton(inputId ="goButton2", label =strong("Update View", style = "color:red"))

      ),


      conditionalPanel(
        condition = "input.plot == 'line'",
        selectInput(
          inputId = "attributx",
          label = "X",
          choices = names(dataset)
        ),
        
        conditionalPanel(
          condition = "input.attributx != '.'",
          checkboxInput(
            inputId ="x_log_trans_line",
            label = "Log transformation of x axis"),
          uiOutput("x_log_label_line")
        ),
      
        selectInput(
          inputId = "attributy",
          label = "Y",
          choices = names(dataset)
        ),
        conditionalPanel(
          condition = "input.attributy != '.'",
          checkboxInput(
            inputId ="y_log_trans_line",
            label = "Log transformation of y axis"),
          uiOutput("y_log_label_line")
        ),
       

        selectInput(inputId ="filline", label ="Fill",
                    choices= c('None'='.',names(dataset))),

        selectInput(inputId ="sizeline", label ="Size",
                    choices= c('None'='.',names(dataset))),

        selectInput(inputId ='facet_rowline', label ='Facet Row',
                    choices=c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_rowline != '.'",
          uiOutput("facet_strip_y_line")
        ),
        
        selectInput(inputId ='facet_colline', label ='Facet Column',
                    choices=c('None'='.',names(dataset))),
        conditionalPanel(
          condition = "input.facet_colline != '.'",
          uiOutput("facet_strip_x_line")
        ),
        
        actionButton(inputId ="goButton4", label =strong("Update View", style = "color:red"))

      ),

      conditionalPanel(
        condition = "input.plot == 'point'" ,
        selectInput(
          inputId = "attrx",
          label = "X",
          choices = names(dataset)
        ),
        conditionalPanel(
          condition = "input.attrx != '.'",
          uiOutput("point_x_trans"),
          uiOutput("point_x_trans_label")
        ),
      
       
        selectInput(
          inputId = "attry",
          label = "Y",
          choices = names(dataset)
        ),
        conditionalPanel(
          condition = "input.attry != '.'",
          uiOutput("point_y_trans"),
          uiOutput("point_y_trans_label")
        ),
       
       
        
        selectInput(
          inputId="radio_point",
          label=strong("Smooth"),
          choices=list("auto"="Auto",
            "lm"="Lm",
            "glm"="Glm",
            "gam"="Gam",
            "loess"="Loess"
          )),
        
        conditionalPanel(
          condition = "input.radio_point != '.'",
        uiOutput("point_smooth_group"),
        uiOutput("smooth_method_args")
        
        ),
        

        selectInput(inputId ="filpoint", label ="Fill",
                    choices=c('None'='.',names(dataset))),

        selectInput(inputId ="sizepoint", label ="Size",
                    choices=c('None'='.',names(dataset))),

        selectInput(inputId ='facet_rowpoint', label ='Facet Row',
                    choices= c('None'='.',names(dataset))),
        
        conditionalPanel(
          condition = "input.facet_rowpoint != '.'",
        uiOutput("facet_strip_y_point")
        ),
        
        selectInput(inputId ='facet_colpoint', label ='Facet Column',
                    choices= c('None'='.',names(dataset))),
        
        conditionalPanel(
          condition = "input.facet_colpoint != '.'",
          uiOutput("facet_strip_x_point")
        ),
        
        actionButton(inputId ="goButton5", label =strong("Update View", style = "color:red"))

      ),
      
      conditionalPanel(
        condition = "input.plot == 'correlation'" ,
        
        checkboxInput(
          inputId ="corr_val",
          label = "Correlation Values")

      ),
      
      conditionalPanel(
        condition = "input.plot == 'pairs'" ,
        selectizeInput(
          inputId ="pairs_var",
          label = "Choose variables",
          choices=names(dataset),multiple=TRUE
        ),
       #uiOutput("choose_pairs"),
       
       actionButton(inputId ="goButtonpairs", label =strong("Update View", style = "color:red"))

      ),
      
      conditionalPanel(
        condition = "input.plot == 'missingness'" 
      )
    ),
    
     checkboxInput(inputId ="reg", label ="Modelling"),
    
     tags$hr(),
    
     conditionalPanel(
       condition = "input.reg == true" ,
    
       #Data imputation choice
       helpText("Perform Data Imputation before modelling. May take some time!"),
       checkboxInput(inputId ="imput", label ="Data Imputation"
                     ),
       
       conditionalPanel(
         condition = "input.imput == true" ,
         
         selectInput(
           inputId ="imput_method",
           label = "Select Imputation Method", choices=c("None", "Amelia","missForest"),selected = "None"),
         conditionalPanel(
           condition = "input.imput_method == 'Amelia'",
           
           uiOutput("makeamelia"),
           uiOutput("makeameliafact")
           
         ),
         actionButton(inputId ="goButtonimput", label =strong("Run Imputation", style = "color:red"))
         
       ),
       
       conditionalPanel(
         condition = "input.reg == 'none'"
    
       ) ,
    
    
       checkboxInput(inputId = "mult", label = strong("Linear Regression")),
       conditionalPanel(
         condition = "input.mult == true",
         
         selectInput(inputId = "mult_reg_y",
                     label = strong("Response variable"),
                     choices = names(dataset) 
         ),
         conditionalPanel(
           condition = "input.mult_reg_y != '.'",
           checkboxInput(
             inputId ="log_y",
             label = "Log tranformation")
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
           selectizeInput(
             inputId ="mult_reg_x",
             label = "Choose Predictor variables",
             choices=names(dataset),multiple=TRUE

            ),
           # selectizeInput(
           #   inputId ="log_x",
           #   label = "Log tranformation of the predictors", choices=names(dataset),multiple=TRUE),
           uiOutput("trans_mod_mul")
         ),
         

         uiOutput("makeFactInteract_mul"),
         uiOutput("makeNumInteract_mul"),
         uiOutput("uiAdded_mul"),
         

         selectInput(
           inputId="radio.plots",
           label=strong("Diagnostic Plots"),
           choices=list(
             "Added_Variable_Plot"="avplot",
             "Residual_Fitted_Plot"="resplot",
             "Marginal_Plot"="margplot",
             "Partial_Residual_Plot"="crplot",
             "Coefficient_Plot"="coefplot_ml"
           )),
    
         actionButton(inputId ="goButtonmod", label =strong("Update", style = "color:red"))
    
       ),
    
       checkboxInput(inputId = "glm", label = strong("GLM")),
       conditionalPanel(
         condition = "input.glm == true",
         selectInput(inputId = "glm_y",
                     label = strong("Response variable"),
                     choices = names(dataset)
         ),
         conditionalPanel(
           condition = "input.glm_y != '.'",
           checkboxInput(
             inputId ="glm_log_y",
             label = "Log tranformation")
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
           selectizeInput(
             inputId = "glm_x",
             label="Choose Predictor variables",
             choices=names(dataset),multiple=TRUE),
           
           # selectizeInput(
           #   inputId ="log_x_glm",
           #   label = "Log tranformation of the predictors", choices=names(dataset),multiple=TRUE),
           
           uiOutput("trans_mod")
           
         ),
         
         uiOutput("sub_mod"),
         
         radioButtons(
           inputId="family",
           label= strong("Choose Family"),
           choices=c('Gaussian'='gauss',
                     'Poisson'='pois',
                     'Binomial'='binom',
                     'Negative Binomial'='neg.bin',
                     'Gamma'='gamma'),
           selected = character(0)
    
         ),
 
         conditionalPanel(
           condition = "input.family == 'gauss'",
           uiOutput("make_gauss_link")
         ),
         conditionalPanel(
           condition = "input.family == 'pois'",
           uiOutput("make_pois_link")
         ),
         conditionalPanel(
           condition = "input.family == 'binom'",
           uiOutput("make_binom_link")
         ),
         conditionalPanel(
           condition = "input.family == 'gamma'",
           uiOutput("make_gamma_link")
         ),
         
         
         
         radioButtons(
           inputId="offset_radio",
           label=strong("Include Offset"),
           choices=list(
             "Yes",
             "No"
           ),
           selected="No"),
         conditionalPanel(
           condition = "input.offset_radio != 'No'",
           selectizeInput(
             inputId ="glm_x_offset",
             label = "Choose Offset variable",
             choices=names(dataset),multiple=TRUE)
           ),
         
         uiOutput("make_weights"),
         
         uiOutput("makeFactInteract"),
         uiOutput("makeNumInteract"),
         uiOutput("uiAdded"),
         
         selectInput(
           inputId="radio.plots.glm",
           label=strong("Diagnostic Plots"),
           choices=list(
             "Added_Variable_Plot"="avplot",
            "Residual_Fitted_Plot"="resplot",
             "Partial_Residual_Plot"="crplot",
            "Coefficient_Plot"="coefplot"
           )),
    
         actionButton(inputId ="goButtonglm", label =strong("Update", style = "color:red"))
    
     ),
     
     checkboxInput(inputId = "mlclass", label = strong("ML")),
     conditionalPanel(
          condition = "input.mlclass == true",
          checkboxGroupInput(inputId = "ml_class_reg",
                             label = "Choose Method"),
     
     # start classification
     checkboxInput(inputId ="ml_class", label ="Supervised Methods",
                   value = FALSE),
     
     conditionalPanel(  
       condition = "input.ml_class == true" ,
       
      
       radioButtons(inputId ="radio_ml_class", label =strong("Methods"),
                    choices= c("Random Forests" = "rf",
                               "Decision Trees" = "tree", "Support Vector Machines"="svm",
                               "Cubist"='cubist',"Neural Networks"="nnet"),selected=""),
       
       #for fandom Forests
       conditionalPanel(
         condition = "input.radio_ml_class == 'rf'"  ,
         selectInput(inputId='rf_y',
                     label=strong('Response Variable'),
                     choices=names(dataset)),
         
         radioButtons(
           inputId="radio_ml",
           label=strong("Predictor variable Selection Type:"),
           choices=list(
             "All",
             "Manual Select"
           ),
           selected="Manual Select"),
         
         conditionalPanel(
           condition = "input.radio_ml != 'All'",
           selectizeInput(
             inputId ="rf_x", 
             label = strong("Choose Predictor variables"),
             choices=names(dataset),multiple=TRUE)
         ),
         uiOutput("slider"),
         
         radioButtons(inputId='plot_rf_var',
                      label=strong('Choose Plot'),
                      choices=list(
                        "Random Forest Plot"="rfplot",
                        "Variance Importance Plot"="varimpplot"
                      ),
                      selected="rfplot"),
         
         actionButton(inputId ="goButtonrf", label =strong("Update", style = "color:red"))
       ),
       
       #for trees
       conditionalPanel(
         condition = "input.radio_ml_class == 'tree'"  ,
         selectInput(inputId='tree_y',
                     label=strong('Response Variable'),
                     choices=names(dataset)),
         
         radioButtons(
           inputId="radiotree",
           label=strong("Predictor variable Selection Type:"),
           choices=list(
             "All",
             "Manual Select"
           ),
           selected="Manual Select"),
         
         conditionalPanel(
           condition = "input.radiotree != 'All'",
           selectizeInput(
             inputId ="tree_x", 
             label = strong("Choose Predictor variables"),
             choices=names(dataset),multiple=TRUE)
         ),
         
         uiOutput("treeslider"),
         actionButton(inputId ="goButtontree", label =strong("Update", style = "color:red"))
       ),
       
       #for svm
       conditionalPanel(
         condition = "input.radio_ml_class == 'svm'"  ,
         selectInput(inputId='svm_y',
                     label=strong('Response Variable'),
                     choices=names(dataset)),
         
         radioButtons(
           inputId="radiosvm",
           label=strong("Predictor variable Selection Type:"),
           choices=list(
             "All",
             "Manual Select"
           ),
           selected="Manual Select"),
         
         conditionalPanel(
           condition = "input.radiosvm != 'All'",
           selectizeInput(
             inputId ="svm_x", 
             label = strong("Choose Predictor variables"),
             choices=names(dataset),multiple=TRUE)
         ),
         
         radioButtons(
           inputId="radiosvmkernel",
           label=strong("Choose SVM kernel:"),
           choices=list(
             "Linear",
             "Polynomial",
             "Radial basis",
             "Sigmoid"
           ),
           selected="Linear"),
         helpText("Scatter plot of the input data of a svm fit for classification models"),

         selectInput(inputId='svm_y_plot',
                     label=strong('Y Variable for Plotting'),
                     choices=names(dataset)),
         selectInput(inputId='svm_x_plot',
                     label=strong('X Variable for Plotting'),
                     choices=names(dataset)),

         actionButton(inputId ="goButtonsvm", label =strong("Update", style = "color:red"))
       ),
       
       #for Cubist
       conditionalPanel(
         condition = "input.radio_ml_class == 'cubist'"  ,
         selectInput(inputId='cubist_y',
                     label=strong('Response Variable'),
                     choices=names(dataset)),
         
         radioButtons(
           inputId="radiocubist",
           label=strong("Predictor variable Selection Type:"),
           choices=list(
             "All",
             "Manual Select"
           ),
           selected="Manual Select"),
         
         conditionalPanel(
           condition = "input.radiocubist != 'All'",
           selectizeInput(
             inputId ="cubist_x", 
             label = strong("Choose Predictor variables"),
             choices=names(dataset),multiple=TRUE)
         ),
         uiOutput("cubistslider"),
         
         helpText("Dotplot on model results between 'splits' and 'coeffs"),
         radioButtons(inputId='plot_cubist_var',
                      label=strong('Choose Plot'),
                      choices=list(
                        "Splits"="splits",
                        "Coeffs"="coeffs",
                        "Tuned CV"="cv"
                      ),
                      selected="splits"),
         
         actionButton(inputId ="goButtoncubist", label =strong("Update", style = "color:red"))
       ),
       #for neural networks
       conditionalPanel(
         condition = "input.radio_ml_class == 'nnet'"  ,
         selectInput(inputId='net_y',
                     label=strong('Response Variable'),
                     choices=names(dataset)),
         
         radioButtons(
           inputId="radio_net",
           label=strong("Predictor variable Selection Type:"),
           choices=list(
             "All",
             "Manual Select"
           ),
           selected="Manual Select"),
         
         conditionalPanel(
           condition = "input.radio_net != 'All'",
           selectizeInput(
             inputId ="net_x", 
             label = strong("Choose Predictor variables"),
             choices=names(dataset),multiple=TRUE)
         ),
         uiOutput("netslider"),
         actionButton(inputId ="goButtonnet", label =strong("Update", style = "color:red"))
       )
       
     ),

     #####
     checkboxInput(inputId ="unsupervised", label ="UnSupervised Methods",
                   value = FALSE),
     
     conditionalPanel(  
       condition = "input.unsupervised == true" ,
       
       radioButtons(inputId ="unsupervisedradio", label =strong("Methods"),
                    choices= c("PCA" = "pca",
                               "Kmeans" = "kmeans",
                               "Hierarchical Clustering" = "clust"),selected=""
       ),
       
       #for PCA
       conditionalPanel(
         condition = "input.unsupervisedradio == 'pca'"  ,
         selectInput(inputId='response_var_pca',
                     label="Choose response variable to exclude",
                     choices=names(dataset)),
         
         radioButtons(inputId='plot_pca_cum',
                      label=strong('Choose Plot'),
                      choices=list(
                        "PCA Plot"="pcaplot",
                        "Biplot"="biplot",
                        "Contribution plot"="contrib"
                      ),
                      selected="pcaplot"),
         
         conditionalPanel(
           condition = "input.plot_pca_cum == 'contrib'" ,
           textInput("contrib_vars","Select how many dimensions to show","")
         ),
         
         actionButton(inputId ="goButtonpca", label =strong("Run PCA", style = "color:red"))
         
       ),
       
       #for kmeans
       conditionalPanel(
         condition = "input.unsupervisedradio == 'kmeans'"  ,
         
         selectInput(inputId='kmeans_y',
                     label=strong('First Variable'),
                     choices=names(dataset)),
         selectInput(inputId='kmeans_x',
                     label=strong('Second Variable'),
                     choices=names(dataset)),
         
         numericInput('clusters', 'Cluster count', 3,
                      min = 1, max = 20),
         
         actionButton(inputId ="goButtonkmeans", label =strong("Run Kmeans", style = "color:red"))
         
       ),
       
       #for clustering
       conditionalPanel(
         condition = "input.unsupervisedradio == 'clust'"  ,
         
         radioButtons(inputId='clust_method',
                      label=strong('Choose Clustering Method'),
                      choices=list(
                        "Complete"="complete",
                        "Ward"="ward",
                        "Single"="single",
                        "Average"="average",
                        "Median"="median",
                        "Centroid"="centroid"),
                      selected="complete"),
         
         uiOutput("clustslider"),
         
         radioButtons(inputId='plot_clust_var',
                      label=strong('Choose Plot'),
                      choices=list(
                        "Cluster Dendrogram"="dendro",
                        "Cluster Plot"="clustplot"
                      ),
                      selected="dendro"),
         
         actionButton(inputId ="goButtonclust", label =strong("Run Hierarchical Clustering", style = "color:red"))
        )
       )
      ) 
     )
  ),
  
  
  mainPanel(tags$head(tags$style(type="text/css", ".tab-content {overflow: visible;}")),

            tabsetPanel(
              tabPanel("Data", tableOutput("contents")),
              navbarMenu("Summary",
                         tabPanel("Raw Data",verbatimTextOutput("summary_raw"),verbatimTextOutput("str_raw")),
                         tabPanel("Imputed Data",verbatimTextOutput("summary_imp"),verbatimTextOutput("str_imp"))
              ),
              #tabPanel("Summary",verbatimTextOutput("summary"),verbatimTextOutput("str")),
              navbarMenu("Plot",
                         tabPanel("EDA", plotOutput("Plot_raw", width = "1000px", height = "800px"),
                                  uiOutput("downloadPlot_raw")), # downloadButton(outputId='downloadPlot_raw', label= 'Download Plot')),
                         tabPanel("Missing Values Plot", plotOutput("Plot_imp", width = "1000px", height = "800px"),
                                  uiOutput("downloadPlot_imp")) # downloadButton(outputId='downloadPlot_imp', label= 'Download Plot'))
              ),
              
             # tabPanel("Plot", plotOutput("Plot", width = "1000px", height = "800px"),
                   #    downloadButton(outputId='downloadPlot', label= 'Download Plot')),
              tabPanel("Linear Regression",verbatimTextOutput(outputId = "mult_reg_text"),plotOutput("Mult_Regresion", width = "1000px", height = "800px"),
                       uiOutput("download")),
              tabPanel("GLM",verbatimTextOutput(outputId = "glm_text"),plotOutput("GLM_Regresion", width = "1000px", height = "800px"),
                       uiOutput("downloadglm")),
              navbarMenu("ML",
                         tabPanel("Supervised Modelling",selectInput(inputId = "help_page",
                                                                          label = strong("Choose Method"),
                                                                          choices = list('None'='.',
                                                                                         "Random Forests" = "rfs",
                                                                                         "Decision Trees" = "trees", 
                                                                                         "Support Vector Machines"="svms",
                                                                                         "Cubist"='cubist',
                                                                                         "Neural Networks"="nnet")),
                                  verbatimTextOutput(outputId = "pred_s"),plotOutput("s.Plot", width = "1000px", height = "800px"),uiOutput("downloadrf")),
                         tabPanel("UnSupervised Modelling",selectInput(inputId = "help_page_un",
                                                            label = strong("Choose Method"),
                                                            choices = list('None'='.',
                                                                           "PCA"="pca_un",
                                                                           "Kmeans"="kmeans_un",
                                                                           "Clustering"="clust_un")),
                                  verbatimTextOutput(outputId = "pred_un"),plotOutput("un.Plot", width = "1000px", height = "800px"))
                         )#,
                     
              # tabPanel("Help", selectInput(inputId = "help_page_g",
              #                              label = strong("Search Help"),
              #                              choices = list('None'='.',
              #                                             "General Help"="gen.help",
              #                                             "Plotting Help"="plot.help",
              #                                             "Modelling Help"="mod.help")),verbatimTextOutput(outputId = "help_text"))

    ))
    
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  data <- reactive({
    if (is.null(input$file1)) {
      return(NULL)
    }
    d <- read.csv(input$file1$datapath, header=input$header)    # read the file, sep=input$sep
    return(d)     
    
  })   
  
  
  
  observe({
    df <- data()
    
    if (!is.null(df)) {
      updateSelectInput(session, 'attributes',  choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributes_dens', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributes.bar',  choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributes.ident', choices = names(df))
      updateSelectInput(session, 'attributesx_box', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributesy_box', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributx', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attributy', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attrx', choices =c(None='.', names(df)))
      updateSelectInput(session, 'attry', choices =c(None='.', names(df)))
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
      updateSelectInput(session, 'facet_col_dens', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row_dens', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_col_bar', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row_bar', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_col_box', choices = c(None='.', names(df)))
      updateSelectInput(session, 'facet_row_box', choices = c(None='.', names(df)))
      updateCheckboxInput(session, 'dodge_hist')
      updateCheckboxInput(session, 'dodge_bar')
      updateSelectizeInput(session, 'pairs_var', choices = c( names(df) ))
      
      updateSelectInput(session, 'response_var_pca', choices = c(None='.', names(df)))
      
      updateSelectInput(session, 'reg_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'reg_x', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'mult_reg_x', choices = c( names(df) ))
      updateSelectInput(session, 'mult_reg_y', choices = c(None='.', names(df)))
      #updateCheckboxInput(session, 'log_y')
      #updateSelectizeInput(session, 'log_x', choices = c( names(df) ))
      #updateSelectizeInput(session, 'log_x_glm', choices = c( names(df) ))
      updateSelectInput(session, 'glm_y', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'glm_x', choices = c(names(df)))
      updateSelectizeInput(session, 'glm_x_offset', choices = c(names(df)))
      updateCheckboxInput(session, 'glm_log_y')
      updateSelectInput(session, 'radio_point', choices=c(None=".",
                                                          auto="Auto",
                                                           lm="Lm",
                                                           glm="Glm",
                                                           gam="Gam",
                                                           loess="Loess"))
      
      updateRadioButtons(session, 'family', choices=c(Gaussian='gauss',
                                                      Poisson='pois',
                                                      Binomial='binom',
                                                      Negative.Binomial='neg.bin',
                                                      Gamma='gamma'))
      updateSelectInput(session, 'radio.plots', choices = c(None='.', Added_Variable_Plot="avplot",
                                                            Residual_Fitted_Plot="resplot",
                                                            Marginal_Plot="margplot",
                                                            Partial_Residual_Plot="crplot",
                                                            Coefficient_Plot="coefplot_ml"))
      updateSelectInput(session, 'radio.plots.glm', choices = c(None='.', Added_Variable_Plot="avplot",
                                                                Residual_Fitted_Plot="resplot",
                                                                Partial_Residual_Plot="crplot",
                                                                Coefficient_Plot='coefplot'))
      updateSelectInput(session, 'help_page_g', choices = c(None='.', General_Help="gen.help",
                                                          Plotting_Help="plot.help",
                                                          Modelling_Help="mod.help"))
      updateSelectizeInput(session, 'rf_x', choices = c(names(df)))
      updateSelectInput(session, 'rf_y', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'tree_x', choices = c(names(df)))
      updateSelectInput(session, 'tree_y', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'svm_x', choices = c(names(df)))
      updateSelectInput(session, 'svm_y', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'cubist_x', choices = c(names(df)))
      updateSelectInput(session, 'cubist_y', choices = c(None='.', names(df)))
      updateSelectizeInput(session, 'net_x', choices = c(names(df)))
      updateSelectInput(session, 'net_y', choices = c(None='.', names(df)))
      #updateCheckboxGroupInput(session, 'clust_x', choices = c(names(df)))
      #updateSelectInput(session, 'clust_y', choices = c(None='.', names(df)))
      updateSelectInput(session, 'kmeans_y', choices = c(None='.', names(which(sapply(df, is.numeric)))))
      updateSelectInput(session, 'kmeans_x', choices = c(None='.', names(which(sapply(df, is.numeric)))))
      updateRadioButtons(session, "radiosvmkernel", choices=list(
        "Linear",
        "Polynomial",
        "Radial basis",
        "Sigmoid"
      ))
      updateSelectInput(session, 'svm_x_plot', choices = c(None='.', names(df)))
      updateSelectInput(session, 'svm_y_plot', choices = c(None='.', names(df)))
    }
  })
  
  # ------------------------------------------------------------------
  # Generate a summary of the dataset
  output$summary_raw <- renderPrint({
    summary(data())
  })
  output$str_raw <- renderPrint({
    str(data())
  })
  
  
  
  # Show the first "n" observations
  output$contents <- renderTable({
    head(data(), n = input$obs)
  })
  
  # ------------------------------------------------------------------
  listN <- reactiveValues()
  makeReactiveBinding("listN")
  #------Rendering the list to the ui
  output$uiAdded <- renderUI({
    checkboxGroupInput('added',
                       'List of combinations', 
                       choices = names(listN))
    #multiple = TRUE,
    #selectize = FALSE)
  })
  observe({
    # Trigger Add actions
    input$goButtonglm
    isolate({
      new_selections <- c(input$makeNumInteract,input$makeFactInteract)
      new_selections_name <- new_selections %>% paste(collapse = ":")
      
      if(new_selections_name != "")
        listN[[new_selections_name]] <- new_selections
    })
  })
  
  listN_mul <- reactiveValues()
  makeReactiveBinding("listN_mul")
  #------Rendering the list to the ui
  output$uiAdded_mul <- renderUI({
    checkboxGroupInput('added_mul',
                       'List of combinations', 
                       choices = names(listN_mul))
    #multiple = TRUE,
    #selectize = FALSE)
  })
  observe({
    # Trigger Add actions
    input$goButtonmod
    isolate({
      new_selections <- c(input$makeNumInteract_mul,input$makeFactInteract_mul)
      new_selections_name <- new_selections %>% paste(collapse = ":")
      
      if(new_selections_name != "")
        listN_mul[[new_selections_name]] <- new_selections
    })
  })
  
 
  output$makeFactInteract <- renderUI({
    selectInput("makeFactInteract", "Factor Variable For Interaction:", names(which(sapply(data(), is.factor))))
  })  
  
  #---------------------Numerical Variable for List of Combinations------------- 
  output$makeNumInteract <- renderUI({
    selectInput("makeNumInteract", "Numeric Variable for Interaction:", names(which(sapply(data(), is.numeric)))[!names(which(sapply(data(), is.numeric))) %in% input$glm_x],names(which(sapply(data(), is.numeric)))[!names(which(sapply(data(), is.numeric))) %in% input$glm_x])
  })
  
  output$makeFactInteract_mul <- renderUI({
    selectInput("makeFactInteract_mul", "Factor Variable For Interaction:", names(which(sapply(data(), is.factor))))
  })  
  
  #---------------------Numerical Variable for List of Combinations------------- 
  output$makeNumInteract_mul <- renderUI({
    selectInput("makeNumInteract_mul", "Numeric Variable for Interaction:", names(which(sapply(data(), is.numeric)))[!names(which(sapply(data(), is.numeric))) %in% input$mult_reg_x],names(which(sapply(data(), is.numeric)))[!names(which(sapply(data(), is.numeric))) %in% input$mult_reg_x])
  })
  
  # -------------------------------------------------------------------
  
  output$slider <- renderUI({
    sliderInput("rf_train", "Size of Train set", min=1, max=nrow(data()), value=1)
  })
  output$treeslider <- renderUI({
    sliderInput("tree_train", "Size of Train set", min=1, max=nrow(data()), value=1)
  })
  output$cubistslider <- renderUI({
    sliderInput("committees", "Number of committees for modelling", min=1, max=100, value=1,step=1)
  })
  output$netslider <- renderUI({
    sliderInput("net_train", "Size of Train set", min=1, max=nrow(data()), value=1)
  })
  output$clustslider <- renderUI({
    sliderInput("clust_tree", "Number of clusters", min=1, max=100, value=1,step=1)
  })
  
  datakmeans <- reactive({
    df <- data()
    df<-data.frame(df[, c(input$kmeans_x,input$kmeans_y)],stringsAsFactors = FALSE)
    sapply(na.omit(df), as.numeric)
  })
  
  clusters <- reactive({
    kmeans(datakmeans(), input$clusters)
  })
  
  output$makeameliafact <- renderUI({
  selectizeInput(
    inputId ="amelia_var_fact",
    label = "Select Factor variables", choices=names(which(sapply(data(), is.factor))),multiple=TRUE)
  })
  
  output$makeamelia <- renderUI({
    selectizeInput(
      inputId ="amelia_var",
      label = "Select ID variables", choices=names(data()),multiple=TRUE)
  })
  
  output$make_weights <- renderUI({
    textInput("weights_glm", "Add weights", "")
  })
  
  output$sub_mod <- renderUI({
    textInput("subsetting", "Subset Data for modelling", "")
  })
  output$trans_mod <- renderUI({
    textInput("transf", "Transformations of Predictors", "")
  })
  output$trans_mod_mul <- renderUI({
    textInput("transf_mul", "Transformations of Predictors", "")
  })
  
  output$point_x_trans <- renderUI({
    checkboxInput(
      inputId ="x_log_trans_point",
      label = "Log transformation of y axis")
  })
  output$point_y_trans <- renderUI({
  checkboxInput(
    inputId ="y_log_trans_point",
    label = "Log transformation of y axis")
  })
  output$point_x_trans_label <- renderUI({
    checkboxInput(
      inputId ="x_log_label_trans_point",
      label = paste0("Log transformation of ", input$attrx, " variable"))
  })
  output$point_y_trans_label <- renderUI({
    checkboxInput(
      inputId ="y_log_label_trans_point",
      label = paste0("Log transformation of ", input$attry, " variable"))
  })
  
  output$log_label_hist <- renderUI({
    checkboxInput(
      inputId ="x_log_label_hist",
      label = paste0("Log transformation of ", input$attributes, " variable"))
  })
  output$log_label_box <- renderUI({
    checkboxInput(
      inputId ="y_log_label_box",
      label = paste0("Log transformation of ", input$attributesy_box, " variable"))
  })
  output$log_label_dens <- renderUI({
    checkboxInput(
      inputId ="x_log_label_dens",
      label = paste0("Log transformation of ", input$attributes_dens, " variable"))
  })
  output$x_log_label_line <- renderUI({
    checkboxInput(
      inputId ="x_log_label_line_x",
      label = paste0("Log transformation of ", input$attributx, " variable"))
  })
  output$y_log_label_line <- renderUI({
    checkboxInput(
      inputId ="y_log_label_line_y",
      label = paste0("Log transformation of ", input$attributy, " variable"))
  })
  output$point_smooth_group <- renderUI({
    checkboxInput(
      inputId ="smooth_group",
      label ="Overall grouping")
  })
  
  output$smooth_method_args <- renderUI({
    textInput("smooth_args", h5("Specify arguments for smooth by typing"), "")
  })
  output$facet_strip_x_point <- renderUI({
    textInput("strip_x_point", h5("strip size for columns"), "")
  })
  output$facet_strip_y_point <- renderUI({
    textInput("strip_y_point", h5("strip size for rows"), "")
  })
  output$facet_strip_x_line <- renderUI({
    textInput("strip_x_line", h5("strip size for columns"), "")
  })
  output$facet_strip_y_line <- renderUI({
    textInput("strip_y_line", h5("strip size for rows"), "")
  })
  output$facet_strip_x_dens <- renderUI({
    textInput("strip_x_dens", h5("strip size for columns"), "")
  })
  output$facet_strip_y_dens <- renderUI({
    textInput("strip_y_dens", h5("strip size for rows"), "")
  })
  output$facet_strip_x_bar <- renderUI({
    textInput("strip_x_bar", h5("strip size for columns"), "")
  })
  output$facet_strip_y_bar <- renderUI({
    textInput("strip_y_bar", h5("strip size for rows"), "")
  })
  output$facet_strip_x_box <- renderUI({
    textInput("strip_x_box", h5("strip size for columns"), "")
  })
  output$facet_strip_y_box <- renderUI({
    textInput("strip_y_box", h5("strip size for rows"), "")
  })
  output$facet_strip_x_hist <- renderUI({
    textInput("strip_x_hist", h5("strip size for columns"), "")
  })
  output$facet_strip_y_hist <- renderUI({
    textInput("strip_y_hist", h5("strip size for rows"), "")
  })
  
  # output$choose_pairs <- renderUI({
  #   
  # })
 
  output$make_gauss_link <- renderUI({
  radioButtons(
    inputId ="gauss_link",
    label = "Choose Link Function",
    choices=c("Log","Identity"))
  })
  output$make_pois_link <- renderUI({
    radioButtons(
      inputId ="pois_link",
      label = "Choose Link Function",
      choices=c("Log","Identity"))
  })
  output$make_binom_link <- renderUI({
    radioButtons(
      inputId ="binom_link",
      label = "Choose Link Function",
      choices=c("Logit","Probit"))
  })
  output$make_gamma_link <- renderUI({
    radioButtons(
      inputId ="gamma_link",
      label = "Choose Link Function",
      choices=c("Log","Inverse"))
  })
  
  
  
  
  output$Plot_raw <- renderPlot({
    if (is.null(data()))
      return(NULL) 
    input$goButton
    input$goButton1
    input$goButton2
    input$goButton3
    input$goButton4
    input$goButton5
    input$goButton6
    input$goButtonpairs
    
    if ( input$plot == "none" ) {
      return(NULL)
    }
    
    
    #plotting parameters
    isolate({
      
      if ( input$plot == "hist" ) {
        if(input$attributes != '.'){
        
        if (input$dodge_hist)  position_d='dodge' else position_d='stack'
        if(input$x_log_label_hist){
          if(input$fil!="." ){
            aes_mapping <- aes_string(x = paste0("log(",input$attributes,")"),group=input$fil,fill=input$fil) 
          }
          else{
            aes_mapping <- aes_string(x =paste0("log(",input$attributes,")"))  
          }
        }else{
          if(input$fil!="." ){
            aes_mapping <- aes_string(x = input$attributes,group=input$fil,fill=input$fil) 
          }
          else{
            aes_mapping <- aes_string(x =input$attributes)  
          }
        }
        

        #plot
        if(input$density_hist){
          p <- ggplot(data(),mapping=aes_mapping ) +
            geom_histogram(aes(y=..density..),binwidth=input$bin,position = position_d)
        }else{
          p <- ggplot(data(),mapping=aes_mapping ) +
            geom_histogram(binwidth=input$bin,position = position_d)
        }
        
        
       
        if(input$x_log_trans){
          p <- p+scale_x_continuous(trans='log10')
        }
        
        }else{
          if(input$vars_trans !=""){
          
          if (input$dodge_hist)  position_d='dodge' else position_d='stack'
            if(input$fil!="." ){
              aes_mapping <- aes_string(x = input$vars_trans,group=input$fil,fill=input$fil)            
          }else{
            aes_mapping <- aes_string(x =input$vars_trans) 
          }
          
          #plot
          if(input$density_hist){
            p <- ggplot(data(),mapping=aes_mapping ) +
              geom_histogram(aes(y=..density..),binwidth=input$bin,position = position_d)
          }else{
            p <- ggplot(data(),mapping=aes_mapping ) +
              geom_histogram(binwidth=input$bin,position = position_d)
          }
        
          if(input$vars_trans_x_log_trans){
            p <- p+scale_x_continuous(trans='log10')
          }
          
        } 

      }
        
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free') 
          if(input$facet_col !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_hist !="" & input$strip_y_hist!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_hist, 0, input$strip_x_hist, 0, "cm")),
                         strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_hist, 0, input$strip_y_hist, "cm")))
          }else if(input$strip_x_hist !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_hist, 0, input$strip_x_hist, 0, "cm")))
            
          }else if(input$strip_y_hist!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_hist, 0, input$strip_y_hist, "cm")))
          }
        }
    
    }
      
  }) 
    
    isolate({
      
      if ( input$plot == "box" ) {
        if(input$attributesx_box != '.' | input$attributesy_box != '.'){
          
          if (class(data()[,input$attributesx_box])=="factor" & length(levels(data()[,input$attributesx_box])) >15) {
            ang <- theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }else{
            ang <- theme(axis.text.x=NULL)
          }

          if(input$y_log_label_box){
            if(input$filbox!="." ){
              aes_mapping <- aes_string(x = input$attributesx_box, y = paste0("log(",input$attributesy_box,")"),group=input$filbox,fill=input$filbox) 
            }
            else{
              aes_mapping <- aes_string(x =input$attributesx_box,y = paste0("log(",input$attributesy_box,")"))  
            }
            
          }else{
            if(input$filbox!="." ){
              aes_mapping <- aes_string(x = input$attributesx_box,y = input$attributesy_box,group=input$filbox,fill=input$filbox) 
            }
            else{
              aes_mapping <- aes_string(x =input$attributesx_box,y = input$attributesy_box)  
            }
          }
        
        #plot
        p <- ggplot(data(),mapping=aes_mapping ) +
          geom_boxplot()+ang
        
        facets <- paste(input$facet_row_box, '~', input$facet_col_box)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free')  #+theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          if(input$facet_col_box !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_box !="" & input$strip_y_box!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_box, 0, input$strip_x_box, 0, "cm")),
                         strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_box, 0, input$strip_y_box, "cm")))
          }else if(input$strip_x_box !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_box, 0, input$strip_x_box, 0, "cm")))
            
          }else if(input$strip_y_box!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_box, 0, input$strip_y_box, "cm")))
          }
        }
        if(input$y_log_trans_box){
          p <- p+scale_y_continuous(trans='log10')
        }
      }
    }
      
  })
    
    isolate({
      
      if ( input$plot == "bar" ) {
        if(input$attributes.bar !='.'){
          
          if (class(data()[,input$attributes.bar])=="factor" & length(levels(data()[,input$attributes.bar])) >15) {
            ang <- theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }else{
            ang <- theme(axis.text.x=NULL)
          }
        
        if (input$dodge_bar)  position_d='dodge' else position_d='stack'
        if(input$filbar!="." ){
          if (input$identity == TRUE ) {
            aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident ,group=input$filbar,fill=input$filbar)
            }
          else {
            aes_mapping <- aes_string(x = input$attributes.bar,group=input$filbar,fill=input$filbar) }
        }
        else if(input$identity == TRUE){
          aes_mapping <- aes_string(x = input$attributes.bar, y=input$attributes.ident)
        }
        else {
          aes_mapping <- aes_string(x =input$attributes.bar)  }
   
        #plot
        p <- ggplot(data(),mapping=aes_mapping )+ang
        
        if (input$identity ==TRUE ){
          p<- p+geom_bar(stat='identity',position = position_d) }
        
        else{
          p<- p+ geom_bar(position = position_d)
        }
        
        facets <- paste(input$facet_row_bar, '~', input$facet_col_bar)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free')  
          if(input$facet_col_bar !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_bar !="" & input$strip_y_bar!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_bar, 0, input$strip_x_bar, 0, "cm")),
                         strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_bar, 0, input$strip_y_bar, "cm")))
          }else if(input$strip_x_bar !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_bar, 0, input$strip_x_bar, 0, "cm")))
            
          }else if(input$strip_y_bar!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_bar, 0, input$strip_y_bar, "cm")))
          }
        }
      }
    }
      
  }) 
    
    
    isolate({
      
      if ( input$plot == "dens" ) {
        if(input$attributes_dens !='.'){
          
          if (class(data()[,input$attributes_dens])=="factor" & length(levels(data()[,input$attributes_dens])) >15) {
            ang <- theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }else{
            ang <- theme(axis.text.x=NULL)
          }

          if(input$x_log_label_dens){
            
            if(input$fil.dens!="." ){
              aes_mapping <- aes_string(x = paste0("log(",input$attributes_dens,")"),group=input$fil.dens,fill=input$fil.dens) 
            } 
            else{
              aes_mapping <- aes_string(x =paste0("log(",input$attributes_dens,")"))  
            }
            
          }else{
            if(input$fil.dens!="." ){
              aes_mapping <- aes_string(x = input$attributes_dens,group=input$fil.dens,fill=input$fil.dens) 
            } 
            else{
              aes_mapping <- aes_string(x =input$attributes_dens)  
            }
          }
       
        
        #plot
        p <- ggplot(data(),mapping=aes_mapping ) +
          geom_density(alpha=.3)+ang
        
        facets <- paste(input$facet_row_dens, '~', input$facet_col_dens)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free') 
          if(input$facet_col_dens !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_dens !="" & input$strip_y_dens!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_dens, 0, input$strip_x_dens, 0, "cm")),
                         strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_dens, 0, input$strip_y_dens, "cm")))
          }else if(input$strip_x_dens !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_dens, 0, input$strip_x_dens, 0, "cm")))
            
          }else if(input$strip_y_dens!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_dens, 0, input$strip_y_dens, "cm")))
          }
        }
        if(input$x_log_trans_dens){
          p <- p+scale_x_continuous(trans='log10')
        }
        
        #p <- p+ang
        }
      }
      
    }) 
    
    isolate({
      
      if ( input$plot == "line" ) {
        if(input$attributx != '.' | input$attributy != '.'){
        
          if((input$x_log_label_line_x & input$y_log_label_line_y)){
            if(input$filline!='.' ){
              if(input$sizeline!='.'){
                aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=paste0("log(",input$attributy,")"),color=input$filline,size=input$sizeline) 
              }else{
                aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=paste0("log(",input$attributy,")"),color=input$filline) 
              }
            }else{
              aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=paste0("log(",input$attributy,")"))   
            }
            
            
          }else if (input$x_log_label_line_x) {
            if(input$filline!='.' ){
              if(input$sizeline!='.'){
                aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=input$attributy,color=input$filline,size=input$sizeline) 
              }else{
                aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=input$attributy,color=input$filline) 
              }
            }else{
              aes_mapping <- aes_string(x =paste0("log(",input$attributx,")"),y=input$attributy)   
            }
            
          }else if(input$y_log_label_line_y) {
            if(input$filline!='.' ){
              if(input$sizeline!='.'){
                aes_mapping <- aes_string(x =input$attributx,y=paste0("log(",input$attributy,")"),color=input$filline,size=input$sizeline) 
              }else{
                aes_mapping <- aes_string(x =input$attributx,y=paste0("log(",input$attributy,")"),color=input$filline) 
              }
            }else{
              aes_mapping <- aes_string(x =input$attributx,y=paste0("log(",input$attributy,")"))   
            }
            
            
          }else{
            if(input$filline!='.' ){
              if(input$sizeline!='.'){
                aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline,size=input$sizeline) 
              }else{
                aes_mapping <- aes_string(x =input$attributx,y=input$attributy,color=input$filline) 
              }
            }else{
              aes_mapping <- aes_string(x =input$attributx,y=input$attributy)   
            }
          }
          
        
        
        #plot
        p <- ggplot(data(),mapping=aes_mapping ) +
          geom_line(group=input$filline)+scale_x_continuous(limits=input$zoom_line)              
        
        facets <- paste(input$facet_rowline, '~', input$facet_colline)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free')  
          if(input$facet_colline !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_line !="" & input$strip_y_line!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_line, 0, input$strip_x_line, 0, "cm")),
                         strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_line, 0, input$strip_y_line, "cm")))
          }else if(input$strip_x_line !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_line, 0, input$strip_x_line, 0, "cm")))
            
          }else if(input$strip_y_line!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_line, 0, input$strip_y_line, "cm")))
          }

        }
        if(input$x_log_trans_line){
          p <- p+scale_x_continuous(trans='log10')
        }
        if(input$y_log_trans_line){
          p <- p+scale_y_continuous(trans='log10')
        }
        
        }
      }
    })
    
    isolate({
      if ( input$plot == "point" ) {
        if(input$attrx != '.' | input$attry != '.'){
          
          if (class(data()[,input$attrx])=="factor" & length(levels(data()[,input$attrx])) >15) {
            ang <- theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }else{
            ang <- theme(axis.text.x=NULL)
          }
        
          if (input$x_log_label_trans_point & input$y_log_label_trans_point) {
            if(input$filpoint!='.' ){
              if(input$sizepoint!='.'){
                aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=paste0("log(",input$attry,")"),color=input$filpoint,size=input$sizepoint) 
              }else{
                aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=paste0("log(",input$attry,")"),color=input$filpoint) 
              }
            }else{
              aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=paste0("log(",input$attry,")"))  
            }
          }else if(input$x_log_label_trans_point){
            if(input$filpoint!='.' ){
              if(input$sizepoint!='.'){
                aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=input$attry,color=input$filpoint,size=input$sizepoint) 
              }else{
                aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=input$attry,color=input$filpoint) 
              }
            }else{
              aes_mapping <- aes_string(x =paste0("log(",input$attrx,")"),y=input$attry)  
            }
         }else if(input$y_log_label_trans_point){
           if(input$filpoint!='.' ){
             if(input$sizepoint!='.'){
               aes_mapping <- aes_string(x =input$attrx,y=paste0("log(",input$attry,")"),color=input$filpoint,size=input$sizepoint) 
             }else{
               aes_mapping <- aes_string(x =input$attrx,y=paste0("log(",input$attry,")"),color=input$filpoint) 
             }
           }else{
             aes_mapping <- aes_string(x =input$attrx,y=paste0("log(",input$attry,")"))  
           }
          }else{
            if(input$filpoint!='.' ){
              if(input$sizepoint!='.'){
                aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint,size=input$sizepoint) 
              }else{
                aes_mapping <- aes_string(x =input$attrx,y=input$attry,color=input$filpoint) 
              }
            }else{
              aes_mapping <- aes_string(x =input$attrx,y=input$attry)  
            }
          }
 
        #plot
        p <- ggplot(data(),mapping=aes_mapping ) +
          geom_point()+ang
        
        
        if(input$radio_point =="Auto"){
          if(input$smooth_group){
            if(input$smooth_args !=""){
              p <- p+geom_smooth(aes(group=1),method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(aes(group=1))
            }
          }else{
            if(input$smooth_args !=""){
              p <- p+geom_smooth(method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth()
            }
          }
        }
        if(input$radio_point =="Lm"){
          
          if(input$smooth_group){
            if(input$smooth_args !=""){
              p <- p+geom_smooth(aes(group=1),method = 'lm',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(aes(group=1),method = 'lm')
            }
          }else{
            if(input$smooth_args !=""){
              p <- p+geom_smooth(method = 'lm',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(method = 'lm')
            }
          }
        }
        else if(input$radio_point =="Glm"){
          
          if(input$smooth_group){
            if(input$smooth_args !=""){
              p <- p+stat_smooth(aes(group=1),method = 'glm',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+stat_smooth(aes(group=1),method = 'glm')
            }
          }else{
            if(input$smooth_args !=""){
              p <- p+stat_smooth(method = 'glm',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+stat_smooth(method = 'glm')
            }
          }
          

        }else if(input$radio_point =="Gam"){
          
          if(input$smooth_group){
            if(input$smooth_args !=""){
              p <- p+geom_smooth(aes(group=1),method = 'gam',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(aes(group=1),method = 'gam')
            }
          }else{
            if(input$smooth_args !=""){
              p <- p+geom_smooth(method = 'gam',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(method = 'gam')
            }
          }
        }
        else if(input$radio_point =="Loess"){
          
          if(input$smooth_group){
            if(input$smooth_args !=""){
              p <- p+geom_smooth(aes(group=1),method = 'loess',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(aes(group=1),method = 'loess')
            }
          }else{
            if(input$smooth_args !=""){
              p <- p+geom_smooth(method = 'loess',method.args = list(eval(parse(text=input$smooth_args))))
            }else{
              p <- p+geom_smooth(method = 'loess')
            }
          }
        }
        
        facets <- paste(input$facet_rowpoint, '~', input$facet_colpoint)
        if (facets != '. ~ .') {
          p <- p + facet_grid(facets,scales='free')  
          if(input$facet_colpoint !="."){
            p <-p +theme(axis.text.x=element_text(angle = 90, hjust = 1,vjust=0.4,size=9))
          }
          if(input$strip_x_point !="" & input$strip_y_point!=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_point, 0, input$strip_x_point, 0, "cm")),
                  strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_point, 0, input$strip_y_point, "cm")))
          }else if(input$strip_x_point !=""){
            p <- p+theme(strip.text.x = element_text(margin = ggplot2::margin(input$strip_x_point, 0, input$strip_x_point, 0, "cm")))
            
          }else if(input$strip_y_point!=""){
            p <- p+theme(strip.text.y = element_text(margin = ggplot2::margin(0, input$strip_y_point, 0, input$strip_y_point, "cm")))
          }
          
        }
        if(input$x_log_trans_point){
          p <- p+scale_x_continuous(trans='log10')
        }
        if(input$y_log_trans_point){
          p <- p+scale_y_continuous(trans='log10')
        }
        
       # p <- p+ang
        
        }
      }
    })

      if ( input$plot == "correlation" ) {
        
        dd <- data() %>%
          dplyr::select_if(is.numeric)
        ind_na <- sapply(1:length(dd),function(x) all(is.na(dd[,x])))
        ind_0 <- sapply(1:length(dd),function(x) all(dd[,x]==0))
        ind_1 <- sapply(1:length(dd),function(x) all(dd[,x]==1))
        rem <- c(colnames(dd)[ind_na],colnames(dd)[ind_0],colnames(dd)[ind_1])
        rem <- rem[!is.na(rem)]
        dd <- dd[, !(colnames(dd) %in% rem)]
        
        p <- melt(cor(dd,use="pairwise.complete.obs")) %>%
               ggplot(aes(X1, X2, fill = value)) + geom_tile() + theme(axis.text.x  = element_text(angle=90,hjust=0.9,vjust=0.5))+
               scale_fill_gradient2(low = "#4CB2B2",  high = "#F04E23", mid = "#2B2878", name = 'Correlation', limits = c(-1,1))+xlab('')+ylab('')
        
        if (input$corr_val) {
          p <- p+geom_text(aes(X1, X2, label = round(value,2),fontface=2))
        }
      }
    
    
   
     if ( input$plot == "pairs" ) {
       isolate({
     
      if(!is.null(input$pairs_var)){
        p <- ggpairs(data()[,c(input$pairs_var)])
      }
      
    })
  }
    
    
    print(p)
  })
  
  output$Outputplot <- downloadHandler(
    filename = function() { paste(input$plot, '.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 1500, height = 1200)
      ggsave(file,  device = device)
    }
  )
  output$downloadPlot_raw <- renderUI({
    if(input$plot != "missingness") {
      downloadButton('Outputplot', 'Download Plot')
    }
  })
  
  output$Plot_imp <- renderPlot({
    
    if ( input$plot == "missingness" ) {
      p <- plot_missing(data())
    }
    print(p)
    
  })
  
  output$Outputplot_imp <- downloadHandler(
    filename = function() { paste(input$file1, '_Missing.png', sep='') },
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 1000, height = 800, res = 300)
      ggsave(file,  device = device)
    }
  )
  output$downloadPlot_imp <- renderUI({
    if(input$plot == "missingness") {
      downloadButton('Outputplot_imp', 'Download Plot')
    }
  })
 
  df_imp <- reactive({
    if(input$goButtonimput>0){
      
      isolate({ 
        if (input$imput_method=="Amelia") {
          # output$summary_imp <- renderPrint({
          withProgress(session, min=0, max=1, {
            setProgress(message = "Running...")
            # Let's say we normally expect the user data loading to take 20% of the total time
            setProgress(value = 0.2, message = "Running...")
            aa <- adms.stat.infill(data(),id.vars =input$amelia_var,noms =input$amelia_var_fact)
            jobLength = 10
            for (i in 1:jobLength) {
              # Do work
              setProgress(value = i)
            }
            return(aa)
            #})
          })    
        }else if (input$imput_method=="missForest"){
          # output$summary_imp <- renderPrint({
          withProgress(session, min=0, max=1, {
            setProgress(message = "Running...")
            # Let's say we normally expect the user data loading to take 20% of the total time
            setProgress(value = 0.2, message = "Running...")
            aa <- adms.stat.infill(data(),method = 'missForest')
            jobLength = 10
            for (i in 1:jobLength) {
              # Do work
              setProgress(value = i)
            }
            return(aa)
            # })
          })
        }
        
      }) 
      
    }
    # 
  })
  
  #ntext <- eventReactive(input$goButtonimput, { df_imp() })
  output$summary_imp <- renderPrint({
    summary(df_imp())
    
  })
  
  # 
 
  
  # Functions for creating models 
  
  make_model <- function(model_type, formula, family=NULL,subset=NULL,weights=NULL,...) {
    
    if(input$imput_method !="None"){
      data <-  reactive ({ df_imp() }) 
    }
    if (input$weights_glm !="") {
      weightss <- with(data(),eval(parse(text=input$weights_glm)))
    }else{
      weightss <- NULL
    }
    
    if(input$subsetting != ""){
    if(!input$glm){
      ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),  ...)) 
    }
    else if(input$family=='pois'){
      if(input$pois_link=="Identity"){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(poisson(link = "identity")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(poisson(link = "log")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }
    }
    else if(input$family=='binom'){
      if(input$binom_link=="Logit"){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(binomial(link = "logit")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(binomial(link = "probit")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }
    }
    else if (input$family=='gamma'){
      if(input$gamma_link=="Log"){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "log")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "inverse")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }
    }
    else if ( (input$family=='gamma') & (input$glm_y <=0) ){
      if(input$gamma_link=="Log"){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "log")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "inverse")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }
    }
    else if (input$family=='gauss'){
      if(input$gauss_link=="Identity"){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(gaussian(link = "identity")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(gaussian(link = "log")),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
      }
    }
    else{
      ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),subset=with(data(),eval(parse(text=input$subsetting))),weights=weightss,  ...))
    }
      ss[["call"]][["subset"]] <- input$subsetting
      ss[["call"]][["weights"]] <- input$input$weights_glm
      return(ss)
    }
    else{
      if(!input$glm){
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),weights=weightss,  ...)) 
      }
      else if(input$family=='pois'){
        if(input$pois_link=="Identity"){
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(poisson(link = "identity")),weights=weightss,  ...))
          }else{
            ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(poisson(link = "log")),weights=weightss,  ...))
          }
      }
      else if(input$family=='binom'){
        if(input$binom_link=="Logit"){
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(binomial(link="logit")),weights=weightss,  ...))
        }else{
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(binomial(link = "probit")),weights=weightss,  ...))
        }
      }
      else if (input$family=='gamma'){
        if(input$gamma_link=="Log"){
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "log")),weights=weightss,  ...))
        }else{
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "inverse")),weights=weightss,  ...))
        }
      }
      else if ( (input$family=='gamma') & (input$glm_y <=0) ){
        if(input$gamma_link=="Log"){
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "log")),weights=weightss,  ...))
        }else{
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(Gamma(link = "inverse")),weights=weightss,  ...))
        }
      }
      else if (input$family=='gauss'){
        if(input$gauss_link=="Identity"){
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(gaussian(link = "identity")),weights=weightss,  ...))
        }else{
          ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),family=quote(gaussian(link = "log")),weights=weightss,  ...))
        }
      }
      else{
        ss <- do.call(model_type, args = list(formula = formula,data = quote(data()),weights=weightss,  ...))
      }
      ss[["call"]][["weights"]] <- input$input$weights_glm
      return(ss)
   }
    
  }
  
  

  output$Mult_Regresion <- renderPlot({
    
    input$goButtonmod
    
    if(input$goButtonmod > 0){
      
      isolate({ 
    
    #make added variable plots
    if(input$radio.plots == "avplot"){
    if( input$radio != "All"){
      
      if (input$log_y) {
        if ( !is.null(input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
          avPlots(make_model("lm", formula)) 
        }
        else{
          formula <- paste("log(",input$mult_reg_y,"+1)", "~", paste(input$mult_reg_x,collapse='+'))
          avPlots(make_model("lm", formula))
        }
       
      }else{
        if ( !is.null( input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
          avPlots(make_model("lm", formula)) 
        }else{
          formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
          avPlots(make_model("lm", formula))
        }
        
      }
      if(!is.null(input$added_mul)){
        formula <- paste0(formula,"+",  paste0(input$added_mul,collapse = "+"))
        avPlots(make_model("lm", formula))
      }else{
        avPlots(make_model("lm", formula))
      }
      if (input$transf_mul != "") {
        formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
        avPlots(make_model("lm", formula))
      }else{
        avPlots(make_model("lm", formula))
      }
   }
    
    else{
      if( input$radio == "All"){
        if (input$log_y) {
          formula <-  paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
          avPlots(make_model("lm", formula))
        }else {
          formula <- paste(input$mult_reg_y, "~.")
          avPlots(make_model("lm", formula))
        }
        if(!is.null(input$added_mul)){
          formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
          avPlots(make_model("lm", formula))
        }else{
          avPlots(make_model("lm", formula))
        }
        if (input$transf_mul != "") {
          formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
          avPlots(make_model("lm", formula))
        }else{
          avPlots(make_model("lm", formula))
        }
      }
    }
    }
    
    #make residual plots
    if(input$radio.plots == "resplot"){
     if( input$radio != "All"){
      if (input$log_y) {
        if ( !is.null(input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
          residualPlots(make_model("lm", formula))
        }
        else{
          formula <- paste("log(",input$mult_reg_y,"+1)", "~", paste(input$mult_reg_x,collapse='+'))
          residualPlots(make_model("lm", formula))
        }

      }
      else{
        if ( !is.null( input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
          residualPlots(make_model("lm", formula)) 
        }else{
          formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
          residualPlots(make_model("lm", formula))
        }
      }
      if (!is.null(input$added_mul)){
        formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
        residualPlots(make_model("lm", formula))
      }else{
        residualPlots(make_model("lm", formula))
      }
       if (input$transf_mul != "") {
         formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
         residualPlots(make_model("lm", formula))
       }else{
         residualPlots(make_model("lm", formula))
       }
    }
    
    else{
      if( input$radio == "All"){
        if (input$log_y) {
          formula <-  paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
          residualPlots(make_model("lm", formula))
        }
        else{
          formula <- paste(input$mult_reg_y, "~.")
          residualPlots(make_model("lm", formula))
        }
        if (!is.null(input$added_mul)){
          formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
          residualPlots(make_model("lm", formula))
        }else{
          residualPlots(make_model("lm", formula))
        }
        if (input$transf_mul != "") {
          formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
          residualPlots(make_model("lm", formula))
        }else{
          residualPlots(make_model("lm", formula))
        }
      }
     } 
    }
    
    #make marginal plots
    if(input$radio.plots == "margplot"){
     if( input$radio != "All"){
      if (input$log_y) {
        if ( !is.null(input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
          marginalModelPlots(make_model("lm", formula))
        }
        else{
          formula <- paste("log(",input$mult_reg_y,"+1)", "~", paste(input$mult_reg_x,collapse='+'))
          marginalModelPlots(make_model("lm", formula))
        }
       
      }
      else{
        if ( !is.null( input$log_x)) {
          form_x <-paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
          marginalModelPlots(make_model("lm", formula)) 
        }else{
          formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
          marginalModelPlots(make_model("lm", formula))
        }
        
      }
      if (!is.null(input$added_mul)){
        formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
        marginalModelPlots(make_model("lm", formula))
      }
      else{
        marginalModelPlots(make_model("lm", formula))
      }
       if (input$transf_mul != "") {
         formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
         marginalModelPlots(make_model("lm", formula))
       }else{
         marginalModelPlots(make_model("lm", formula))
       }
    }
    
    else{
      if( input$radio == "All"){
        if (input$log_y) {
          formula <-  paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
          marginalModelPlots(make_model("lm", formula))
        }
        else{
          formula <- paste(input$mult_reg_y, "~.")
          marginalModelPlots(make_model("lm", formula))
        }
        if (!is.null(input$added_mul)){
          formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
          marginalModelPlots(make_model("lm", formula))
        }else{
          marginalModelPlots(make_model("lm", formula))
        }
        if (input$transf_mul != "") {
          formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
          marginalModelPlots(make_model("lm", formula))
        }else{
          marginalModelPlots(make_model("lm", formula))
        }
      }
     } 
    }
    #make cr plots
    if(input$radio.plots == "crplot"){
     if( input$radio != "All"){
      if (input$log_y) {
        if ( !is.null(input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
          crPlots(make_model("lm", formula))
        }
        else{
          formula <- paste("log(",input$mult_reg_y,"+1)", "~", paste(input$mult_reg_x,collapse='+'))
          crPlots(make_model("lm", formula))
        }
       
      }
      else{
        if ( !is.null( input$log_x)) {
          form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
          formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
          crPlots(make_model("lm", formula)) 
        }else{
          formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
          crPlots(make_model("lm", formula))
        }
        
      }
      if (!is.null(input$added_mul)){
        formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
        crPlots(make_model("lm", formula))
      }else{
        crPlots(make_model("lm", formula))
      }
       if (input$transf_mul != "") {
         formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
         crPlots(make_model("lm", formula))
       }else{
         crPlots(make_model("lm", formula))
       }
    }
    
    else{
      if( input$radio == "All"){
        if (input$log_y) {
          formula <-  paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
          crPlots(make_model("lm", formula))
        }
        else{
          formula <- paste(input$mult_reg_y, "~.")
          crPlots(make_model("lm", formula))
        }
        if (!is.null(input$added_mul)){
          formula <-paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
          crPlots(make_model("lm", formula))
        }else{
          crPlots(make_model("lm", formula))
        }
        if (input$transf_mul != "") {
          formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
          crPlots(make_model("lm", formula))
        }else{
          crPlots(make_model("lm", formula))
        }
      }
     }
    }
        
    #make coeff plots
    if(input$radio.plots == "coefplot_ml"){
     if( input$radio != "All"){
          if (input$log_y) {
            if ( !is.null(input$log_x)) {
              form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
              formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
              coef.plot(model.coef(make_model("lm", formula)))
            }
            else{
              formula <- paste("log(",input$mult_reg_y,"+1)", "~", paste(input$mult_reg_x,collapse='+'))
              coef.plot(model.coef(make_model("lm", formula)))
            }
          }
          else{
            if ( !is.null( input$log_x)) {
              form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
              formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
              coef.plot(model.coef(make_model("lm", formula)))
            }else{
              formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+'))
              coef.plot(model.coef(make_model("lm", formula)))
            }
            
          }
          if (!is.null(input$added_mul)){
            formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
            coef.plot(model.coef(make_model("lm", formula)))
          }else{
            coef.plot(model.coef(make_model("lm", formula)))
          }
         if (input$transf_mul != "") {
           formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
           coef.plot(model.coef(make_model("lm", formula)))
         }else{
           coef.plot(model.coef(make_model("lm", formula)))
         }
    }
        
        else{
          if( input$radio == "All"){
            if (input$log_y) {
              formula <-  paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
              coef.plot(model.coef(make_model("lm", formula)))
            }
            else{
              formula <- paste(input$mult_reg_y, "~.")
              coef.plot(model.coef(make_model("lm", formula)))
            }
            if (!is.null(input$added_mul)){
              formula <-paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
              coef.plot(model.coef(make_model("lm", formula)))
            }else{
              coef.plot(model.coef(make_model("lm", formula)))
            }
            if (input$transf_mul != "") {
              formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
              coef.plot(model.coef(make_model("lm", formula)))
            }else{
              coef.plot(model.coef(make_model("lm", formula)))
            }
          }
        }
       }
     })
    }
  })
  
  output$GLM_Regresion <- renderPlot({
    
    input$goButtonglm
    
    if(input$goButtonglm > 0){
      
      isolate({
    
    #make added variable plots  
    if(input$radio.plots.glm == "avplot"){
     if( input$radioglm != "All"){

         if (input$glm_log_y) {
           if ( !is.null(input$log_x_glm)) {
             form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
             formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
           }
           else{
             formula <- paste("log(",input$glm_y,"+1)", "~", paste(input$glm_x,collapse='+'))
           }
         }else{
           if ( !is.null(input$log_x_glm)) {
             form_glm <-paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
             formula <- paste(input$glm_y, "~",form_glm ) 
           }
           else{
             formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
           }
        }
       if( (input$family!='neg.bin')){
         
         if (!is.null(input$added)) {
           formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
           avPlots(make_model("glm", formula))
         }
         else{
           avPlots(make_model("glm", formula))
         }
         if (input$transf != "") {
           formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
           avPlots(make_model("glm", formula))
         }else{
           avPlots(make_model("glm", formula))
         }
       }
       
       if( (input$family == 'neg.bin')) {
         if (!is.null(input$added)) {
           formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
           avPlots(make_model("glm.nb", formula))
         }
         else{
           avPlots(make_model("glm.nb", formula))
         }
         if (input$transf != "") {
           formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
           avPlots(make_model("glm.nb", formula))
         }else{
           avPlots(make_model("glm.nb", formula))
         }
       }
        else{
          NULL
       }
      }
    
    else{
      if( input$radioglm == "All"){
      
          if (input$glm_log_y) {
            formula <- paste("log(",input$glm_y,"+1)", "~.")
          }else{
            formula <- paste(input$glm_y, "~.")
          }
          
          if( (input$family!='neg.bin')){
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              avPlots(make_model("glm", formula))
            }
            else{
              avPlots(make_model("glm", formula))
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              avPlots(make_model("glm", formula))
            }else{
              avPlots(make_model("glm", formula))
            }
          }
          
          if( (input$family == 'neg.bin') ){
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              avPlots(make_model("glm.nb", formula))
            }
            else{
              avPlots(make_model("glm.nb", formula))
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              avPlots(make_model("glm.nb", formula))
            }else{
              avPlots(make_model("glm.nb", formula))
            }
          }
          else{
            NULL
          }
         }
        }
      }
    
    #make diagnostic plots
    if(input$radio.plots.glm == "resplot"){
     if( input$radioglm != "All"){
      
       if (input$glm_log_y) {
         if ( !is.null(input$log_x_glm)) {
           form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
           formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
         }
         else{
           formula <- paste("log(",input$glm_y,"+1)", "~", paste(input$glm_x,collapse='+'))
         }
       }else{
         if ( !is.null(input$log_x_glm)) {
           form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
           formula <- paste(input$glm_y, "~",form_glm ) 
         }
         else{
           formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
         }
       }
       
       if( (input$family!='neg.bin')){
         
         if (!is.null(input$added)) {
           formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
           glm.diag.plot(make_model("glm", formula))         
           }
         else{
           glm.diag.plot(make_model("glm", formula))
         }
         if (input$transf != "") {
           formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
           glm.diag.plot(make_model("glm", formula))
         }else{
           glm.diag.plot(make_model("glm", formula))
         }
       }
       
       if( (input$family == 'neg.bin')) {
         if (!is.null(input$added)) {
           formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
           glm.diag.plot(make_model("glm.nb", formula))
         }
         else{
           glm.diag.plot(make_model("glm.nb", formula))
         }
         if (input$transf != "") {
           formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
           glm.diag.plot(make_model("glm.nb", formula))
         }else{
           glm.diag.plot(make_model("glm.nb", formula))
         }
       }
        else{
          NULL
        }
      }
    else{
      if( input$radioglm == "All"){
          if (input$glm_log_y) {
            formula <- paste("log(",input$glm_y,"+1)", "~.")
          }else{
            formula <- paste(input$glm_y, "~.")
          }
        
          if (input$family!='neg.bin'){
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              glm.diag.plot(make_model("glm", formula))
            }
            else{
              glm.diag.plot(make_model("glm", formula))
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              glm.diag.plot(make_model("glm", formula))
            }else{
              glm.diag.plot(make_model("glm", formula))
            }
          }
          
          if( (input$family == 'neg.bin')){
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              glm.diag.plot(make_model("glm.nb", formula))
            }
            else{
              glm.diag.plot(make_model("glm.nb", formula))
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              glm.diag.plot(make_model("glm.nb", formula))
            }else{
              glm.diag.plot(make_model("glm.nb", formula))
            }
          }
          else{
            NULL
          }
      }
     }
    }
    
    #make crplots
      if(input$radio.plots.glm == "crplot"){
        if( input$radioglm != "All"){
          
          if (input$glm_log_y) {
            if ( !is.null(input$log_x_glm)) {
              form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
              formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
            }
            else{
              formula <- paste("log(",input$glm_y,"+1)", "~", paste(input$glm_x,collapse='+'))
            }
          }else{
            if ( !is.null(input$log_x_glm)) {
              form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
              formula <- paste(input$glm_y, "~",form_glm ) 
            }
            else{
              formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
            }
          }
          
          if( (input$family!='neg.bin')){
            
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              crPlots(make_model("glm", formula))
            }
              else{
              crPlots(make_model("glm", formula))
              }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              crPlots(make_model("glm", formula))
            }else{
              crPlots(make_model("glm", formula))
            }
          }
          
          if( (input$family == 'neg.bin')) {
            if (!is.null(input$added)) {
              formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
              crPlots(make_model("glm.nb", formula))
            }
            else{
              crPlots(make_model("glm.nb", formula))
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              crPlots(make_model("glm.nb", formula))
            }else{
              crPlots(make_model("glm.nb", formula))
            }
          }
        else{
          NULL
        }
    }
    else{
      if( input$radioglm == "All"){
          if (input$glm_log_y) {
            formula <- paste("log(",input$glm_y,"+1)", "~.")
          }else{
            formula <- paste(input$glm_y, "~.")
          }
        
        if( (input$family!='neg.bin')){
          if (!is.null(input$added)) {
            formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
            crPlots(make_model("glm", formula))
          }
          else{
            crPlots(make_model("glm", formula))
          }
          if (input$transf != "") {
            formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
            crPlots(make_model("glm", formula))
          }else{
            crPlots(make_model("glm", formula))
          }
        }
        
        if( (input$family == 'neg.bin') ){
          if (!is.null(input$added)) {
            formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
            crPlots(make_model("glm.nb", formula))
          }
          else{
            crPlots(make_model("glm.nb", formula))
          }
          if (input$transf != "") {
            formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
            crPlots(make_model("glm.nb", formula))
          }else{
            crPlots(make_model("glm.nb", formula))
          }
        }
          else{
            NULL
        }
      }
    }
  }
    
    
    #make coefplot 
    if(input$radio.plots.glm == "coefplot"){  
     if( input$radioglm != "All"){
       
       if (input$glm_log_y) {
         if ( !is.null(input$log_x_glm)) {
           form_glm <-paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
           formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
         }
         else{
           formula <- paste("log(",input$glm_y,"+1)", "~", paste(input$glm_x,collapse='+'))
         }
       }else{
         if ( !is.null(input$log_x_glm)) {
           form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
           formula <- paste(input$glm_y, "~",form_glm ) 
         }
         else{
           formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
         }
       }
       
       if( (input$family!='neg.bin')){
         
         if (!is.null(input$added)) {
           formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
           coef.plot(model.coef(make_model("glm", formula)))
         }
           else{
             coef.plot(model.coef(make_model("glm", formula)))
           }
         if (input$transf != "") {
           formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
           coef.plot(model.coef(make_model("glm", formula)))
         }else{
           coef.plot(model.coef(make_model("glm", formula)))
         }
         }
         
         if( (input$family == 'neg.bin')) {
           if (!is.null(input$added)) {
             formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
             coef.plot(model.coef(make_model("glm.nb", formula)))
           }
           else{
             coef.plot(model.coef(make_model("glm.nb", formula)))
           }
           if (input$transf != "") {
             formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
             coef.plot(model.coef(make_model("glm.nb", formula)))
           }else{
             coef.plot(model.coef(make_model("glm.nb", formula)))
           }
         }
        else{
          NULL
        }
    }
    
    else{
      if( input$radioglm == "All"){
          if (input$glm_log_y) {
            formula <- paste("log(",input$glm_y,"+1)", "~.")
          }else{
            formula <- paste(input$glm_y, "~.")
          }
        
        if( (input$family!='neg.bin')){
          if (!is.null(input$added)) {
            formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
            coef.plot(model.coef(make_model("glm", formula)))
          }
          else{
            coef.plot(model.coef(make_model("glm", formula)))
          }
          if (input$transf != "") {
            formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
            coef.plot(model.coef(make_model("glm", formula)))
          }else{
            coef.plot(model.coef(make_model("glm", formula)))
          }
        }
        
        if( (input$family == 'neg.bin') ){
          if (!is.null(input$added)) {
            formula <-paste0(formula,"+", paste0(input$added,collapse = "+"))
            coef.plot(model.coef(make_model("glm.nb", formula)))
          }
          else{
            coef.plot(model.coef(make_model("glm.nb", formula)))
          }
          if (input$transf != "") {
            formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
            coef.plot(model.coef(make_model("glm.nb", formula)))
          }else{
            coef.plot(model.coef(make_model("glm.nb", formula)))
          }
        }
        else{
            NULL
          }
        }
      }
    }
    
  })
 }
    
})
  
 ##--------------------------------------------------------######
 
  makereg <-reactive({ 
    input$goButtonmod
    
    if(input$goButtonmod > 0){
      
      isolate({
        
        if(input$radio == "All"){
          if (input$log_y) {
            
            formula <- paste(paste("log(",input$mult_reg_y,"+1)"), "~.")
            make_model("lm", formula)
            
          }else{
            formula <- paste(input$mult_reg_y, "~.")
            make_model("lm", formula) 
          }
          
          if(!is.null(input$added_mul)){
            formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
            make_model("lm", formula)
          }else{
            make_model("lm", formula)
          }
          if (input$transf_mul != "") {
            formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
            make_model("lm", formula)
          }else{
            make_model("lm", formula)
          }
          
        } 
        else{
          
          if( input$radio != "All"){
            
            if (input$log_y) {
              if ( !is.null(input$log_x)) {
                form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
                formula <- paste("log(",input$mult_reg_y,"+1) ~",form_x ) #paste(input$mult_reg_x,collapse='+')
                make_model("lm", formula) 
              }
              else{
                formula <- paste("log(",input$mult_reg_y,"+1) ~",paste(input$mult_reg_x,collapse='+') ) #
                make_model("lm", formula) 
              }
              
            }else{
              if ( !is.null( input$log_x)) {
                form_x <- paste( paste0("log(",input$log_x,"+1)",collapse = "+"),paste0(input$mult_reg_x,collapse = "+"),sep  = "+")
                formula <- paste(input$mult_reg_y, "~", form_x) #paste(input$mult_reg_x,collapse='+')
                make_model("lm", formula) 
              }else{
                formula <- paste(input$mult_reg_y, "~", paste(input$mult_reg_x,collapse='+')) #
                make_model("lm", formula) 
              }
            }
            
            if(!is.null(input$added_mul)){
              formula <- paste0(formula,"+", paste0(input$added_mul,collapse = "+"))
              make_model("lm", formula)
            }else{
              make_model("lm", formula)
            }
            if (input$transf_mul != "") {
              formula <- paste0(formula,"+",paste0(input$transf_mul,collapse = "+"))
              make_model("lm", formula)
            }else{
              make_model("lm", formula)
            }
          }
        }
      })
    } 
    else{
      return(cat('Must choose one or more predictor variables and hit Update \n'))
    }
  })
  
  output$mult_reg_text <- renderPrint({ 
    summary(makereg())
  })
  output$OutputFile <- downloadHandler(
    
    filename = function() {
      paste0("Predictions", "_",Sys.time(),".csv")
    },
    content = function(file) {
      write.csv( predict(makereg(),newdata=data(),type = "response"), file,
                row.names = FALSE)
    }
  )

  output$download <- renderUI({
    if(!is.null(makereg())) {
      downloadButton('OutputFile', 'Download Predictions')
    }
  })
  
##########################################
  makeglm <-reactive({
    
    input$goButtonglm
    
    if(input$goButtonglm > 0){
      
      isolate({
        
        if( input$radioglm != "All"){
          
          if( (input$family != 'neg.bin') ){
            if (input$glm_log_y) {
              if ( !is.null(input$log_x_glm)) {
                
                form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
                formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
              }
              else{
                formula <- paste("log(",input$glm_y,"+1) ~",paste(input$glm_x,collapse='+')) #
              }

            }else{
              if ( !is.null(input$log_x_glm)) {
                form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
                formula <- paste(input$glm_y, "~",form_glm ) 
              }
              else{
                formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
              }
            }

            if (input$offset_radio=='Yes') {
              formula <- paste0(formula,"+",paste0("offset(log(",paste0(input$glm_x_offset,collapse = "*"),"+1))"))
              make_model("glm", formula)
              }
              else{
                make_model("glm", formula) 
              }

            if(!is.null(input$added)){
              formula <- paste0(formula,"+", paste0(input$added,collapse = "+"))
              make_model("glm", formula)
            }else{
              make_model("glm", formula)
            }
            
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              make_model("glm", formula)
            }else{
              make_model("glm", formula)
            }
          }
          
          else if( (input$family == 'neg.bin') ){
            if (input$glm_log_y) {
              if ( !is.null(input$log_x_glm)) {
                form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
                formula <- paste("log(",input$glm_y,"+1) ~",form_glm ) 
              }
              else{
                formula <- paste("log(",input$glm_y,"+1) ~",paste(input$glm_x,collapse='+'))
              }

            }else{
              if ( !is.null(input$log_x_glm)) {
                form_glm <- paste( paste0("log(",input$log_x_glm,"+1)",collapse = "+"),paste0(input$glm_x,collapse = "+"),sep  = "+")
                formula <- paste(input$glm_y, "~",form_glm ) 
              }
              else{
                formula <- paste(input$glm_y, "~", paste(input$glm_x,collapse='+'))#
              }
            }
            if (input$offset_radio=='Yes') {
              formula <- paste0(formula,"+",paste0("offset(log(",paste0(input$glm_x_offset,collapse = "*"),"+1))"))
              make_model("glm.nb", formula)
            }else{
              make_model("glm.nb", formula) 
            }
            
            if(!is.null(input$added)){
              formula <- paste0(formula,"+", paste0(input$added,collapse = "+"))
              make_model("glm.nb", formula)
            }else{
              make_model("glm.nb", formula)
            }
            if (input$transf != "") {
              formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
              make_model("glm.nb", formula)
            }else{
              make_model("glm.nb", formula)
            }
          }
        } 
        
        else{
          
          if( input$radioglm == "All"){
            
            if( (input$family != 'neg.bin')){
              if (input$glm_log_y) {
                formula <- paste("log(",input$glm_y,"+1) ~.")
              }else{
                formula <- paste(input$glm_y, "~.")
              }
              if (input$offset_radio=='Yes') {
                formula <- paste0(formula,"+",paste0("offset(log(",paste0(input$glm_x_offset,collapse = "*"),"+1))"))
                make_model("glm", formula)
              }else{
                make_model("glm", formula)
              }
              if(!is.null(input$added)){
                formula <- paste0(formula,"+", paste0(input$added,collapse = "+"))
                make_model("glm", formula)
              }else{
                make_model("glm", formula)
              }
              if (input$transf != "") {
                formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
                make_model("glm", formula)
              }else{
                make_model("glm", formula)
              }
            }
            
            else if( (input$family == 'neg.bin')  ){
              if (input$glm_log_y) {
                formula <- paste("log(",input$glm_y,"+1) ~.")
              }else{
                formula <- paste(input$glm_y, "~.")
              }
              if (input$offset_radio=='Yes') {
                formula <- paste0(formula,"+",paste0("offset(log(",paste0(input$glm_x_offset,collapse = "*"),"+1))"))
                make_model("glm.nb", formula)
              }else{
                make_model("glm.nb", formula)
              }
              if(!is.null(input$added)){
                formula <- paste0(formula,"+", paste0(input$added,collapse = "+"))
                make_model("glm.nb", formula)
              }else{
                make_model("glm.nb", formula)
              }
              if (input$transf != "") {
                formula <- paste0(formula,"+",paste0(input$transf,collapse = "+"))
                make_model("glm.nb", formula)
              }else{
                make_model("glm.nb", formula)
              }
            }
          }
        }
        
      })
      
    } else{
      return(cat('Must choose one or more predictor variables and hit Update \n'))
    }
    
    
  })
  
  output$glm_text <- renderPrint({ 
    summary(makeglm())
  })
  output$OutputFileglm <- downloadHandler(
    
    filename = function() {
      paste0("Predictions", "_",Sys.time(),".csv")
    },
    content = function(file) {
      if(input$imput_method !="None"){
        data <-  reactive ({ df_imp() }) 
      }
      write.csv( predict(makeglm(),newdata=data(),type = "response"), file,
                 row.names = FALSE)
    }
  )
  
  output$downloadglm <- renderUI({
    if(!is.null(makeglm())) {
      downloadButton('OutputFileglm', 'Download Predictions')
    }
  })
  

####################################################
  #creating supervised
  makesuper <- reactive({
    
    if(input$help_page=='rfs'){
      make_model <- function(model_type, formula,subset=NULL,...) {
        if(input$imput_method !="None"){
          data <-  reactive ({ df_imp() }) 
        }
        do.call(model_type, args = list(formula = formula,data = quote(data()),subset=train,...))
      }
      
      input$goButtonrf
      if(input$goButtonrf > 0){
        isolate({
          
          if(input$imput_method !="None"){
            data <-  reactive ({ df_imp() }) 
          }
          
          train <- sample(input$rf_train)
          df.rf<-as.data.table(data())
          rf.test=df.rf[,input$rf_y][-train]
          
          if(input$radio_ml == "All"){    
            formula <- as.formula(paste(input$rf_y, "~."))
          } 
          else{
            formula <- as.formula(paste(input$rf_y ,"~",paste(input$rf_x, collapse= "+")))            
          }
          rf_mod <- make_model("randomForest", formula,na.action=na.omit,importance =TRUE)
          
        })     
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }
      return(rf_mod)
    }
    
    if(input$help_page=='trees'){
      make_model <- function(model_type, formula,subset=NULL,...) {
        if(input$imput_method !="None"){
          data <-  reactive ({ df_imp() }) 
        }
        do.call(model_type, args = list(formula = formula,data = quote(data()),subset=train_tr,...))
      }
      
      input$goButtontree
      if(input$goButtontree > 0){ 
        isolate({
          if(input$imput_method !="None"){
            data <-  reactive ({ df_imp() }) 
          }
          
          train_tr <- sample(input$tree_train)
          df_tree<-as.data.table(data())
          tree_test=df_tree[-train_tr,]
          
          if(input$radiotree == "All"){
            formula <- as.formula(paste(input$tree_y, "~.")) 
          } 
          else{  
            formula <- as.formula(paste(input$tree_y ,"~",paste(input$tree_x, collapse= "+")))
          }
            tree_mod <- make_model("ctree", formula)
              
        })     
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }  
      return(tree_mod)
    }
    
    if(input$help_page=='svms'){
      
      input$goButtonsvm
      if(input$goButtonsvm > 0){
        isolate({
          
          if(input$radiosvm== "All"){    
            formula <- as.formula(paste(input$svm_y, "~.")) 
          } 
          else{
            formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
          }
          if(input$radiosvmkernel== "Linear"){
            sv <- summary(make_model("svm", formula,kernel ="linear", cost =10,scale =FALSE))
            }
          else if(input$radiosvmkernel== "Polynomial"){
            sv <- summary(make_model("svm", formula,kernel ="polynomial", cost =10,scale =FALSE))
            }
          else if(input$radiosvmkernel== "Radial basis"){
           sv <- summary(make_model("svm", formula,kernel ="radial", cost =10,scale =FALSE))
                
            }
          else if(input$radiosvmkernel== "Sigmoid"){
            sv <- summary(make_model("svm", formula,kernel ="sigmoid", cost =10,scale =FALSE))
                 
            }       
        })      
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }
      return(sv)
    }
    if (input$help_page=='cubist') {
      
      
      input$goButtoncubist
      if(input$goButtoncubist > 0){
        isolate({
          if(input$imput_method !="None"){
            data <-  reactive ({ df_imp() }) 
          }
          
          if(input$radiocubist == "All"){    
            x1 <- data() %>%
              dplyr::select(., -one_of(input$cubist_y))
              #dplyr::select(input$cubist_x)
            y1 <-data()[,input$cubist_y]
          } 
          else{
            x1 <- data() %>%
              dplyr::select(input$cubist_x)
            y1 <- data()[,input$cubist_y]        
          }
          
          cubist_mod <- summary(cubist(x=x1,y=y1,committees=input$committees))
          v <- varImp(cubist(x=x1,y=y1,committees=input$committees))
        })     
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }
      return(list(cubist_mod,Variable_Importance=v))
      
      
    }
    if(input$help_page=='nnet'){
      make_model <- function(model_type, formula,subset=NULL,...) {
        if(input$imput_method !="None"){
          data <-  reactive ({ df_imp() }) 
        }
        do.call(model_type, args = list(formula = formula,data = quote(data()),...))
      }
      
      input$goButtonnet
      if(input$goButtonnet > 0){
        isolate({
          
          if(input$imput_method !="None"){
            data <-  reactive ({ df_imp() }) 
          }
          
          train_net <- sample(input$net_train)
          df.net<-as.data.table(data())
          net.test=df.net[,input$net_y][-train_net]
          
          if(input$radio_net == "All"){    
            formula <- as.formula(paste(input$net_y, "~",paste(names(data())[!names(data()) %in% input$net_y], collapse = " + ")))
          } 
          else{
            formula <- as.formula(paste(input$net_y ,"~",paste(input$net_x, collapse= "+")))            
          }
            net_mod <- make_model("nnet", formula, size=1)
          
        })     
      } 
      else{
        return(cat('Must choose one or more predictor variables and hit Update Model \n'))
      }
      return(net_mod)
             
    }
    
  })

  
  output$pred_s <- renderPrint({ 
    makesuper()
  })
  output$OutputFilerf <- downloadHandler(
    
     filename = function() {
      paste0("Predictions", "_",Sys.time(),".csv")
    },
     content = function(file) {
       if(input$imput_method !="None"){
         data <-  reactive ({ df_imp() }) 
       }
      if (input$help_page=='rfs') {
      train <- sample(input$rf_train)
      df.rf<-as.data.table(data())
      write.csv( predict(makesuper(),newdata =df.rf[-train,]), file,
                 row.names = FALSE)
      }
      if(input$help_page=='trees'){
        train_tr <- sample(input$tree_train)
        df_tree<-as.data.table(data())
        tree_test=df_tree[-train_tr,]
        
        write.csv( predict(makesuper(),newdata =tree_test,type='response'), file,
                   row.names = FALSE)
      }
       if(input$help_page=='svms'){
         if(input$radiosvm== "All"){    
           formula <- as.formula(paste(input$svm_y, "~.")) 
         } 
         else{
           formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
         }
        if(input$radiosvmkernel== "Linear"){
         write.csv(predict(make_model("svm", formula,kernel ="linear", cost =10,scale =FALSE),newdata=data()), file,
                   row.names = FALSE)
        }
        else if(input$radiosvmkernel== "Polynomial"){
         write.csv(predict(make_model("svm", formula,kernel ="polynomial", cost =10, scale =FALSE),newdata=data()), file,
                   row.names = FALSE)
        }
        else if(input$radiosvmkernel== "Radial basis"){
         write.csv( predict(make_model("svm", formula,kernel ="radial", cost =10,scale =FALSE),newdata=data()), file,
                    row.names = FALSE)
        }
        else if(input$radiosvmkernel== "Sigmoid"){
         write.csv(predict(make_model("svm", formula,kernel ="sigmoid", cost =10,scale =FALSE),newdata=data()), file,
                   row.names = FALSE)
        }  
       }
       if (input$help_page=='cubist') {
         if(input$radiocubist == "All"){    
           x1 <- data() %>%
             dplyr::select(., -one_of(input$cubist_y))
           #dplyr::select(input$cubist_x)
           y1 <-data()[,input$cubist_y]
         } 
         else{
           x1 <- data() %>%
             dplyr::select(input$cubist_x)
           y1 <- data()[,input$cubist_y]        
         }
         write.csv( predict(cubist(x=x1,y=y1,committees=input$committees),newdata =data()), file,
                    row.names = FALSE)
       }
      
        if (input$help_page=='nnet') {
             
           train_net <- sample(input$net_train)
           df.net<-as.data.table(data())
           net.test=df.net[,input$net_y][-train_net]
           if(input$radio_net == "All"){    
             formula <- as.formula(paste(input$net_y, "~",paste(names(data())[!names(data()) %in% input$net_y], collapse = " + ")))
           } 
           else{
             formula <- as.formula(paste(input$net_y ,"~",paste(input$net_x, collapse= "+")))            
           }
             write.csv( predict(nnet(formula,size=1),newdata=df.net[-train_net,],type='class'), file,row.names = FALSE)
           
          }
    }
  )
  
  output$downloadrf <- renderUI({
    if(!is.null(makesuper())) {
        downloadButton('OutputFilerf', 'Download Predictions')
    }
  })
  

###########################################################  
  #plotting supervised
  output$s.Plot <- renderPlot({ 
      
      if(input$help_page=='rfs'){
        
        input$goButtonrf
        if(input$goButtonrf > 0){
          isolate({
            
            # plot random forest model
            
            if(input$radio == "All"){ 
              formula <- as.formula(paste(input$rf_y, "~."))
            } 
            else{ 
              formula <- as.formula(paste(input$rf_y ,"~ ",paste(input$rf_x, collapse= "+")))
            }
            
            if(input$plot_rf_var=='rfplot'){
              plot(make_model("randomForest", formula,subset=train,na.action=na.omit,importance =TRUE),main="")
            }
            else if(input$plot_rf_var=='varimpplot'){
              #plot importance measures
              varImpPlot(make_model("randomForest", formula,subset=train,na.action=na.omit,importance =TRUE),main="Dotchart of variable importance as measured by a Random Forest")
            }
          })
        }
      }
      
      
      if(input$help_page=='trees'){
        
        input$goButtontree
        if(input$goButtontree > 0){
          isolate({
            
            # plot tree model
            if(input$radiotree == "All"){
              formula <- as.formula(paste(input$tree_y, "~."))
            } 
            else{
              formula <- as.formula(paste(input$tree_y ,"~ ",paste(input$tree_x, collapse= "+")))
            }
            plot(makesuper())
           
          })
        }
      }
      
      if(input$help_page=='svms'){
        
        input$goButtonsvm
        if(input$goButtonsvm > 0){
          isolate({
            if(input$imput_method !="None"){
              data <-  reactive ({ df_imp() }) 
            }
            
            # plot svm model
            if(input$radiosvm == "All"){
              
              formula <- as.formula(paste(input$svm_y, "~."))
            } 
            else{
              
              formula <- as.formula(paste(input$svm_y ,"~ ",paste(input$svm_x, collapse= "+")))
            }
            formula_plot <- as.formula(paste(input$svm_y_plot ,"~ ",paste(input$svm_x_plot, collapse= "+")))
            
            if(input$radiosvmkernel== "Linear"){
              plot(make_model("svm", formula,kernel ="linear", cost =10,
                              scale =FALSE),data=data(),formula=formula_plot) #,formula=formula_plot
              }
            else if(input$radiosvmkernel== "Polynomial"){
              plot(make_model("svm", formula,kernel ="polynomial", cost =10,
                              scale =FALSE),data=data(),formula=formula_plot)
              }
            else if(input$radiosvmkernel== "Radial basis"){
              plot(make_model("svm", formula,kernel ="radial", cost =10,
                              scale =FALSE),data=data(),formula=formula_plot)
              }
            else if(input$radiosvmkernel== "Sigmoid"){
              plot(make_model("svm", formula,kernel ="sigmoid", cost =10,
                              scale =FALSE),data=data(),formula=formula_plot)
              } 
          })
        }
      }
    
      if (input$help_page=='cubist') {
        
        input$goButtoncubist
        if(input$goButtoncubist > 0){
          isolate({
            
            if(input$imput_method !="None"){
              data <-  reactive ({ df_imp() }) 
            }
            
            if(input$radiocubist == "All"){    
              x1 <- data() %>%
                dplyr::select(., -one_of(input$cubist_y))
              #dplyr::select(input$cubist_x)
              y1 <-data()[,input$cubist_y]
            } 
            else{
              x1 <- data() %>%
                dplyr::select(input$cubist_x)
              y1 <- data()[,input$cubist_y]        
            }
            if (input$plot_cubist_var=='splits') {
              print(dotplot(cubist(x=x1,y=y1,committees=input$committees),what='splits'))
            }
            else if(input$plot_cubist_var=='coeffs'){
              print(dotplot(cubist(x=x1,y=y1,committees=input$committees),what='coefs'))
            }
            else if(input$plot_cubist_var=='cv'){
              grid <- expand.grid(committees = c(1, 10, 50, 100),
                                  neighbors = c(0, 1, 5, 9))
              cub_tuned <- train(
                x = x1,
                y = y1,
                method = "cubist",
                tuneGrid = grid,
                trControl = trainControl(method = "cv")
              )
              print(ggplot(cub_tuned))
            }
            
          })     
        }
      }
      if (input$help_page=='nnet') {
        
        input$goButtonnet
        if(input$goButtonnet > 0){
          isolate({
            
            if(input$imput_method !="None"){
              data <-  reactive ({ df_imp() }) 
            }
            
            if(input$radio_net == "All"){    
              formula <- as.formula(paste(input$net_y, "~",paste(names(data())[!names(data()) %in% input$net_y], collapse = " + ")))
            } 
            else{
              formula <- as.formula(paste(input$net_y ,"~",paste(input$net_x, collapse= "+")))            
            }
            plotnet(make_model("nnet", formula, size=1))
          })     
        }
      }
    
    
  })
      
#######################################
  #PCA,Kmeans, Hierarchical Clustering
  
  output$pred_un <- renderPrint({ 
    
    if(input$imput_method !="None"){
      data <-  reactive ({ df_imp() }) 
    }

      df<-sapply(data(),as.numeric)
      
      #PCA
      if(input$help_page_un=='pca_un'){
        
        input$goButtonpca
        if(input$goButtonpca > 0){
          isolate({
            
            df <- df[,which(colSums(df != 0) != 0)]
            df <- df[ , which(apply(df, 2, var) != 0)]
            df[ , !(names(df) %in% input$response_var_pca)]
            pcaout<-prcomp(df,scale.=TRUE)
            pr.var<-pcaout$sdev ^2
            pve<-pr.var/sum(pr.var)
            res.pca <- PCA(df, graph = FALSE)
            res.desc <- dimdesc(res.pca, axes = c(1,2))
            
            
            print(list(Proportion_variance_explained=pve,top_significant_variables_dimension_1=res.desc$Dim.1,top_significant_variables_dimension_2=res.desc$Dim.2),row.names = FALSE)      
          })     
        }
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
      
      #Kmeans
      if(input$help_page_un=='kmeans_un'){
        
        input$goButtonkmeans
        if(input$goButtonkmeans > 0){
          isolate({
            
            print(list("Cluster centres"=clusters()$centers,"Total Sum of Squares"=clusters()$totss,
                       "Within-Cluster Sum of Squares"=clusters()$withinss,
                       "Between-Cluster Sum of Squares"=clusters()$betweenss))  
          })     
        }
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
      
      #clustering
      if(input$help_page_un=='clust_un'){
        
        input$goButtonclust
        if(input$goButtonclust > 0){ 
          isolate({
            
            if(input$clust_method=='complete'){
              clust_mod<-hclust(dist(scale(df)), method ="complete")
            }
            if(input$clust_method=='ward'){
              clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
            }
            if(input$clust_method=='single'){
              clust_mod<-hclust(dist(scale(df)), method ="single")
            }
            if(input$clust_method=='average'){
              clust_mod<-hclust(dist(scale(df)), method ="average")
            }
            if(input$clust_method=='median'){
              clust_mod<-hclust(dist(scale(df)), method ="median")
            }
            if(input$clust_method=='centroid'){
              clust_mod<-hclust(dist(scale(df)), method ="centroid")
            }
            print(clust_mod)
            
          })     
        } 
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
      
    
  }) 
  
########################################
  #Plotting pca,clustering
  output$un.Plot <- renderPlot({ 
    
    if(input$imput_method !="None"){
      data <-  reactive ({ df_imp() }) 
    }
      df<-sapply(data(),as.numeric)
      #PCA
      if(input$help_page_un=='pca_un' ){ 
        
        input$goButtonpca
        if(input$goButtonpca > 0){
          isolate({
            
            df <- df[,which(colSums(df != 0) != 0)]
            df <- df[ , which(apply(df, 2, var) != 0)]
            df[ , !(names(df) %in% input$response_var_pca)]
            
            pca <- prcomp(df,scale.=TRUE)
            var_exp <- pca$sdev ^ 2
            pve <- var_exp/sum(var_exp)
            pc_var <- data.table(
              "pc" = paste0("PC", seq_along(pca$sdev)),
              "var" = var_exp,
              "pct" = var_exp / sum(var_exp),
              "cum_pct" = cumsum(var_exp) / sum(var_exp)
            )
            
            min_cum_pct <- min(pc_var$cum_pct)
            pc_var2 <- pc_var[cum_pct <= max(1, min_cum_pct)]
            
            
            if(input$plot_pca_cum=='pcaplot'){
              varexp_plot <- ggplot(pc_var2, aes(x = reorder(pc, pct), y = pct)) +
                geom_bar(stat = "identity") +
                geom_text(aes(label = percent(cum_pct)), color = "white", hjust = 1.1) +
                scale_y_continuous(labels = percent) +
                coord_flip() +
                ggtitle(
                  label = "% Variance Explained By Principle Components",
                  subtitle = "Note: White texts indicate cumulative % explained variance"
                ) +
                labs(x = "Principle Components", y = "% Variance Explained")
              print(varexp_plot)
              }
            else if(input$plot_pca_cum=='biplot'){
              print(ggbiplot(pca,ellipse=TRUE,  labels=rownames(df)))
            }else{
              if(input$contrib_vars!=""){
              res.pca <- PCA(df, graph = FALSE)
              p1 <- fviz_contrib(res.pca, choice = "var", axes = 1:input$contrib_vars, top = 10)+theme_gray()+theme(axis.text.x = element_text(angle=90, hjust = 1,vjust=0.4,size=8))
              p2 <- fviz_pca_var(res.pca, col.var="contrib") +
                scale_color_gradient2(low="white", mid="blue", 
                                      high="red", midpoint=50) + theme_minimal()
              print(grid.arrange(p1, p2, ncol=1))
              }
            }
            
            
            
          })
      }
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
      
      #Kmeans
      if(input$help_page_un=='kmeans_un' ){ 
        
        input$goButtonkmeans
        if(input$goButtonkmeans > 0){
          isolate({
            
            if(input$imput_method !="None"){
              data <-  reactive ({ df_imp() }) 
            }
            
            x <- tapply(datakmeans()[,input$kmeans_x],clusters()$cluster,mean)
            y <- tapply(datakmeans()[,input$kmeans_y],clusters()$cluster,mean)
            kcenters <- data.frame(x,y)
            p1 <- ggplot(data.frame(cbind(datakmeans(),cluster=as.factor(clusters()$cluster))),aes_string(x=input$kmeans_x,y=input$kmeans_y,color = "factor(cluster)"))  +
              geom_point() +labs(colour = "Cluster")+
              geom_point(data=data.frame(cbind(kcenters, cluster=as.factor(1:nrow(kcenters)))),aes(x,y, colour=cluster),pch=4,size=4,stroke = 3)
            #print(p1)
            
            res.kmeans <- lapply(1:10, function(i) {
              kmeans(datakmeans(), centers = i)
            })
            
            ## SS for each cluster (1 cluster to 10 clusters)
            lapply(res.kmeans, function(x) x$withinss)
            ## Sum up SS
            res.within.ss <- sapply(res.kmeans, function(x) sum(x$withinss))
            
            p2 <- ggplot(data.frame(cluster = 1:10, within.ss = res.within.ss), aes(cluster, within.ss)) +
              geom_point() + geom_line() +
              scale_x_continuous(breaks = 0:10)
            
            print(grid.arrange(p1,p2,ncol=1,nrow=2))
            
          })
        }
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
      
      #Hierarchical Clustering
      if(input$help_page_un=='clust_un' ){ 
        
        input$goButtonclust
        if(input$goButtonclust > 0){
          isolate({
            
            if(input$clust_method=='complete'){
              clust_mod<-hclust(dist(scale(df)), method ="complete")
            }
            if(input$clust_method=='ward'){
              clust_mod<-hclust(dist(scale(df)), method ="ward.D2")
            }
            if(input$clust_method=='single'){
              clust_mod<-hclust(dist(scale(df)), method ="single")
            }
            if(input$clust_method=='average'){
              clust_mod<-hclust(dist(scale(df)), method ="average")
            }
            if(input$clust_method=='median'){
              clust_mod<-hclust(dist(scale(df)), method ="median")
            }
            if(input$clust_method=='centroid'){
              clust_mod<-hclust(dist(scale(df)), method ="centroid")
            }
            if(input$plot_clust_var=='dendro'){
              plot(clust_mod, main ="Hierarchical Clustering with Scaled Features")
              if(input$clust_tree>1) rect.hclust(clust_mod, k = input$clust_tree, border = 2:5)
            }
            else if(input$plot_clust_var=='clustplot'){
              df <- df[,which(colSums(df != 0) != 0)]
              df <- df[ , which(apply(df, 2, var) != 0)]
              clust_mod<- hcut(dist(scale(df)), k = input$clust_tree, hc_method = input$clust_method)
              fviz_cluster(clust_mod,data = df, cluster = input$clust_tree, geom = "point")
            }
  
          })
        }
        else{
          return(cat('Select from Side Panel also \n'))
        }   
      }
     
    })  
  
      
  output$help_text <- renderText({ 
    
    if(input$help_page_g=='gen.help'){
      
      'Data Visualization and Modelling application using an uploaded csv.
      
      Part of the data and data summary is shown immediately after the uploaded data set(by clicking on the specified tabs).
      The user is able to choose the number of rows to appear as well as the csv format and if header will appear or not.
      The user is able to choose between plotting and modelling by "clicking" the boxes.' 
    }
    
    else if(input$help_page_g=='plot.help'){
      
      
      'Plotting consists of histogram,boxplot, barplot, density plot, line plot and point plot with the associated colour, size and faceting capabilities.
      For example when histogram is chosen , x axis is requested and then there are options for filling and faceting. The user chooses the 
      appropriate data columns.
      Barplot has one more option that of "identity", when the user "click" the box,the heights of the bars represent the specific y-value in the data, 
      and map a value to the y aesthetic
      The same exist with line and point plots with size option(the size of point or line) as an addition.'
    }
    
    else if(input$help_page_g=='mod.help'){
      
    'Modelling consists of Multiple regression Generalized Linear Model(GLM) and Machine Learning algorithms. By "clicking" the respective box
     more options are available. 
    
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
     There are diagnostic plots as in multiple regression plus a different residual plot with the following plots: 
     The plot on the top left is a plot of the jackknife deviance residuals against the fitted values.
     The plot on the top right is a normal QQ plot of the standardized deviance residuals. 
     The dotted line is the expected line if the standardized residuals are normally distributed, i.e. it is the line with intercept 0 and slope 1.
     The bottom two panels are plots of the Cook statistics. On the left is a plot of the Cook statistics against the standardized leverages. 
     In general there will be two dotted lines on this plot. The horizontal line is at 8/(n-2p) where n is the number of observations and p 
     is the number of parameters estimated. Points above this line may be points with high influence on the model. The vertical line is at 
     2p/(n-2p) and points to the right of this line have high leverage compared to the variance of the raw residual at that point. 
     If all points are below the horizontal line or to the left of the vertical line then the line is not shown.
     The final plot again shows the Cook statistic this time plotted against case number enabling us to find which observations are influential.
     
     and a coefplot, that is the coefficients and standard errors from a fitted model.
     ' 
    }           
    
    }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

