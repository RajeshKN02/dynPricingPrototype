#'@title: Fitting Buying Probabilities Class

#' Cullen_and_Grey_plot
#'@parameter data: dataset containing prices 
#'@parameter product_type: Light, Classic or Flex
#'@parameter boot: number of boots, default = 500
#'@return: Cullen and Grey Plot

FIT_Cullen_and_Grey_Plot <- function(data, prod_split= TRUE, product_type, boot=500){
  
  data_split <- split(data, data$DOW)
  
  DOW <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
  
  cullen.par <- par(mfrow = c(2,4))
  
  if(prod_split == TRUE){
    
    for (i in (1:length(DOW))){
      
      descdist(data_split[[i]]$PRICE[which(data_split[[i]]$PRODUCT_TYPE == product_type)], 
               discrete = FALSE, boot = 500)
      
      title(main="")
      
      text(1, 2, DOW[i])
      
      }
  
  } else{
    
    for (i in (1:length(DOW))){
      
      descdist(data_split[[i]]$PRICE, discrete = FALSE, boot = 500)
      
      title(main="")
      
      text(1, 2, DOW[i])
      
    }
  }
  
  par(cullen.par)
  
}


#' FIT_Price_Distr_Function
#'@parameter data: dataset containing prices 
#'@parameter product_type: Light, Classic or Flex
#'@return: list with parameters of Distribution functions and saves the graphs
#'for the best distributions

FIT_Price_Distr_Function <- function(data, product_type, method = "AIC"){
  
  data_split <- split(data, data$DOW)
  
  Results <- list()
  
  DOW <- unique(data$DOW)
  
  fitnames <- c("norm", "weibull", "gamma", "lnorm", "unif", 
                "exp")
                #"llogis", "Burr", "Pareto")
  
  for (i in (1:length(DOW))){
    
    dataToFit <- data_split[[i]]$PRICE[which(data_split[[i]]$PRODUCT_TYPE == product_type)]
  
    fits <- list(
                 norm = fitdist(dataToFit, "norm"),
                 weibull = fitdist(dataToFit, "weibull"),
                 gamma = fitdist(dataToFit, "gamma"),
                 lnorm = fitdist(dataToFit, "lnorm"),
                 unif = fitdist(dataToFit, "unif"),
                 exp = fitdist(dataToFit, "exp")
                 #log = fitdist(dataToFit, "llogis"),
                 #burr = fitdist(dataToFit, "burr"),
                 #pareto = fitdist(dataToFit, "pareto")
          )
    
    if(method == "AIC"){
    
      fits_aic <- list(
                      norm = fitdist(dataToFit, "norm")$aic,
                      weibull = fitdist(dataToFit, "weibull")$aic,
                      gamma = fitdist(dataToFit, "gamma")$aic,
                      lnorm = fitdist(dataToFit, "lnorm")$aic,
                      unif = fitdist(dataToFit, "unif")$aic,
                      exp = fitdist(dataToFit, "exp")$aic
                      #log = fitdist(dataToFit, "llogis")$aic,
                      #burr = fitdist(dataToFit, "burr")$aic,
                      #pareto = fitdist(dataToFit, "pareto")$aic
                )
    
      # Choose distribution with minimum AIC
    
      best_Fit <- names(which.min(fits_aic))
    
    } else{
    
      fits_ks <- gofstat(fits, fitnames = fitnames)
    
      # Choose distribution with minimum K-S statistic
    
      best_Fit <- names(which.min(fits_ks$ks))
    
    }
  
    # Correct "normal" output
  
    if(best_Fit == "normal"){
    
      best_Dist <- fitdist(dataToFit, "norm")
    
    }else best_Dist <- fitdist(dataToFit, best_Fit)
    
    # Save goodnes of fitting report for DOW = i
    # Work on this!...
    #GoF.report <- gofstat(fits, fitnames = fitnames)
    
    #GoF.exportable <- list(AIC = GoF.report$AIC, 
    #                       "Kolgomorov Smirnov Statistics" = GoF.report$ks)
    
    #df.GoF <- data.frame(matrix(unlist(GoF.exportable), nrow = 2, byrow = T))
    
    #SaveinCSVFile(df.GoF, 
    #              paste0("BuyingProb_GoF_", product_type, "_", i))
    
    # Save best distribution in a list
  
    Results[[i]] <- list(
                        Product = product_type,
                        DOW = unique(data_split[[i]]$DOW),
                        Best_Distribution = best_Dist$distname,
                        Parameters = best_Dist$estimate
                        )
    
    # Save a plot of the best distribution
    # It does not work!
    pdf(file = paste0("Output/Plot_BP_", product_type, "_", i, ".pdf"), 
        width = 8, height = 6)
    plot(best_Dist)
    dev.off
  
  }
  
  return(Results)

}

