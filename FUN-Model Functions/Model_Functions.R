#' Model functions.R

MODEL_G <- function(p){
  
  G <- exp(-p/100)
  
  return(G)
}

MODEL_G_accumulated <- function(p){
  G <- 1 - G(p)
}

#'The number of events in any interval of length delta_t is Poisson distributed 
#'with mean (lambda*delta_t) and n remaining seats

MODEL_prob_n_arrivals_poisson <- function(lambda, delta_t, n){
  
  accumulated_prob_sum <- c()
  
  for (i in 1:n){
    #' Poisson process
    accumulated_prob_sum[i] <- exp(-lambda * delta_t)*(lambda * delta_t)^i / factorial(i)
  } 
  
  prob <- sum(accumulated_prob_sum)
  
  return(prob)

}

MODEL_Revenue <- function(p){
  
  Revenue <- p * G(p)
  
  return(Revenue)
}

MODEL_Total_Revenue <- function(p, lambda, delta_t, n){
  
  partial <- c()
  for (i in 1:n){
  partial[i] <- p * G(p) * i * exp(-lambda*delta_t) * (lambda*delta_t)^i/factorial(i)
  
  }
  total <- sum(partial)
  
  return(total)
}

MODEL_Total_Revenue_2 <- function(p,lambda,delta_t,n){
  partial <- c()
  for (i in 1:n){
    partial[i] <- p*G(p)*i*exp(-lambda*delta_t)*(lambda*delta_t)^i/factorial(i)
  }
  total <- sum(partial)
  
  return(total)
}

MODEL_plot_reserve_prices_k<-function(data,type,all=FALSE){
  subset_product<-data[data$PRODUCT_TYPE==type,]
  ecdf_subset<-ecdf(subset_product$PRICE)
  if(all){
    subset_product_classic<-data[data$PRODUCT_TYPE=="Classic",]
    ecdf_subset_classic<-ecdf(subset_product_classic$PRICE)
    subset_product_light<-data[data$PRODUCT_TYPE=="Light",]
    ecdf_subset_light<-ecdf(subset_product_light$PRICE)
    subset_product_flex<-data[data$PRODUCT_TYPE=="Flex",]
    ecdf_subset_flex<-ecdf(subset_product_flex$PRICE)
    max_1<-max(max(subset_product_light$PRICE), max(subset_product_light$PRICE), max(subset_product_light$PRICE))
    min_1<-min(min(subset_product_light$PRICE), min(subset_product_light$PRICE), min(subset_product_light$PRICE))
    curve(1-ecdf_subset_classic(x), min_1, max_1, col="red",xlab="p",ylab="G(p)")
    curve(1-ecdf_subset_light(x), min_1, max_1, col="blue",add=TRUE)
    curve(1-ecdf_subset_flex(x), min_1, max_1, col="green",add=TRUE)
    #legend(200,0.95, # places a legend at the appropriate place 
     #      c('Light','Classic','Flex'), # puts text in the legend
      #     lty=c(1,1,1), # gives the legend appropriate symbols (lines)
       #    lwd=c(2.5,2.5),col=c('blue','red','green'))
    
  }else{
  curve(1-ecdf_subset(x),from=min(subset_product$PRICE),max(subset_product$PRICE))}
}

MODEL_plot_revenue_k<-function(data,type,all=FALSE){
  subset_product<-data[data$PRODUCT_TYPE==type,]
  ecdf_subset<-ecdf(subset_product$PRICE)
  if(all){
    
    subset_product_classic<-data[data$PRODUCT_TYPE=="Classic",]
    ecdf_subset_classic<-ecdf(subset_product_classic$PRICE)
    
    subset_product_light<-data[data$PRODUCT_TYPE=="Light",]
    ecdf_subset_light<-ecdf(subset_product_light$PRICE)
    
    subset_product_flex<-data[data$PRODUCT_TYPE=="Flex",]
    ecdf_subset_flex<-ecdf(subset_product_flex$PRICE)
    
    max_1<-max(max(subset_product_light$PRICE), max(subset_product_light$PRICE), max(subset_product_light$PRICE))
    min_1<-min(min(subset_product_light$PRICE), min(subset_product_light$PRICE), min(subset_product_light$PRICE))
    
    curve((1-ecdf_subset_flex(x)+1-ecdf_subset_light(x)+1-ecdf_subset_classic(x))*x, min_1, max_1,
          col="grey",xlab="p",ylab="Revenue(p)")
    curve((1-ecdf_subset_flex(x))*x, min_1, max_1, col="green",add=TRUE)
    curve(x*(1-ecdf_subset_light(x)), min_1, max_1, col="blue",add=TRUE)
    curve(x*(1-ecdf_subset_classic(x)), min_1, max_1, col="red",add=TRUE)
    legend(200,0.95, # places a legend at the appropriate place 
           c('Light','Classic','Flex'), # puts text in the legend
           lty=c(1,1,1), # gives the legend appropriate symbols (lines)
           lwd=c(2.5,2.5),col=c('blue','red','green'))
  }else{
    curve(x*(1-ecdf_subset(x)),from=min(subset_product$PRICE),max(subset_product$PRICE))}
}
