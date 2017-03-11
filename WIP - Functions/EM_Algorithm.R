#' @tile "EM Algorithm.R"
#' @author Verbelen R. (2013)
#' @description  EM Algorithm class for fitting a mixture of Erlangs for right censored data
# 

## Libraries
# library(adk)
library(MASS)
library(VGAM)
library(survival)
library(actuar)

## Initial values
erlang.initial<-function(data,censored,M=10,spread.factor= 1,only.uncensored=TRUE){
  # use only uncensored data if there is enough data available
   if(only.uncensored){
    data <- data[censored==FALSE]
    }
  theta <- max(data)/M
  shape <- seq(1,M)
  alpha <-rep(NA,M) # create an empty vector of alpha (initial probabilities)
    for(i in 1:M){
      alpha[i]<-sum(data <= i*theta & data>(i-1) * theta)
     }
  shape <- shape[alpha>0]
  alpha <- alpha[alpha>0]/sum(alpha)
  # spread out shapes and adjust theta
  shape <- spread.factor*shape
  theta <- theta/spread.factor
  list(theta=theta, shape=shape, alpha=alpha)
}

## Log likelihood
erlang.loglikelihood<-function(x,rc,alpha, theta,shape){
  # matrix containing densities(uncensored)
  x.densities<-outer(x,shape,dgamma,scale=theta)
  #matrix containing alpha*density(uncensored)
  x.components <- sweep(x.densities,2,alpha,FUN="*" )
  #likelihood
  likelihood.contribution<-rowSums(x.components)
  if(length(rc)!= 0){
    #matrix containing survivals(censored)
    rc.survivals<- 1-outer(rc,shape,pgamma,scale=theta)
    # matrix containing alpha * survivals(censored)
    rc.components<- sweep(rc.survivals,2,alpha,FUN="*")
    #likelihood
    likelihood.contribution<-c(likelihood.contribution,rowSums(rc.components))
  }
    #log likelihood
    loglikelihood.contribution<- ifelse(likelihood.contribution>0,log(likelihood.contribution),-1000)
    sum(loglikelihood.contribution)
}

## z_{ij}^{k}:posterior probabilities(uncensored)
erlang.z<-function(x,alpha,theta,shape){
  x.densities<-outer(x,shape,dgamma,scale=theta)
  x.components <-sweep(x.densities,2,alpha,FUN="*" )
  z<-sweep(x.components,1,rowSums(x.components),FUN="/")
  #in case all z_{ij]^{k} for j =1,...,M are numerically 0
  z[is.nan(z)]=1/length(shape)
  z
}

## posterior probabilities(censored)
erlang.z.rc<-function(rc,alpha, theta, shape){
  rc.survivals<-1-outer(rc,shape,pgamma,scale=theta)
  rc.components<-sweep(rc.survivals,2,alpha,FUN="*" )
  z.rc<- sweep(rc.components , 1 ,rowSums(rc.components),FUN="/" )
  #in case all z_{ij}^{k} for j=1,...,M are numerically 0
  z.rc[is.nan(z.rc)]=1/length(shape)
  z.rc
}

## Expected value of ri } h t censored observations
erlang.expected.rc<-function(rc,alpha, theta, shape){
  denominator.survivals<-1-outer(rc,shape,pgamma,scale=theta)
  denominator.components<-sweep(denominator.survivals,2,alpha,FUN="*" )
  numerator.survivals<-1-outer(rc,shape+1,pgamma,scale=theta)
  numerator.components <- sweep(numerator.survivals,2,alpha*shape,FUN="*" )
  rc.exp<-theta*rowSums(numerator.components)/rowSums(denominator.components)
  # in cas e a l l z f i j g^fkg f o r j =1, . . . ,M ar e nume r i c a l ly 0
  rc.exp[is.nan(rc.exp)]=0
  rc.exp
}

## EM algorithm
erlang.em<-function(data, censored, alpha, theta, shape, eps=1e-03,print=TRUE){
  # creates separate vector for uncensored and right censored data
  x <- data[censored==FALSE]
  rc <- data[censored==TRUE]
  censoring<-(length(rc)!= 0)
  n<-length(data)
  iteration<-1
  loglikelihood<-erlang.loglikelihood(x,rc,alpha,theta,shape)
  old.loglikelihood<--Inf
  history.loglikelihood<-loglikelihood
  while(loglikelihood-old.loglikelihood>eps){
	  old.loglikelihood<-loglikelihood
	  #E step
	  z<-erlang.z(x,alpha,theta,shape)
    	if(censoring){
    	z.rc<-erlang.z.rc(rc,alpha, theta, shape)
	    rc.exp<-erlang.expected.rc(rc,alpha,theta,shape)
    	}
	  #M step
	  if(censoring){alpha<-(colSums(z)+colSums(z.rc))/n}else{alpha<-colSums(z)/n}
	  if(censoring){theta<-((sum(x)+sum(rc.exp))/n)/sum(alpha*shape)}else{theta<-(sum(x)/n)/sum(alpha*shape)}
	  iteration<-iteration+1
	  loglikelihood<-erlang.loglikelihood(x,rc,alpha, theta, shape)
	  if(print) print(loglikelihood)
	  history.loglikelihood<-c(history.loglikelihood,loglikelihood)
	}
	list(alpha = alpha, shape = shape, theta=theta, loglikelihood=loglikelihood,
	history.loglikelihood= history.loglikelihood, iteration = iteration,
	AIC=-2*loglikelihood+2*(2*length(alpha)+1),BIC=-2*loglikelihood+(2*length(alpha)+1)*log(length(data)))
}

## Shape adjustments
erlang.shapes <- function(data,censored, alpha, theta, shape, eps=1e-03, print=TRUE,
                          alpha.limit = 0.0001){
  M <- length(shape)
  fit <- erlang.em(data, censored, alpha, theta, shape, eps, print=F)
  loglikelihood <- fit$loglikelihood 
  theta <- fit$theta
  alpha <- fit$alpha
  # before and after are the loglikelihoods used in the outer while loop
  before.loglikelihood <- -Inf
  after.loglikelihood  <- loglikelihood 
  iteration<- 1
  while(after.loglikelihood > before.loglikelihood + eps){
	  if(print) cat("iteration=", iteration , "\n" )
	  before.loglikelihood <- after.loglikelihood 
	  # Try increasing the shapes
  	for(i in M: 1){
	    improve <- T
	    while((improve == T) && (alpha[i]>alpha.limit) && (i== M||(shape[i]<shape[i+1]-1))){
	    new.shape <- shape
	    new.shape[i] <- new.shape[i]+1
	    fit <- erlang.em(data,censored,alpha,theta,new.shape,eps,print=F)
	    new.loglikelihood <- fit$loglikelihood
	    if(new.loglikelihood>loglikelihood+eps){
	      loglikelihood <- new.loglikelihood 
	      shape <- new.shape
	      theta <- fit$theta
	      alpha <- fit$alpha
	      if(print) cat ( " loglikelihood = " , loglikelihood  , ", shape = " , shape , "\n" , "
          theta = " , theta , ", alpha = " , alpha , "\n" )
    } else {improve<-F}
   }
  }
    # Try decreasing the shapes
    for(i in 1:M){
    improve <- T
    while((improve==T) && (alpha[i]>alpha.limit)&&((i==1) || shape[i]>shape[i-1]+1)&&shape[i]>1){
      new.shape <- shape
      new.shape[i] <- new.shape[i] - 1
      fit<- erlang.em(data,censored,alpha, theta,new.shape,eps,print=F)
      new.loglikelihood <- fit$loglikelihood
      if(new.loglikelihood > loglikelihood + eps){
	    loglikelihood <- new.loglikelihood 
	    shape <- new.shape
	    theta <- fit$theta
	    alpha <- fit$alpha
	    if(print) cat ("loglikelihood=",loglikelihood,",shape=",shape,"\n","
	       theta = " , theta , ", alpha = " , alpha, "\n" )
        } else{improve<-F}
	   }
    }
  after.loglikelihood <- loglikelihood 
  iteration<-iteration+1
  }
  list(alpha = alpha , shape = shape , theta = theta , loglikelihood  = loglikelihood  , AIC=-2*loglikelihood+2*(2*length(alpha)+1),BIC=-2*loglikelihood+(2*length(alpha)+1)*log(length(data)))
  }

## Reduction of M based on an information criterium: AIC and BIC implemented

erlang.IC<-function(data, censored, alpha, theta, shape, criterium = "BIC", eps=1e-03, print=TRUE, alpha.limit=0.0001){
  fit <- erlang.shapes(data, censored, alpha, theta, shape, eps, print=F, alpha.limit)
	loglikelihood <- fit$loglikelihood 
	IC <- fit[[criterium]]
	shape <- fit$shape
	theta <- fit$theta
	alpha <- fit$alpha
	M <- length(shape)
	if (print) cat( "M = " , M, ", " , criterium, " = " , IC , ", shape = " , shape , "\n" , " theta = " , theta , ", alpha = " , alpha , "\n" )
	improve <- T
  while((improve == T) && length(shape)>1){
    new.shape <- shape[alpha != min(alpha)]
    new.alpha <- alpha[alpha != min(alpha)]
    new.alpha <- new.alpha/sum(new.alpha)
    fit <- erlang.shapes(data, censored, new.alpha, theta, new.shape, eps, print=F, alpha.limit)
    new.IC <- fit[[criterium]]
    if(new.IC < IC){
      IC <- new.IC
      loglikelihood <- fit$loglikelihood 
      shape <- fit$shape
      theta <- fit$theta
      alpha <-fit$alpha
      M <- length(shape)
      if(print) cat( "M = " , M, ", " , criterium , " = " , IC , ", shape = " , shape , "\n" , "
                    theta = " , theta , ", alpha = " , alpha , "\n" )
    } else{improve<-F}
  }
  list(M = M, alpha = alpha, shape=shape, theta=theta, loglikelihood=loglikelihood, AIC=-2*loglikelihood+2*(2*length(alpha)+1),BIC=-2*loglikelihood + (2*length(alpha)+1)*log(n))
}

## Compare different sets of initial values obtained by varying the spread factor using AIC or BIC
erlang.IC.S<- function(data, censored, M=10, max.spread.factor = 5, criterium="AIC", eps = 1e-03, print=TRUE, alpha.limit= 0.0001, only.uncensored=TRUE){
  initial <- erlang.initial(data, censored, M=M, spread.factor=1, only.uncensored = only.uncensored)
  fit <- erlang.IC(data, censored, initial$theta, initial$shape, initial$alpha, criterium = criterium, eps=1e-03, print=F, alpha.limit=0.0001)
  IC <- fit[[criterium]]
  loglikelihood <- fit$loglikelihood 
  spread <- 1
  best.M <- fit$M
  shape <- fit$shape
  theta <- fit$theta
  alpha <- fit$alpha
  if( print ) cat ( " spread = 1, M = " , best.M , ", " , criterium , " = " , IC , ", shape = " ,
  shape , "\n" , " theta = " , theta , ", alpha = " , alpha , "\n")
    for(i in 2:max.spread.factor){
      initial<- erlang.initial(data , censored , M=M, spread.factor=i, only.uncensored = only.uncensored )
      fit <- erlang.IC(data,censored, initial$theta, initial$shape, initial$alpha, criterium=criterium, eps=1e-03, print=F, alpha.limit=0.0001)
      new.IC <- fit[[criterium]]
        if(print) cat(" spread = " , i , ", M = " , fit$M, ", " , criterium , " = " , new.IC , ",
        shape = " , fit $ shape , "\n" , " theta = " , fit $ theta , ", alpha = " , fit $ alpha , "\n")
        if(new.IC <- new.IC){
        loglikelihood <-fit$loglikelihood 
        spread <- i
        best.M <- fit$M
        shape <- fit$shape
        theta <- fit$theta
        alpha <- fit$alpha
        }
    }
    list(spread=spread, M = best.M, alpha = alpha, shape = shape, theta=theta, loglikelihood=loglikelihood, AIC=-2*loglikelihood+2*(2*length(alpha)+1), BIC=-2*loglikelihood +(2* length(alpha)+1)*log(n))
}

## Density function
erlang.density<- function(x,alpha, theta,shape){
  x.densities<-outer(x,shape,dgamma,scale=theta)
  x.components<-sweep(x.densities, 2, alpha, FUN="*" )
  rowSums (x.components)
}

## Cumulative distribution function (cdf)
erlang.cdf<- function(x, alpha,theta,shape){
  x.cdf<-outer(x,shape,pgamma,scale=theta)
  x.components <- sweep(x.cdf,2,alpha,FUN="*")
  rowSums(x.components)
}

## Survival function
erlang.survival <- function(x,alpha, theta,shape){
  x.survival <- 1 - outer(x, shape, pgamma, scale=theta)
  x.components <- sweep( x.survival,2,alpha,FUN="*" )
  rowSums( x.components )
}

## Non central moments of order k (can be vector)
erlang.moment<-function(k,alpha, theta,shape){
  factorials<-sweep(gamma(outer(k,shape,'+' )),2,gamma(shape),FUN="/" )
  alpha.factorials<-sweep(factorials,2,alpha,FUN="*" )
  rowSums(alpha.factorials)*theta^k
}

## Random generation
erlang.random<-function(n,alpha, theta,shape){
	#The Erlang Distribution is the same as the Gamma,
	#shape parameter but with the shape parameter an integer. 
	#It is often expressed using a rate rather than a scale as the second parameter 
	#(remember that the rate is the reciprocal of the scale)
	rgamma(n,shape=sample(shape,size=n,replace=TRUE,prob=alpha),
	scale=theta)
}

## Plot mixture of Erlangs
erlang.plot<-function(alpha, theta,shape,lower=0,upper=10,...){
  x <- seq(lower,upper,by=(upper-lower)/10000)
  y <- erlang.density(x, alpha, theta, shape)
  plot(x,y,type="l",...)
}

## Plot survival of mixture of Erlangs
erlang.plot.survival<-function(alpha, theta, shape,lower=0,upper=10,...){
  x<-seq(lower,upper,by=(upper-lower)/10000)
  y<-erlang.survival(x,alpha, theta,shape)
  plot(x,y,type="l",...)
}

## Plot mixture of Erlangs and (true) histogram of data
erlang.plot.data<-function(data,alpha, theta,shape,nbins=50,lower=max(c(min(data)-0.1*(max(data)-
      min(data)),0)),upper=max(data)+0.1*(max(data)-min(data)), xlab="",legend=TRUE, lwd=2, ...){
    x<-seq(lower, upper, by=(upper-lower)/nbins)
    y<-erlang.density(x, alpha, theta, shape)
    truehist(data, h=(upper-lower)/nbins, xlim=c(lower,upper), xlab=xlab, col="lightblue1", ...)
    lines(x,y,col="midnightblue",lwd=1.5, lty=2)
    if(legend){
      legend(cex = 0.75,'topright',legend=c("Fitted Density","Observed Relative Frequency"),col=c("midnightblue","lightblue1"),pch=c(NA,15),pt.cex=1.0,lty = c(19,NA),lwd=c(lwd,NA))
	    }
}

## Plot survival of mixture of Erlangs and Kaplan-Meier estimate
erlang.plot.survival.data<-function(data,censored, alpha, theta, shape,lower=0,
  upper=max(data)+0.1*(max(data)-min(data)),xlab="" ,legend=TRUE,lwd=2, ...){
  x <- seq(lower,upper,by=(upper-lower)/10000)
  y <- erlang.survival(x,alpha, theta,shape)
  plot(survfit(Surv(data,1-censored)~1, conf.type="none"),col=" blue ",mark.time=F, xlim=c(lower,upper),xlab=xlab,lwd=lwd , ... )
  lines(x,y,col="midnightblue",lwd=lwd)
  if(legend){
  legend('topright', legend = c("Fitted Survival","Kaplan - Meier"),col = c("midnightblue", "blue "),pch=c(NA,NA), pt.cex=2, lty =c(19,19),lwd=c(lwd,lwd))
	}
}