#'@title: Buying Probabilites EDA Flex
#'@return: list with parameters for product Flex and DOW

Distributions_flex<-list()

#' Fitting for a day and a product 
#' @details: Monday, Flex

Mon_Flex<-BookingData_DOW[[2]]$PRICE[which(BookingData_DOW[[2]]$PRODUCT_TYPE=="Flex")]

fg <- fitdist(Mon_Flex, "gamma")
fw <- fitdist(Mon_Flex, "weibull")
fn <- fitdist(Mon_Flex, "norm")
fl <- fitdist(Mon_Flex, "lnorm")

fitted_distr<-list(fg,fw,fn,fl)

par(mfrow = c(2, 2))
fitnames <- c("gamma", "Weibull","normal","lognormal")
denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6,6),fitcol = c("lightcoral", "blue","palegreen4","hotpink"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6,6),fitcol = c("lightcoral", "blue","palegreen4","hotpink"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4","hotpink"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)


Distributions<-append(Distributions,list("Monday","Flex","Weibull",fw$estimate))

#' @details: Tuesday, Flex

Tues_Flex<-BookingData_DOW[[3]]$PRICE[which(BookingData_DOW[[3]]$PRODUCT_TYPE=="Flex")]

fg <- fitdist(Tues_Flex, "gamma")
fn <- fitdist(Tues_Flex, "norm")
fw <- fitdist(Tues_Flex, "weibull")
fu <- fitdist(Tues_Flex, "unif")
fitted_distr<-list(fg,fn,fw,fu)

par(mfrow = c(2, 2))
fitnames <- c("gamma","norm","Weibull", "uniform")

denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6,6),fitcol = c("lightcoral", "blue","palegreen4","hotpink"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6,6),fitcol = c("lightcoral", "blue","palegreen4","hotpink"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4","hotpink"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)

Distributions<-append(Distributions,list("Tuesday","Flex","Weibull",fw$estimate))

#' @details: Wednesday, Flex
 
Wednesday_Flex<-BookingData_DOW[[4]]$PRICE[which(BookingData_DOW[[4]]$PRODUCT_TYPE=="Flex")]

fn <- fitdist(Wednesday_Flex, "norm")
fu <- fitdist(Wednesday_Flex, "unif")
fg <- fitdist(Wednesday_Flex, "gamma")
fitted_distr<-list(fn,fu,fg)

par(mfrow = c(1, 2))
fitnames <- c("normal", "unif","gamma")
denscomp(fitted_distr, legendtext = fitnames, 
         xlab="Price", xlegend=-0.1,ylegend=0.0065)
cdfcomp(fitted_distr, legendtext = fitnames, 
        xlab="Price", xlegend="topleft")

gofstat(fitted_distr,fitnames=fitnames)
summary(fu)

Distributions<-append(Distributions,list("Wednesday","Flex","Uniform",fu$estimate))

#' @details: Thursday, Flex

Thurs_Flex<-BookingData_DOW[[5]]$PRICE[which(BookingData_DOW[[5]]$PRODUCT_TYPE=="Flex")]

fn <- fitdist(Thurs_Flex, "norm")
fu <- fitdist(Thurs_Flex, "unif")
fw <- fitdist(Thurs_Flex, "weibull")
fitted_distr<-list(fn,fu,fw)

par(mfrow = c(1, 2))
fitnames <- c("normal", "unif","Weibull")
denscomp(fitted_distr, legendtext = fitnames, 
         xlab="Price", xlegend=-0.1,ylegend=0.0065)
cdfcomp(fitted_distr, legendtext = fitnames, 
        xlab="Price", xlegend="topleft")

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)

Distributions<-append(Distributions,list("Thursday","Flex","Weibull",fw$estimate))

#' @details: Friday, Flex

Fri_Flex<-BookingData_DOW[[6]]$PRICE[which(BookingData_DOW[[6]]$PRODUCT_TYPE=="Flex")]

fg <- fitdist(Fri_Flex, "gamma")
fw <- fitdist(Fri_Flex, "weibull")
fn <- fitdist(Fri_Flex, "norm")
fitted_distr<-list(fg,fw,fn)

par(mfrow = c(2, 2))
fitnames <- c("gamma", "Weibull","normal")
denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)


gofstat(fitted_distr,fitnames=fitnames)
summary(fg)

Distributions<-append(Distributions,list("Friday","Flex","Gamma",fg$estimate))

#' @details: Satudary, Flex
 
Sat_Flex<-BookingData_DOW[[7]]$PRICE[which(BookingData_DOW[[7]]$PRODUCT_TYPE=="Flex")]

fg <- fitdist(Sat_Flex, "gamma")
fu <- fitdist(Sat_Flex, "unif")
fn <- fitdist(Sat_Flex, "norm")
fitted_distr<-list(fg,fu,fn)
par(mfrow = c(2, 2))
fitnames <- c("gamma", "uniform","normal")
denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fu)

Distributions<-append(Distributions,list("Satudray","Flex","Uniform",fu$estimate))

#' @details: Sunday, Flex

Sun_Flex<-BookingData_DOW[[1]]$PRICE[which(BookingData_DOW[[1]]$PRODUCT_TYPE=="Flex")]

fg <- fitdist(Sun_Flex, "gamma")
fe <- fitdist(Sun_Flex, "exp")
fw <- fitdist(Sun_Flex, "weibull")
fitted_distr<-list(fg,fe,fw)
par(mfrow = c(2, 2))
fitnames <- c("gamma", "exponential","Weibull")
denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fg)

Distributions<-append(Distributions,list("Sunday","Flex","Gamma",fg$estimate))
