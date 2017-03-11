#'@title: Buying Probabilites EDA Light
#'@return: list with parameters for product Light and DOW

BookingData_DOW <- split(BookingData, BookingData$DOW)
Distributions_light <- list()

#' Fitting for a day and a product 
#' @details: Monday, Light

Mon_Light <- BookingData_DOW[[2]]$PRICE[which(BookingData_DOW[[2]]$PRODUCT_TYPE == "Light")]

fu <- fitdist(Mon_Light, "unif")
fw <- fitdist(Mon_Light, "weibull")
fn <- fitdist(Mon_Light, "norm")

fitted_distr <- list(fu,fw,fn)

par(mfrow = c(2, 2))

fitnames <- c("uniform", "Weibull","normal")

denscomp(fitted_distr, xlegend="topright", fitlty = c(1,5,6),
         fitcol = c("lightcoral", "blue", "palegreen4"),
         #demp = TRUE, dempcol = "lightblue",
         addlegend = TRUE, legendtext = fitnames, 
         datacol = "papayawhip", xlab = "Price")

qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"), 
       legendtext = fitnames)

cdfcomp(fitted_distr, fitlty = c(1,5,6), 
        fitcol = c("lightcoral", "blue", "palegreen4"),
        legendtext = fitnames, xlab="Price", xlegend = "right")

ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"), 
       legendtext = fitnames)

a <- gofstat(fitted_distr, fitnames = fitnames)

summary(fu)

Mon_light_param<-list(Info="Monday-Light", Distribution="Uniform",Parameters=fu$estimate)

#' @details: Tuesday, Light

Tues_Light<-BookingData_DOW[[3]]$PRICE[which(BookingData_DOW[[3]]$PRODUCT_TYPE=="Light")]

fu <- fitdist(Tues_Light, "unif")
fw <- fitdist(Tues_Light, "weibull")
fn <- fitdist(Tues_Light, "norm")
fitted_distr<-list(fu,fw,fn)

par(mfrow = c(2, 2))
fitnames <- c("uniform","Weibull", "normal")

denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)

Tues_light_param<-list(Info="Tuesday-Light", Distribution="Weibull",Parameters=fw$estimate)


#' @details: Wednesday, Light
 
Wednesday_Light<-BookingData_DOW[[4]]$PRICE[which(BookingData_DOW[[4]]$PRODUCT_TYPE=="Light")]

fn <- fitdist(Wednesday_Light, "norm")
fw <- fitdist(Wednesday_Light, "weibull")
fu <- fitdist(Wednesday_Light, "unif")

fitted_distr<-list(fn,fw,fu)

par(mfrow = c(2, 2))
fitnames <- c("normal","Weibull","uniform")

denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)

Wed_light_param<-list(Info="Wednesday-Light", Distribution="Weibull",Parameters=fw$estimate)

#' @details: Thursday, Light

Thurs_Light<-BookingData_DOW[[5]]$PRICE[which(BookingData_DOW[[5]]$PRODUCT_TYPE=="Light")]

fn <- fitdist(Thurs_Light, "norm")
fu <- fitdist(Thurs_Light, "unif")
fw <- fitdist(Thurs_Light, "weibull")
fitted_distr<-list(fn,fw,fu)

par(mfrow = c(2, 2))
fitnames <- c("normal","Weibull","uniform")

denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)
summary(fw)

Thurs_light_param<-list(Info="Thursday-Light", Distribution="Weibull",Parameters=fw$estimate)

#' @details: Friday, Light

Fri_Light<-BookingData_DOW[[6]]$PRICE[which(BookingData_DOW[[6]]$PRODUCT_TYPE=="Light")]

descdist(Fri_Light, boot = 500)

fg <- fitdist(Fri_Light, "gamma")
fl <- fitdist(Fri_Light, "lnorm")
fe <- fitdist(Fri_Light, "exp")
fitted_distr<-list(fg,fl,fe)

par(mfrow = c(2, 2))
fitnames <- c("gamma", "lognormal","exponential")
denscomp(fitted_distr, xlegend="topright",fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),
         #demp=TRUE, dempcol = "lightblue",
         addlegend=TRUE,legendtext=fitnames, datacol="papayawhip",
         xlab="Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames)
cdfcomp(fitted_distr, fitlty=c(1,5,6),fitcol = c("lightcoral", "blue","palegreen4"),legendtext = fitnames, 
        xlab="Price",xlegend="right")
ppcomp(fitted_distr,fitcol = c("lightcoral", "blue", "palegreen4"),legendtext = fitnames)

gofstat(fitted_distr,fitnames=fitnames)

summary(fl)

Fri_light_param <- list(Info = "Friday-Light", Distribution = "Lognormal",
                        Parameters = fl$estimate)

#' @details: Satudary, Light
 
Sat_Light <- BookingData_DOW[[7]]$PRICE[which(BookingData_DOW[[7]]$PRODUCT_TYPE == "Light")]

fg <- fitdist(Sat_Light, "gamma")
fw <- fitdist(Sat_Light, "weibull")
fl <- fitdist(Sat_Light, "lnorm")

fitted_distr <- list(fg, fw, fl)

par(mfrow = c(2, 2))

fitnames <- c("gamma", "Weibull","lognormal")
denscomp(fitted_distr, xlegend = "topright",fitlty = c(1,5,6),
         fitcol = c("lightcoral", "blue", "palegreen4"),
         #demp = TRUE, dempcol = "lightblue",
         addlegend = TRUE,legendtext = fitnames, datacol = "papayawhip",
         xlab = "Price")
qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),
       legendtext = fitnames)
cdfcomp(fitted_distr, fitlty = c(1,5,6), 
        fitcol = c("lightcoral", "blue", "palegreen4"), legendtext = fitnames, 
        xlab = "Price", xlegend = "right")
ppcomp(fitted_distr, fitcol = c("lightcoral", "blue", "palegreen4"), 
       legendtext = fitnames)

gofstat(fitted_distr, fitnames = fitnames)

summary(fl)

Sat_light_param<-list(
  Info="Saturday-Light", 
  Distribution = "Lognormal", 
  Parameters = fl$estimate
  )

#' @details: Sunday, Light

Sun_Light<-BookingData_DOW[[1]]$PRICE[which(BookingData_DOW[[1]]$PRODUCT_TYPE == "Light")]

fg <- fitdist(Sun_Light, "gamma")
fw <- fitdist(Sun_Light, "weibull")
fl <- fitdist(Sun_Light, "lnorm")

fitted_distr <- list(fg, fw, fl)

par(mfrow = c(2, 2))
fitnames <- c("gamma", "Weibull", "lognormal")

denscomp(fitted_distr, xlegend = "topright", fitlty=c(1,5,6), 
         fitcol = c("lightcoral", "blue", "palegreen4"),
         #demp = TRUE, dempcol = "lightblue",
         addlegend = TRUE, legendtext = fitnames, 
         datacol = "papayawhip", xlab="Price")

qqcomp(fitted_distr,fitcol = c("lightcoral", "blue","palegreen4"),
       legendtext = fitnames)

cdfcomp(fitted_distr, fitlty = c(1,5,6),
        fitcol = c("lightcoral", "blue", "palegreen4"), legendtext = fitnames, 
        xlab="Price", xlegend = "right")

ppcomp(fitted_distr, fitcol = c("lightcoral", "blue", "palegreen4"),
       legendtext = fitnames)

gofstat(fitted_distr, fitnames = fitnames)
summary(fl)

Sun_light_param <- list(
  Info = "Sunday-Light",
  Distribution = "Lognormal",
  Parameters = fl$estimate)

##############################################################################################

Distributions_light <- list(
  Mon_light_param,
  Tues_light_param,
  Wed_light_param,
  Thurs_light_param,
  Fri_light_param,
  Sat_light_param,
  Sun_light_param
)

Distributions_light