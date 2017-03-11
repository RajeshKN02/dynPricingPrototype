#'@title: Buying Probabilites EDA Classic
#'@return: list with parameters for product Classic and DOW

Distributions_classic_classic<-list()

#' Fitting for a day and a product 
#' @details: Monday, Classic

Mon_Classic<-filled_BookingData_DOW[[2]]$PRICE[which(filled_BookingData_DOW[[2]]$PRODUCT_TYPE=="Classic")]

fg <- fitdist(Mon_Classic, "gamma")
fw <- fitdist(Mon_Classic, "weibull")
fn <- fitdist(Mon_Classic, "norm")

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

summary(fn)

Distributions_classic_classic<-append(Distributions_classic_classic,
                                       list("Monday", "Classic","Normal",fn$estimate))

#' @details: Tuesday, Classic

Tues_Classic<-filled_BookingData_DOW[[3]]$PRICE[which(filled_BookingData_DOW[[3]]$PRODUCT_TYPE=="Classic")]

fn <- fitdist(Tues_Classic, "norm")
fw <- fitdist(Tues_Classic, "weibull")
fu <- fitdist(Tues_Classic, "unif")
fitted_distr<-list(fn,fw,fu)

par(mfrow = c(2, 2))
fitnames <- c("norm","Weibull", "uniform")

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
summary(fw)

Distributions_classic<-append(Distributions_classic,list("Tuesday","Classic","Weibull",fw$estimate))

#' @details: Wednesday, Classic
 
Wednesday_Classic<-filled_BookingData_DOW[[4]]$PRICE[which(filled_BookingData_DOW[[4]]$PRODUCT_TYPE=="Classic")]

fn <- fitdist(Wednesday_Classic, "norm")
fw <- fitdist(Wednesday_Classic, "weibull")
fu <- fitdist(Wednesday_Classic, "unif")

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

Distributions_classic<-append(Distributions_classic,list("Wednesday","Classic","Weibull",fw$estimate))

#' @details: Thursday, Classic

Thurs_Classic<-filled_BookingData_DOW[[5]]$PRICE[which(filled_BookingData_DOW[[5]]$PRODUCT_TYPE=="Classic")]

fn <- fitdist(Thurs_Classic, "norm")
fu <- fitdist(Thurs_Classic, "unif")
fw <- fitdist(Thurs_Classic, "weibull")
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

Distributions_classic<-append(Distributions_classic,list("Thursday","Classic","Weibull",fw$estimate))

#' @details: Friday, Classic

Fri_Classic<-filled_BookingData_DOW[[6]]$PRICE[which(filled_BookingData_DOW[[6]]$PRODUCT_TYPE=="Classic")]

fg <- fitdist(Fri_Classic, "gamma")
fw <- fitdist(Fri_Classic, "weibull")
fe <- fitdist(Fri_Classic, "exp")
fitted_distr<-list(fg,fw,fe)

par(mfrow = c(2, 2))
fitnames <- c("gamma", "Weibull","exponential")
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

out <- capture.output(gofstat(fitted_distr,fitnames=fitnames))
cat("Friday Classic", out, file="Output/Friday_Classic_Best_FIT.txt", sep="n", append=TRUE)

Distributions_classic<-append(Distributions_classic,list("Friday","Classic","Gamma",fg$estimate))

#' @details: Satudary, Classic
 
Sat_Classic<-BookingData_DOW[[7]]$PRICE[which(BookingData_DOW[[7]]$PRODUCT_TYPE=="Classic")]

fg <- fitdist(Sat_Classic, "gamma")
fw <- fitdist(Sat_Classic, "weibull")
fe <- fitdist(Sat_Classic, "exp")
fitted_distr<-list(fg,fw,fe)
par(mfrow = c(2, 2))
fitnames <- c("gamma", "Weibull","exponential")
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

Distributions_classic<-append(Distributions_classic,list("Satudray","Classic","Gamma",fg$estimate))

#' @details: Sunday, Classic

Sun_Classic<-BookingData_DOW[[1]]$PRICE[which(BookingData_DOW[[1]]$PRODUCT_TYPE=="Classic")]

fg <- fitdist(Sun_Classic, "gamma")
fw <- fitdist(Sun_Classic, "weibull")
fl <- fitdist(Sun_Classic, "lnorm")
fitted_distr<-list(fg,fw,fl)
par(mfrow = c(2, 2))
fitnames <- c("gamma","Weibull","lognormal")
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

Distributions_classic<-append(Distributions_classic,list("Sunday","Classic","Lognormal",fl$estimate))
