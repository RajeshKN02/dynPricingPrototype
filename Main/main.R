################################################################################
#' @title Main.R 
#' @description Fitting of the Transition Model 
################################################################################

#===============================================================================
#'SOURCE
#===============================================================================
source("sourceAll.R") # source all packages and functions
#sourceAll()
BookingData <- loadData()

BookingData <- prepare_data(BookingData)

#' General flight info in time frame
flightinfo(BookingData)

#' Specific date
# flightinfo(subset(BookingData,BookingData$DEPARTURE_DATE=="12.07.2016"))

#' Demand over the time frame

total_demand <- setNames(
                  aggregate(BookingData$PAX_COUNT ~ BookingData$DEPARTURE_DATE, 
                    data = BookingData, FUN = 'sum'), c("Departure Date","PAX")
                         )

#===============================================================================
#'Plotting part - Seasonality Analysis 
#===============================================================================

plotbookingcurve(BookingData, "01.08.2016")

# multiplebookingcurves(14, 27)  

#' Save 6x14 inches Thesis
multiplebookingcurves(BookingData, 1,14,'AUG', 7, 2)

#===============================================================================
#'Add DOW Variable and general graphical Analysis of aggregated data 
#===============================================================================

#######' Split into DOW
# 1: Sunday, 2:Monday, ...

BookingData_DOW <- split(BookingData, BookingData$DOW)

DOW <- c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

#' Show Box Plot of the days

# Save for thesis LateX file 

path_Latex <- dynConfig$private$path_latex

pdf(file = paste0(path_Latex, "/Boxplot_prices.pdf"), width = 8, height = 6)
plot(BookingData$PRICE ~ BookingData$DOW, data = BookingData,
     xlab = "", ylab = "Price")
dev.off()

#' Cullen and Frey Graph

pdf(file = paste0(path_Latex, "/Cullen_and_Frey_all.pdf"), width = 15, 
    height = 10)
FIT_Cullen_and_Grey_Plot(BookingData, FALSE)
dev.off()

##' Boxplot of products for every DOW

BookingData_Products <- split(BookingData,BookingData$PRODUCT_TYPE)

PRODUCTS <- unique(BookingData$PRODUCT_TYPE)

# Box Plot

box.par <- par(mfrow = c(3,1))

for (i in (1:length(PRODUCTS))){

  plot(BookingData_Products[[i]]$PRICE ~ BookingData_Products[[i]]$DOW, 
     data = BookingData_Products[[i]],
     xlab = "", ylab = "Price", main = paste("Product",PRODUCTS[i]))
  
}

par(box.par)

##' Box plot of mean values for every DOW

#'Aggregate with DOW and Product 

means <- setNames(aggregate(BookingData$PRICE ~ BookingData$DOW + 
                              BookingData$PRODUCT_TYPE, data = BookingData, 
                            FUN = 'mean'),
                c("DOW","Product","Price"))

# Save plot 

pdf(file = paste0(path_Latex,"/Bar_plot_all.pdf"), width = 8, height = 6)
ggplot(means, aes(x = DOW, y = Price, fill = factor(Product))) +
  geom_bar(stat = "identity",  
           position = position_dodge(width = 0.90), 
           colour = "black") +
  scale_fill_discrete(name = "Product",
                      labels = c("Classic","Flex","Light")) +
  xlab("") + ylab("Mean Prices")+ theme_bw()
dev.off()

################################################################################
#' Cullen and Frey Plot for each product, for every DOW


#' @details: Light

FIT_Cullen_and_Grey_Plot(BookingData, TRUE, "Light")

#' @details: Classic

FIT_Cullen_and_Grey_Plot(BookingData, TRUE, "Classic")

#' @details: Flex 

FIT_Cullen_and_Grey_Plot(BookingData, TRUE, "Flex")

#' Fitting for a day: Monday - NO splitting for product type

plotdist(BookingData_DOW[[2]]$PRICE, histo = TRUE, demp = TRUE)

#' Fit distributions by maximum likelihood estimation (MLE)

fw <- fitdist(BookingData_DOW[[2]]$PRICE, "weibull")
fn <- fitdist(BookingData_DOW[[2]]$PRICE, "norm")
fg <- fitdist(BookingData_DOW[[2]]$PRICE, "gamma")

fitted_distr <- list(fw, fn, fg)

par(mfrow = c(1, 2))
fitnames <- c("Weibull", "normal", "gamma")
denscomp(fitted_distr, legendtext = fitnames, 
         xlab = "Price", xlegend = 170, ylegend = 0.011, demp = TRUE)
#qqcomp(fitted_distr, legendtext = fitnames)
cdfcomp(fitted_distr, legendtext = fitnames, 
        xlab = "Price", xlegend = 150, ylegend = 0.4, use.ppoints = TRUE)
#ppcomp(fitted_distr, legendtext = fitnames)

# Compare statistics: K-S, A-D, AIC, BIC
gofstat(fitted_distr, fitnames = fitnames)

###############################################################################
#' Get distribution parameters for every product type
###############################################################################

# For specific graphical and goodness of fitting (gof) anaylsis check: 
# FIT_Distr_Light.R, FIT_Distr_Classic.R, FIT_Distr_Flex.R

# Using AIC

dist_light_aic <- FIT_Price_Distr_Function(BookingData, "Light")
capture.output(dist_light_ks, file="output/dist_light_aic.txt")

dist_classic_aic <- FIT_Price_Distr_Function(BookingData, "Classic")
capture.output(dist_light_ks, file="output/dist_classic_aic.txt")

dist_flex_aic <- FIT_Price_Distr_Function(BookingData, "Flex")
capture.output(dist_light_ks, file="output/dist_flex_aic.txt")

# Using Kolgomorov-Smirnov
# Advantage: does not discard uniform distributions

dist_light_ks <- FIT_Price_Distr_Function(BookingData, "Light", "KS")
capture.output(dist_light_ks, file="output/dist_light_ks.txt")

dist_classic_ks <- FIT_Price_Distr_Function(BookingData, "Classic", "KS")
capture.output(dist_light_ks, file="output/dist_classic_ks.txt")


dist_flex_ks <- FIT_Price_Distr_Function(BookingData, "Flex", "KS")
capture.output(dist_light_ks, file="output/dist_flex_ks.txt")

###############################################################################
#' Get arrival process
###############################################################################

# Achtung: check that initial_BH > Data's initial BH and numerical vector

#result <- fillNull_previousDates(BookingData, "03.02.2016", 100)

initial_BH <- max(BookingData$DAYS_TO_DEPARTURE)

# Extreme time expensive! (5-10 minutes 8GB RAM)
final_result <- fillNull_all(BookingData, initial_BH)

write.csv(final_result, "Output/filled_BookingData.csv")

filled_BookingData_DOW <- split(final_result, final_result$DOW)

# Build "representative" DOW behavior for booking curves


list_colors <- c("blue","red","green","violet","orange","indianred4", 
                 "darkviolet")
x <- rev(sort(unique(final_result$DAYS_TO_DEPARTURE)))

pdf(file = "output/Arrival_Pattern_DOW.pdf", width = 14, height = 12)
par(mfrow=c(3,3)) 
for (i in (1:7)){
    aggdata <- aggregate(acumulate ~ DAYS_TO_DEPARTURE, 
                         data = filled_BookingData_DOW[[i]], mean)
    
    plot(aggdata, xlim = rev(range(x)), col = list_colors[i], 
         ylab="mean acumulate", main = unique(filled_BookingData_DOW[[i]]$DOW))
}

for (i in (1:7)){
  aggdata <- aggregate(acumulate ~ DAYS_TO_DEPARTURE, 
                       data = filled_BookingData_DOW[[i]], mean)
  
  plot(aggdata, xlim = rev(range(x)),  ylim=range(c(0,120)), 
       col = list_colors[i], main="All", axes = TRUE, xlab = "", ylab = "")
  par(new = TRUE)
}
dev.off()

# All without subseting

pdf(file = "output/Arrival_Pattern.pdf", width = 8, height = 6)

  aggdata <- aggregate(acumulate ~ DAYS_TO_DEPARTURE, 
                       data = final_result, mean)
  
  plot(aggdata, xlim = rev(range(x)), col = "black", ylab= "mean acumulate",
       main="Arrival pattern for OD single-leg TXL-FRA Jan-Dec 2016" )
dev.off()


