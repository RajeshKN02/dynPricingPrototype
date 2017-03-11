#' @details Probability distribution function fitting 

#' Fit distributions
library(fitdistrplus)
library(logspline)


x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

descdist(x, discrete = FALSE, boot=1000)


fit.weibull <- fitdist(x, "weibull")
fit.norm <- fitdist(x, "norm")
plot(fit.norm)
plot(fit.weibull)



fit.weibull$aic
fit.norm$aic

#' Kolmogorov-Smirnov test simulation

n.sims <- 5e4

stats <- replicate(n.sims, {      
  r <- rweibull(n = length(x)
                , shape= fit.weibull$estimate["shape"]
                , scale = fit.weibull$estimate["scale"]
  )
  as.numeric(ks.test(r
                     , "pweibull"
                     , shape= fit.weibull$estimate["shape"]
                     , scale = fit.weibull$estimate["scale"])$statistic
  )      
})

plot(ecdf(stats), las = 1, main = "KS-test statistic simulation (CDF)", col = "darkorange", lwd = 1.7)
grid()

fit <- logspline(stats)
fit
1 - plogspline(ks.test(x
                       , "pweibull"
                       , shape= fit.weibull$estimate["shape"]
                       , scale = fit.weibull$estimate["scale"])$statistic
               , fit
)

#' Bootstrap 
#' add pointwise confidence intervals to the estimated Weibull PDF or CDF:

xs <- seq(10, 65, len=500) # parameter from estimation

true.weibull <- rweibull(1e6, shape= fit.weibull$estimate["shape"]
                         , scale = fit.weibull$estimate["scale"])

boot.pdf <- sapply(1:1000, function(i) {
  xi <- sample(x, size=length(x), replace=TRUE)
  MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
  dweibull(xs, shape=MLE.est$estimate["shape"],  scale = MLE.est$estimate["scale"])
}
)

boot.cdf <- sapply(1:1000, function(i) {
  xi <- sample(x, size=length(x), replace=TRUE)
  MLE.est <- suppressWarnings(fitdist(xi, distr="weibull"))  
  pweibull(xs, shape= MLE.est$estimate["shape"],  scale = MLE.est$estimate["scale"])
}
)   

#-----------------------------------------------------------------------------
# Plot PDF
#-----------------------------------------------------------------------------

par(bg="white", las=1, cex=1.2)
plot(xs, boot.pdf[, 1], type="l", col=rgb(.6, .6, .6, .1), ylim=range(boot.pdf),
     xlab="x", ylab="Probability density")
for(i in 2:ncol(boot.pdf)) lines(xs, boot.pdf[, i], col=rgb(.6, .6, .6, .1))

# Add pointwise confidence bands

quants <- apply(boot.pdf, 1, quantile, c(0.025, 0.5, 0.975))
min.point <- apply(boot.pdf, 1, min, na.rm=TRUE)
max.point <- apply(boot.pdf, 1, max, na.rm=TRUE)
lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[2, ], col="darkred", lwd=2)

#-----------------------------------------------------------------------------
# Plot CDF
#-----------------------------------------------------------------------------

par(bg="white", las=1, cex=1.2)
plot(xs, boot.cdf[, 1], type="l", col=rgb(.6, .6, .6, .1), ylim=range(boot.cdf),
     xlab="x", ylab="F(x)")
for(i in 2:ncol(boot.cdf)) lines(xs, boot.cdf[, i], col=rgb(.6, .6, .6, .1))

# Add pointwise confidence bands

quants <- apply(boot.cdf, 1, quantile, c(0.025, 0.5, 0.975))
min.point <- apply(boot.cdf, 1, min, na.rm=TRUE)
max.point <- apply(boot.cdf, 1, max, na.rm=TRUE)
lines(xs, quants[1, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[3, ], col="red", lwd=1.5, lty=2)
lines(xs, quants[2, ], col="darkred", lwd=2)
#lines(xs, min.point, col="purple")
#lines(xs, max.point, col="purple")

#' use ad test
#' library(ad.te)
