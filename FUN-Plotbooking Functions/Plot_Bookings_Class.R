#' Plot_Bookings_Class.R

#' @title subsetbookings function
#' @description Subsets BookingData for a particular departure date
#' @param departure_date: departure date
#' @return Filtered data frame

subsetbookings_date <- function(data, departure_date){
	
	subset1 <- subset(data, data$DEPARTURE_DATE == departure_date)
	return(subset1)
	
}

subsetbookings_class <- function(data, fareclass){
	
	subset1 <- subset(data, data$FARECLASS == fareclass)
	return(subset1)
	
}

#' @title multipleplot function

#' @descritpion objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' @param cols: Number of columns in layout
#' @param layout: A matrix specifying the layout. If present, 'cols' is ignored.
#' @example: If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
	require(grid)
	
	# Make a list from the ... arguments and plotlist
	plots <- c(list(...), plotlist)
	
	numPlots = length(plots)
	
	# If layout is NULL, then use 'cols' to determine layout
	if (is.null(layout)) {
		# Make the panel
		# ncol: Number of columns of plots
		# nrow: Number of rows needed, calculated from # of cols
		layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
				ncol = cols, nrow = ceiling(numPlots/cols))
	}
	
	if (numPlots==1) {
		print(plots[[1]])
		
	} else {
		# Set up the page
		grid.newpage()
		pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
		
		# Make each plot, in the correct location
		for (i in 1:numPlots) {
			# Get the i,j matrix positions of the regions that contain this subplot
			matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
			
			print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
							layout.pos.col = matchidx$col))
		}
	}
}


#' @title plotbookingcurve function (single)

#' @description Plot the Booking Curve for a particular flight (departure date) 
#' @param x: departure date in ""
#' @return a accumulated arrival ggplot curve for a particular departure date in the data set 

plotbookingcurve <- function(data, x){
	
	subset1 <- subsetbookings_date(data, x)
	
	# Accumulate function
	acumulate <- Reduce("sum", subset1$PAX_COUNT, accumulate = TRUE)
	
	# Attach accumulate
	subset1 <- data.frame(subset1, acumulate)
	
	## Plot the Booking curve
	
	# Last DCP=22 
	
	Load_factor <- round( max(acumulate)/subset1$CAPACITY, 2)
	
	Departure_date <- 0
	
	x <- as.Date(x, "%d.%m.%Y")
	
	## PLOT
	plot <- ggplot(subset1, aes(x = subset1$DAYS_TO_DEPARTURE, y = subset1$acumulate)) +
			# step graph
			geom_step(direction = "hv", color = 'black', size = 0.5) +
			# in case you want a line unifying the points
			#geom_line(color = 'steelblue', size = 1) +
			#geom_point(size = 0.5, color = 'blue') +
			scale_shape_manual(values = 16) +
			#geom_hline(yintercept = max(subset1$CAPACITY), linetype = "dashed") +
			geom_vline(xintercept = Departure_date, linetype = "dashed", color = "black") +
			# Scale
			scale_x_reverse(lim = c(100,0)) +
			scale_y_continuous(limits = c(0, 150), breaks = seq(0,  150, by = 50)) +
			# Labels 
			theme_bw() +
			#theme_light() +
			theme(
					plot.title = element_text(lineheight = .8, size = 12, 
							face = "bold", color = "black"),
					panel.border = element_rect(size = 1),
					axis.text = element_text(colour = "black", size = 12),
					axis.title.x = element_text(size = 12),
					axis.title.y = element_text(size = 12),
					axis.ticks.length = unit(0.25, "cm")
			) +
			labs(title = paste("", format(x, "%d.%m.%y")), y = "N(t)", x = "Days to departure")
	
	return(plot)
}



#' @title plotbookingcurve function for multiplebooking curve

#' 'fixes axes and size of fonts
#' @description Plot the Booking Curve for a particular flight (departure date) 
#' @param x: departure date in ""
#' @return a accumulated arrival ggplot curve for a particular departure date in
#' the data set 

plotbookingcurve_for_multiple <- function(data, x){
  
  subset1 <- subsetbookings_date(data, x)
  # Accumulate function
  acumulate <- Reduce("sum", subset1$PAX_COUNT, accumulate = TRUE)

    # Attach accumulate
  subset1 <- data.frame(subset1, acumulate)
  
  ## Plot the Booking curve
  
  # Last DCP = 22 
  Load_factor <- round(max(acumulate) / subset1$CAPACITY, 2)
  
  Departure_date <- 0
  
  x <- as.Date(x, "%d.%m.%Y")
  
  ## PLOT
  plot1 <- ggplot(subset1, aes(x=subset1$DAYS_TO_DEPARTURE, y=subset1$acumulate)) +
    # step graph
    geom_step(direction = "hv", color = 'black', size = 0.5) +
    # in case you want a line unifying the points
    #geom_line(color = 'steelblue', size = 1) +
    #geom_point(size = 0.5, color = 'blue') +
    scale_shape_manual(values = 16) +
    #geom_hline(yintercept = max(subset1$CAPACITY), linetype="dashed") +
    geom_vline(xintercept = Departure_date, linetype = "dashed", color = "black") +
    # Scale
    scale_x_reverse(lim = c(100,0)) +
    scale_y_continuous(limits = c(0, 150), breaks = seq(0,  150, by = 50)) +
    # Labels 
    theme_bw() +
    #theme_light() +
    theme(
      plot.title = element_text(lineheight = .8, size = 10, 
                                face = "bold", color = "black"),
      panel.border = element_rect(size = 1),
      axis.text = element_text(colour = "black", size = 10),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.ticks.length = unit(0.25,"cm")
    ) +
    labs(title = paste("", format(x, "%d.%m.%y"), "", subset1$DOW), y = "N(t)", x = "")
  
  return(plot1)
}

#' @title multiplebookingcurves function

#' @description Plots multiple booking curves
#' multiplebookingcurves(n,m,j,cols,rows) arguments
#' @param n: initial day to plot
#' @param m: last day to plot
#' @param j: month, example: 'JUN'
#' @param cols: length of columns
#' @param rows: length of rows

multiplebookingcurves <- function(data, n, m, j, cols = 7, rows = 2){
	dates <- c()
	
	if (j == 'JAN'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".01.2016", sep = "")
			} else dates[i] <- paste(i, ".01.2016", sep = "")
		}
	} else if (j == 'FEB'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".02.2016", sep = "")
			} else dates[i] <- paste(i, ".02.2016", sep = "")
		}
	} else if (j == 'MAR'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".03.2016", sep="")
			} else dates[i] <- paste(i, ".03.2016", sep="")
		}
	} else if (j == 'APR'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".04.2016", sep = "")
			} else dates[i] <- paste(i, ".04.2016", sep = "")
		}
	} else if (j == 'MAY'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".05.2016", sep = "")
			} else dates[i] <- paste(i, ".05.2016", sep = "")
		}
	} else if (j == 'JUN'){
		for (i in n:m){
			if (i < 10){
				dates[i]<-paste("0", i, ".06.2016", sep = "")
			} else dates[i]<-paste(i, ".06.2016", sep = "")
		}
	} else if (j == 'JUL'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".07.2016", sep = "")
			} else dates[i] <- paste(i, ".07.2016", sep = "")
		}
	} else if (j == 'AUG'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0",i, ".08.2016", sep = "")
			} else dates[i] <- paste(i, ".08.2016", sep = "")
		}
	} else if (j == 'SEP'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".09.2016", sep = "")
			} else dates[i] <- paste(i, ".09.2016", sep = "")
		}
	} else if (j == 'OCT'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".10.2016", sep = "")
			} else dates[i] <- paste(i, ".10.2016", sep = "")
		}
	} else if (j == 'NOV'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".11.2016", sep = "")
			} else dates[i] <- paste(i, ".11.2016", sep = "")
		}
	} else if (j == 'DEC'){
		for (i in n:m){
			if (i < 10){
				dates[i] <- paste("0", i, ".12.2016", sep = "")
			} else dates[i] <- paste(i, ".12.2016", sep = "")
		}
	}
	
	plots <- list() # needs to be defined in the global environment
	for (i in n:m){
		p1 = plotbookingcurve_for_multiple(data, dates[i])
		plots[[i]] <- p1
	}
	layout <- matrix(c(seq(1, (m - n) + 1, 1)), ncol = cols, nrow=rows, byrow = TRUE)
	return(multiplot(plotlist = plots[n:m] , layout = layout))
}
