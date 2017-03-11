# TODO: Add comment
# 
# Author: Fernando
###############################################################################

#' fillNull_previousDates for a certain departure_date
#' @paramtere data: Booking data
#' @parameter departure_date: specific departure date 
#' @parameter initial_BH: beginning of the Booking Horizon
#' @return: filled data frame with previous null BKs since initial_BH

fillNull_previousDates <- function(data, departure_date, initial_BH){
	
	# focus on one single date
	
	subset1 <- subsetbookings_date(data, departure_date)
	
	# get acumulated sum
	
	acumulate <- Reduce("sum", subset1$PAX_COUNT, accumulate = TRUE) 
	
	# add acumulated sum

	subset1 <- data.frame(subset1, acumulate)
	
	# fill days to complete the sequence before first BK
	
	a <- sort(seq(subset1$DAYS_TO_DEPARTURE[1], initial_BH, by = 1), 
			decreasing = TRUE) 
	
	# copy the data frame without rows
	
	dat <- slice(BookingData, 0) 
	
	for (i in (1 : (length(a) - 1))){ # until one before the first BK
		
		dat[i, ] <- list(
		                 unique(subset1$ORIGIN), 
		                 unique(subset1$DESTINATION), 
		                 unique(subset1$AIRLINE_CODE),
		                 unique(subset1$FLIGHT_NR),
		                 unique(subset1$ROUTING),
		                 departure_date, 
		                 unique(subset1$DEPARTURE_TIME), 
		                 NA, NA, NA, NA ,NA ,NA, 0, NA,  
		                 a[i], 
		                 unique(subset1$CAPACITY),
		                 unique(subset1$DOW)
		                 )
		acumulate <- Reduce("sum", dat$PAX_COUNT, accumulate = TRUE)
		
		dat_filled <- data.frame(dat, acumulate)
	}
	
	filled_data <- rbind(dat_filled, subset1)
	
	return(filled_data)
}

#' fillNull_previousDates for the complete data set
#' Loop over all dates in BookingData and generate filled data set
#' @paramtere data: Booking data
#' @parameter initial_BH: beginning of the Booking Horizon
#' @return: filled data frame with previous null BKs since initial_BH

fillNull_all <- function(data, initial_BH){
  
  dates1 <- unique(data$DEPARTURE_DATE)
  
  filled_dataSet <- slice(data, 0)
  filled_dataSet$"acumulate" <- numeric(0)
  
  for (i in 1:length(dates1)){
    
    filled_departureDate <- fillNull_previousDates(data, dates1[i], initial_BH)
    filled_dataSet <- rbind(filled_dataSet, filled_departureDate)
  }
  
  return (filled_dataSet)
}
