#' Prepare data function 
#' @param x: data set
#' Sets Date format, filters for individual bookings and economy compartment

prepare_data<-function(x){

  data_set <- x
  
  #' Change date format departure date
  data_set$DEPARTURE_DATE <- as.Date(data_set$DEPARTURE_DATE, "%Y-%m-%d", 
                                   tz = "GMT")
  data_set$DEPARTURE_DATE <- format(data_set$DEPARTURE_DATE, "%d.%m.%Y")
  
  #' Add DOW variable
  dow <- lubridate::wday(as.Date(data_set$DEPARTURE_DATE, format="%d.%m.%Y"), 
                       label=TRUE)
  data_set[,"DOW"] <- dow
  
  #' Change date format for EOT
  data_set$EOT_TIMESTAMP <- as.POSIXct(data_set$EOT_TIMESTAMP, 
                                     "%Y-%m-%d %H:%M:%S", tz="GMT")
  data_set$EOT_TIMESTAMP <- format(data_set$EOT_TIMESTAMP, "%d.%m.%Y %H:%M:%S")
  
  #' Exclude group bookings Bks > 9
  data_set <- data_set[data_set$PAX_COUNT < 9,]
  
  #' Exclude compartment C bzw. filter with Compartment M
  data_set <- data_set[data_set$SEGMENT_COMPARTMENT == "M",]
  
  return(data_set)
}