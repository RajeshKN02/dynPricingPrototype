#' @title flightinfo function
#' @descritpion Returns a list of properties of the selected flight in the selected time period
#' @param data: data set input
#' @return Origin, Destination, Route, Departure time, Departure dates, Capacities.

flightinfo <- function(x){
  return(list("Origin" = unique(x$ORIGIN), 
        "Destination" = unique(x$DESTINATION),
        "Route" = unique(x$ROUTING),
        "Depature time" = unique(x$DEPARTURE_TIME),
        "Depature dates" = unique(x$DEPARTURE_DATE)
             )
       )
}