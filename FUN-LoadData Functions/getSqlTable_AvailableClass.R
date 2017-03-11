#' use a sql query in order to get data for the available class at a certain time at the booking horizon
#
#' @param origin
#' @param destination
#' @param depTime
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated'
#' @return a data table with 8 relevant columns: AIRLINE_CODE, FLIGHT_NR, ORIGIN, DESTINATION, 
#'                                               DEPARTURE TIME, IS_ON_SALE, SEGMENT_AVAILABLE, DAYS TO DEPARTURE,

getSqlTable_AvailableClass <- function(origin, destination, depTime, startDepDate, endDepDate){
  dbConnection <- openOracleConnection()
  origin<- quoteSql(origin)
  destination <-quoteSql(destination)
  depTime <-quoteSql(depTime)
  startDepDate <- quoteSql(startDepDate)
  endDepDate <- quoteSql(endDepDate)
  conditions <- c()
  
  conditions <- append(conditions,paste("scd_flights.ORIGIN = ", origin))
  conditions <- append(conditions,paste("scd_flights.DESTINATION = ", destination))
  conditions <- append(conditions,paste("scd_flights.DEPARTURE_TIME =", depTime))
  conditions <- appendBetweenCondition(conditions,"scd_flights.DEPARTURE_DATE", paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
  conditions <- append(conditions,"scd_flights.import_state = 1")

  query <- paste("--
                 --
                 SELECT
                 scd_flights.flight_key,
                 scd_flights.airline_code,
                 scd_flights.flight_nr,
                 scd_flights.departure_date,
                 scd_flights.DEPARTURE_TIME,
                 --scd_flights.flight_key,
                 scd_segment_class_availability.FARECLASS,
                 scd_segment_class_availability.IS_ON_SALE,
                 scd_segment_class_availability.SEGMENT_AVAILABILITY,
                 scd_segment_class_availability.DAYS_TO_DEPARTURE
                 FROM scd_flights
                 inner join scd_segment_class_availability
                 on scd_segment_class_availability.SEGMENT_KEY = scd_flights.FLIGHT_key
                ", buildWhere(conditions),"
                 ORDER by
                 scd_flights.departure_date,
                 scd_segment_class_availability.DAYS_TO_DEPARTURE desc
                 
                 ")

  result <- data.table(dbGetQuery(dbConnection, query))
  return(result)
}
