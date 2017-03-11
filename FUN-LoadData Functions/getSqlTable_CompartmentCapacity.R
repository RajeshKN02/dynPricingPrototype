#' use a sql query in order to get data for the compartment capacity at a certain time at the booking horizon
#
#' @param origin
#' @param destination
#' @param depTime
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated'
#' @return a data table with 8 relevant columns: AIRLINE_CODE, FLIGHT_NR, ORIGIN, DESTINATION, 
#'                                               DEPARTURE TIME, PHYSICAL CAPACITY, ADJUSTED CAPACITY,
#'                                               TOTAL CAPACITY, DIFFERENCE

getSqlTable_CompartmentCapacity <- function(origin, destination, depTime, startDepDate, endDepDate){
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
                 scd_leg_compartment.COMPARTMENT,
                 scd_leg_compartment.PHYSICAL_CAPACITY,
                 scd_leg_compartment.ADJUSTED_CAPACITY,
                 sum(scd_leg_compartment.PHYSICAL_CAPACITY) over (partition by scd_flights.DEPARTURE_DATE) as Total_Capacity_SCD,
                 scd_flights.CAPACITY,
                 round((scd_flights.CAPACITY-(sum(scd_leg_compartment.PHYSICAL_CAPACITY) over (partition by scd_flights.DEPARTURE_DATE)))/(sum(scd_leg_compartment.PHYSICAL_CAPACITY) over (partition by scd_flights.DEPARTURE_DATE))*100,2) as Difference_percentage
                 FROM scd_flights
                 inner join scd_leg_compartment
                 on scd_leg_compartment.leg_key = scd_flights.FLIGHT_key
                ", buildWhere(conditions),"
                 ORDER by
                 scd_flights.departure_date
                 
                 ")

  result <- data.table(dbGetQuery(dbConnection, query))
  return(result)
}
