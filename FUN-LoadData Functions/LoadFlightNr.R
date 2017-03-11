
#' Takes routing and the time span into account and returns the odo_key with the most 
#' booking data for those parameters'
#' @param origin: i.e. 'TXL'
#' @param destination
#' @param depTime  
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated
#' @return flight_key

getFlightKey <- function(origin, destination, depTime, startDepDate, endDepDate){
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
                    SELECT
                    scd_flights.flight_key
                    FROM scd_flights   
                  ",buildWhere(conditions),"
                    ")
    result <- data.table(dbGetQuery(dbConnection, query))
    print(result)
    return(result)
}



