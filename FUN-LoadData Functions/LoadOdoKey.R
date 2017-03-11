
#' Takes routing and the time span into account and returns the odo_key with the most 
#' booking data for those parameters'
#' @param routing: the routing as string, i.e. 'TXL FRA'
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated
#'
#' @return odo_key, the odo_key with the most booking data

getOdoKeyFromRouting <- function(routing, startDepDate, endDepDate){
    dbConnection<-conn
    conditions <- c()
    conditions <- appendInCondition(conditions,"oad_routing.routing", quoteSql(routing))
    conditions <- appendBetweenCondition(conditions,"oad_pnr_to_odifpos.departure_date",paste("to_date(",quoteSql(startDepDate),")"), paste("to_date(",quoteSql(endDepDate),")"))

    query <- paste("select * from (select oad_pnr_to_odifpos.ODO_KEY, count(*)
                    from oad_pnr_to_odifpos
                    inner join oad_odo
                    on oad_pnr_to_odifpos.odo_key = oad_odo.odo_key
                    inner join oad_routing
                    on oad_odo.routing_key = oad_routing.routing_key",
                    buildWhere(conditions),"
                    group by oad_pnr_to_odifpos.odo_key
                    order by
                    count(*) desc) 
                    where rownum=1")
    result <- data.table(dbGetQuery(dbConnection, query))$ODO_KEY
    print(result)
    return(result)
}



