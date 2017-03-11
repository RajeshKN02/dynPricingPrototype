#' uses a sql query in order to get data to estimate a demand curve
#' it views all trips of a specific odo between two dates
#'
#' @param odoKey, the odo-key of the trip to be analyzed
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated
#'
#' @return a data table with 3 columns, price, numbofcustomers and value
#' value is used for differentiating between the three lufthansa product
#' numbofcustomers denotes the number of customers who paid this specific price

getDataTableFromSqlForLegs <- function(odoKey, startDepDate, endDepDate){
  dbConnection <- openOracleConnection()
  startDepDate <- quoteSql(startDepDate)
  endDepDate <- quoteSql(endDepDate)
  conditionsForOdo <- c()
  conditionsForTravelBegin <- c()
  conditionsForTravelEnd <- c()
  conditionsForValidFrom <- c()
  conditionsForPnrData <- c()
  conditionsForRestrictingPnrData <- c()
  
  conditionsForOdo <- appendInCondition(conditionsForOdo,"oad_odo.odo_key", odoKey)
  conditionsForTravelBegin <- append(conditionsForTravelBegin, paste("to_date(prd_fareclass.travel_begin) <= to_date(",endDepDate,")", sep=""))
  conditionsForTravelEnd <- append(conditionsForTravelEnd, paste("to_date(prd_fareclass.travel_end) >= to_date(",startDepDate,")", sep=""))
  conditionsForValidFrom <- append(conditionsForValidFrom, paste("to_date(prd_fareclass.valid_from) <= to_date(",endDepDate,")", sep=""))
  
  conditionsForPnrData<- appendInCondition(conditionsForPnrData, "oad_pnr_to_odifpos.odo_key", odoKey)
  conditionsForPnrData <- appendBetweenCondition(conditionsForPnrData, "oad_pnr_to_odifpos.DEPARTURE_DATE", paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
  
  conditionsForRestrictingPnrData <- appendInCondition(conditionsForRestrictingPnrData,"oad_odi.odo_key", odoKey)
  conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.PAX_COUNT != 0")
  conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.BOOKING_TYPE_IND = 0")
  conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_inv_map.compartment = 'M' ")
  conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"(restr_prd_fc_1.value is NULL or restr_prd_fc_1.value='ADT')")
  conditionsForRestrictingPnrData <- appendBetweenCondition(conditionsForRestrictingPnrData,"oad_odi.DEPARTURE_DATE" ,paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
  conditionsForRestrictingPnrData <- appendInCondition(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.odo_key", odoKey)
  conditionsForRestrictingPnrData <- appendBetweenCondition(conditionsForRestrictingPnrData, "oad_pnr_to_odifpos.departure_date",paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")") )
  conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData," substr(oad_pnr_to_odifpos.segment_farebase_code_curve,10,9) is null")
  
  query <- paste("-- aim: estimate a demand curve, so we are selecting the prices and number of passengers buying it
                 -- input: odoKey and first dep date and last dep date
                 
                 select
                 prd_fareclass_mod.avg_farebase_price1 as fare_base_amount,
                 sum(oad_pnr_to_odifpos.Pax_count) as number_of_customers,
                 trunc(oad_pnr_to_odifpos.departure_date - oad_pnr_to_odifpos.first_booked) as DTD,
                 restr_prd_fc_36.value as rebooking_fee
                 from   oad_odi
                 inner join oad_odo
                 on oad_odi.odo_key = oad_odo.odo_key
                 inner join oad_routing
                 on oad_odo.ROUTING_KEY = oad_routing.ROUTING_KEY
                 inner join oad_od
                 on oad_od.od_key = oad_routing.od_key
                 inner join oad_pnr_to_odifpos
                 on oad_odi.odo_key = oad_pnr_to_odifpos.odo_key
                 and (oad_pnr_to_odifpos.DEPARTURE_DATE) = (oad_odi.DEPARTURE_DATE)
                 -- getting city codes since prd_fc uses them and oad_od uses airport codes
                 inner join sta_airport origin_sta
                 on oad_od.origin = origin_sta.IATA_AP_CODE
                 inner join sta_airport dest_sta
                 on oad_od.destination = dest_sta.IATA_AP_CODE
                 -- prd_fareclass doesn't have a foreign key so just group by the attributes we have and choose an arbitrary one
                 -- they seem not to differ in price so that's legitimate for our purpose
                 inner join (select
                 -- selecting one row of the possible ones by using max of the primary key
                 max(prd_fareclass.fareclass_key) as max_fareclasskey,
                 sum(prd_fareclass.amount) / count(*) as avg_farebase_price1,
                 prd_fareclass.FARECLASS_CODE,
                 prd_fareclass.ORIGIN,
                 prd_fareclass.DESTINATION,
                 prd_fareclass.sale_begin,
                 prd_fareclass.sale_end,
                 prd_fareclass.TRAVEL_BEGIN,
                 prd_fareclass.travel_end,
                 prd_fareclass.valid_from
                 from   prd_fareclass
                 -- filter the table in order to make group by somewhat efficient
                 where prd_fareclass.origin in (select distinct sta_airport.IATA_CITY_CODE
                 from sta_airport
                 inner join oad_od
                 on oad_OD.origin=sta_airport.IATA_AP_CODE
                 inner join oad_routing
                 on oad_routing.od_key=oad_od.od_key
                 inner join oad_odo
                 on oad_odo.routing_key=oad_routing.routing_key
                 ",buildWhere(conditionsForOdo),")
                 and prd_fareclass.destination in (select distinct sta_airport.IATA_CITY_CODE
                 from sta_airport
                 inner join oad_od
                 on oad_OD.destination=sta_airport.IATA_AP_CODE
                 inner join oad_routing
                 on oad_routing.od_key=oad_od.od_key
                 inner join oad_odo
                 on oad_odo.routing_key=oad_routing.routing_key
                 ",buildWhere(conditionsForOdo),")
                 and", conditionsForTravelBegin,"
                 and", conditionsForTravelEnd,"
                 and", conditionsForValidFrom,"
                 and prd_fareclass.sale_begin <= (select max(oad_pnr_to_odifpos.first_booked)
                 from oad_pnr_to_odifpos
                 ",buildWhere(conditionsForPnrData),")
                 and prd_fareclass.sale_end >= (select min(oad_pnr_to_odifpos.first_booked)
                 from oad_pnr_to_odifpos
                 ",buildWhere(conditionsForPnrData),")
                 group  by
                 prd_fareclass.FARECLASS_CODE,
                 prd_fareclass.ORIGIN,
                 prd_fareclass.DESTINATION,
                 prd_fareclass.sale_begin,
                 prd_fareclass.sale_end,
                 prd_fareclass.TRAVEL_BEGIN,
                 prd_fareclass.travel_end,
                 prd_fareclass.valid_from) prd_fareclass_mod
                 on prd_fareclass_mod.ORIGIN = origin_sta.iata_city_code
                 and prd_fareclass_mod.DESTINATION = dest_sta.iata_city_code
                 and trim(substr(oad_pnr_to_odifpos.segment_farebase_code_curve, 1,9)) = prd_fareclass_mod.FARECLASS_CODE
                 and to_date(oad_pnr_to_odifpos.first_booked) between to_date(prd_fareclass_mod.sale_begin) and to_date(prd_fareclass_mod.sale_end)
                 and to_date(oad_odi.departure_date) between to_date(prd_fareclass_mod.TRAVEL_BEGIN) and to_date(prd_fareclass_mod.travel_end)
                 and to_date(oad_pnr_to_odifpos.FIRST_BOOKED) >= to_date(prd_fareclass_mod.valid_from)
                 
                 -- we try to restrict products by joing prd_fareclass_restriction
                 -- category 36 tells us for how much the reservation is changeable
                 -- we can conclude which product the customer has bought(light, classic, flex)
                 -- light= 1.7976931348623157E308  , classic=65, flex=NULL
                 left join (select
                 prd_fareclass_restriction.CATEGORY_ID,
                 prd_fareclass_restriction.value,
                 prd_fareclass_restriction.FARECLASS_KEY
                 from   prd_fareclass_restriction
                 where  prd_fareclass_restriction.CATEGORY_ID = 36)
                 restr_prd_fc_36
                 on prd_fareclass_mod.max_fareclasskey = restr_prd_fc_36.FARECLASS_KEY
                 -- category 1 is the passenger type, e.g. adult....
                 left join (select
                 prd_fareclass_restriction.CATEGORY_ID,
                 prd_fareclass_restriction.value,
                 prd_fareclass_restriction.FARECLASS_KEY
                 from   prd_fareclass_restriction
                 where  prd_fareclass_restriction.CATEGORY_ID = 1)
                 restr_prd_fc_1
                 on prd_fareclass_mod.max_fareclasskey = restr_prd_fc_1.FARECLASS_KEY
                 -- we join sta_inventory_map to get the compartment
                 inner join(select
                 sta_inventory_map.fareclass,
                 sta_inventory_map.compartment
                 from   sta_inventory_map
                 where  sta_inventory_map.inventory_map_id = 'OAD') oad_inv_map
                 on oad_inv_map.fareclass = oad_pnr_to_odifpos.fareclass
                 -- now we restrict our query
                 ", buildWhere(conditionsForRestrictingPnrData),"
                 group by
                 restr_prd_fc_36.value, prd_fareclass_mod.avg_farebase_price1, trunc(oad_pnr_to_odifpos.departure_date - oad_pnr_to_odifpos.first_booked)
                 order by
                 restr_prd_fc_36.value
                 
                 ")
  
  result <- data.table(dbGetQuery(dbConnection, query))
  return(result)
}
