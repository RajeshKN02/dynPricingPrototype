#' use a sql query in order to get data for the Transition Model estimation'
#' it displays all trips for a specific odo between two dates' 
#
#' @param odoKey, the odo-key of the trip to be analyzed
#' @param startDepDate, the first date of the time span that should be evaluated
#' @param endDepDate, the last date of the time span that should be evaluated'
#' @param save, save table in a .csv
#' @return a data table with 15 relevant columns: ORIGIN, DESTINATION, ROUTING, DEPARTURE TIME, FARECLASS, SEGMENT COMPARTMENT, 
#                                                 ' PRICE, PRODUCT TYPE, PAX_COUNT, EOT TIMESTAMP, DAYS TO DEPARTURE, CAPACITY'

getSqlTable_Bookings <- function(odoKey, startDepDate, endDepDate, save=FALSE){
    dbConnection <- openOracleConnection()
    startDepDate <- quoteSql(startDepDate)
    endDepDate <- quoteSql(endDepDate)
    conditionsForOdo <- c()
    conditionsForTravelBegin <- c()
    conditionsForTravelEnd <- c()
    conditionsForValidFrom <- c()
    conditionsForSCDflights <- c()
    conditionsForPnrData <- c()
    conditionsForRestrictingPnrData <- c()
    
    conditionsForSCDflights <- appendBetweenCondition(conditionsForSCDflights, "scd_flights.departure_date", paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
    
    conditionsForOdo <- appendInCondition(conditionsForOdo,"oad_odo.odo_key", odoKey)
    conditionsForTravelBegin <- append(conditionsForTravelBegin, paste("to_date(prd_fareclass.travel_begin) <= to_date(",endDepDate,")", sep=""))
    conditionsForTravelEnd <- append(conditionsForTravelEnd, paste("to_date(prd_fareclass.travel_end) >= to_date(",startDepDate,")", sep=""))
    conditionsForValidFrom <- append(conditionsForValidFrom, paste("to_date(prd_fareclass.valid_from) <= to_date(",endDepDate,")", sep=""))
  
    conditionsForPnrData <- appendInCondition(conditionsForPnrData, "oad_pnr_to_odifpos.odo_key", odoKey)
    conditionsForPnrData <- appendBetweenCondition(conditionsForPnrData, "oad_pnr_to_odifpos.DEPARTURE_DATE", paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
  
    conditionsForRestrictingPnrData <- appendInCondition(conditionsForRestrictingPnrData,"oad_odi.odo_key", odoKey)
    conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.PAX_COUNT != 0")
    conditionsForRestrictingPnrData <- appendBetweenCondition(conditionsForRestrictingPnrData,"oad_odi.DEPARTURE_DATE" ,paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))
    conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_odo.shadow_state='L'")
    conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.BOOKING_TYPE_IND = 0")
    conditionsForRestrictingPnrData <- appendInCondition(conditionsForRestrictingPnrData,"oad_pnr_to_odifpos.odo_key", odoKey)
    conditionsForRestrictingPnrData <- appendBetweenCondition(conditionsForRestrictingPnrData, "oad_pnr_to_odifpos.departure_date",paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")") )
    conditionsForRestrictingPnrData <- append(conditionsForRestrictingPnrData," substr(oad_pnr_to_odifpos.segment_farebase_code_curve,10,9) is null")
    
    query <- paste("--
                    --
                   SELECT
                   -- about flight characteristics
                   oad_od.origin,
                   oad_od.destination,
                   oad_flights.airline_code,
                   oad_flights.flight_nr,
                   oad_routing.routing,
                   oad_pnr_to_odifpos.departure_date,
                   scd_odo.BASIC_MIN_DEPARTURE_TIME as departure_time,
                   -- about seat
                   oad_pnr_to_odifpos.fareclass,
                   oad_inv_map.order_nr,
                   sta_booking_class.description,
                   oad_inv_map.SEGMENT_COMPARTMENT,
                   -- about price
                   prd_fareclass_mod.avg_farebase_price1 as price,
                   case restr_prd_fc_36.value
                   when '1.7976931348623157E308' then 'Light'
                   when '65' then 'Classic'
                   else 'Flex'
                   end as Product_Type,
                   -- about booking
                   oad_pnr_to_odifpos.Pax_count as Pax_count,
                   oad_pnr_to_odifpos.eot_timestamp,
                   trunc(oad_pnr_to_odifpos.departure_date-oad_pnr_to_odifpos.eot_timestamp)as days_to_departure,
                   oad_flights.capacity
                   
                   FROM
                   oad_odi
                   inner join oad_odo
                   on oad_odi.odo_key = oad_odo.odo_key
                   inner join oad_routing
                   on oad_odo.ROUTING_KEY = oad_routing.ROUTING_KEY
                   inner join oad_od
                   on oad_od.od_key = oad_routing.od_key
                   inner join oad_pnr_to_odifpos
                   on oad_odi.odo_key = oad_pnr_to_odifpos.odo_key
                   and (oad_pnr_to_odifpos.DEPARTURE_DATE) = (oad_odi.DEPARTURE_DATE)
                   -- it needs filters to work, snapshot_grid
                   -- inner join sta_snapshot_control
                   -- on oad_pnr_to_odifpos.snap_nr = sta_snapshot_control.snap_nr
                   inner join sta_booking_class
                   on oad_pnr_to_odifpos.fareclass = sta_booking_class.identifier
                   inner join oad_odo_to_scd_odo
                   on  oad_odo_to_scd_odo.odo_key=oad_odi.odo_key
                   -- join segment
                   inner join scd_odo
                   on  scd_odo.scd_odo_key = oad_odo_to_scd_odo.scd_odo_key
                   
                   -- getting city codes since prd_fc uses them and oad_od uses airport codes
                   inner join sta_airport origin_sta
                   on oad_od.origin = origin_sta.IATA_AP_CODE
                   inner join sta_airport dest_sta
                   on oad_od.destination = dest_sta.IATA_AP_CODE
                   
                   inner join(select *
                   from scd_flights
                  ",buildWhere(conditionsForSCDflights),"
                   and import_state=1)
                   oad_flights
                   on oad_odo_to_scd_odo.scd_odo_key = oad_flights.scd_odo_key
                   and oad_pnr_to_odifpos.DEPARTURE_DATE=oad_flights.departure_date
                   
                   --################################################################################
                   --prd_fareclass join
                   --################################################################################
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
                   prd_fareclass.valid_from)
                   prd_fareclass_mod
                   on prd_fareclass_mod.ORIGIN = origin_sta.iata_city_code
                   and prd_fareclass_mod.DESTINATION = dest_sta.iata_city_code
                   and trim(substr(oad_pnr_to_odifpos.segment_farebase_code_curve, 1,9)) = prd_fareclass_mod.FARECLASS_CODE
                   and to_date(oad_pnr_to_odifpos.first_booked)
                   between to_date(prd_fareclass_mod.sale_begin) and to_date(prd_fareclass_mod.sale_end)
                   and to_date(oad_odi.departure_date)
                   between to_date(prd_fareclass_mod.TRAVEL_BEGIN) and to_date(prd_fareclass_mod.travel_end)
                   and to_date(oad_pnr_to_odifpos.FIRST_BOOKED) >= to_date(prd_fareclass_mod.valid_from)
                   --################################################################################
                   -- we try to restrict products by join prd_fareclass_restriction
                   -- category 36 tells us for how much the reservation is changeable
                   --we can conclude which product the customer has bought(light, classic, flex)
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
                   sta_inventory_map.SEGMENT_COMPARTMENT,
                   sta_inventory_map.compartment,
                   sta_inventory_map.order_nr
                   from   sta_inventory_map
                   where  sta_inventory_map.inventory_map_id = 'OAD')
                   oad_inv_map
                   on oad_inv_map.fareclass = oad_pnr_to_odifpos.fareclass
                   
                   -- restrictions
                  ", buildWhere(conditionsForRestrictingPnrData),"
                   
                   ORDER by
                   oad_pnr_to_odifpos.departure_date,
                   oad_pnr_to_odifpos.EOT_TIMESTAMP,
                   oad_pnr_to_odifpos.fareclass
                   
                   ")
    
    result <- data.table(dbGetQuery(dbConnection, query))
    
    # save a .csv file from query
    
    if(save == TRUE){
      write.csv(result, paste0("data/BookingData_", unique(result$ORIGIN),
                               "_", unique(result$DESTINATION), "_", 
                              startDepDate, "_", endDepDate, ".csv"),
                row.names = FALSE)}
    
    return(result)
}
