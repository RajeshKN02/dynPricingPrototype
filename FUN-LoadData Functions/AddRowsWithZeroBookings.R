#' gets a data table when a certain booking class of an odi opens/closes
#'
#' @param odikey: the odikey of the odi
#'
#' @return a data table with booking class and dtd when booking class changes its status

getBookingClassStatus <- function(odiKey){
    conditions <- c()
    conditions <- appendInCondition(conditions,"oad_basic.odi_key", odiKey)

    query <- paste("select oad_odibpos_availability.DAYS_TO_DEPARTURE,
                    oad_basic_for_flight.booking_class,
                    oad_odibpos_availability.closed
                    from oad_odibpos_availability
                    inner join
                    (select oad_basic.ODIBPOS_KEY, oad_basic.BOOKING_CLASS from oad_basic",buildWhere(conditions)," and oad_basic.POS=1) oad_basic_for_flight
                    on oad_basic_for_flight.odibpos_key=oad_odibpos_availability.ODIBPOS_KEY
                    order by oad_basic_for_flight.booking_class, oad_odibpos_availability.DAYS_TO_DEPARTURE DESC")
    result <- data.table(dbGetQuery(dbConnection, query))
    return(result)
}

#' gets a data table when a certain booking class of an odi opens/closes
#'
#' @param odoKey: odo key
#' @param startDepDate: the first departure date of the interval that is considered
#' @param endDepDate: the last departure date of the interval that is considered
#'
#' @return a data table with booking class and dtd when booking class changes its status

getAllFareclassCodes <- function(odoKey, startDepDate, endDepDate){
    startDepDate <- quoteSql(startDepDate)
    endDepDate <- quoteSql(endDepDate)
    saleEndDate <- startDepDate
    saleEndDate[9] <- as.integer(saleEndDate[9]) -1
    conditionsForOdo <- c()
    conditionsForTravelBegin <- c()
    conditionsForSaleBegin <- c()
    conditionsForSaleEnd <- c()
    conditionsForTravelEnd <- c()
    conditionsForValidFrom <- c()
    conditionsForPnrData <- c()

    conditionsForOdo <- appendInCondition(conditionsForOdo,"oad_odo.odo_key", odoKey)
    conditionsForTravelBegin <- append(conditionsForTravelBegin, paste("to_date(prd_fareclass.travel_begin) <= to_date(",endDepDate,")", sep=""))
    conditionsForSaleBegin <- append(conditionsForSaleBegin, paste("to_date(prd_fareclass.sale_begin) <= to_date(",endDepDate,")", sep=""))
    conditionsForSaleEnd <- append(conditionsForSaleEnd, paste("to_date(prd_fareclass.sale_end) >= to_date(",saleEndDate,")", sep=""))
    conditionsForTravelEnd <- append(conditionsForTravelEnd, paste("to_date(prd_fareclass.travel_end) >= to_date(",startDepDate,")", sep=""))
    conditionsForValidFrom <- append(conditionsForValidFrom, paste("to_date(prd_fareclass.valid_from) <= to_date(",endDepDate,")", sep=""))
    conditionsForPnrData<- appendInCondition(conditionsForPnrData, "oad_pnr_to_odifpos.odo_key", odoKey)
    conditionsForPnrData <- appendBetweenCondition(conditionsForPnrData, "oad_pnr_to_odifpos.DEPARTURE_DATE", paste("to_date(",startDepDate,")"), paste("to_date(",endDepDate,")"))

    query <- paste("select
                    prd_fareclass.fareclass_key,
                    prd_fareclass.amount as FARE_BASE_AMOUNT,
                    prd_fareclass.dow as DOW,
                    substr(prd_fareclass.fareclass_code,1,1) as BOOKING_CLASS,
                    prd_fareclass.FARECLASS_CODE as FC_CODE,
                    restr_prd_fc_36.value as REBOOKING_FEE,
                    restr_prd_fc_9.value as APEX_LAST,
                    restr_prd_fc_3.value as SOURCE
                    from   prd_fareclass
                    left join (select
                    prd_fareclass_restriction.CATEGORY_ID,
                    prd_fareclass_restriction.value,
                    prd_fareclass_restriction.FARECLASS_KEY
                    from   prd_fareclass_restriction
                    where  prd_fareclass_restriction.CATEGORY_ID = 36) restr_prd_fc_36
                    on prd_fareclass.fareclass_key = restr_prd_fc_36.FARECLASS_KEY
                    left join (select
                    prd_fareclass_restriction.CATEGORY_ID,
                    prd_fareclass_restriction.value,
                    prd_fareclass_restriction.FARECLASS_KEY
                    from   prd_fareclass_restriction
                    where  prd_fareclass_restriction.CATEGORY_ID = 3) restr_prd_fc_3
                    on prd_fareclass.fareclass_key = restr_prd_fc_3.FARECLASS_KEY
                    left join (select
                    prd_fareclass_restriction.CATEGORY_ID,
                    prd_fareclass_restriction.value,
                    prd_fareclass_restriction.FARECLASS_KEY
                    from   prd_fareclass_restriction
                    where  prd_fareclass_restriction.CATEGORY_ID = 9) restr_prd_fc_9
                    on prd_fareclass.fareclass_key = restr_prd_fc_9.FARECLASS_KEY
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
                    and", conditionsForSaleBegin,"
                    and", conditionsForSaleEnd,"
                    and", conditionsForValidFrom,"
                    and prd_fareclass.sale_begin <= (select max(oad_pnr_to_odifpos.first_booked)
                    from oad_pnr_to_odifpos
                    ",buildWhere(conditionsForPnrData),")
                    and prd_fareclass.sale_end >= (select min(oad_pnr_to_odifpos.first_booked)
                    from oad_pnr_to_odifpos
                    ",buildWhere(conditionsForPnrData),")
                    group  by
                    prd_fareclass.FARECLASS_CODE, prd_fareclass.fareclass_key, prd_fareclass.amount, restr_prd_fc_36.value, prd_fareclass.dow, restr_prd_fc_9.value, restr_prd_fc_3.value")
            result <- data.table(dbGetQuery(dbConnection, query))
            return(result)
}

#' creates a data table with pairs of BC, DTD when BC is opened
#'
#' @param BookingClassDataTable: a data table just like described in return of the method getBookingClassStatus
#'
#' @return a data table with the pairs of (BC, DTD) when bc is open

evaluateBookingClassStatus <- function(BookingClassDataTable){
    v1 <- c()
    v2 <- c()
    bookingClasses <- unique(BookingClassDataTable$BOOKING_CLASS)
    for (i in bookingClasses){
        startOpen <- 360
        open <- c()
        for (j in 1:nrow(BookingClassDataTable)){
            if (BookingClassDataTable[j]$BOOKING_CLASS==i){
                if (BookingClassDataTable[j]$CLOSED=="T"){
                    open <- c(open, seq(startOpen,BookingClassDataTable[j]$DAYS_TO_DEPARTURE))
                }
                if (BookingClassDataTable[j]$CLOSED=="F"){
                    startOpen <-  BookingClassDataTable[j]$DAYS_TO_DEPARTURE
                }
            }
        }
            if (!(startOpen %in% open)){
                open= c(open, seq(startOpen,0))}
            v1 <- c(v1,open)
            v2 <- c(v2,rep(i, length(open)))
        }
    OpenBookingClasses <- data.table(BOOKING_CLASS = v2, DTD = v1)
    return(OpenBookingClasses)
}

#' adds zero bookings to demandDataTable,i.e. add rows where a specific tariff is available but there are no customers to buy it
#'
#' @param demandDataTable: a data table with fare base amount, number of customers.. of the actual bookings
#' @param allFcCodes: a data table just like described in return of the method getAllFcCodes
#'
#' @return a demandDataTable where zero bookings are added

addRowsToDemandDataTable <- function(demandDataTable, allFcCodes){
    allFcCodes <- allFcCodes[is.na(SOURCE)==TRUE | SOURCE=='ATPCO-C']
    odiKeys <- unique(demandDataTable$ODI_KEY)
    result <- data.table()
    for (odiKey in odiKeys){
        # add dow of certain departure date
        weekdayOfDepDate <- wday(unique(demandDataTable[ODI_KEY==odiKey]$DAY_OF_DEPARTURE))
        # get table with booking class and DTD, when bc is open
        fcCodesWithOdi <- evaluateBookingClassStatus(getBookingClassStatus(odiKey))
        fcCodesWithOdi[,ODI_KEY := odiKey]
        fcCodesWithOdi[,DOW_OF_DEP := weekdayOfDepDate]
        # merge this with all Fc Codes to get when a fare is available
        fcCodesWithOdi <- merge(allFcCodes, fcCodesWithOdi, by ="BOOKING_CLASS", allow.cartesian=TRUE)
        fcCodesWithOdi[DOW_OF_DEP==0]$DOW_OF_DEP <- 7
        # dep dow has to match dow
        fcCodesWithOdi <- fcCodesWithOdi[mapply(grepl, DOW_OF_DEP, DOW, fixed = TRUE)]
        # either apex_last is empty, i.e. fare is always available or DTD has to be bigger than apex_last
        fcCodesWithOdi <- fcCodesWithOdi[is.na(APEX_LAST)== TRUE | DTD >= APEX_LAST]
        # maybe change the key to make pos/triptype code relevant too
        setkey(fcCodesWithOdi,FC_CODE,DTD)
        fcCodesWithOdi <- unique(fcCodesWithOdi)
        # merge with odi_key
        temp <- merge(x = demandDataTable[demandDataTable$ODI_KEY == odiKey], y= fcCodesWithOdi, by =c("FC_CODE","DTD","ODI_KEY"), all=TRUE, allow.cartesian=TRUE)
        temp[is.na(temp)] <-0
        # if exists choose attributes from x data table(ifelse)
        temp$FARE_BASE_AMOUNT <- ifelse(temp$FARE_BASE_AMOUNT.x !=0, temp$FARE_BASE_AMOUNT.x, temp$FARE_BASE_AMOUNT.y)
        temp$REBOOKING_FEE <- pmax(temp$REBOOKING_FEE.x, temp$REBOOKING_FEE.y)
        result <- rbind(result,temp)
    }
    result <- result[,.(FARE_BASE_AMOUNT,NUMBER_OF_CUSTOMERS, DTD, REBOOKING_FEE,ODI_KEY)]
    result[REBOOKING_FEE==0]$REBOOKING_FEE <- NA
    return(result)
}




