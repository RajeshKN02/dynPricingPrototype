#' loadData.R
#' Loads data of bookings

loadData <- function(){
    
    # Read configuration file 
    
    configFile <- "config.yml"
  
    dynConfig <<- yaml.load_file(configFile)
  
    if (!dynConfig$dumb$readDump){
      
      cat("###########################\nLoading data from file...\n")
      
      BookingData<-rbind(
        BookingData1 <- read_csv("data/BookingData_TXL_FRA_'01.01.2016'_'30.06.16'.csv"),
        BookingData2 <- read_csv("data/BookingData_TXL_FRA_'01.07.2016'_'31.12.16'.csv")
                  )
      
      # Remove data sets
      rm(BookingData1, BookingData2)
     
    } else {
      
      cat("###########################\nRetrieving data from server...\nIt might take some minutes...")
      
      # Connect to DB
      conn <- openOracleConnection()
      
      # Get data splited in semesters for a quicker query
      
      BookingData1 <- getSqlTable_Bookings(85875, '01.01.2016', '30.06.16', TRUE)
      BookingData2 <- getSqlTable_Bookings(85875, '01.07.2016', '31.12.16', TRUE)
      
      BookingData <- rbind(BookingData1, BookingData2)
      
      rm(BookingData1, BookingData2)
      
      # close Oracle connection 
      closeOracleConnection(conn)
      
    }
    
    # Remove configuration file
    dynConfig <- NULL
    
    cat("Data correclty loaded\n###########################\n")
    
    # Return Booking Data
    return(BookingData)
    
}