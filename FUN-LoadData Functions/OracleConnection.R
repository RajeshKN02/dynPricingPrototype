#' Oracle database access; 
#' Parameters are retrieved from configuration file: config.yml 
#'
#' @return  connection object
#' 
openOracleConnection <- function() {
    
  drv <- JDBC(dynConfig$db$JDBC_driver, 
              classPath = dynConfig$db$driver_path)
  
  dbConnect(drv, 
            dynConfig$db$jdbc.url, 
            dynConfig$db$username, 
            dynConfig$db$password)
  
}

#' Closes a connection.
#'
#' @param con The connection to be closed.

closeOracleConnection <- function(con){
  
    dbDisconnect(con)

}
