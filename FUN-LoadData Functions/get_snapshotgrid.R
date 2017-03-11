#' use a sql query in order to get snap shot grid
#
#' 
#' @return a data table snapshot grid
#' 

get_snap_shotgrid<-function(grid=1){
  dbConnection <- openOracleConnection()
  query <- paste("--
                 SELECT 
                 SNAP_NR,
                 DAYS_TO_DEPARTURE
                 FROM
                 STA_SNAPSHOT_CONTROL
                 WHERE",
                 paste0("STA_SNAPSHOT_CONTROL.SNAPSHOT_GRID_KEY=",grid),"
                 ")
  
  result <- data.table(dbGetQuery(dbConnection, query))
  write.csv(result, "data/snapshot_grid.csv")
  return(result)
}          