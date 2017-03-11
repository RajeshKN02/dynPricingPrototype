
#' Saves output in Excel file

SaveinExcelFile <- function(mydata, fileName){
  
  write.xlsx(mydata, paste0("Output/", fileName, ".xlsx"), row.names = FALSE,
             sheetName="Results_Output", col.names=TRUE)

}

#' Saves output in CSV file

SaveinCSVFile <- function(mydata, fileName){
  
  write.csv(mydata, paste0("Output/", fileName, ".csv"), row.names = FALSE)
  
}

SaveinFile <- function(mydata, fileName, option){
  
  if(option == "csv"){
    SaveinCSVFile(mydata, fileName)
  } else{
      if(option == "excel"){
        SaveinExcelFile(mydata, fileName)
      }
    }
}