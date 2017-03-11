#' Source all neccesary packages and function

sourceAll <- function(){
  
  #' Load all packages
  
  cat("###########################\nSourcing packages...\n")
  
  #' Check if packages are installed, if not install and source them
  
  ipak <- function(pkg){
    
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
    }
  
  packages <- c("RJDBC",   # For Oracle Connection
                "data.table", # For SQL Query
                "ggplot2", # Ploting
                "plyr",
                "dplyr",
                "plyr",
                "grid",
                "gridExtra",
                "lubridate", # DOW
                "fitdistrplus", # For fitting
                "Hmisc", # latex tables
                "readr", #' .csv files 
                "yaml", # yaml reading function
                "xlsx", # excel data export
                "MASS", # for fitting
                "actuar" #
                ) 
  
  ipak(packages)
  
  #' Load functions in "FUN-" folders
  
  cat("###########################\nSourcing built functions...\n")
  
  file.sources = list.files(c(folder_0 = "FUN-Utils", # Utilities
                              folder_1 = "FUN-LoadData Functions", # Load Data
                              folder_2 = "FUN-Fitting Functions", # Fitting 
                              folder_3 = "FUN-Plotbooking Functions", # Plots 
                              folder_4 = "FUN-Model Functions", # Model related
                              folder_5 = "FUN-Fitting Functions"), # Fitting
                            pattern="*.R$", full.names = TRUE, 
                            ignore.case = TRUE)

  sapply(file.sources, source, .GlobalEnv)
  
  cat("Sourcing ended\n###########################")
} 

sourceAll()