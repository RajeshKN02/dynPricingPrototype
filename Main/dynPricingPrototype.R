#source("Main/dynPricingPrototype.R")
source("sourceAll.R")

dynPricingPrototype <- function(){
# not yet finished...
  
    # Read configuration file 
    configFile <- "config.yml"
  
    dynConfig <<- yaml.load_file(configFile)
    
    # Set seed for random generators
    set.seed(dynConfig$technical$random.seed)

    # Request necessary memory (see help(memory.size))
    #memory.limit(dynConfig$technical$memory.required)

    # Start main code
    cat("###########################\ndynPricingPrototype started...\n")
    
    ######################### 1. Load data #####################################
    
    # Open and closes Oracle connection
    BookingData <- loadData()
    
    # Clean data
    BookingData <- prepare_data(BookingData)
    
    ######################### 2. Fit ###########################################
    
    ###### 2.1 Fit Buying Probabilities
    cat("###########################\nFitting Buying Probabilities...
############################\n")
    
    # Fit, Plot and save best Fit and GoF report (21 Graphs and Files)
    
    BestFit_Light <- FIT_Price_Distr_Function(BookingData, "Light")
    BestFit_Classic <- FIT_Price_Distr_Function(BookingData, "Classic")
    BestFit_Flex <- FIT_Price_Distr_Function(BookingData, "Flex")
    
    df.light <- data.frame(matrix(unlist(BestFit_Light), nrow = 7, byrow = T))
    df.classic <- data.frame(matrix(unlist(BestFit_Classic), nrow = 7, byrow = T))
    df.flex <- data.frame(matrix(unlist(BestFit_Flex), nrow = 7, byrow = T))
    
    # Output Best probability distributions
    df.final <- rbind(df.light, df.classic, df.flex)
    
    colnames(df.final) <- c("Product", "DOW", "Distribution",
                            "Parameter 1", "Parameter 2") 
    
    # Remove global objects 
    rm(list = c("df.light", "df.classic", "df.flex"))
    
    # Save output
    SaveinFile(df.final, "Buying_Prob_Distributions", 
               dynConfig$process$save)
    
    ###### 2.2 Fit Arrival Process
    
    # Set max distance first booking - departure, i.e. homogeneous BH
    #initial_BH <- max(unique(BookingData$DAYS_TO_DEPARTURE))
    
    # Add rows from start BH until first recorded booking (takes some time)
    #fillNull_all(BookingData, initial_BH)
    
    #rm(initial_BH)
    
    #cat("###########################\nAdding rows...\n###########################")
    
    # Plot and save arrival pattern for every DOW
    

    # Fit NHPP to each arrival pattern 
    
    #cat("###########################\nFitting NHPP to DOW Booking Curves...\n###########################")
    
    # Plot and save best FIT and summary of Info (7 Graphs and Files)
    
    # Remove global object
    dynConfig <<- NULL
    
    # End main code
    cat("dynPricingPrototype ended\n###########################\n")
    
}

# Start program
dynPricingPrototype()