# This function takes outcomes (e.g heart attack) 
# and a num (corresponding to a rank) as arguments
# reads through a csv file of hospital data
# returns a dataframe of all hospitals across each state that
# correspond to the outcome and rank (given by the number/rate of death in a 30-day period 
# for that outcome)
#
# If number of hospitals in a state is smaller than the user-given rank, 
# returns NA for the hospital name

rankall <- function(outcome, num){
    
    #stipulates column to extract by using outcome argument
    #Also checks validity of outcome. Spelling should be right
    if (outcome=='heart attack'){
        pos <- 11
    } else if (outcome=='heart failure'){
        pos <- 17
    } else if (outcome=='pneumonia'){
        pos <- 23
    } else {
        print(paste('Error in best(', "'",state,"'", ', ', "'", 
                    outcome, "'", '): invalid outcome', sep=''))
        break
    }
    
    #Read data into dataframe, df
    df <- read.csv('../data/outcome-of-care-measures.csv', colClasses='character', na.strings = 'Not Applicable')
    dat <- cbind(df['Hospital.Name'], df['State'], df[pos]) #extract needed columns, dat
    names(dat) <- c('hospital', 'State', 'Rate') #rename the column headers
    dat$Rate <- as.numeric(dat$Rate) #converts Rate column into numeric class
    dat$State <- as.factor(dat$State) #convert State column into factor class
    
    #Removes observations with NA values as Rates and order according to 
    #State, Rate and hospital name. This might be redundant (see next code)
    #but I left it to optimize later. 
    ordered <- dat[with(dat, order(dat$State, dat$Rate, dat$hospital)), ]
    
    # Rank within each state
    final <- as.data.frame(ordered %>% group_by(State) %>% mutate(Rank = order(order(Rate))))
    
    stateList <- unique(final$State) #get a list of all state abbreviations
    
    output <- data.frame() #initialize a dataframe, output
    
    # Loop through stateList, subset by state abbrev. and 
    # extract hospital and corresponding rank input by user,
    # in a row-wise fashion, then append to a dataframe, finalResult. 
    # Finally, rbind into output.
    
    for (state in stateList){
        stateData <- subset(final, State==state)
        
        if(num == "best") {
            rank <- 1
        } else if(num == "worst") {
            rank <- nrow(stateData)
        } else {
            rank <- num 
        }
        finalResult <- data.frame(hospital = stateData[rank,1], State = state)
        output <- rbind(output, finalResult)
        
    }
    
    # fix each column as character columns
    output$hospital <- as.character(output$hospital)
    output$State <- as.character(output$State)
    
    #return output
    output
    
}

#example
rankall('heart attack', 20)
