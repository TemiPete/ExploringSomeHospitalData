# This function takes state name (abbreviations), outcomes (e.g heart attack) 
# and a num (corresponding to a rank) as arguments
# reads through a csv file of hospital data
# returns the name of the hospital having the num-given rank for that outcome. 
# Ranking is by the 30-day mortality rate for that disease (outcome),
# lower is better. 
# If number of hospitals in a state is smaller than the user-given rank, 
# returns NA for the hospital name

rankhospital <- function(state, outcome, num){
    
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
    df <- read.csv('../data/outcome-of-care-measures.csv', colClasses='character')
    
    #Check validity of state name
    if (!is.element(state, df$State)){
        print(paste('Error in best(', "'",state,"'", ', ', "'", 
                    outcome, "'", '): invalid state', sep=''))
        break
    }
    
    dat <- df[(df$State==state), ][c(2, pos)] #extracts needed columns into dat
    names(dat)[names(dat) == names(dat[2])] <- 'Rate' #renames column 2 of dat
    dat[,2] <- as.numeric(dat[,2]) #converts Rate column into numeric class
    
    #orders dat by Rate, then alphabetically, coerces NA values
    ordered <- dat[with(dat, order(dat$Rate, dat$Hospital.Name, na.last=NA)), ] 
    
    #creates Rank column and ranks by Rate in the form:1,2,3,4,5,6,7...
    ordered['Rank'] <- rank(ordered$Rate, ties.method = 'first')
    
    #returns best ranking, if asked for 'best', returns worst state if asked for 'worst
    if (num=='best') {
        num <- 1
    } else if (num=='worst') {
        num <- tail(ordered$Rank, n=1)
    } else if (num > nrow(df)){
        print('NA')
        break
    }
    #output
    #print(head(ordered, 20))
    ordered[ordered$Rank==num, ]$Hospital.Name
}

#example
rankhospital('TX', 'heart failure', num=3)

