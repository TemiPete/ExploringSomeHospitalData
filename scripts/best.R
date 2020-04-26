# This function takes state name (abbreviations), outcomes (e.g heart attack) as arguments
# reads through a csv file of hospital data
# returns the name of the hospital having the best rate for that outcome. 
# Ranking is by the 30-day mortality rate for that disease (outcome),
# lowest is best 

best <- function(state, outcome){
    
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
    
    # read data into dataframe, df
    df <- read.csv('../data/outcome-of-care-measures.csv', colClasses='character')
    
    # returns error message if state name is incorrect and breaks code
    # if state name is valid, extracts needed columns into dat, stipulates rate column as numeric
    # orders column based on rank
    if (!is.element(state, df$State)){
        print(paste('Error in best(', "'",state,"'", ', ', "'", 
                    outcome, "'", '): invalid state', sep=''))
        break
    } else {
        dat <- df[(df$State==state), ][c(2, pos)] 
        dat[,2] <- as.numeric(dat[,2])
        ordered <- dat[order(dat[2], na.last=NA), ]
    }
    
    # returns first column of first row of ordered dataframe.
    # corresponds to best hospital
    ordered[1,1]
}

#example
best('TX', 'heart attack')


