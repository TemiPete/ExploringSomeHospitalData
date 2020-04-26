## SOME CODES TO EXPLORE THE DATA


#read in data and explore first rows
outcome <- read.csv('data/outcome-of-care-measures.csv', colClasses='character')
head(outcome)

#See number of columns
ncol(outcome)

#see number of rows
nrow(outcome)

#see names of columns
names(outcome)

#30-day mortality rates of heart attack
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

    