best <- function(state, outcome){
hosp <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
hosp[, 11] <- as.numeric(hosp[, 11])
hosp[, 17] <- as.numeric(hosp[, 17])
hosp[, 23] <- as.numeric(hosp[, 23])
outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
my_data <- hosp[, c(2,7,outcomes[outcome])]
names(my_data)<-c("hospital","state","outcome")
try <- split(my_data,my_data$state)
what <- as.data.frame(try[state])
best <- what[order(what[3],na.last=NA,decreasing=FALSE),]
return(best[1,1])
}

