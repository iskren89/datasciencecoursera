rankall <- function(outcome, num="best"){
  hosp <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", stringsAsFactors=FALSE)
  hosp[, 11] <- as.numeric(hosp[, 11])
  hosp[, 17] <- as.numeric(hosp[, 17])
  hosp[, 23] <- as.numeric(hosp[, 23])
  outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if (!outcome %in% names(outcomes)) {result <- "Invalid Outcome"}
  else{
    my_data <- hosp[, c(2,7,outcomes[outcome])]
    good <- complete.cases(my_data)
    clean <- my_data[good, ]
    names(clean)<-c("hospital","state","outcome")
    need <- clean[order(clean[2],clean[3],clean[1],na.last=NA),]
    try <- split(need,need$state)
    my_names <- sapply(try, function(elt) { elt[,1] })
    if (num=="best"){result <- as.matrix(lapply(my_names, function(elt) { elt[1] }))}
    else if (num=="worst"){result <- as.matrix(lapply(my_names,function(elt) { elt[length(my_names)] }))}
    else {result <- as.matrix(lapply(my_names, function(elt) { elt[num] }))}
  }
  return(result)
}