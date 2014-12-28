rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    by_state <- readOutcomeDataByState()

    ## Check if state input is valid
    state_info <- checkStateInfoValid(by_state, state)

    ## Check if outcome input is valid
    checkOutcomeValid(outcome)

    ## Sort data by outcome and hospital name
    sorted_data <- sortDataByOutcome(state_info, outcome)

    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    hospital_name <- getHospitalFromRank(sorted_data, num)

    hospital_name
}

readOutcomeDataByState <- function() {
    setwd('/Users/asjedh/Desktop/Coursera R Course/rprog_data_ProgAssignment3-data/')
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    by_state <- split(outcomes, outcomes$State)
}

checkStateInfoValid <- function(by_state, state) {
    state_info <- by_state[[state]]
    if(is.null(state_info)) {
        stop("invalid state")
    }
    state_info
}

checkOutcomeValid <- function(outcome) {
    valid_outcome <- any(outcome == c("heart attack", "heart failure", "pneumonia"))
    if(!valid_outcome) {
        stop("invalid outcome")
    }
}

sortDataByOutcome <- function(state_info, outcome) {
    if(outcome == "heart attack") {
        state_info[[11]] <- as.numeric(state_info[[11]])
        ordered_outcome <- order(state_info[[11]], state_info[[2]], na.last = NA)
    } else if(outcome == "heart failure") {
        state_info[[17]] <- as.numeric(state_info[[17]])
        ordered_outcome <- order(state_info[[17]], state_info[[2]], na.last = NA)
    } else {
        state_info[23] <- as.numeric(state_info[[23]])
        ordered_outcome <- order(state_info[[23]], state_info[[2]], na.last = NA)
    }
    state_info[ordered_outcome, ]
}

getHospitalFromRank <- function(sorted_data, num) {
    if(num == "best") num <- 1
    if(num == "worst") num <- length(sorted_data[[2]])

    sorted_data[num, 2]
}
