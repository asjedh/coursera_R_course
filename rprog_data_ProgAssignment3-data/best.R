best <- function(state, outcome) {
    ## Read outcome data and split data by state

    by_state <- readOutcomeDataByState()

    ## Check if state input is valid
    state_info <- checkStateInfoValid(by_state, state)

    ## Check if outcome input is valid
    checkOutcomeValid(outcome)

    ## Extract correct outcome from data
    outcome_data <- extractOutcomeData(state_info, outcome)

    ## Figure out which rows have the minimum value
    min_rows <- findRowsWithMinimum(outcome_data)

    ## Get the names of the hospitals using the minimum rows
    getHospitalName(state_info, min_rows)
}

findRowOfMin <- function(outcome_row, min) {
    if(is.na(outcome_row)) {
        FALSE
    } else if(outcome_row == min) {
        TRUE
    } else {
        FALSE
    }
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

extractOutcomeData <- function(state_info, outcome) {
    outcome_data <- if(outcome == "heart attack") {
        state_info[11]
    } else if(outcome == "heart failure") {
        state_info[17]
    } else {
        state_info[23]
    }
    outcome_data <- as.data.frame(lapply(outcome_data, as.numeric))
}

findRowsWithMinimum <- function(outcome_data) {
    state_min <- min(outcome_data, na.rm = TRUE)
    logical_rows <- apply(outcome_data, 1, findRowOfMin, min = state_min)
    min_rows <- which(logical_rows)
}

getHospitalName <- function(state_info, min_rows) {
    hospital_names <- state_info[min_rows, 2]
    sorted_hospital_names <- sort(hospital_names)
    sorted_hospital_names[1]
}

readOutcomeDataByState <- function() {
    setwd('/Users/asjedh/Desktop/Coursera R Course/rprog_data_ProgAssignment3-data/')
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
    by_state <- split(outcomes, outcomes$State)
}
