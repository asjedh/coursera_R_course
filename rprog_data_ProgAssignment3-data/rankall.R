rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name

    ## Read outcome data
    checkOutcomeValid(outcome)
    outcome <- if(outcome == "heart attack") {
        11
    } else if(outcome == "heart failure") {
        17
    } else {
        23
    }

    data <- readOutcomeData()

    simplified_data <- data[, c(2, 7, outcome)]

    split_data <- split(simplified_data, simplified_data[[2]])

    hospital <- lapply(split_data, getHospital, num = num)

    ranked_df <- as.data.frame(cbind(hospital, state = names(best_hospitals)))

}

getHospital <- function(df, num) {
    df[[3]] <- as.numeric(df[[3]])
    ordered_vector <- order(df[[3]], df[[1]], na.last = NA)
    df <- df[ordered_vector, ]
    rank <- getRank(num, ordered_vector)
    df[rank, 1]
}

readOutcomeData <- function() {
    setwd('/Users/asjedh/Desktop/Coursera R Course/rprog_data_ProgAssignment3-data/')
    outcomes <- read.csv('outcome-of-care-measures.csv', colClasses = "character")
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

getRank <- function(num, ordered_vector) {
    if(num == "best") num <- 1
    if(num == "worst") num <- length(ordered_vector)

    num
}
