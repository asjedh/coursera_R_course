corr <- function(directory, threshold = 0) {
    setCorrectWD()

    all_correlations <- numeric()
    for(id in 1:332){
        current_data <- openFile(id)
        if(aboveThreshold(current_data, threshold)) {
            all_correlations <- c(all_correlations, newCorrelation(current_data))
        }
    }

    all_correlations
}

aboveThreshold <- function(data, threshold) {
    sum_complete_cases <- sum(complete.cases(data))
    if(sum_complete_cases > threshold) {
        TRUE
    } else {
        FALSE
    }
}

newCorrelation <- function(data) {
    new_correlation <- cor(data$sulfate, data$nitrate, use ="complete.obs")
    new_correlation
}

openFile <- function(id) {
    file_name <- getFileName(id)
    data <- read.csv(file_name)
}

getFileName <- function(id) {
    if(id < 10) {
        file_name <- paste("00", as.character(id), sep = "")
    }
    else if(id < 100) {
        file_name <- paste("0", as.character(id), sep = "")
    }
    else {
        file_name <- as.character(id)
    }
    paste(file_name, ".csv", sep="")
}

setCorrectWD <- function() {
    wd <- getwd()
    if(wd == "/Users/asjedh/Desktop/Coursera R Course/Week 2/specdata") {

    } else {
        setwd("/Users/asjedh/Desktop/Coursera R Course/Week 2/specdata")
    }
}
