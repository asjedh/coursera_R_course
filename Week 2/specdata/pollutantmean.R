pollutantmean <- function(directory, pollutant, id = 1:332) {
    setCorrectWD()

    total_sum_pollutant <- 0
    total_sum_rows <- 0
    for(id in id) {
        file_name <- getFileName(id)
        current_data <- read.csv(file_name)
        sum_pollutant <- getSumPollutant(current_data, pollutant)
        sum_rows <- getSumRows(current_data, pollutant)

        total_sum_pollutant <- total_sum_pollutant + sum_pollutant
        total_sum_rows <- total_sum_rows + sum_rows
    }

    mean <- total_sum_pollutant/total_sum_rows
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
}

getSumPollutant <- function(data, pollutant) {
    sum(data[,pollutant], na.rm = TRUE)
}

getSumRows <- function(data, pollutant) {
    not_NA_rows <- !is.na(data[, pollutant])
    sum_rows <- sum(not_NA_rows)
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
