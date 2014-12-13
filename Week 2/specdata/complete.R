complete <- function(directory, id = 1:332) {
    setCorrectWD()

    complete_data <- data.frame()

    for(id in id) {
        file_name <- getFileName(id)
        current_data <- read.csv(file_name)
        sum_complete_obs <- getSumCompleteObs(current_data)
        new_row <- c(id, sum_complete_obs)
        complete_data <- rbind(complete_data, new_row)
    }

    colnames(complete_data) <- c("id", "nobs")
    complete_data
}


setCorrectWD <- function() {
    wd <- getwd()
    if(wd == "/Users/asjedh/Desktop/Coursera R Course/Week 2/specdata") {

    } else {
        setwd("/Users/asjedh/Desktop/Coursera R Course/Week 2/specdata")
    }
}

getSumCompleteObs <- function(data) {
    sum(complete.cases(data))
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
