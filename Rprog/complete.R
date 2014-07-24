complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        x <- numeric(length(id))
        y <- numeric(length(id))
        i <- 1
        for (n in id) {
                filename <- paste(directory, "/", substr(1000 + n, 2, 4), ".csv", sep="")
                monitor_data <- read.csv(filename)
                x[i] <- n
                y[i] <- nrow(monitor_data[!is.na(monitor_data$sulfate) & !is.na(monitor_data$nitrate), ])
                i <- i + 1
        }
        data.frame(id = x, nobs = y)
}
