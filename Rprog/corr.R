corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        filenames <- dir(directory)
        cv <- numeric()
        i <- 0
        for (filename in filenames) {
                mon_data <- read.csv(paste(directory, "/", filename, sep=""))
                mon_na <- mon_data[!is.na(mon_data$sulfate) & !is.na(mon_data$nitrate), ]
                if (threshold > 0 & nrow(mon_na) <= threshold) {
                        next #for
                }
                cv_temp <- cor(mon_na$sulfate, mon_na$nitrate)
                if (!is.na(cv_temp)) {
                        i <- i + 1
                        cv[i] <- cv_temp
                }
        }
        if (i == 0) {
                numeric()
        } else {
                cv
        }

}
