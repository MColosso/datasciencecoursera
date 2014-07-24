pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)

	first_file <- TRUE
	for (n in id) {
		filename <- paste(directory, "/", substr(1000 + n, 2, 4), ".csv", sep="")
		monitor_data <- read.csv(filename)
		if (first_file) {
			pollutant_data <- monitor_data[ , pollutant]
			first_file <- FALSE
		} else {
			pollutant_data <- append (pollutant_data, monitor_data[ , pollutant])
		}
	}
	mean(pollutant_data, na.rm=TRUE)
}
