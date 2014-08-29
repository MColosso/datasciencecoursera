# You should create one R script called run_analysis.R that does the following. 
#  1. Merges the training and the test sets to create one data set.
#  2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#  3. Uses descriptive activity names to name the activities in the data set
#  4. Appropriately labels the data set with descriptive variable names. 
#  5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(plyr)

run_analysis <- function() {

  # Define dataset and result folders
  dataset.folder    <- "./UCI HAR Dataset"
  results.folder    <- "./TidyData"
  
  # Define paths to train and test folders
  train.path        <- paste(dataset.folder, "/train", sep="")
  test.path         <- paste(dataset.folder, "/test",  sep="")

  
  # Test if <dataset.folder> exists in current folder
  if (!file.exists(dataset.folder)) {
  	stop(merge("Folder '", dataset.folder, "' not in current folder", sep=""))
  }

  # Create results folder
  if (!file.exists(results.folder)) {
  	dir.create(results.folder)
  }
  

### 1. Merges the training and the test sets to create one data set. #######
  
  # Read train and test data and concatenate both files
  data.from.train <- read.table(paste(train.path, "/", "X_train.txt", sep=""))
  data.from.test  <- read.table(paste(test.path,  "/", "X_test.txt",  sep=""))
  all.data        <- rbind(data.from.train, data.from.test)

  # Read train and test subjects and concatenate both files
  data.from.train <- read.table(paste(train.path, "/", "subject_train.txt", sep=""))
  data.from.test  <- read.table(paste(test.path,  "/", "subject_test.txt",  sep=""))
  subjects        <- rbind(data.from.train, data.from.test)

  # Read train and test activities and concatenate both files
  data.from.train <- read.table(paste(train.path, "/", "y_train.txt", sep=""))
  data.from.test  <- read.table(paste(test.path,  "/", "y_test.txt",  sep=""))
  activities      <- rbind(data.from.train, data.from.test)
  

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

  # Read features (prospective column names)
  features.data   <- read.table(paste(dataset.folder, "/", "features.txt", sep=""), stringsAsFactors=FALSE)[, 2]

  # Select mean() and std() features
  selected.features <- grep("mean[(])|std[(])", features.data)   # (Ignore special meaning of '()' sequence)
  
  # Select only data and features (prospective column names) with mean() or std()
  all.data        <- all.data[, selected.features]
  features.data   <- features.data[selected.features]
  

### 3. Uses descriptive activity names to name the activities in the data set

  # Read activity labels and apply to each activity row
  activity.labels <- read.table(paste(dataset.folder, "/", "activity_labels.txt", sep=""), stringsAsFactors=FALSE)
  for (i in 1:nrow(activities)) {
    activities[i, 1] <- activity.labels[activities[i, 1], 2]
  }


### 4. Appropriately labels the data set with descriptive variable names. #####

  # Addapt column names to standar (all lower case when possible, not have
  #                                 underscores or dots or white spaces)
  #   'tBodyAcc-correlation()-X,Y' -> 'tbodyacccorrelationxy'

  features.data <- tolower(gsub("[ )(.,_-]", "", features.data)) # Discard spaces, parenthesis,
                                                                 # dots, commas, minus and underscore signs
                           
  # Clean column names, improving readability  --DISCARDED CODE--
  #   'tBodyAcc-correlation()-X,Y' -> 'tBodyAcc.correlation.X_Y'
  # features.data   <- gsub(" ", "", features.data)                # Discard white spaces
  # features.data   <- gsub("()", "", features.data, fixed=TRUE)   # Discard '()'
  # features.data   <- gsub("-", ".", features.data)               # Converts '-' in '.'
  # features.data   <- gsub("[(),]", "_", features.data)           # Converts '(', ')' and ',' in '_'
  # features.data   <- gsub("_+", "_", features.data)              # Collapse multiples '_'
  # features.data   <- gsub("_$", "", features.data)               # Discard trailer '_'
  
  
  # Concatenate subject and activities to measures read
  all.data <- cbind(subjects, activities, all.data)

  # Assign column names
  colnames(all.data) <- c("subject", "activity", features.data)
  
  # Write result in results folder
  write.table(all.data,
              file=paste(results.folder, "/", "assignment1.txt", sep=""),
              row.names = FALSE,
              col.names = TRUE)


### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

###########################################################################
#
#   # Create a list of factors to be used in tapply function combining
#   # subject code (as 3 digits, zero padded) and activiy label: '025 WALKING_DOWNSTAIRS'
#   index <- character(nrow(all.data))
#   for (i in 1:nrow(all.data)) {
#     index[i] <- paste(substr(as.character(1000 + all.data$subject[i]), 2, 4),
#                       all.data$activity[i])
#   }
# 
#   # Calculate average for first data column (column 3 of all.data)
#   y <-tapply(all.data[,3], index, mean, na.rm = TRUE)
# 
#   # Convert row names of previous result in subject code and activity labels
#   sub.codes <- as.numeric(substr(rownames(y), 1, 3))
#   act.codes <- substr(rownames(y), 5, 999)
# 
#   # Create initial tidy data set
#   tidy.dataset <- cbind(sub.codes, act.codes, y)
# 
#   # Calculate average for following columns
#   for (i in 4:ncol(all.data)) {
#     y <-tapply(all.data[ , i], index, mean, na.rm = TRUE)
#     tidy.dataset <- cbind(tidy.dataset, y)
#   }
# 
#   # Change column names (same as all.data)
#   colnames(tidy.dataset) <- colnames(all.data)
# 
###########################################################################
#
# All previous code can be replaced with the invocation of a single function
# of plyr package: ddply

  tidy.dataset <- ddply(all.data, .(subject, activity), numcolwise(mean))

# Write result in results folder
  write.table(tidy.dataset,
              file=paste(results.folder, "/", "tidy_dataset.txt", sep=""),
              row.names = FALSE,
              col.names = TRUE)

  return (TRUE)

}