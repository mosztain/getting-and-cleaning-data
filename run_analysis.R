## run_analysis.R
## This script should perform the following:

## 1. Merges the training and the test sets to create one 
## data set.
## 2. Extracts only the measurements on the mean and standard
## deviation for each measurement.
## 3. Uses descriptive activity names to name the activities 
## in the data set
## 4. Appropriately labels the data set with descriptive
## variable names
## 5. From the data set in step 4, creates a second,
## independent tidy data set with the average of each
## variable for each activity and each subject

## Pre-processing: The following files need to be loaded into
## data frames to be processed
## Measurement files: 
## - subject_train.txt: subject ID (1 column)
## - X_train.txt: measures (561 columns)
## - Y_train.txt: activity ID (1 column)
## - subject_test.txt: subject ID (1 column)
## - X_test.txt: measures (561 columns)
## - Y_test.txt: activity ID (1 column)
## Reference files: 
## - features.txt: labels for the 561 variables (column values)
## - activity_labels.txt:  labels for each activity

## I'll keep the name of the dfs the same as the file they 
## come from for simplicity.  Make sure you download the
## files on your working directory or adapt the script to
## have the correct path.  The train and test files should 
## be in the appropriate sub-directories

## Loading measurement files 
lines <- scan(file="train/subject_train.txt", what = "", sep="\t")
subject_train <- read.table(text=lines)
lines <- scan(file="train/X_train.txt", what = "", sep="\t")
X_train <- read.table(text=lines)
lines <- scan(file="train/Y_train.txt", what = "", sep="\t")
Y_train <- read.table(text=lines)
lines <- scan(file="test/subject_test.txt", what = "", sep="\t")
subject_test <- read.table(text=lines)
lines <- scan(file="test/X_test.txt", what = "", sep="\t")
X_test <- read.table(text=lines)
lines <- scan(file="test/Y_test.txt", what = "", sep="\t")
Y_test <- read.table(text=lines)

## Loading reference files
lines <- scan(file="features.txt", what = "", sep="\t")
features <- read.table(text=lines)
lines <- scan(file="activity_labels.txt", what = "", sep="\t")
activity_labels <- read.table(text=lines)

## 1. Merges the training and the test sets to create one 
## data set.

## before merging train and test, create a unique file for each with the following format
## (subject, X, Y)
train_data <- cbind(subject_train, X_train, Y_train)
names(train_data)[1] <- 'S'
names(train_data)[ncol(train_data)] <- 'Y'
test_data <- cbind(subject_test, X_test, Y_test)
names(test_data)[1] <- 'S'
names(test_data)[ncol(test_data)] <- 'Y'
## Now I can merge both _data dfs
all_data <- rbind(train_data, test_data)

## 2. Extracts only the measurements on the mean and standard
## deviation for each measurement.

## I'll look first for all the variables that contain either mean or standard deviation
## To do that I need to find all elements of features that contain either 'mean()' or 'std()'
## at the end of their character string.   
## I'll create a logical vector that will have 1's at each position corresponding to a row that
## contains a label for mean or std.  I'll use a reg.expression to find "mean(" or "std("
## I included the "(" on the search as there are some records that have the word "mean" or "std" 
## as part of their string but do not invoke either function (ie. fBodyBodyGyroJerkMag-meanFreq())
needed_cols <- grepl("mean\\(|std\\(", as.character(features[,2]))

## Need to add two more elements to needed_cols to account for the first column ('S') and the last
## one ('Y')
needed_cols <- c(TRUE, needed_cols, TRUE)

## I'll create a new df having only the subject ('S'), mean and std, and activity ('Y') columns
all_data2 <- all_data[,needed_cols]

## 3. Uses descriptive activity names to name the activities 
## in the data set

## I'll add a new column to the set connecting the last column ('Y') to the appropriate 
## label from activity_labels (note that the new column heading will be "V2.y")
all_data3 <- merge(all_data2, activity_labels, by.x = 'Y', by.y = 'V1', sort = FALSE)

## 4. Appropriately labels the data set with descriptive
## variable names

## The names for the label are stored in the features df.
## I'll create a new vector with all the names using the needed_cols logical vector
## I'll use a temporary adaptation of this vector (I had added a first and last element for step 2)

temp_needed_cols <- needed_cols[2:(length(needed_cols)-1)]
new_names <- as.character(features[temp_needed_cols,2])
rm("temp_needed_cols")   ## this temporary variable is not needed any longer
## incorporate descriptive labels for activity and subject
new_names <- c("activity_code", "subject_code", new_names, "activity_description")
names(all_data3) <- new_names

## 5. From the data set in step 4, creates a second,
## independent tidy data set with the average of each
## variable for each activity and each subject

## Before summarising the data I have to group_by the set
## I chose to group by activity and subject (in that order) to follow
## the request.  It could also be done by subject and activity reversing the 
## columns on the group_by statement
all_data4 <- group_by(all_data3, activity_description, subject_code)

## Summarise all the mean values into the final table
all_data_final <- summarise_all(all_data4, .funs = mean)

## Write table into file
write.table(all_data_final, file = "assignment.txt", row.names = FALSE)
