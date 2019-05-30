## Load required packages
library(dplyr)


###############################################################################
## STEP 1: Merge the training and the test sets to create one data set

## Read in the invididual data files from the UCI HAR Dataset
testSubject <- read.table("./UCI HAR Dataset/test/subject_test.txt",
                          col.names = "subject")
testLabel <- read.table("./UCI HAR Dataset/test/Y_test.txt",
                        col.names = "activity")
testSet <- read.table("./UCI HAR Dataset/test/X_test.txt")
    
trainSubject <- read.table("./UCI HAR Dataset/train/subject_train.txt",
                           col.names = "subject")
trainLabel <- read.table("./UCI HAR Dataset/train/Y_train.txt",
                         col.names = "activity")
trainSet <- read.table("./UCI HAR Dataset/train/X_train.txt")

## Create the test and train data frames
dfTest <- cbind(testSubject, testLabel, testSet)
dfTrain <- cbind(trainSubject, trainLabel, trainSet)

## Combine the training and testing data frames
dfAll <- rbind(dfTest, dfTrain)

## Clean up unused objects to save memory
rm(testLabel, testSubject, testSet, trainLabel, trainSubject, trainSet, dfTest,
   dfTrain)


################################################################################
## STEP 2: Extract only the values of the mean and standard deviation
## for each measurement.

## Read in features.txt, which contains variable names
features <- read.table("./UCI HAR Dataset/features.txt",
                       colClasses = "character")

## Set the names of the activity variables, leaving the "subject" and "activity" 
## columns intact
varnames <- c("subject", "activity", features$V2)
names(dfAll) <- varnames

## Find the columns in the data set whose names contain "mean()" or "std()"
mean_std <- grep("(mean\\(\\)|std\\(\\))", names(dfAll))

## Get a new frame containing only those columns, plus the subject and activity
dfFilter <- dfAll[ , c(1,2, mean_std)]




################################################################################
## STEP 3: Use descriptive activity names to name the activities in the data set

## Create a character vector corresponding to the types of activity
activities = c("walking", "walkingUpstairs", "walkingDownstairs", "sitting", 
               "standing", "laying")

## Use the activities vector to assign descriptive names
dfFilter$activity <- sapply(dfFilter$activity, function(x) x <- activities[[x]])

## Clean up again
rm(dfAll, activities, mean_std, varnames, features)


################################################################################
##STEP 4: Appropriately label the data set with descriptive variable names

## Get rid of special characters
names(dfFilter) <- gsub("[\\(\\)-]", "", names(dfFilter))

## Clean up the formatting
names(dfFilter) <- gsub("mean", "Mean", names(dfFilter))
names(dfFilter) <- gsub("std", "Std", names(dfFilter))
names(dfFilter) <- gsub("Gyro", "Gyroscope", names(dfFilter))
names(dfFilter) <- gsub("Acc", "Accelerometer", names(dfFilter))
names(dfFilter) <- gsub("BodyBody", "Body", names(dfFilter))


################################################################################
## STEP 5: Create a second, independent tidy data set with the average of each
## variable for each activity and each subject.

## Group the data by subject and label
tidy <- group_by(dfFilter, subject, activity)

## Get the means by group
tidy <- summarize_all(tidy, mean)

## Write the data
write.csv(tidy, "tidy_data.csv", quote = FALSE, row.names = FALSE)
