##############################################################################
#
# run_analysis.R
#
#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and
#3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
#You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
  
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

#Here are the data for the project:
  
#  https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#

install.packages("dplyr")
library(dplyr)

#-------------------------------------------------------------------------------
# STEP 1 - Get data
#-------------------------------------------------------------------------------

# download zip file containing data
ziplink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(ziplink, zipFile, mode = "wb")
}

# decompress zip file containing data
datafilePath <- "UCI HAR Dataset"
if (!file.exists(datafilePath)) {
  unzip(zipFile)
}


#-------------------------------------------------------------------------------
# STEP 2 - Read data
#-------------------------------------------------------------------------------

# read training data
trainingSubjects <- read.table(file.path(datafilePath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(datafilePath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(datafilePath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(datafilePath, "test", "subject_test.txt"))
testValues <- read.table(file.path(datafilePath, "test", "X_test.txt"))
testActivity <- read.table(file.path(datafilePath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(datafilePath, "features.txt"), as.is = TRUE)

# read activity labels
activities_labels <- read.table(file.path(datafilePath, "activity_labels.txt"))
colnames(activities_labels) <- c("activityId", "activityLabel")


#-------------------------------------------------------------------------------
# Step 3 - Merge the training and the test sets to create one data set
#-------------------------------------------------------------------------------

# concatenate individual data tables to make single data table
mergedataset <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(mergedataset) <- c("subject", features[, 2], "activity")


#-------------------------------------------------------------------------------
# Step 4 - Extract only the measurements on the mean and standard deviation
#          for each measurement
#-------------------------------------------------------------------------------

# determine columns of data set to keep based on column name...
keepcolumns <- grepl("subject|activity|mean|std", colnames(mergedataset))

# keep data in these columns only
mergedataset <- mergedataset[, keepcolumns]


#-------------------------------------------------------------------------------
# Step 5 - Use descriptive activity names to name the activities in the data
#          set
#-------------------------------------------------------------------------------

# replace activity values with named factor levels
mergedataset$activity <- factor(mergedataset$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


#-------------------------------------------------------------------------------
# Step 4 - Appropriately label the data set with descriptive variable names
#-------------------------------------------------------------------------------

# get column names
mergedatasetCols <- colnames(mergedataset)

# remove special characters
mergedatasetCols <- gsub("[\\(\\)-]", "", mergedatasetCols)

# expand abbreviations and clean up names
mergedatasetCols <- gsub("^f", "frequencyDomain", mergedatasetCols)
mergedatasetCols <- gsub("^t", "timeDomain", mergedatasetCols)
mergedatasetCols <- gsub("Acc", "Accelerometer", mergedatasetCols)
mergedatasetCols <- gsub("Gyro", "Gyroscope", mergedatasetCols)
mergedatasetCols <- gsub("Mag", "Magnitude", mergedatasetCols)
mergedatasetCols <- gsub("Freq", "Frequency", mergedatasetCols)
mergedatasetCols <- gsub("mean", "Mean", mergedatasetCols)
mergedatasetCols <- gsub("std", "StandardDeviation", mergedatasetCols)

# correct typo
mergedatasetCols <- gsub("BodyBody", "Body", mergedatasetCols)

# use new labels as column names
mergedatasetCols
colnames(mergedataset) <- mergedatasetCols
mergedataset

#-------------------------------------------------------------------------------
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
#-------------------------------------------------------------------------------

# group by subject and activity and summarise using mean
mergedatasetMean <- mergedataset %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(mergedatasetMean, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)