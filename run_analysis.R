##
## Coursera : Getting & Cleaning Data
## Assignment-1
##
## Author : Brian Greiner
##
##
## Assumptions : the working directory has already been set prior to running this script
##               the data is in the folder "UCI HAR Dataset"
##
## ACTIONS :
##  merge the training and test sets to create one data set
##  extract the measurements of the mean and standard deviation for each measurement
##  name the activities in the data set
##  label the data set with activity names
##  produce a tidy data set with the average of each variable for each activity and each subject
#
#

## libraries required
library(reshape)
library(reshape2)

##### merge the test and training data sets
# read the test data
rawTestData <- read.table("UCI HAR Dataset\\test\\X_test.txt", header=FALSE, fill=TRUE)
rawTestActivity <- read.table("UCI HAR Dataset\\test\\y_test.txt", header=FALSE, fill=TRUE)
rawTestSubject <- read.table("UCI HAR Dataset\\test\\subject_test.txt", header=FALSE, fill=TRUE)
ncol(rawTestData)
nrow(rawTestData)
#
# merge activity with test data
rawTestData <- cbind(rawTestData, rawTestActivity)
rawTestData <- cbind(rawTestData, rawTestSubject)
#
# read the training data
rawTrainData <- read.table("UCI HAR Dataset\\train\\X_train.txt", header=FALSE, fill=TRUE)
rawTrainActivity <- read.table("UCI HAR Dataset\\train\\y_train.txt", header=FALSE, fill=TRUE)
rawTrainSubject <- read.table("UCI HAR Dataset\\train\\subject_train.txt", header=FALSE, fill=TRUE)
ncol(rawTrainData)
nrow(rawTrainData)
# merge activity with test data
rawTrainData <- cbind(rawTrainData, rawTrainActivity)
rawTrainData <- cbind(rawTrainData, rawTrainSubject)

# merge the data sets 
mergedData <- rbind(rawTestData, rawTrainData)
ncol(mergedData)
nrow(mergedData)


##### set the column names
colnames(mergedData)[1] <- "tBodyAcc-mean-X"
colnames(mergedData)[2] <- "tBodyAcc-mean-Y"
colnames(mergedData)[3] <- "tBodyAcc-mean-Z"
colnames(mergedData)[4] <- "tBodyAcc-std-X"
colnames(mergedData)[5] <- "tBodyAcc-std-Y"
colnames(mergedData)[6] <- "tBodyAcc-std-Z"
colnames(mergedData)[41] <- "tGravityAcc-mean-X"
colnames(mergedData)[42] <- "tGravityAcc-mean-Y"
colnames(mergedData)[43] <- "tGravityAcc-mean-Z"
colnames(mergedData)[44] <- "tGravityAcc-std-X"
colnames(mergedData)[45] <- "tGravityAcc-std-Y"
colnames(mergedData)[46] <- "tGravityAcc-std-Z"
colnames(mergedData)[81] <- "tBodyAccJerk-mean-X"
colnames(mergedData)[82] <- "tBodyAccJerk-mean-Y"
colnames(mergedData)[83] <- "tBodyAccJerk-mean-Z"
colnames(mergedData)[84] <- "tBodyAccJerk-std-X"
colnames(mergedData)[85] <- "tBodyAccJerk-std-Y"
colnames(mergedData)[86] <- "tBodyAccJerk-std-Z"
colnames(mergedData)[121] <- "tBodyGyro-mean-X"
colnames(mergedData)[122] <- "tBodyGyro-mean-Y"
colnames(mergedData)[123] <- "tBodyGyro-mean-Z"
colnames(mergedData)[124] <- "tBodyGyro-std-X"
colnames(mergedData)[125] <- "tBodyGyro-std-Y"
colnames(mergedData)[126] <- "tBodyGyro-std-Z"
colnames(mergedData)[161] <- "tBodyGyroJerk-mean-X"
colnames(mergedData)[162] <- "tBodyGyroJerk-mean-Y"
colnames(mergedData)[163] <- "tBodyGyroJerk-mean-Z"
colnames(mergedData)[164] <- "tBodyGyroJerk-std-X"
colnames(mergedData)[165] <- "tBodyGyroJerk-std-Y"
colnames(mergedData)[166] <- "tBodyGyroJerk-std-Z"
colnames(mergedData)[201] <- "tBodyAccMag-mean"
colnames(mergedData)[202] <- "tBodyAccMag-std"
colnames(mergedData)[214] <- "tGravityAccMag-mean"
colnames(mergedData)[215] <- "tGravityAccMag-std"
colnames(mergedData)[227] <- "tBodyAccJerkMag-mean"
colnames(mergedData)[228] <- "tBodyAccJerkMag-std"
colnames(mergedData)[240] <- "tBodyGyroMag-mean"
colnames(mergedData)[241] <- "tBodyGyroMag-std"
colnames(mergedData)[253] <- "tBodyGyroJerkMag-mean"
colnames(mergedData)[254] <- "tBodyGyroJerkMag-std"
colnames(mergedData)[266] <- "fBodyAcc-mean-X"
colnames(mergedData)[267] <- "fBodyAcc-mean-Y"
colnames(mergedData)[268] <- "fBodyAcc-mean-Z"
colnames(mergedData)[269] <- "fBodyAcc-std-X"
colnames(mergedData)[270] <- "fBodyAcc-std-Y"
colnames(mergedData)[271] <- "fBodyAcc-std-Z"
colnames(mergedData)[345] <- "fBodyAccJerk-mean-X"
colnames(mergedData)[346] <- "fBodyAccJerk-mean-Y"
colnames(mergedData)[347] <- "fBodyAccJerk-mean-Z"
colnames(mergedData)[348] <- "fBodyAccJerk-std-X"
colnames(mergedData)[349] <- "fBodyAccJerk-std-Y"
colnames(mergedData)[350] <- "fBodyAccJerk-std-Z"
colnames(mergedData)[424] <- "fBodyGyro-mean-X"
colnames(mergedData)[425] <- "fBodyGyro-mean-Y"
colnames(mergedData)[426] <- "fBodyGyro-mean-Z"
colnames(mergedData)[427] <- "fBodyGyro-std-X"
colnames(mergedData)[428] <- "fBodyGyro-std-Y"
colnames(mergedData)[429] <- "fBodyGyro-std-Z"
colnames(mergedData)[503] <- "fBodyAccMag-mean"
colnames(mergedData)[504] <- "fBodyAccMag-std"
colnames(mergedData)[529] <- "fBodyBodyGyroMag-mean"
colnames(mergedData)[530] <- "fBodyBodyGyroMag-std"
colnames(mergedData)[542] <- "fBodyBodyGyroJerkMag-mean"
colnames(mergedData)[543] <- "fBodyBodyGyroJerkMag-std"
colnames(mergedData)[562] <- "activity"
colnames(mergedData)[563] <- "subject"


##### extract the mean and std_dev data
# define which columns we want
wantedColumns <- c(1,2,3,4,5,6,41,42,43,44,45,46,81,82,83,84,85,86,121,122,123,124,125,126,161,162,163,164,165,166,201,202,214,215,227,228,240,241,253,254,266,267,268,269,270,271,345,346,347,348,349,350,424,425,426,427,428,429,503,504,529,530,542,543,562,563)

# extract just the columns we want
extractedData <- mergedData[,wantedColumns]
ncol(extractedData)
nrow(extractedData)

# change activity names from numeric to descriptive text
activityName<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
allActivity<-extractedData$activity
for(i in 1:nrow(extractedData)){extractedData$activity[i]=activityName[allActivity[i]]}
head(extractedData)



##### create a tidy data set with average of each variable for each activity and subject
fullMelt <- melt(extractedData, id=c("subject","activity"))
fullCast <- cast(fullMelt, activity+subject~variable, mean)
head(fullCast)
tail(fullCast)
#
# save the tidy data set as a CSV file
write.csv(fullCast, file="TidyDataSet.csv")


#####
##### -----  end of script  -----
#####
