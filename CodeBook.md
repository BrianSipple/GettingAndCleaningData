## CodeBook for script run_analysis.R

## Data
The data was obtained from the following site :
http://d396qusza40orc.cloudfront.net/getdata/projectfiles/UCI%20HAR%20Dataset.zip

This dataset is from an experiment that monitored the physical activities 30 volunteers.  The volunteers wore a smartphone, and the data from the embedded accelerometer and gyroscopes within the smartphone were recorded.  A more extensive description of the experiment, the sensors, and the data can be obtained from the documentation contained within the data set file.


## Actions of the script
* The data consists of two sets, a training set and and a testing set.  
* The script reads both sets of data, and merges them into a single data set.
* From this combined data set, the mean and stadard deviation for each measurement are extracted
* The columns of the combined data set consisted of the activity, subject_ID, and the various measurements
* A descriptive name is applied to each type of measurement.  These names were obtained from the documentation supplied with the data set.
* The numeric code describing each activity is replaced with a descriptive name.  These names were obtained from the documentation supplied with the data set.
* A second independent data set is created (using"tidy" data set principles), consisting of the average of each variable for each activity and each subject.  This data set is stored as a CSV file.


## The tidy data set
*This data set consists of 180 rows, which represent each of the 6 activities for each of the 30 subjects.  
*Each column consists of one variable (detailed below).


## VARIABLES INCLUDED IN THE SECOND INDEPENDENT DATA SET
activity
* WALKING
* WALKING_UPSTAIRS
* WALKING_DOWNSTAIRS
* SITTING
* STANDING
* LAYING
  
subject
* integer value 1 - 30
  
tBodyAcc-mean-X
tBodyAcc-mean-Y
tBodyAcc-mean-Z
* body accelleration signals, mean value
 
tBodyAcc-std-X
tBodyAcc-std-Y
tBodyAcc-std-Z
* body accelleration signals, standard deviation value
 
tGravityAcc-mean-X
tGravityAcc-mean-Y
tGravityAcc-mean-Z
* gravity accelleration signals, mean value

tGravityAcc-std-X
tGravityAcc-std-Y
tGravityAcc-std-Z 
* gravity accelleration signals, mean value
 
tBodyAccJerk-mean-X
tBodyAccJerk-mean-X
tBodyAccJerk-mean-X
* body jerk signals obtained from linear acceleration and angular velocity, mean value

tBodyAccJerk-std-X
tBodyAccJerk-std-Y
tBodyAccJerk-std-Z
* body jerk signals obtained from linear acceleration and angular velocity, standard deviation value

tBodyGyro-mean-X
tBodyGyro-mean-Y
tBodyGyro-mean-Z
* body gyroscope signals, mean value

tBodyGyro-std-X 
tBodyGyro-std-Y 
tBodyGyro-std-Z
* body gyroscope signals, standard deviation value

tBodyGyroJerk-mean-X
tBodyGyroJerk-mean-Y 
tBodyGyroJerk-mean-Z
* body gyroscope jerk signals obtained from linear acceleration and angular velocity, mean value

tBodyGyroJerk-std-X 
tBodyGyroJerk-std-Y 
tBodyGyroJerk-std-Z
* body gyroscope jerk signals obtained from linear acceleration and angular velocity, standard deviation value

tBodyAccMag-mean
tBodyAccMag-std 
* magnitude of the body three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

tGravityAccMag-mean 
tGravityAccMag-std 
* magnitude of the gravity three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

tBodyAccJerkMag-mean 
tBodyAccJerkMag-std 
* magnitude of the body jerk three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

tBodyGyroMag-mean
tBodyGyroMag-std 
* magnitude of the body gyroscope three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

tBodyGyroJerkMag-mean 
tBodyGyroJerkMag-std 
* magnitude of the body gyroscope jerk three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

fBodyAcc-mean-X 
fBodyAcc-mean-Y 
fBodyAcc-mean-Z
* Fast Fourier Transform applied to the body accelleration, mean values

fBodyAcc-std-X 
fBodyAcc-std-Y 
fBodyAcc-std-Z 
* Fast Fourier Transform applied to the body accelleration, standard deviation values

fBodyAccJerk-mean-X 
fBodyAccJerk-mean-Y 
fBodyAccJerk-mean-Z
* Fast Fourier Transform applied to the body accelleration jerk, mean values

fBodyAccJerk-std-X 
fBodyAccJerk-std-Y 
fBodyAccJerk-std-Z 
* Fast Fourier Transform applied to the body accelleration jerk, standard deviation values

fBodyGyro-mean-X 
fBodyGyro-mean-Y 
fBodyGyro-mean-Z
* Fast Fourier Transform applied to the body gyroscope, mean values

fBodyGyro-std-X 
fBodyGyro-std-Y 
fBodyGyro-std-Z 
* Fast Fourier Transform applied to the body gyroscope, standard deviation values

fBodyAccMag-mean 
fBodyAccMag-std 
* FFT applied to the magnitude of the body three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

fBodyBodyGyroMag-mean
fBodyBodyGyroMag-std 
* FFT applied to the magnitude of the body gyroscope three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values

fBodyBodyGyroJerkMag-mean 
fBodyBodyGyroJerkMag-std 
* FFT applied to the magnitude of the body gyroscope jerk three-dimenstional signals calculated using the Euclidean norm, mean and standard deviation values
