# Load required packages
library(data.table)
library(dplyr)


# 1-Merges the training and the test sets to create one data set.

# read in the feature names and the activity labels 
feature_labels <- read.table("UCI HAR Dataset/features.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")

# read in sbject train, activities train, and features train
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
Y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")

# read in subject test, activity test and features test
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
Y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")

# combine subjects(train & test)
subject <- rbind(subject_train, subject_test)
# combine X variables (train & test)
features <- rbind(X_train, X_test)
# combine Y variables(train & test)
activity <- rbind(Y_train, Y_test)

# read in the feature names whic is in the second column transposed
colnames(features) <- t(feature_labels$V2)

# rename the activity column and the subject column with proper names
colnames(activity) <- "Activity"
colnames(subject) <- "SubjectID"
dataSet <- cbind(features,activity,subject)

# 2-Extracts only the measurements on the mean and standard deviation for each measurement.

cols_mean_STD <- grep(".*Mean.*|.*Std.*", names(dataSet), ignore.case=TRUE)

# save the required columns
needed_cols <- c(cols_mean_STD, 562, 563)

needed_data <- dataSet[,needed_cols]

# 3- Uses descriptive activity names to name the activities in the data set
# rename the activity column from activityId to activity Label

needed_data$Activity <- as.character(needed_data$Activity)
for (i in 1:6){
  needed_data$Activity[needed_data$Activity == i] <- as.character(activity_labels[i,2])
}

# 4- Appropriately labels the data set with descriptive variable names.

needed_data$Activity <- as.factor(needed_data$Activity)



# 4. Appropriately label the data set with descriptive activity names. 

names(needed_data)<-gsub("\\()", "", names(needed_data))
names(needed_data)<-gsub("Acc", "Accelerometer", names(needed_data))
names(needed_data)<-gsub("Gyro", "Gyroscope", names(needed_data))
names(needed_data)<-gsub("BodyBody", "Body", names(needed_data))
names(needed_data)<-gsub("^t", "Time", names(needed_data))
names(needed_data)<-gsub("^f", "Frequency", names(needed_data))
names(needed_data)<-gsub("tBody", "TimeBody", names(needed_data))
names(needed_data)<-gsub("-mean()", "Mean", names(needed_data))
names(needed_data)<-gsub("-std()", "STDDev", names(needed_data))
names(needed_data)<-gsub("-freq()", "Frequency", names(needed_data))
names(needed_data)<-gsub("angle", "Angle", names(needed_data))
names(needed_data)<-gsub("gravity", "Gravity", names(needed_data))
names(needed_data)<-gsub("GyroMag", "GyroMagnitude", names(needed_data))
names(needed_data)<-gsub("JerkMag", "JerkMagnitude", names(needed_data))
names(needed_data)<-gsub("([Bb]odyaccjerkmag)", "BodyAccJerkMagnitude", names(needed_data))


# 5- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#needed_data$Subject <- as.factor(needed_data$Subject)
needed_data <- data.table(needed_data)

tidy_data <- aggregate(. ~SubjectID + Activity, needed_data, mean)

tidy_data <- tidy_data[order(tidy_data$SubjectID,tidy_data$Activity),]

write.table(tidy_data, file = "Tidy.txt", row.names = FALSE)

