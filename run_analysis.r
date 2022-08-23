getwd()

library(dplyr)

# Downloading and unzpping the data

filename <- "wearables.zip"

if(!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(filename) 
}


# Converting the TXT files into Data Frames and naming columns for inspection and identification of which ones are to be merged.

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")


# After creating the Data Frames and exploring the data it was concluded that x_test and x_train are the only Data Frames which
# contain measurements on the mean and standard deviation, therefore, they'll be merged.

X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
XY <- cbind(subject, Y, X)

# Extracts only the measurements on the mean and standard deviation for each measurement

mean_sd <- XY %>% select(subject, code, contains("mean"), contains("std"))

# Applying descriptive activity names to name the activities in the data set

mean_sd$code <- activities[mean_sd$code, 2]

names(mean_sd)[2] = "activity"
names(mean_sd)<-gsub("Acc", "Accelerometer", names(mean_sd))
names(mean_sd)<-gsub("Gyro", "Gyroscope", names(mean_sd))
names(mean_sd)<-gsub("BodyBody", "Body", names(mean_sd))
names(mean_sd)<-gsub("Mag", "Magnitude", names(mean_sd))
names(mean_sd)<-gsub("^t", "Time", names(mean_sd))
names(mean_sd)<-gsub("^f", "Frequency", names(mean_sd))
names(mean_sd)<-gsub("tBody", "TimeBody", names(mean_sd))
names(mean_sd)<-gsub("-mean()", "Mean", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("-std()", "STD", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("-freq()", "Frequency", names(mean_sd), ignore.case = TRUE)
names(mean_sd)<-gsub("angle", "Angle", names(mean_sd))
names(mean_sd)<-gsub("gravity", "Gravity", names(mean_sd))

# Creating an independent tidy data set with the average of each variable for each activity and each subject

TidyWearable <- mean_sd %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(TidyWearable, "TidyWearable.txt", row.name=FALSE)
View(TidyWearable)
