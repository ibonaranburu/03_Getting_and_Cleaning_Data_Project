#************************************Getting and Cleaning Data***************************************************
#*****************************************Peer Assignment********************************************************

##1-Merges the training and the test sets to create one data set.
##2-Extracts only the measurements on the mean and standard deviation for each measurement. 
##3-Uses descriptive activity names to name the activities in the data set
##4-Appropriately labels the data set with descriptive activity names. 
##5-Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##1-Merges the training and the test sets to create one data set.
#total is the dataset
features<-read.table("./UCI HAR Dataset/features.txt")

S_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
X_train<-read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train<-read.table("./UCI HAR Dataset/train/Y_train.txt")

S_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
X_test<-read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test<-read.table("./UCI HAR Dataset/test/Y_test.txt")

S_total<-rbind(S_train,S_test)
X_total<-rbind(X_train,X_test)
Y_total<-rbind(Y_train,Y_test)

total<-c(S_total,X_total,Y_total)
total<-as.data.frame(total)
variable_names<-c("subject",as.character(features$V2), "activity")
names(total)<-variable_names

##2-Extracts only the measurements on the mean and standard deviation for each measurement.
##selected_total is the extracted measurments
library(stringr)

mean_variable<-str_detect(variable_names, "mean")
std_variable<-str_detect(variable_names, "std")
extracted_variables<-ifelse(mean_variable==TRUE | std_variable==TRUE, TRUE,FALSE)

mean_std_variables<-variable_names[which(extracted_variables==TRUE)]
selected_variables<-which(names(total) %in% mean_std_variables)
total[,c(which(names(total) %in% mean_std_variables))]
selected_total<-total[,selected_variables]

##3-Uses descriptive activity names to name the activities in the data set
activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
Y_total[,1] = activities[Y_total[,1], 2]

##4-Appropriately labels the data set with descriptive activity names.
total[,563] = activities[total[,563], 2]
write.table(total, "merged_clean_dataset.txt")

##5-Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
average_data <- aggregate(. ~ activity + subject, total, FUN=mean)
write.table(average_data, "average_dataset.txt")



