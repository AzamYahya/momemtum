library(reshape2)
library(dplyr)
#Reading test sets
setwd("Z:/Ferguson 13 march/other assignment/DS/Coursera/Data Cleaning/Course Project/Data/UCI HAR Dataset/test")
subject_test <- read.table("subject_test.txt")
X_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")


#Reading training sets
setwd("Z:/Ferguson 13 march/other assignment/DS/Coursera/Data Cleaning/Course Project/Data/UCI HAR Dataset/train")
subject_train <- read.table("subject_train.txt")
X_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")

setwd("Z:/Ferguson 13 march/other assignment/DS/Coursera/Data Cleaning/Course Project/Data/UCI HAR Dataset")
header <- read.table("features.txt") #read features in a column
header <- header[c(-1)] #remove reduntant columns
header <- as.character(header$V2) #the feature column has colnames as V2.The class is changed to character
header <- as.data.frame(gsub("[[:punct:]]", "",header)) # remove non alpha-numeric characters
colnames(X_test) <- header[1:561,] #apply the rows in headers as column names for test set
colnames(X_train) <- header[1:561,]#apply the rows in headers as column names for training set

#string manipulation for testing set
select_mean_test <- X_test[,grep("mean", colnames(X_test))] #select those columns that contains the word mean
select_std_test <- X_test[,grep("std", colnames(X_test))] # select those columns that contains the word std(i.e standard deviation)
X_test <- cbind(select_mean_test,select_std_test) # combine colums for mean and std

#string manipulation for training set
select_mean_train <- X_train[,grep("mean", colnames(X_train))] #select those columns that contains the word mean
select_std_train <- X_train[,grep("std", colnames(X_train))]# select those columns that contains the word std(i.e standard deviation)
X_train <- cbind(select_mean_train,select_std_train)# combine colums for mean and std


#For activity vector contains 6 elemetns of activities
activity <- c('WALKING', 'WALKING_UPSTAIRS', 'WALKING_DOWNSTAIRS', 'SITTING','STANDING','LAYING')
y_train <- as.vector(y_train$V1)# the V1 column contain activities. The class is changed into vector
y_train <- activity[y_train]
y_test <- as.vector(y_test$V1)
y_test <- activity[y_test]# the V1 column contain activities. The class is changed into vector


#cbind the test
combined_test <- cbind(subject_test,y_test,X_test) #combine suject, activity and testing set
combined_train <- cbind(subject_train,y_train,X_train) #combine suject, activity and training set

#The first and second columns are renames as subject and Activity_Label as respectively
colnames(combined_train)[1:2] <- c("subject","Activity_Label") 
colnames(combined_test)[1:2] <- c("subject","Activity_Label")

#Combine the rows of testing and training sets
data = rbind(combined_test, combined_train)


id_labels   = c("subject", "Activity_Label") #list
data_labels = setdiff(colnames(data), id_labels) # list of column names other than id_labels

melt_data      = melt(data, id = id_labels, measure.vars = data_labels) #To make data narrow for the requirement

# Apply mean function to dataset using dcast function
tidy_data1233   = dcast(melt_data, subject+Activity_Label~variable, mean) # calculate the mean for the each subject and each activity

write.table(tidy_data1233, file = "tidy_data123.txt", row.name = FALSE) # write results into notepad++


