#You should create one R script called run_analysis.R
#that does the following. 
#Merges the train and the test sets to create one data set.
#Extracts only the measurements on the mean and #standard deviation for each measurement. 


#Uses descriptive activity names to name the activities in the data set Appropriately labels the data set with 
##descriptive variable names. 


#From the data set in step 4, creates a second, independent tidy 
#data set with the average of each variable for each activity and each subject.

#set the working directory

setwd("C:/Users/Steve/Desktop/Data Science Cousera/Getting Data/UCI HAR Dataset")


#import text,train,subject and features into R
features<-read.table("./features.txt",header = F)
activitylabel<-read.table("./activity_labels.txt",header = F)
xtrain<-read.table("./train/X_train.txt",header = F)
ytrain<-read.table("./train/y_train.txt",header = F)
subjecttrain<-read.table("./train/subject_train.txt",header = F)
xtest<-read.table("./test/X_test.txt",header = F)
ytest<-read.table("./test/y_test.txt",header = F)
subjecttest<-read.table("./test/subject_test.txt",header = F)



###merging the train and test data and names these data
#merge subject data and name it 
allsubject<-rbind(subjecttest,subjecttrain)
names(allsubject)<-"subject"

#merge y data and name it 
all_ydata<-rbind(ytest,ytrain)
names(all_ydata)<-"activityid"

#merge x data and name it 
all_xdata<-rbind(xtest,xtrain)
names(all_xdata)<-features$V2


#add activity names into all_ydata
names(activitylabel)<-c("activityid","activityname")
all_ydata$activityname<- activitylabel[all_ydata$activityid,][[2]]

#extract only the mean and standard deviation
extract<-grepl("mean",names(all_xdata))|grepl("std",names(all_xdata))
allmeansd<-all_xdata[,extract]


#merge all the data together
allsubact<-cbind(allsubject,all_ydata)
alldata<-cbind(allsubact,allmeansd)


###label the data set with descriptive names
names(alldata)<-gsub("std()", "SD", names(alldata))
names(alldata)<-gsub("mean()", "MEAN", names(alldata))
names(alldata)<-gsub("^t", "time", names(alldata))
names(alldata)<-gsub("^f", "frequency", names(alldata))
names(alldata)<-gsub("Acc", "Accelerometer", names(alldata))
names(alldata)<-gsub("Gyro", "Gyroscope", names(alldata))
names(alldata)<-gsub("Mag", "Magnitude", names(alldata))
names(alldata)<-gsub("BodyBody", "Body", names(alldata))


#create a new tidy data set
library(plyr)
newdata<-aggregate(.~subject + activityname,alldata,mean)

#rearrange the new data and output it
tidydata<-newdata[order(newdata$subject,newdata$activityname),]
write.table(tidydata,file = "tidydata.txt",row.names = FALSE)
