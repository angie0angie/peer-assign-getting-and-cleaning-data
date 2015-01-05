

#step1:Merges the training and the test sets to create one data set.

trdata<-read.table("C:/Users/user/Desktop/peer assign/train/x_train.txt")

trlabel<-read.table("C:/Users/user/Desktop/peer assign/train/y_train.txt")

trsub<-read.table("C:/Users/user/Desktop/peer assign/train/subject_train.txt")

tsdata<-read.table("C:/Users/user/Desktop/peer assign/test/X_test.txt")

tslabel<-read.table("C:/Users/user/Desktop/peer assign/test/y_test.txt")

tssub<-read.table("C:/Users/user/Desktop/peer assign/test/subject_test.txt")

joindata<-rbind(trdata,tsdata)

joinlabel<-rbind(trlabel,tslabel)

joinsub<-rbind(trsub,tssub)

#step2:Extracts only the measurements on the mean and standard deviation for each measurement.

feature<-read.table("C:/Users/user/Desktop/peer assign/features.txt")

meanstddev<-grep("mean\\(\\)|std\\(\\)",feature[,2])

joindata<-joindata[,meanstddev]

#step3:Uses descriptive activity names to name the activities in the data set

act<-read.table("C:/Users/user/Desktop/peer assign/activity_labels.txt")

act2<- tolower(gsub("_","",act[,2]))

substr2<- toupper(substr(act[2,2],8,8))

substr3<- toupper(substr(act[2,3],8,8))

actlabel<- act[joinlabel[,1],2]

joinlabel[,1]<-actlabel

names(joinlabel)<-"activity"

#step4:Appropriately labels the data set with descriptive variable names. 

names(joinsub)<-"subject"

cleandata<-cbind(joinsub,joinlabel,joindata)

#step5:From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

subjectLen <- length(table(joinsub))

activityLen <- dim(act)[1]

columnLen <- dim(cleandata)[2]

result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)

result <- as.data.frame(result)

colnames(result) <- colnames(cleandata)

row <- 1

for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinsub)[, 1])[i]
    result[row, 2] <- act[j, 2]
    bool1 <- i == cleandata$subject
    bool2 <- act[j, 2] == cleandata$activity
    result[row, 3:columnLen] <- colMeans(cleandata[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

head(result)
write.table(result, "data_with_means.txt")

