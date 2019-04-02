run_analysis <- function() {
  #load the zip file with the raw data first
  data<-unzip("getdata_projectfiles_UCI HAR Dataset.zip")
  
  #get the feature names for the 561 attributes
  features<-read.table("./UCI HAR Dataset/features.txt") #features
  activity<-read.table("./UCI HAR Dataset/activity_labels.txt") #activity
  colnames(activity) <- c("ActivityID", "Activity")
  
  ##get the training values
  train<-read.table("./UCI HAR Dataset/train/X_train.txt")
  activity_train<-read.table("./UCI HAR Dataset/train/y_train.txt")
  subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  ##then get the test values
  test<-read.table("./UCI HAR Dataset/test/X_test.txt")
  activity_test<-read.table("./UCI HAR Dataset/test/y_test.txt")
  subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  #merge them together
  merged<-rbind(train,test)
  activity_merge<-rbind(activity_train,activity_test)
  subject_merge<-rbind(subject_train,subject_test)

  #apply the column names to the merged data set
  colnames(merged) <- features[,2] #first column is just the column numbers again
  colnames(activity_merge) <- "ActivityID"
  colnames(subject_merge) <- "SubjectID"
  
  #then merge the whole data set together with labels
  Total_merge<-cbind(subject_merge, activity_merge, merged)
  
  #now select only the columns with mean and std in them for the mean and standard deviation of each measurement
  smnames<-grep("*mean\\(\\)|*std\\(\\)|ActivityID|SubjectID",names(Total_merge))
  final<-Total_merge[ , smnames]

  #Now add in the descriptive names
  labelled<-merge(final, activity, by="ActivityID") 
  labelled<-labelled[, c(2,ncol(labelled), 3:(ncol(labelled)-1))]

  tidy<-aggregate(.~SubjectID+Activity,labelled,mean)
  tidy<-arrange(tidy,SubjectID)
  write.table(tidy, "week 4 answer.txt", row.names = FALSE, quote = FALSE)

  ret<- list(final,tidy)
  return(ret)
}
