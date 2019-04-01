run_analysis <- function() {
  #load the zip file with the raw data first
  data<-unzip("getdata_projectfiles_UCI HAR Dataset.zip")

  ##get the X&Y test values
  X_test<-read.table(data[15])
  Y_test<-read.table(data[16])
  
  ##then get the training values
  X_train<-read.table(data[27])
  Y_train<-read.table(data[28])
  
  #merge them together
  X_merge<-merge(X_test,X_train,all=TRUE)
  #Y_merge<-merge(Y_test,Y_train,all=TRUE) not sure why made a DF of millions of rows
  Y_merge<-rbind(Y_test,Y_train) #this worked though
  
  #save some memory
  rm(X_test)
  rm(Y_test)
  rm(X_train)
  rm(Y_train)
  
  #get the feature names for the 561 attributes
  features<-read.table(data[2])
  names<-as.vector(features[,2]) #first column is just the column numbers again
  
  #apply the column names to the merged data set
  colnames(X_merge) <- names
  
  #now select only the columns with mean and std in them for the mean and standard deviation of each measurement
  smnames<-grep("std|mean",names)
  SM_merge<-X_merge[,smnames]
  
  #Now add a column for the activity type to the filtered data attach the activity type to each measurement
  #so as to produce the full data
  final<-cbind(Y_merge,SM_merge)
  #add its description for the activity type
  colnames(final)[1] <- "Activity"
  #and then change the numbers to strings for easier reading.
  ##read in the data labels, the first file in the zip file
  labels<-read.table(data[1])
  activity_labels<-as.vector(labels[,2])
  final$Activity <- as.factor(final$Activity) #set it as a factor
  levels(final$Activity) <- activity_labels #then label the levels
  
  #Now produce the average for each column according to the 6 factor levels
  #get it started , then do remaining columns to average out
  final_mean<-tapply(final[[2]],final$Activity,mean) 
  final_mean<-as.data.frame(final_mean)
  colnames(final_mean)[1] <- colnames(final)[2]
  
  for(i in (2:ncol(final)-1)) {
    temp<-tapply(final[[i+1]],final$Activity,mean)
    final_mean<-cbind(final_mean,temp)
    colnames(final_mean)[i] <- colnames(final)[i+1] #plus one because somehow the first column became a factor and not a numeric vector
  }
  ret<- list(final,final_mean)
  return(ret)
}
