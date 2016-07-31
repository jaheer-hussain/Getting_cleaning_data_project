## Set the file path and the working directory
filesPath <- "~/Documents/jaheer-work/R/datacleanup-assignments/UCI HAR Dataset"
setwd(filesPath)

## Make sure that the dplyr , data.table and tidyr libraries are installed

##  Download and unzip the Data
f_dwnload <- function() {
  if(!file.exists("./data"))
  {
    dir.create("./data")
  }
  fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")
  
  ###Unzip DataSet to /data directory
  unzip(zipfile="./data/Dataset.zip",exdir="./data")
}

## Read the files and create data tables.

f_create_tidyset <- function() {

  # Read subject files
  dSubjectTrain <- tbl_df(read.table(file.path(filesPath, "train", "subject_train.txt")));
  dSubjectTest  <- tbl_df(read.table(file.path(filesPath, "test" , "subject_test.txt" )));
  
  # Read activity files
  dActivityTrain <- tbl_df(read.table(file.path(filesPath, "train", "Y_train.txt")));
  dActivityTest  <- tbl_df(read.table(file.path(filesPath, "test" , "Y_test.txt" )));
  
  #Read data files.
  dTrain <- tbl_df(read.table(file.path(filesPath, "train", "X_train.txt" )));
  dTest  <- tbl_df(read.table(file.path(filesPath, "test" , "X_test.txt" )));
  
##}

##f_merge <- function() {
  # for both Activity and Subject files this will merge the training and the test sets by row binding 
  #and rename variables "subject" and "activityNum"
  alldataSubject <- rbind(dSubjectTrain, dSubjectTest)
  setnames(alldataSubject, "V1", "subject")
  alldataActivity<- rbind(dActivityTrain, dActivityTest)
  setnames(alldataActivity, "V1", "activityNum")
  
  #combine the DATA training and test files
  dataTable <- rbind(dTrain, dTest)
  
  # name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
  dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
  setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
  colnames(dataTable) <- dataFeatures$featureName
  
  #column names for activity labels
  activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
  setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))
  
  # Merge columns
  alldataSubjAct<- cbind(alldataSubject, alldataActivity)
  dataTable <- cbind(alldataSubjAct, dataTable)
# }
# 
# f_extract <- function()
# {
  # Reading "features.txt" and extracting only the mean and standard deviation
  dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name
  
  # Taking only measurements for the mean and standard deviation and add "subject","activityNum"
  
  dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
  dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 
# }
# 
# f_arrange <- function()
# {
  ##enter name of activity into dataTable
  dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
  dataTable$activityName <- as.character(dataTable$activityName)
  
  ## create dataTable with variable means sorted by subject and Activity
  dataTable$activityName <- as.character(dataTable$activityName)
  dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
  dataTable<- tbl_df(arrange(dataAggr,subject,activityName))
# }
# f_assign <- function()
# {
  names(dataTable)<-gsub("std()", "SD", names(dataTable))
  names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
  names(dataTable)<-gsub("^t", "time", names(dataTable))
  names(dataTable)<-gsub("^f", "frequency", names(dataTable))
  names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
  names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
  names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
  names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))
# }
# 
# f_create <- function()
# {
  write.table(dataTable, "FinalTidyData.txt", row.name=FALSE)
}

main_function <- function ()
  {
  f_dwnload();
  f_create_tidyset();

}
