# Formats file name depending on isTrain flag
# * isTrain: flag which oiints in which appendix we should add to file name. If isTrain = FALSE, file name will be "..._test", otherwise it'll be "..._train"
# Returns: formatted file name. 
formatFileName <- function(name, isTrain){
    if(isTrain)
    {
      name <- paste(name, "_train", sep="")
    }
    else
    {
      name <- paste(name, "_test", sep="")
    }
    name <- paste(name, "txt", sep=".")
    name
}

# Formats path to a file
# * path: Directory which contains the file
# * name: File name
# Returns: Path to a file 
formatFilPath <- function(path, name){
    filePath <- paste(path, name, sep="/")
    filePath
}

# Loads Subject, X and Y datasets from a folder
# * path: Directory which contains "test" and "train" folders
# * isTrain: Flag which points which folded should be used. If isTrain = FALSE, "test" folder will be used, otherwise "train" folder will be used.
# Returns: a list which contains loaded datasets in following format: "subject, x, y"
loadData <- function(path, isTrain){
    subjectFileName <- formatFileName("subject", isTrain)
    xFileName <- formatFileName("x", isTrain)
    yFileName <- formatFileName("y", isTrain)

    folder <- "test"
    if(isTrain)
    {
      folder <- "train"
    }
  
    subject = read.table(formatFilPath(paste(path, folder, sep="/"),subjectFileName),header=FALSE)
    x = read.table(formatFilPath(paste(path, folder, sep="/"),xFileName),header=FALSE)
    y = read.table(formatFilPath(paste(path, folder, sep="/"),yFileName),header=FALSE)
  
    list(subject = subject, x = x, y = y)
}

# Loads and combining datasets from "train" and "test" folders
# * path: Directory which contains "test" and "train" folders
# Returns:  a list which combined datasets in following format: "xDataSet, yDataSet, subjectDataSet"
mergeData <- function (path) {
    trainData <- loadData(path, TRUE)
    testData <- loadData(path, FALSE)
    
    #Organizing and combining raw data sets into single one.
    xDataSet <- rbind(trainData$x, testData$x)
    yDataSet <- rbind(trainData$y, testData$y)
    subjectDataSet <- rbind(trainData$subject, testData$subject)
    
    list(xDataSet = xDataSet, yDataSet = yDataSet, subjectDataSet = subjectDataSet)
}

# Filters "features" dataset to keep only desired columns, i.e. mean() and std().
# * path: Directory which contains "features.txt" file
# Returns:  Filtered "features" subet
getFilteredFeatures <- function (xDataSet, path) {
    #xData subset based on the logical vector to keep only desired columns, i.e. mean() and std().
    xDataSet_mean_std <- xDataSet[, grep("-(mean|std)\\(\\)", read.table(paste( path, "features.txt", sep="/"))[, 2])]
    names(xDataSet_mean_std) <- read.table(paste( path, "features.txt", sep="/"))[grep("-(mean|std)\\(\\)", read.table(paste( path, "features.txt", sep="/"))[, 2]), 2] 
    xDataSet_mean_std
}

# Names the activities in the data set
# * yDataSet: dataset which contains the activities
# * path: Directory which contains "activity_labels.txt" file
# Returns:  dataset with names Activities
nameActivities <- function (yDataSet, path) {
    #Use descriptive activity names to name the activities in the data set.
    yDataSet[, 1] <- read.table(paste( path, "activity_labels.txt", sep="/"))[yDataSet[, 1], 2]
    names(yDataSet) <- "Activity"
    yDataSet
}

#Preparing result dataset and labels the data set with descriptive variable names
# * path: working directory which contains all files from archive
#Returns:  data set with descriptive variable names
prepareDataSet <-function (path) {
    mergedData <- mergeData(path)
    names(mergedData$subjectDataSet) <- "Subject"
    filteredDataSet <- getFilteredFeatures(mergedData$xDataSet, path)
    
    # Organizing and combining all data sets into single one.
    singleDataSet <- cbind(filteredDataSet, nameActivities(mergedData$yDataSet, path), mergedData$subjectDataSet)
    
    # Defining descriptive names for all variables.
    names(singleDataSet) <- make.names(names(singleDataSet))
    names(singleDataSet) <- gsub('Acc',"Acceleration",names(singleDataSet))
    names(singleDataSet) <- gsub('GyroJerk',"AngularAcceleration",names(singleDataSet))
    names(singleDataSet) <- gsub('Gyro',"AngularSpeed",names(singleDataSet))
    names(singleDataSet) <- gsub('Mag',"Magnitude",names(singleDataSet))
    names(singleDataSet) <- gsub('^t',"TimeDomain.",names(singleDataSet))
    names(singleDataSet) <- gsub('^f',"FrequencyDomain.",names(singleDataSet))
    names(singleDataSet) <- gsub('\\.mean',".Mean",names(singleDataSet))
    names(singleDataSet) <- gsub('\\.std',".StandardDeviation",names(singleDataSet))
    names(singleDataSet) <- gsub('Freq\\.',"Frequency.",names(singleDataSet))
    names(singleDataSet) <- gsub('Freq$',"Frequency",names(singleDataSet))
    
    singleDataSet
}

#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
# * path: working directory which contains all files from archive
#New dataset is saved into "tidydata.txt" file
crateSecondDataSet <- function (path) {
    singleDataSet <- prepareDataSet(path)
    
    Data2<-aggregate(. ~Subject + Activity, singleDataSet, mean)
    Data2<-Data2[order(Data2$Subject,Data2$Activity),]
    write.table(Data2, file = paste(path, "tidydata.txt", sep="/"),row.name=FALSE)
}


#programm start

#load necessary libraries
library(plyr)
library(data.table)

directory <- "UCI HAR Dataset"
if (!file.exists(directory)) {
    # Download the archive if it not exists
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    tmp_file <- "./temp.zip"
    download.file(url,tmp_file)
    # Unzip downloaded archive
    unzip(tmp_file, exdir="./")
    unlink(tmp_file)
}

crateSecondDataSet(directory)
