## Load the libraries

library(data.table)
library(dplyr)

## Read the feature names into a character vector 
features <- fread("./data/features.txt",select=c(2))[[1]]

## Filter mean and standard deviation variable names
filterFeaturesIndex <- (grepl("mean\\(\\)", features) | grepl("std\\(\\)", features)) ## Get the column index 
filterFeaturesNames <- features[filterFeaturesIndex] ## Get the column names 

## Tidying the variable names
filterFeaturesNames <- gsub('\\-(\\w?)', '\\U\\1', filterFeaturesNames, perl=TRUE) ##  Remove '-' and capitalize following letter
filterFeaturesNames <- gsub("\\W", "\\U\\1", filterFeaturesNames, perl=TRUE) ## Remove the non word characters
filterFeaturesNames <- gsub("^t", "time", filterFeaturesNames, perl=TRUE) ## Making varibles more descriptive
filterFeaturesNames <- gsub("^f", "frequency", filterFeaturesNames, perl=TRUE) ## Making varibles more descriptive
filterFeaturesNames <- gsub("BodyBody","Body", filterFeaturesNames, perl=TRUE) ## Get rid of duplicate problem

## Read the training data and combine them
trainingSubjects <- fread("./data/train/subject_train.txt") ## Read the training subjects

## Note: Below dataset causes fread to crash... open bug in data.table with this data...
trainingSet <- as.data.table(read.table("./data/train/X_train.txt",header=F, sep="", as.is=T)) ## Read the training set
trainingActivity <- fread("./data/train/y_train.txt") ## Read training activity labels
trainingSet <-  cbind(trainingSet,trainingSubjects,trainingActivity) ## Merge the training data

## Read the test data and combine them
testSubjects <- fread("./data/test/subject_test.txt") ## Read the test subjects

## Note: Below dataset causes fread to crash... open bug in data.table with this data...
testSet <- as.data.table(read.table("./data/test/X_test.txt",header=F, sep="", as.is=T)) ## Read the test set
testActivity <- fread("./data/test/y_test.txt") ## Read test activity labels
testSet <-  cbind(testSet,testSubjects,testActivity) ## Merge the test data

## Merge training and test data
mergedData <- rbindlist(list(trainingSet,testSet))

## Extract std and mean measurements along with subject and labels 
filteredData <- mergedData[,c(filterFeaturesIndex,T,T),with=FALSE]
## Assign descriptive column names
setnames(filteredData,c(filterFeaturesNames,"subject","activity"))

## Assign descriptive names to the activities
filteredData[, `:=`(activity= ifelse(activity==1,"WALKING",
                                     ifelse(activity==2,"WALKING_UPSTAIRS",
                                            ifelse(activity==3,"WALKING_DOWNSTAIRS",
                                                   ifelse(activity==4,"SITTING",
                                                          ifelse(activity==5,"STANDING","LAYING"))))))
             ]

## Tidy data set with the average of each variable for each activity and each subject.
tidyData <- filteredData %>% group_by(subject,activity) %>% summarise_each(funs(mean,max,min))

tidyData## Output the tody dataset
write.table(tidyData, "./Subject_Activity_Averages.txt",row.names=FALSE)
