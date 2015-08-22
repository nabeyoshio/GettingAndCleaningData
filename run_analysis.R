run_analysis <- function(){
 
  #0. Pre-Req - reading data
  allFeatures <- read.table(".\\Data\\UCIHARDataset\\features.txt")
  activityLabels <-read.table(".\\Data\\UCIHARDataset\\activity_labels.txt")
  
  testSet <- read.table(".\\Data\\UCIHARDataset\\test\\X_test.txt")
  testLabels <- read.table(".\\Data\\UCIHARDataset\\test\\y_test.txt")
  testSubject <- read.table(".\\Data\\UCIHARDataset\\test\\subject_test.txt")
  trainingSet <- read.table(".\\Data\\UCIHARDataset\\train\\X_train.txt")
  trainingLabels <- read.table(".\\Data\\UCIHARDataset\\train\\y_train.txt")
  trainingSubject <- read.table(".\\Data\\UCIHARDataset\\train\\subject_train.txt")
  
  #1.Merges the training and the test sets to create one data set. 
  combinedSet <- rbind(testSet, trainingSet)
  combinedLabels <- rbind(testLabels, trainingLabels)
  combinedSubject <- rbind(testSubject, trainingSubject)
  
  names(combinedLabels) = "Activity"
  names(combinedSubject) = "Subject"
  names(combinedSet) = allFeatures$V2
  
  combinedData <- cbind(combinedSubject, combinedLabels, combinedSet)
  
  #2.Extracts only the measurements on the mean and standard deviation for each measurement.
  combinedData[,grep("std\\(|mean\\(", names(combinedData))]
  
  #3.Uses descriptive activity names to name the activities in the data set
  combinedData$Activity <- activityLabels[combinedLabels[,1], 2]
  
  #4.Appropriately labels the data set with descriptive variable names
  names(combinedData)<-gsub("^t", "time", names(combinedData))
  names(combinedData)<-gsub("^f", "frequency", names(combinedData))
  names(combinedData)<-gsub("Acc", "Accelerometer", names(combinedData))
  names(combinedData)<-gsub("Gyro", "Gyroscope", names(combinedData))
  names(combinedData)<-gsub("Mag", "Magnitude", names(combinedData))
  names(combinedData)<-gsub("BodyBody", "Body", names(combinedData))
  
  #5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  DT <- data.table(combinedData)
  tidyData <- DT5[,lapply(.SD,mean),by="Activity,Subject"]
  write.table(tidyData, file = "tidyData.txt", row.names = FALSE)
}
