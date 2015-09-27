# Getting and Cleaning Data - Project

# Question 1
# Merge training and test sets

# 1.1 Train data: read data, labels and subject codes
XTrainData    <- read.table("./train/X_train.txt")       # data
yTrainData    <- read.table("./train/y_train.txt")       # labels
trainSubjects <- read.table("./train/subject_train.txt") # subjects

# 1.2 Test data: read data, labels and subject codes
XTestData    <- read.table("./test/X_test.txt")         # data
yTestData    <- read.table("./test/y_test.txt")         # labels
testSubjects <- read.table("./test/subject_test.txt")   # subjects



# Merge test & train data tables by row-binding
XTrainTestData <- rbind(XTrainData,XTestData)

# Merge the two sets of activity labels (coded as integers)
yTrainTestLabels    <- rbind(yTrainData,yTestData)

# merge the two subject codes lists
trainTestSubjectCodes <- rbind(trainSubjects,testSubjects)



# Question 2
# Extract the measurements on the mean and sd for each measurement

# Read in the feature names  
featureNames   <- read.table("./features.txt")

# Identify features that are means or standard deviations of measurements
# The following code identifies a vector of boolean values that correspond to the means and
# standard deviation measures.
meanORsd  <- grepl("(-std\\(\\)|-mean\\(\\))",featureNames$V2)

# Delete columns not features' means nor SD's
XTrainTestDataLite <- XTrainTestData[, which(meanORsd==TRUE)]



# Question 3
# Uses descriptive activity names to name the activities in the data set

# Read the set of activity labels from the txt file
activityNames  <- read.table("./activity_labels.txt")

# Take "yTrainTestLabels" from (integer) codes to factors and make levels intelligible
activity           <- as.factor(yTrainTestLabels$V1)
levels(activity)   <- activityNames$V2

# Take subject codes to factors (required for later)
subject <- as.factor(trainTestSubjectCodes$V1)

# bind as a column the allLabels vector to the dataset
XTrainTestDataLite <- cbind(subject,activity,XTrainTestDataLite)





# Question 4
# Appropriately labels the data set with descriptive variable names  
# In this step, the mean and
# standard deviation feature names are cleaned of hyphens and parentheses, and then attached as
# column names to the data set.

# Grab the un"scrubbed" names off featureNames
scrubbedFeatures <- (cbind(featureNames,meanORsd)[meanORsd==TRUE,])$V2

# Scrub the names into an intelligible form and paste them onto the data.frame XTrainTestDataLite
scrubbedFeatures <- sapply(scrubbedFeatures,function(s){ tolower(gsub("(\\(|\\)|\\-)","",s)) })
names(XTrainTestDataLite)[3:ncol(XTrainTestDataLite)] <- scrubbedFeatures

# Output "XTrainTestDataLite" to text file
write.table(XTrainTestDataLite, "XTrainTestDataLite.txt", sep="\t")






# Question 5
# From the data set in step 4, creates a second, independent XTrainTestDataLiteTidy data set
# with the average of each variable for each activity and each subject


# Notes:
# Using the reshape2 library and the melt function, aggregate XTrainTestDataLite
library(reshape2)

# Prepare the data set XTrainTestDataLite for aggregation (by melting it)
XTrainTestDataLiteMelt <- melt(XTrainTestDataLite,id.vars=c("subject","activity"))

# cast the XTrainTestDataLiteMelt data set into a collapsed XTrainTestDataLiteTidy dataset
XTrainTestDataLiteTidy <- dcast(XTrainTestDataLiteMelt, subject + activity ~ variable, mean)

# write the dataset to a file
write.table(XTrainTestDataLiteTidy, "XTrainTestDataLiteTidy.txt", sep="\t")
