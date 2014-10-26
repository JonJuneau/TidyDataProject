#run_Analysis.R
#       Uses the following packages:
library(reshape2)
library(dplyr)
library(tidyr)
###             GET THE DATA FROM FILES
#       Get the names of the variables
VarNames <- read.table("../Class Project/UCI HAR Dataset/features.txt")
# VarNames <- tbl_df(VarNames)
#       Get the Activity Labels
ActivityLabels <-
        read.table("../Class Project/UCI HAR Dataset/activity_labels.txt")
ActivityLabels <- tbl_df(ActivityLabels)
#       Get the train data
DataDir <- "../Class Project/UCI HAR Dataset/train"
data_subject_train <- tbl_df(read.table(paste0(DataDir,"/subject_train.txt"),
                                        col.names = c('Subject')))
data_X_train <- tbl_df(read.table(paste0(DataDir,"/X_train.txt"),
                                  col.names = VarNames$V2))
# names(data_X_train) <- VarNames[,2]
data_Y_train <- tbl_df(read.table(paste0(DataDir,"/Y_train.txt"),
                                  col.names = c('Activity')))
#       Get the test data
DataDir <- "../Class Project/UCI HAR Dataset/test"
data_subject_test <- tbl_df(read.table(paste0(DataDir,"/subject_test.txt"),
                                       col.names = c('Subject')))
data_X_test <- tbl_df(read.table(paste0(DataDir,"/X_test.txt"),
                                 col.names = VarNames$V2))
data_Y_test <- tbl_df(read.table(paste0(DataDir,"/Y_test.txt"),
                                 col.names = c('Activity')))
#       Merge the data
data_merged <- tbl_df(rbind(cbind(data_Y_train,data_subject_train,data_X_train),
                            cbind(data_Y_test, data_subject_test, data_X_test)))
#       Remove all of the temporary data tables
rm(data_subject_train)
rm(data_subject_test)
rm(data_X_train)
rm(data_X_test)
rm(data_Y_train)
rm(data_Y_test)
rm(DataDir)
gc()
###             EXTRACT ONLY THE MEAN AND STANDARD DEVIATION VALUES
MeanStdString <- 'mean()|std()'
GoodColumns <- grep(MeanStdString,VarNames[[2]])
data_step2 <- data_merged[,c(1,2,GoodColumns+2)]
###             CLEAN UP THE ACTIVITY NAMES AND THE LABELS
#       Clean up the ActivityLabels
newActivityLabels <- gsub("_","",tolower(ActivityLabels$V2))
#       Define a function that will clean the activity labels
getLabel <- function(i) {newActivityLabels[i]}
#       Estabilish the new activity labels
data_step2$Activity <- getLabel(data_step2$Activity)
#       Clean up the column labels
newColumnNames <- names(data_step2)
#               Remove the underscores and dots
newColumnNames <- gsub("_","",newColumnNames)
newColumnNames <- gsub("\\.","",newColumnNames)
#               Make all lower case
newColumnNames <- tolower(newColumnNames)
#               Make the first character meaningful
newColumnNames <- sub("^t","time",newColumnNames)
newColumnNames <- sub("^f","frequency",newColumnNames)
names(data_step2) <- newColumnNames
###             MAKE THE DATA INTO A TIDY DATA SET
#       Melt the data (make sure to convert to data frame)
data_molten <- melt(as.data.frame(data_step2),
                    id.vars = c('activity','subject'),
                    factorsAsStrings=FALSE)
#       Write functions to determine characteristics of the measurement
#               First is the domain
getDomain <- function(name) {
        if(grepl("^t",name)) { return('time') }
        if(grepl("^f",name)) { return('frequency') }
}
#               Next is the dimension
getDimension <- function(name) {
        if(grepl("x$",name)) { return('X') }
        if(grepl("y$",name)) { return('Y') }
        if(grepl("z$",name)) { return('Z') }
        if(grepl("mag",name)) {return('magnitude')}
}
#               Next is the instrument
getInstrument <- function(name) {
        if(grepl("acc",name)) { return('accelerometer') }
        if(grepl("gyro",name)) { return('gyroscope') }
}
#               Next is the Derivative
getDerivative <- function(name) {
        if(grepl("jerk",name)) { return('jerk') }
        else { return('acceleration') }
}
#               Next is the object
getObject <- function(name) {
        if(grepl("body",name)) {return('body')}
        else if(grepl("gravity",name)) {return('gravity')}
        else {return('NA')}
}
#               Finally is the statistic
getStatistic <- function(name) {
        if(grepl("mean",name)) {return('mean')}
        else if(grepl("std",name)) {return('standard deviation')}
        else {return(NA)}
}
#               Maybe do the moment (linear or angular)
#       Establish new columns for the tidy data
data_tidy <- mutate(data_molten,statistic = getStatistic(variable))
data_tidy <- mutate(data_tidy,domain = getDomain(variable))
data_tidy <- mutate(data_tidy,dimension = getDimension(variable))
data_tidy <- mutate(data_tidy,instrument = getInstrument(variable))
data_tidy <- mutate(data_tidy,derivative = getDerivative(variable))
data_tidy <- mutate(data_tidy,object = getObject(variable))
tidyData <- select(data_tidy,activity,subject,
                domain,object,instrument,derivative,dimension,
                statistic,value)
###             OBTAIN THE SUMMARIZED TIDY DATA
summary_data_tidy <- mutate(tidyData, groupid = 
                                    paste0(activity,subject,domain,
                                           object,instrument,derivative,
                                           dimension,statistic))
averages_group <- aggregate(value~groupid,summary_data_tidy,mean)
summary_data_tidy <- merge(x=summary_data_tidy,y=averages_group,
                           by="groupid",all.y = TRUE)
summary_data_tidy <- select(summary_data_tidy,activity,subject,
                            domain, object,instrument,derivative,dimension,
                            statistic,average=value.y)
FinalTidyData <- unique(summary_data_tidy)
###             WRITE OUT THE DATA INTO A txt FILE
#       Write the final tidy data to a txt file
write.table(FinalTidyData,file = "./Project_tidy_data.txt",row.names=FALSE)