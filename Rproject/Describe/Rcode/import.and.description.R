


downloaded

### load libraries
require(plyr)

#### data import and initial description 
setwd("/Users/Teleak/Desktop/RCourse/DataAnalysis/AnalysisAssignment1/Rproject/Describe")
### raw data imported as raw
raw<-read.csv("/Users/Teleak/Desktop/RCourse/DataAnalysis/AnalysisAssignment1/Rproject/WorkingData/loansData.csv")
#downloaded<-date() ###"Sun Feb  3 20:25:46 2013"

### Describe raw data 
names(raw)
### Some munging
new.names<-c('requested', 'loaned', 'interest', 'length', 'purpose', 'debts', 'state', 'home', 'income', 'fico', 'o.credit', 'r.credit', 'inquiries', 'employed')
old.names<-names(raw)
names.convert<-as.data.frame(cbind(old.names, new.names))
data<-raw
names(data)<-new.names
names(data)
### further description of data 
dim(data)
str(data)
head(data, n=5)
apply(data, 2, function(col) length(which(is.na(col)))) ### shows count of NA values by column
apply(data, 2, function(col) length(which(col == ""))) ### shows count of blank values by column
(has.na<-data[!(complete.cases(data)),]) ### rows with NA data 
### some cleanup
data$interest<-as.numeric(sub("%", "", data$interest)) ### remove percent sign on interest
data$debts<-as.numeric(sub("%", "", data$debts)) ### remove percent sign on interest
### looking at data in columns
sapply(data[ ,c(1:3, 6, 9, 11:13)], summary, na.rm=TRUE)
#### look at factor data
head(data, n=3)
sapply(data[ ,c(4, 5, 7, 8, 10, 14)], unique)
data$length<-as.numeric(sub(" months", "", data$length)) ### strip "months" text off of months column


