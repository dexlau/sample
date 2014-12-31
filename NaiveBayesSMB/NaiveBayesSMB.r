## dexterlau@google.com 20141208
## v0.1 Classifier for predicting SMB signups using TNS, SFDC, and FDS data.
library(ROCR)
library(e1071)

train <- read.csv('20141210_SMBTrainingData_TNS_Full.csv',header=T)
test <- read.csv('20141212_SMBTestData_TNS_Full.csv',header=T)
range <- 3:237
classifier<-naiveBayes(train[,range],train[,1])
table(Predicted=predict(classifier, train[,range]), Actual=train[,1])
summary(predict(classifier, test[,range]))