data<-read.csv(file.choose())
str(data)
summary(data)
#data cleaning
sum(is.na(data))
data<-na.omit(data)
#data processing
normalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}
data<-normalize(data)
print(data)
#box plotting
boxplot(data$chol,main="Cholesterol Levels",ylab="Cholesterol",col="yellow")
#eda
#install.packages('corrplot')
library(corrplot)
#correlation between variables
corrplot(cor(data[,-14]),method="shade")
#plot target variable distribution
table(data$output)
barplot(table(data$output),main="Output Distribution",col=c("red","green"))
#understand relationships with ggplot2
library(ggplot2)
ggplot(data,aes(x=chol,y=thalachh,color=as.factor(output)))+geom_point()+labs(title="Cholsterol Vs Max Heart Rate",color="Output")

# predictive modeling
set.seed(123)
library(caret)
sample_index<-sample(1*nrow(data),0.8*nrow(data))
train_index<-data[sample_index,]
test_index<-data[-sample_index,]
#now traing a classification model
#logistic rgrssion
model<-glm(output~.,data=train_index)
summary(model)
plot(model)
#Decision tree
library(rpart)
library(rpart.plot)
tree_model<-rpart(output~.,data=train_index,method="class")
rpart.plot(tree_model)
#random forest
library(randomForest)
rf_model<-randomForest(output~.,data=train_index,importance=TRUE)
rf_model
plot(rf_model)
#svm
library(e1071)
svm_model<-svm(output~.,data=train_index,kernel="linear")
summary(svm_model)
#predictions now
predictions<-predict(rf_model,newdata=test_data,type="response")
#confusion matrix now
confusion_matrix<-table(pre=predictions,actual=test_index$output)
print(confusion_matrix)
#if need to check accuracy
accuracy=sum(diag(confusion_matrix))/sum(confusion_matrix)
print(paste("Accuracy:",accuracy))