## Titanic Kaggle

dataTrain<-read.csv("./data/train.csv")
dataTest<-read.csv("./data/test.csv")
str(dataTrain)
summary(dataTrain)

#dataTrain<-dataTrain[complete.cases(dataTrain),]
# impute na to median
dataTrain$Age[is.na(dataTrain$Age)] =median(dataTrain$Age, na.rm=TRUE)
dataTest$Age[is.na(dataTest$Age)] =median(dataTest$Age, na.rm=TRUE)
dataTest$Fare[is.na(dataTest$Fare)] =median(dataTest$Fare, na.rm=TRUE)

dataTrain$Cabin<-as.character(dataTrain$Cabin)
dataTrain$Cabin<-substr(dataTrain$Cabin, 1, 1)
#dataTrain$Cabin<-as.factor(dataTrain$Cabin)

for (i in 1:length(dataTrain$Cabin)){
  if (dataTrain$Cabin[i]=="") dataTrain$Cabin[i]="H"
}


dataTest$Cabin<-as.character(dataTest$Cabin)
dataTest$Cabin<-substr(dataTest$Cabin, 1, 1)
#dataTest$Cabin<-as.factor(dataTest$Cabin)

for (i in 1:length(dataTest$Cabin)){
  if (dataTest$Cabin[i]=="") dataTest$Cabin[i]="H"
}

# set.seed(1000)
# train_index=sample(seq(nrow(dataTrain)),418,replace=FALSE)
# train=dataTrain[train_index,]
# test=dataTrain[-train_index,]

library(caTools)
set.seed(1000)
spl = sample.split(dataTrain$Survived, SplitRatio = 0.8)
train = subset(dataTrain, spl==TRUE)
test = subset(dataTrain, spl==FALSE)

# logistic Regression model
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)
fol=formula(Survived~Pclass+Sex+Fare)
fol2=formula(Survived~Pclass+Sex+Fare+Age)
glm.fit=glm(fol2,data=train,family=binomial)
glm.probs<- predict(glm.fit, newdata = test, type = "response")


maxThreshold<-0
glm.accMax<-0
thresholds<-seq(.5, .9, length.out=50 )
glm.acc=rep(0,length(thresholds))
for (i in 1:length(thresholds)){
    glm.pred=rep(0,length(test$Survived))
    glm.pred[glm.probs>thresholds[i]]=1
    glm.acc[i]<-sum(glm.pred==test$Survived)/nrow(test)
    if (glm.acc[i] > glm.accMax) {
        glm.accMax = glm.acc[i]
        maxThreshold=thresholds[i]
    }
}
plot(thresholds,glm.acc)
glm.pred=rep(0,length(test$Survived))
glm.pred[glm.probs>maxThreshold]=1
sum(glm.pred==test$Survived)/nrow(test)


glm.probs<- predict(glm.fit, newdata = dataTest, type = "response")
glm.pred=rep(0,nrow(dataTest))
glm.pred[glm.probs>maxThreshold]=1

submission<-data.frame(dataTest$PassengerId,glm.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/glm2_imputed.csv",sep=",",row.names=FALSE)

# Decision tree using CART
library(rpart)
library(rpart)
library(rpart.plot)

fol1=formula(Survived~Pclass+Sex+Fare)
fol2=formula(Survived~Pclass+Sex+Fare+Age) # Age na rows removed
fol3=formula(Survived~Pclass+Sex+Fare+Age+Cabin) # Age na rows removed
CARTmodel <- rpart(fol3, method="class", data=train)
print(CARTmodel)
prp(CARTmodel)

#accuracy of CART model on test set
PredictCARTmodel = predict(CARTmodel, newdata = test, type = "class")
sum(PredictCARTmodel==test$Survived)/nrow(test)

#submit
CART.pred<-predict(CARTmodel, newdata = dataTest, type = "class")
submission<-data.frame(dataTest$PassengerId,as.numeric(CART.pred)-1)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/cart3_impute.csv",sep=",",row.names=FALSE)


# Random forest model
library(randomForest)
library(caret)
library(rpart)
library(tree)
library(randomForest)
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)
fol2<-formula(Survived~Pclass+Sex+Fare+Age)
fol3<-formula(Survived~Pclass+Sex+Fare+Age+Cabin)
forestmodel<-randomForest(fol3, data=train)

rf.probs = predict(forestmodel, newdata = test)
sum(rf.probs==test$Survived)/nrow(test)
importance(forestmodel)

#submit

rf.pred<-predict(forestmodel, newdata = dataTest)
submission<-data.frame(dataTest$PassengerId,as.numeric(rf.pred)-1)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/rf2_impute.csv",sep=",",row.names=FALSE)

# SVM Model
library(e1071)
fol1=formula(Survived~Pclass+Sex+Fare)
fol2=formula(Survived~Pclass+Sex+Fare+Age)
fol3<-formula(Survived~Pclass+Sex+Fare+Age+Cabin)
SVMmodel <- svm(fol3, data=train)
svm.probs = predict(SVMmodel, newdata = test)

svm.pred=rep(0,length(test$Survived))
svm.pred[svm.probs>0.5]=1
sum(svm.pred==test$Survived)/nrow(test)

maxThreshold<-0
svm.accMax<-0
thresholds<-seq(0, 1, length.out=100 )
svm.acc=rep(0,length(thresholds))
for (i in 1:length(thresholds)){
  svm.pred=rep(0,length(test$Survived))
  svm.pred[svm.probs>thresholds[i]]=1
  svm.acc[i]<-sum(svm.pred==test$Survived)/nrow(test)
  if (svm.acc[i] > svm.accMax) {
    svm.accMax = svm.acc[i]
    maxThreshold=thresholds[i]
  }
}
plot(thresholds,svm.acc)
svm.pred=rep(0,length(test$Survived))
svm.pred[svm.probs>maxThreshold]=1
sum(svm.pred==test$Survived)/nrow(test)

#submit
#dataTest$Survived=rep(0,length(nrow(dataTest)))
SVMmodel <- svm(fol1, data=train)
svm.probs = predict(SVMmodel, newdata = dataTest)
svm.pred=rep(0,nrow(dataTest))
svm.pred[svm.probs>0.5]=1
submission<-data.frame(dataTest$PassengerId,svm.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/svm1.csv",sep=",",row.names=FALSE)

