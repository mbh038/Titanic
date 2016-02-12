## Titanic Kaggle

mbh<-function(){
  print ("Hello World")
}
  set.seed(1)
  rm(list=ls())
  dataTrain<-read.csv("./data/train.csv")
  dataTest<-read.csv("./data/test.csv")
  #str(dataTrain)
  #summary(dataTrain)
  
   dataTrain<-dataTrain[complete.cases(dataTrain),]
  # impute na to median
  dataTrain$Age[is.na(dataTrain$Age)] =median(dataTrain$Age, na.rm=TRUE)
  dataTest$Age[is.na(dataTest$Age)] =median(dataTest$Age, na.rm=TRUE)
  dataTest$Fare[is.na(dataTest$Fare)] =median(dataTest$Fare, na.rm=TRUE)
  
  dataTest$Cabin<-as.factor(dataTest$Cabin)
  dataTrain$Cabin<-as.factor(dataTrain$Cabin)
  
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
    if (dataTest$Cabin[i]=="G") dataTest$Cabin[i]="H"
  }

  
  # set.seed(1000)
  # train_index=sample(seq(nrow(dataTrain)),418,replace=FALSE)
  # train=dataTrain[train_index,]
  # test=dataTrain[-train_index,]
  
  
  library(caTools)
  set.seed(1000)
  spl = sample.split(dataTrain$Survived, SplitRatio = 0.5)
  train = subset(dataTrain, spl==TRUE)
  test = subset(dataTrain, spl==FALSE)
  
  train$Cabin[271]="H"
  dataTest$Cabin[118]="H"
  
#   # train on one gender at a time
#   train<-subset(train,Sex=="male")
#   #train$Sex<-factor(train$Sex)
#   test<-subset(test,Sex=="male")
#   #test$Sex<-factor(test$Sex)
#   dataTest<-subset(dataTest,Sex=="male")
#   #dataTest$Sex<-factor(dataTest$Sex)

  train$Survived<-as.factor(train$Survived)
  test$Survived<-as.factor(test$Survived)
  #train$Cabin<-as.factor(train$Cabin)
  #test$Cabin<-as.factor(test$Cabin)
  #train$Cabin<-factor(train$Cabin)
  #test$Cabin<-factor(test$Cabin)
  #levels(test$Cabin) <- levels(train$Cabin)
  
## Models

model<-function(x){
md=list(5)  
md[[1]]<-as.formula("Survived~Pclass+Fare")
md[[2]]<-as.formula("Survived~Pclass+Fare+Age")
md[[3]]<-as.formula("Survived~Pclass+Fare+Age+SibSp+Parch") # Age na rows removed
md[[4]]<-as.formula("Survived~Pclass+Fare+Age")
md[[5]]<-as.formula("Survived~Pclass+Fare+Age+SibSp+Parch+Ticket")
md[[x]]
}



# logistic Regression model

for (j in 1:4){
#Tstart()
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)

glm.fit=glm(model(j),data=train,family=binomial)
glm.probs<- predict(glm.fit, newdata = test, type = "response")

# find optimum threshold
maxThreshold<-0
glm.accMax<-0
thresholds<-seq(.2, .9, length.out=50 )
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
print (sum(glm.pred==test$Survived)/nrow(test))
}

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

for (i in 1:5){
#Tstart()
CARTmodel <- rpart(model(i), method="class", data=train)
#print(CARTmodel)
prp(CARTmodel)

#accuracy of CART model on test set
PredictCARTmodel = predict(CARTmodel, newdata = test, type = "class")
print (sum(PredictCARTmodel==test$Survived)/nrow(test))
}

#submit
CART.pred<-predict(CARTmodel, newdata = dataTest, type = "class")
submission<-data.frame(dataTest$PassengerId,as.numeric(CART.pred)-1)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/cart3_impute.csv",sep=",",row.names=FALSE)

# CART with cross validation
## SELECTING CP BY CROSS-VALIDATION

#4.1
library(lattice)
library(ggplot2)
library(rpart)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Number of folds
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)

#Test cp values from 0.002 to 0.1 in 0.002 increments
cartGrid = expand.grid( .cp = seq(0.001,0.1,0.001))

# Cross-validation
#start()
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)
tr = train(fol5, data = train, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
tr

# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# fit CART model with optimal cp = 0.006
library(rpart)
library(rpart.plot)
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)

for (i in 1:5){
#Tstart()
CARTcp = rpart(model(i), data=train, method="class",cp=0.006)
prp(CARTcp)

# 2.4 accuracy

PredictCARTcp = predict(CARTcp, newdata = test, type = "class")
print (sum(PredictCARTcp==test$Survived)/nrow(test))
}

CARTcp = rpart(model(2), data=train, method="class",cp=0.006)
prp(CARTcp)
PredictCARTcp = predict(CARTcp, newdata = test, type = "class")
print (sum(PredictCARTcp==test$Survived)/nrow(test))

#submit
PredictCARTcp<-predict(CARTcp, newdata = dataTest, type = "class")
submission<-data.frame(dataTest$PassengerId,as.numeric(PredictCARTcp)-1)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/cart2_impute_cv.csv",sep=",",row.names=FALSE)

# Random forest model

library(randomForest)
library(caret)
library(rpart)
library(tree)
library(randomForest)
accbest=0
ibest=0
for (i in 1:4){
#Tstart()
train$Survived<-as.factor(train$Survived)
test$Survived<-as.factor(test$Survived)
forestmodel<-randomForest(model(i), data=train)

rf.probs = predict(forestmodel, newdata = test)
acc=sum(rf.probs==test$Survived)/nrow(test)
print (acc)
if (acc > accbest){
  accbest=acc
  ibest =i
}
importance(forestmodel)
}

#submit
#train$Cabin<-factor(train$Cabin)
dataTest$Cabin<-as.factor(dataTest$Cabin)
levels(dataTest$Cabin)<-levels(train$Cabin)
forestmodel<-randomForest(model(ibest), data=train)
rf.pred<-predict(forestmodel, newdata = dataTest)
submission<-data.frame(dataTest$PassengerId,rf.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/rf3_f_impute.csv",sep=",",row.names=FALSE)

males<-read.csv("./submissions/rf3_m_impute.csv")
females<-read.csv("./submissions/rf3_f_impute.csv")

mf<-rbind(males,females)
mf <- mf[order(mf$PassengerId),] 
write.table(mf,"./submissions/rf3_mf_impute.csv",sep=",",row.names=FALSE)

mean(mf$Survived)

# SVM Model

library(e1071)

for (j in 1:5){
#start()
SVMmodel <- svm(model(j), data=train)
svm.probs = predict(SVMmodel, newdata = test)
svm.pred=rep(0,length(svm.probs))
#svm.pred[svm.probs>0.5]=1
print (sum(svm.pred==test$Survived)/nrow(test))
}


#submit
#dataTest$Survived=rep(0,length(nrow(dataTest)))
SVMmodel <- svm(fol1, data=train)
svm.probs = predict(SVMmodel, newdata = dataTest)
svm.pred=rep(0,nrow(dataTest))
svm.pred[svm.probs>0.5]=1
submission<-data.frame(dataTest$PassengerId,svm.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/svm1.csv",sep=",",row.names=FALSE)

