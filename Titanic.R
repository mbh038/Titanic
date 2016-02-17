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
  
  dataTest$Survived <- NA
  combi <- rbind(dataTrain, dataTest)
  
  summary(combi)
  summary(combi$Embarked)
  combi$Embarked[which(combi$Embarked == '')]<-"S"
  summary(combi$Embarked)
  combi$Embarked <- factor(combi$Embarked)
  summary(combi$Embarked)
  
  # make title variable
  combi$Name <- as.character(combi$Name)
  #strsplit(combi$Name[1], split='[,.]')[[1]][2]
  combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
  combi$Title <- sub(' ', '', combi$Title)
  table(combi$Title)
  combi$Title[combi$Title %in% c('Mme', 'Mlle','Ms')] <- 'Mlle'
  combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir','Jonkheer')] <- 'Sir'
  combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
  table(combi$Title)
  combi$Title<-factor(combi$Title)
  
  # make family size variable
  combi$FamilySize <- combi$SibSp + combi$Parch + 1
  
  # make familyID variable
  combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
  combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
  combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
  famIDs <- data.frame(table(combi$FamilyID))
  famIDs <- famIDs[famIDs$Freq <= 2,]
  combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
  combi$FamilyID <- factor(combi$FamilyID)
  table(combi$FamilyID)
  
  # reduce levels
  combi$FamilyID2 <- combi$FamilyID
  combi$FamilyID2 <- as.character(combi$FamilyID2)
  combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
  combi$FamilyID2 <- factor(combi$FamilyID2)
  
  
  # make cabin variable
  cabins<-function(x){
      cabinLevels<-c("A","B","C","D","E","F","G","H","T")
      x<-as.character(x)
      x<-substr(x, 1, 1)
      x[x==""] <- "H"
      newX<-as.factor(x)
      levels(newX)<-cabinLevels
      invisible(newX)
  }
  
  combi$Cabin<-cabins(combi$Cabin)
  
  #normalise age variable
  combi$Age<-(combi$Age-mean(combi$Age,na.rm=TRUE))/sd(combi$Age,na.rm=TRUE)
  
  library(rpart)
  # impute missing age values
  Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
  combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
  summary(combi$Age)
  
  # impute missing fare value
  Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Title + FamilySize,
                   data=combi[!is.na(combi$Fare),], method="anova")
  combi$Fare[is.na(combi$Fare)] <- predict(Farefit, combi[is.na(combi$Fare),])
  summary(combi$Fare)
  
  Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Embarked + Title + FamilySize,
                  data=combi[combi$Fare!=0,], method="anova")
  combi$Fare[combi$Fare==0] <- predict(Farefit, combi[combi$Fare==0,])
  summary(combi$Fare)
  
  
  #transform fare values
  combi$Fare<-log(combi$Fare)
  hist(combi$Fare)
  summary(combi$Fare)
  
  dataTrain <- combi[1:891,]
  dataTest <- combi[892:1309,]
  
  table(dataTrain$Cabin)
  table(dataTest$Cabin)
  
  library(caTools)
  set.seed(1000)
  spl = sample.split(dataTrain$Survived, SplitRatio = 0.8)
  train = subset(dataTrain, spl==TRUE)
  test = subset(dataTrain, spl==FALSE)
  table(train$Cabin)
  table(test$Cabin)
  
#   sex="female"
#   # train on one gender at a time
#   train<-subset(train,Sex==sex)
#   #train$Sex<-factor(train$Sex)
#   test<-subset(test,Sex==sex)
#   #test$Sex<-factor(test$Sex)
#   dataTest<-subset(dataTest,Sex==sex)
#   #dataTest$Sex<-factor(dataTest$Sex)

  #train$Survived<-as.factor(train$Survived)
  #test$Survived<-as.factor(test$Survived)
#   train$Cabin<-as.factor(train$Cabin)
#   test$Cabin<-as.factor(test$Cabin)
#   train$Cabin<-factor(train$Cabin)
#   test$Cabin<-factor(test$Cabin)
#   levels(test$Cabin) <- levels(train$Cabin)
  
## Models

model<-function(x){
md=list(8)  
md[[1]]<-c(as.formula("as.factor(Survived)~Sex+Pclass"),2)
md[[2]]<-c(as.formula("as.factor(Survived)~Sex+Pclass+Fare"),3)
md[[3]]<-c(as.formula("as.factor(Survived)~Sex+Pclass+Fare+FamilySize"),4)
md[[4]]<-c(as.formula("as.factor(Survived)~Sex+Pclass+Fare+Age+FamilySize"),5) # Age na rows removed
md[[5]]<-c(as.formula("as.factor(Survived)~Sex+Pclass+Fare+Age+Title +FamilyID2+FamilySize +Cabin"),8)
md[[6]]<-c(as.formula("as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID"),10)
md[[7]]<-c(as.formula("as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID+Cabin"),11)
md[[8]]<-c(as.formula("as.factor(Survived) ~ Pclass + Sex + Age +Fare + Embarked + Title + FamilySize + FamilyID+Cabin"),9)
md[[x]]
}



## LOGISTIC REGRESSION MODEL

# Number of folds
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)
#Test cp values from 0.002 to 0.1 in 0.002 increments
cartGrid = expand.grid( .cp = seq(0.001,0.1,0.001))

for (j in 1:8){

    glm.fit=glm(model(j)[[1]],data=train,family=binomial)
    #levels(train$FamilyID)<-levels(train$FamilyID)
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
    print (paste(round(maxThreshold,3)," ",sum(glm.pred==test$Survived)/nrow(test)))
}


glm.fit=glm(model(8)[[1]],data=dataTrain,family=binomial)
glm.probs<- predict(glm.fit, newdata = dataTest, type = "response")
glm.pred=rep(0,nrow(dataTest))
glm.pred[glm.probs>0.44]=1

submission<-data.frame(dataTest$PassengerId,glm.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/glm8.csv",sep=",",row.names=FALSE)



## CART with cross validation
## Selecting Cp by cross-validation

library(lattice)
library(ggplot2)
library(rpart)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

# Number of folds
set.seed(820)
tr.control = trainControl(method = "cv", number = 10)
#Test cp values from 0.002 to 0.1 in 0.002 increments
cartGrid = expand.grid( .cp = seq(0.001,0.01,0.0001))

# Cross-validation
#start()
bestCp<-vector()
for (i in 1:8){
    set.seed(820)
    tr = train(model(i)[[1]], data = dataTrain, method = "rpart", trControl = tr.control, tuneGrid = cartGrid)
    tr
    # Extract tree
    best.tree = tr$finalModel
    prp(best.tree)
    bestCp[i]=tr$results$cp[which(tr$results$Accuracy==max(tr$results$Accuracy))][1]
    print (paste("Model: ",i,"Cp: ",bestCp[i],"accuracy: ",max(tr$results$Accuracy),sep=" "))
    set.seed(820)
    CARTcp = rpart(model(i)[[1]], data=dataTrain, method="class",cp=bestCp[i])
    prp(CARTcp)
    
    # 2.4 accuracy
    
#     PredictCARTcp = predict(CARTcp, newdata = test, type = "class")
#     acc<-sum(PredictCARTcp==test$Survived)/nrow(test)
#     print (acc)
#     if (acc > accbest){
#         accbest=acc
#         ibest =i
#     }    
}
set.seed(820)
CARTcp = rpart(model(8)[[1]], data=dataTrain, method="class",cp=0.0082)
prp(CARTcp)
set.seed(2)
#submit
PredictCARTcp<-predict(CARTcp, newdata = dataTest, type = "class")
submission<-data.frame(dataTest$PassengerId,as.numeric(PredictCARTcp)-1)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/cart8_cv.csv",sep=",",row.names=FALSE)

# Random forest model

library(randomForest)
library(caret)
library(rpart)
library(tree)

# Number of folds
set.seed(1)
tr.control = trainControl(method = "cv", number = 10)
bestmtry<-vector()
for (i in 6:8){
    treeGrid = expand.grid( .mtry= seq(1,model(i)[[2]],1))
    tr = train(model(i)[[1]], data = dataTrain, method = "rf", trControl = tr.control, tuneGrid = treeGrid)
    tr
    # Extract ntree
    best.forest = tr$finalModel
    prp(best.tree)
    bestmtry[i]=tr$results$mtry[which(tr$results$Accuracy==max(tr$results$Accuracy))][1]
    forestmodel<-randomForest(model(i)[[1]], data=dataTrain,importance=TRUE,mtry=bestmtry[i],ntree=2000)
    varImpPlot(forestmodel)
    print (paste("Model: ",i, "ntree= ",2000,"mtry= ",bestmtry[i],"Accuracy= ",max(tr$results$Accuracy),sep=" "))
}

#submit
set.seed(1)
forestmodel<-randomForest(model(7)[[1]], data=dataTrain,importance=TRUE,ntree=2000,mtry=5)
varImpPlot(forestmodel)
set.seed(1)
rf.pred<-predict(forestmodel, newdata = dataTest)
submission<-data.frame(dataTest$PassengerId,rf.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/rf7n2000m5.csv",sep=",",row.names=FALSE)

## Conditional Forest

#install.packages('party')
# names(getModelInfo()) gives available train methods
library(party)
set.seed(1)
tr.control = trainControl(method = "cv", number = 10)
for (i in 6:8){
    set.seed(1)
    cGrid = expand.grid( .mtry= c(4,5,6))
    tr = train(model(i)[[1]], data = dataTrain, method = "cforest", trControl = tr.control, tuneGrid = cGrid)
    bestmtry=tr$results$mtry[which(tr$results$Accuracy==max(tr$results$Accuracy))][1]
    print (paste("Model: ",i, "ntree= ",2000,"mtry= ",bestmtry,"Accuracy= ",max(tr$results$Accuracy),sep=" "))
}

set.seed(1)
# used FamilySize, not FamilySize2 for best score
# names(getModelInfo()) gives available train methods
cmodel<-cforest(model(7)[[1]], data=train,controls=cforest_unbiased(ntree=2000, mtry=5))
c.pred<-predict(cmodel, newdata = test,OOB=TRUE, type = "response")
acc<-sum(c.pred==test$Survived)/nrow(test)
acc

# submission
cmodel<-cforest(model(7)[[1]], data=dataTrain,controls=cforest_unbiased(ntree=2000, mtry=3))
c.pred<-predict(cmodel, newdata = dataTest,OOB=TRUE, type = "response")
submission<-data.frame(dataTest$PassengerId,c.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/cf8_m5_n2000.csv",sep=",",row.names=FALSE)


## SVM Model

# Number of folds
library(e1071)
library(kernlab)
ibest<-0
costbest<-0
accbest<-0
gammabest<-0
costs<-c(10,15,20,25,30,35,40,45,50,55,60)

set.seed(2)
tr.control = trainControl(method = "cv", number = 10)

for (j in 8:8){
    #start()
    SVMGrid = expand.grid( .C=c(1000,1200,1400,1600),.sigma= c(0.001,0.003,0.01)/model(j)[[2]])
    set.seed(2)
    tr = train(model(j)[[1]], data = dataTrain, method = "svmRadial", trControl = tr.control, tuneGrid = SVMGrid)
    print (tr$results)
    bestaccuracy<-max(tr$results$Accuracy)
    bestcost=tr$results$C[which(tr$results$Accuracy==max(tr$results$Accuracy))][1]
    bestgamma=tr$results$sigma[which(tr$results$Accuracy==max(tr$results$Accuracy))][1]
    print (paste("Model: ",j,"cost: ",bestcost,"gamma: ",round(bestgamma,6), "Accuracy: ",round(bestaccuracy,6),sep=" "))
}

#submit
#dataTest$Survived=rep(0,length(nrow(dataTest)))
set.seed(2)
SVMmodel <- svm(model(8)[[1]], data=dataTrain,cost=1200,gamma=0.0011)
svm.pred = predict(SVMmodel, newdata = dataTest)
#acc<-sum(svm.pred==test$Survived)/nrow(test)
#acc
submission<-data.frame(dataTest$PassengerId,svm.pred)
names(submission)<-c("PassengerId","Survived")
write.table(submission,"./submissions/svm8c1200g0011.csv",sep=",",row.names=FALSE)



SVMmodel <- svm(model(7)[[1]], data=train,cost=bestcost,gamma=bestgamma)
svm.pred = predict(SVMmodel, newdata = test)
acc<-sum(svm.pred==test$Survived)/nrow(test)
acc