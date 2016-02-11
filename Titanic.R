## Titanic Kaggle

dataTrain<-read.csv("./data/train.csv")
dataTest<-read.csv("./data/test.csv")
str(data)
summary(data())

library(caTools)
set.seed(1000)
spl = sample.split(dataTrain$Survived, SplitRatio = 0.5)
train = subset(ddataTrain, spl==TRUE)
test = subset(dataTrain, spl==FALSE)

# logistic Regression model

fol=formula(Survived~Pclass+Sex+Fare)
glm.fit=glm(fol,data=train,family=binomial)
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
write.table(submission,"./submissions/glm1.csv",sep=",",row.names=FALSE)

# Decision tree using CART
library(rpart)
library(rpart)
library(rpart.plot)

fol=formula(Survived~Pclass+Sex+Fare)
CARTmodel <- rpart(fol, method="class", data=train)
print(CARTmodel)
prp(CARTmodel)

#accuracy of CART model on test set
PredictCARTmodel = predict(CARTmodel, newdata = test, type = "class")
sum(PredictCARTmodel==test$Survived)/nrow(test)

