#Part 1

titanic = read.csv("~/MDS-6306-Doing-Data-Science/Unit 6/titanic_train.csv",header = T)

#Replace Age NAs with mean(Age) or median(Age)
Age_t = titanic %>% group_by(is.na(Age)) %>% summarize("Mean Age"=mean(Age),"Median Age"=median(Age))
Age_t = as.data.frame(Age_t)
mean_t = Age_t[1,2] #mean = 29.7
median_t = Age_t[1,3] #median = 28
titanic$Age1 = ifelse(is.na(titanic$Age),mean_t,titanic$Age) #Replace NA with mean
titanic$Age2 = ifelse(is.na(titanic$Age),median_t,titanic$Age) #Replace NA with median
summary(titanic)
dim(titanic)

#Recode Survived as a factor to predict with naiveBayes
titanic$Survived=as.factor(titanic$Survived) 

#1a. Predict survival for Age 30 in Classes 1, 2, and 3

library(e1071)

model_t = naiveBayes(Survived~Pclass+Age1,data = titanic)
df_t = data.frame(Pclass = 1, Age1=30)
predict(model_t,df_t, type = "raw")
predict(model_t,df_t) 

#Does it matter if Class is an integer or a factor? Yes, somewhat.

titanic$Pclass = as.factor(titanic$Pclass)
model_t = naiveBayes(Survived~Pclass+Age1,data = titanic)
df_t = data.frame(Pclass = as.factor(c(1,2,3)), Age1=c(30,30,30))
predict(model_t,df_t, type = "raw")
predict(model_t,df_t)

#What about mean vs. median?  No meaningful difference.

model_t = naiveBayes(Survived~Pclass+Age2,data = titanic)
df_t = data.frame(Pclass = as.factor(c(1,2,3)), Age2=c(30,30,30))
predict(model_t,df_t, type = "raw")


#1b. 70/30 training/test splot

titanicClean = titanic #I already cleaned the data
set.seed(4)
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

head(trainTitanic)
head(testTitanic)


#1c.  

model4 = naiveBayes(trainTitanic[,c(3,13)],trainTitanic$Survived)
predict(model4,testTitanic[,c(3,13)])
confusionMatrix(table(predict(model4,testTitanic[,c(3,13)]),testTitanic$Survived))

#1d. Repeat 3-4 random seeds and compare

seed=c()
Acc=c()
Sens=c()
Spec=c()

for (i in 1:4)
{
seed[i]=i
set.seed(i)
trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
trainTitanic = titanicClean[trainIndices,]
testTitanic = titanicClean[-trainIndices,]

model3 = naiveBayes(trainTitanic[,c(3,13)],trainTitanic$Survived)
predict3=predict(model3,testTitanic[,c(3,13)])
CM=confusionMatrix(table(predict3,testTitanic$Survived))
Acc[i]=CM$overall[1]
Sens[i]=CM$byClass[1]
Spec[i]=CM$byClass[2]
}

cbind(seed,Acc,Sens,Spec)

#1e. Repeat 100 random seeds and compare

seed=c()
Acc=c()
Sens=c()
Spec=c()
meanAcc=c()
meanSens=c()
meanSpec=c()

for (i in 1:100)
{
  seed[i]=i
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  
  model3 = naiveBayes(trainTitanic[,c(3,13)],trainTitanic$Survived)
  predict3=predict(model3,testTitanic[,c(3,13)])
  CM=confusionMatrix(table(predict3,testTitanic$Survived))
  Acc[i]=CM$overall[1]
  Sens[i]=CM$byClass[1]
  Spec[i]=CM$byClass[2]
  meanAcc[i]=mean(Acc)
  meanSens[i]=mean(Sens)
  meanSpec[i]=mean(Spec)
}

random100=cbind(Acc,Sens,Spec)
mean100=cbind(meanAcc,meanSens,meanSpec)
tail(mean100,n=1)

#1f. Including Sex (column 5) in the NB Model  

model_s = naiveBayes(trainTitanic[,c(3,5,13)],trainTitanic$Survived)
confusionMatrix(table(predict(model4,testTitanic[,c(3,5,13)]),testTitanic$Survived))

#1e. Repeat 100 random seeds and compare with sex in NB model

seed=c()
Acc=c()
Sens=c()
Spec=c()
meanAcc=c()
meanSens=c()
meanSpec=c()

for (i in 1:100)
{
  seed[i]=i
  set.seed(i)
  trainIndices = sample(seq(1:length(titanicClean$Age)),round(.7*length(titanicClean$Age)))
  trainTitanic = titanicClean[trainIndices,]
  testTitanic = titanicClean[-trainIndices,]
  
  model3 = naiveBayes(trainTitanic[,c(3,5,13)],trainTitanic$Survived)
  predict3=predict(model3,testTitanic[,c(3,5,13)])
  CM=confusionMatrix(table(predict3,testTitanic$Survived))
  Acc[i]=CM$overall[1]
  Sens[i]=CM$byClass[1]
  Spec[i]=CM$byClass[2]
  meanAcc[i]=mean(Acc)
  meanSens[i]=mean(Sens)
  meanSpec[i]=mean(Spec)
}

random100=cbind(Acc,Sens,Spec)
mean100=cbind(meanAcc,meanSens,meanSpec)
tail(mean100,n=1)