library(jsonlite)
library(curl)

titanic.url = readLines(curl(url="https://public.opendatasoft.com/api/records/1.0/search/?dataset=titanic-passengers&rows=2000&facet=survived&facet=pclass&facet=sex&facet=age&facet=embarked"))

titanic.json = fromJSON(titanic.url)

titanic.df = titanic.json$records$fields 

#Inspect the training data
dim(titanic.df) #891 rows = passengers
titanic.train=titanic.df[order(titanic.df$passengerid),] #sort data by Passenger ID
head(titanic.train) #matches the titanic_train.csv on github
summary(titanic.train$age) #NA values present
summary(titanic.train$pclass) #Looks good

#Remove NA's from age
titanic.train.clean = titanic.train %>% filter(!is.na(age))
summary(titanic.train.clean$age) 
dim(titanic.train.clean) #714 passengers with age data

#Classify Survival for my age by class

df1=data.frame(age=c(37,37,37),pclass=c(1,2,3)) #Use for kNN
df=data.frame(age=c(37,37,37),pclass=c(1,2,3)) #Use to build kNN table

df$k3=knn(titanic.train.clean[,c(4,6)], df1, titanic.train.clean$survived , prob=T, k = 3)
df$k3.prob=attr(df$k3,"prob")
df$k5=knn(titanic.train.clean[,c(4,6)], df1, titanic.train.clean$survived , prob=T, k = 5)
df$k5.prob=attr(df$k5,"prob")
df$k10=knn(titanic.train.clean[,c(4,6)], df1, titanic.train.clean$survived , prob=T, k = 10)
df$k10.prob=attr(df$k10,"prob")
df$k50=knn(titanic.train.clean[,c(4,6)], df1, titanic.train.clean$survived , prob=T, k = 50)
df$k50.prob=attr(df$k50,"prob")
df


#Classify the 418 random passengers from the test set

##Import, inspect, and clean the test set
titanic.test=read.csv('C:/Users/aleppla/GitRepos/MDS-6306-Doing-Data-Science/Unit 6/titanic_test.csv',header=T)
summary(titanic.test[,5]) #NA's present for Age (col5)
summary(titanic.test[,2]) #Class (col2) looks good
titanic.test1=titanic.test %>% filter(!is.na(titanic.test$Age))
summary(titanic.test1[,5])


titanic.knn=knn(titanic.train.clean[,c(4,6)], titanic.test1[,c(5,2)], titanic.train.clean$survived , prob=T, k = 10)
summary(titanic.knn)


#Confusion Matrix

#Split into training and test sets
set.seed(1)
splitPerc = .75
trainIndices = sample(1:dim(titanic.train.clean)[1],round(splitPerc * dim(titanic.train.clean)[1]))
titanic_train = titanic.train.clean[trainIndices,]
titanic_test = titanic.train.clean[-trainIndices,]

titanic_model_k3=knn(titanic_train[,c(4,6)], titanic_test[,c(4,6)], titanic_train$survived , prob=T, k = 3)
confusionMatrix(table(titanic_test$survived,titanic_model_k3))

titanic_model_k5=knn(titanic_train[,c(4,6)], titanic_test[,c(4,6)], titanic_train$survived , prob=T, k = 5)
confusionMatrix(table(titanic_test$survived,titanic_model_k5))

titanic_model_k10=knn(titanic_train[,c(4,6)], titanic_test[,c(4,6)], titanic_train$survived , prob=T, k = 10)
confusionMatrix(table(titanic_test$survived,titanic_model_k10))

