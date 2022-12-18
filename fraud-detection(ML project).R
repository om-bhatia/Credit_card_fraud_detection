library(caret)
library(data.table)
library(ranger)
creditcard1 <- read.csv("C:/Users/user/OneDrive/Desktop/creditcard.csv/creditcard.csv", header=TRUE)
dim(creditcard1)
table(creditcard1$Class)
summary(creditcard1)
names(creditcard1)
head(creditcard1,6)
var(creditcard1$Amount)
sd(creditcard1$Amount)


#scaling data
creditcard1$Amount=scale(creditcard1$Amount)
credit_data<-creditcard1[,-c(1)]

#preparing sets
set.seed(100)
partition<-sample(1:nrow(credit_data),size = 0.8*nrow(credit_data))
train_data<-credit_data[partition,]
test_data<-credit_data[-partition,]
dim(train_data)
dim(test_data)



#model
credit_model<-glm(formula = Class ~.,family = "binomial",data = test_data)
summary(credit_model)
plot(credit_model)

#ROC curve
library(pROC)
predictor<-predict(credit_model,train_data, probability=TRUE)
cur1=roc(train_data$Class,predictor,plot=TRUE,col="red")




#decision tree
library(rpart)
library(rpart.plot)
credit_tree<-rpart(Class ~ .,credit_data,method = 'class')
pval<-predict(credit_tree,credit_data,type='class')
prob<-predict(credit_tree,credit_data,type='prob')
rpart.plot(credit_tree)



