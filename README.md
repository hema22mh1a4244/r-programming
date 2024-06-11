# r-programming
#Credit Card Fraud Detection on Sample-Dataset
#importing libraries
library(ranger)
library(caret)
library(data.table)

data <- read.csv("C:\\Users\\HEMALATHA\\Downloads\\creditcard.csv")

#exploration
data.table(data)


summary(data)
table(data$Class)
names(data)

#summary of amount
summary(data$Amount)
sd(data$Amount)
IQR(data$Amount)
var(data$Amount)

#manipulation
data$Amount <- scale(data$Amount)

data2 <- data[,-c(1)]
head(data2)

set.seed(12)
library(caTools)


sample_data <- sample.split(data2$Class, SplitRatio = 0.80)

train_data <- subset(data2, sample_data==TRUE)
test_data <- subset(data2, sample_data ==FALSE)


dim(train_data)
dim(test_data)

#fit logit on data
logistic_Model <- glm(Class~., test_data,family = binomial())
summary(logistic_Model)

plot(logistic_Model)

logistic_Model1 <- glm(Class~., test_data,family = binomial())
summary(logistic_Model1)

plot(logistic_Model)

#we need ROC curve visit bigguery tutorial to learn about ROC
library(pROC)
lr.predict <- predict(logistic_Model1,test_data,probability = TRUE)
auc.gb <- roc(test_data$Class, lr.predict,plot = TRUE,col="green")


library(rpart)
library(rpart.plot)

desicion_model <- rpart(Class ~ . , data, method = "class")
predicted_val <- predict(desicion_model,data,type = "class")
probability <- predict(desicion_model,data, type = 'prob')
rpart.plot(desicion_model)

library(neuralnet)
NN_model <- neuralnet::neuralnet(Class~.,train_data,linear.output = FALSE)
plot(NN_model)

predNN <--compute(NN_model,test_data)
resutNN <-- predNN$net.result
resultNN=ifelse(resultNN>0.6,1,0)
