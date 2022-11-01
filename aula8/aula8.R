train <-read.csv("C:/Users/alexandre/Documentos/Scripts R/train_.csv")
test <-read.csv("C:/Users/alexandre/Documentos/Scripts R/test_.csv")

test$Survived <-NA
complete_data <- rbind(train,test)
str(complete_data)

colSums(is.na(complete_data))
colSums(complete_data=='')

sapply(complete_data, function(x) length(unique(x)))
complete_data$Embarked[complete_data$Embarked==""] <- "S"
complete_data$Age[is.na(complete_data$Age)] <- median(complete_data$Age,na.rm=T)

library(dplyr)
titanic_data <- complete_data %>% select(-c(Cabin, PassengerId, Ticket, Name))
for (i in c("Sex","Embarked")){
  titanic_data[,i]=as.factor(titanic_data[,i])}



library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(titanic_data))
#titanic_data$Sex <- as.factor(titanic_data$Sex)
newdata <- one_hot(as.data.table(titanic_data))


newdata <- one_hot(as.data.table(titanic_data$Embarked))
#titanic_data$Embarked <- as.factor(titanic_data$Embarked)
newdata <- one_hot(as.data.table(titanic_data))

#treinamento do modelo
train <- newdata[1:667,]
test <- newdata[668:889,]

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")

result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

library(caret)

test$Survived <- as.factor(test$Survived)
result <- as.factor(result)
confusionMatrix(data=result, reference=test$Survived)

library(ROCR)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7), print.cutoffs.at = seq(0,1,0.1))
