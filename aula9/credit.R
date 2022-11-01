#-------------------------------------------------------------
#Case: Previsão do pagamento de um empréstimoou não.
#Tipo de problema: Classificação
#Dados: https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud
#-------------------------------------------------------------

#importa pacotes
library(ROCR)
library(e1071)

#carrega dataset
credit<-read.csv("D:/alexa/Work/R/R_classes/aula9/creditcard.csv")
head(credit)

set.seed(1984)

#separa dados entre treino e teste
T = sample(1:nrow(credit),round(0.3*nrow(credit)))
credit_test=credit[T,]
credit_train=credit[-T,]

#cria modelo Naive Bayes
fit=naiveBayes(Class~.,data=credit_train,laplace = 1)
fit$apriori
fit$tables
fit$levels

#faz predições
predict_test=predict(fit, newdata=credit_test)
head(predict_test)

#predict_test=predict(fit,newdata=credit_test, type="raw")
head(predict_test)

#cria matriz de confusão
c_matrix = table(credit_test$Class,predict_test)
print(c_matrix)

#imprime acurácia
cat('Accuracy:', sum(diag(c_matrix))/sum(c_matrix)*100, '%')

#cria curva ROC
pr=prediction(as.numeric(predict_test),as.numeric(credit_test$Class))
prf=performance(pr,measure = "tpr",x.measure="fpr")
plot(prf,colorize=TRUE)
auc=performance(pr,measure="auc")
auc=auc@y.values[[1]]
auc
