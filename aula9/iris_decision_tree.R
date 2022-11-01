#-------------------------------------------------------------
#Case: Previsão do tipo de flor (Setosa, Versicolor, Virginica)
#Tipo de problema: Classificação
#Dados: do próprio R
#-------------------------------------------------------------

#imprime primeiras linhas do dataset iris
head(iris)

#divide em treino e teste
L <- sample (1:nrow(iris),round(nrow(iris)/3))
train<-iris[-L,]
test<-iris[L,]

#treina o modelo árvore de decisão
fit <- ctree(Species~.,train)

#faz predições
predict_test = predict(fit, newdata=test)

#cria matriz de confusão
c_matrix = table(test$Species,predict_test)
print(c_matrix)

#calcula acurácia
cat('Accuracy:', sum(diag(c_matrix))/sum(c_matrix)*100, '%')

#plot da árvore de decisão
plot(fit)
