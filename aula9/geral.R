#Data Source: https://www.kaggle.com/datasets/mlg-ulb/creditcardfraud


#Árvore de decisão
library(party)
iris
L <- sample (1:nrow(iris),round(nrow(iris)/3))
train<-iris[-L,]
test<-iris[L,]
fit<-ctree(Species~.,train)
predict_test=predict(fit, newdata=test)
c_matrix=table(test$Species,predict_test)
print(c_matrix)
cat('Accuracy:', sum(diag(c_matrix))/sum(c_matrix)*100, '%')
plot(fit)
#Clusterização
library(ggplot2)
data.frame(iris)
idx<-sample(1:nrow(iris),40)
irisSample$Species<-NULL
hc<-hclust(dist(irisSample),method="complete")
plot(hc,hang=-1, labels=iris$Species[idx])
rect.hclust(hc,k=3)
groups<-cutree(hc,k=3)
groups
table(groups,iris$Species[idx])
groups[groups==1]="virginica"
groups[groups==2]="setosa"
groups[groups==3]="versicolor"
table(groups,iris$Species[idx])
hc2<-hclust(dist(iris[,3:4]))
groups2<-cutree(hc2,3)
table(groups2,iris$Species)
groups2[groups2==3]=0
groups2[groups2==2]=3
groups2[groups2==0]=2
table(groups2,iris$Species)
ggplot(iris, aes(Petal.Length,Petal.Width, color=iris$Species))+geom_point(alpha=0.4,size=3.5)+geom_point(col=groups2)+scale_color_manual(values=c('black','red','green'))
#K-médias
data.frame(iris)
idx<-sample(1:nrow(iris),40)
irisSample<-iris[idx,]
irisSample$Species<-NULL
c1<-kmeans(irisSample,3)
c1
table(c1$cluster,iris$Species[idx])
c1$cluster[c1$cluster==1]="setosa"
c1$cluster[c1$cluster==2]="versicolor"
c1$cluster[c1$cluster==3]="virginica"
table(c1$cluster,iris$Species[idx])
c12<-kmeans(iris[,3:4],3)
c12$cluster
table(c12$cluster,iris$Species)
c1$cluster[c1$cluster==3]=0
c1$cluster[c1$cluster==2]=3
c1$cluster[c1$cluster==1]=2
c1$cluster[c1$cluster==0]=1
table(c12$cluster,iris$Species)

ggplot(iris, aes(Petal.Length,Petal.Width, color=as.factor(c12$cluster)))+geom_point()
library(ROCR)
library(e1071)
getwd()       
credit<-read.csv("C:/Users/alexandre/Documentos/Scripts R/aula_arvore/creditcard.csv")
head(credit)
set.seed(1984)
T=sample(1:nrow(credit),round(0.3*nrow(credit)))
credit_test=credit[T,]
credit_train=credit[-T,]
fit=naiveBayes(Class~.,data=credit_train,laplace = 1)
fit$apriori
fit$tables
fit$levels
predict_test=predict(fit, newdata=credit_test)
head(predict_test)
#predict_test=predict(fit,newdata=credit_test, type="raw")
head(predict_test)
c_matrix=table(credit_test$Class,predict_test)
print(c_matrix)
cat('Accuracy:', sum(diag(c_matrix))/sum(c_matrix)*100, '%')
#ROC Curve
pr=prediction(as.numeric(predict_test),as.numeric(credit_test$Class))
prf=performance(pr,measure = "tpr",x.measure="fpr")
plot(prf,colorize=TRUE)
auc=performance(pr,measure="auc")
auc=auc@y.values[[1]]
auc