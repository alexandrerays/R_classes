#-------------------------------------------------------------
#Case: Agrupamento por tipo de flor (Setosa, Versicolor, Virginica)
#Tipo de problema: Agrupamento
#Dados: do próprio R
#-------------------------------------------------------------


#Árvore de decisão
library(party)

#Clusterização
library(ggplot2)

data.frame(iris)
idx <- sample(1:nrow(iris),40)
iris$Species <- NULL

#aplica o algoritmo de clustering hierárquico (hclust)
hc <- hclust(dist(iris),method="complete")
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
getwd()   
