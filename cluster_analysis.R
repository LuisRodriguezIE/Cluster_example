
library(cluster)

rm(list=ls()) 


setwd("C:/Users/lr_29/Desktop/Big Data Goal/R/Proyectos R Studio/Cluster")

utilities<-read.csv(file="utilities.csv",header=TRUE)
View(utilities)
summary(utilities)
attach(utilities)

#Grafica de puntos
plot(Fuel_Cost~Sales,col='blue',pch=16)
with(utilities,text(Fuel_Cost~Sales,labels = Company,pos=4,cex=0.7))

#Normalizacion
z<-utilities[,-c(1,1)]
z

m<-apply(z,2,mean) #Media, 2 significa que se aplica sobre columnas, 1 sobre filas.
s<-apply(z,2,sd) #Desviación estandar
z<-scale(z,m,s)
m
s


#Calulo de la distancia euclidiana
distance<-dist(z)
print(distance,digits = 3)

#Diseño del cluster
hc.c<-hclust(distance)
#plot(hc.c)
plot(hc.c,labels = utilities$Company,hang = -1,main = "Dendograma cluster")

#Enlazamiento promedio (Average Linkage)
hc.a<-hclust(distance,method = "average")
plot(hc.a,labels = utilities$Company,hang = -1,main = "Dendograma cluster")

#Cluster Membership
member.c<-cutree(hc.c,3)
member.a<-cutree(hc.a,3)

table(member.c,member.a)

#Cluster Means
aggregate(z, list(member.c),mean)
aggregate(utilities[,-c(1,1)], list(member.c),mean)

#Silhoute Plot

plot(silhouette(cutree(hc.c,3),distance),col="gold",main = "Grafica Clustering")

#Scree Plot
wss<-(nrow(z)-1)*sum(apply(z,2,var))
for (i in 2:20) wss[i]<-sum(kmeans(z,centers=i)$withinss)
  plot(1:20,wss,type="b",xlab = "Numero de clusters",ylab = "Dentro del grupo SS")
  
kc<-kmeans(z,3)
kc
plot(Sales~D.Demand,utilities,col=kc$cluster,pch=16)



