
library(psych)
library(ggplot2)
library(dplyr)
setwd("C:/R")

cluster<-read.csv("DataSet.csv",sep = ",",header = T)

cluster1<-subset(cluster, Cluster==1)
cluster2<-subset(cluster, Cluster==2)
summary(cluster1$disable)
summary(cluster2$disable)
dev.off()

#density plot
plot(density(cluster2$disable), col="red", lwd=2, main = "Density plot")
lines(density(cluster1$disable), col="blue", lwd=2, main = "Density plot")
abline(v=mean(cluster1$disable), col="blue", lty=1)
abline(v=median(cluster1$disable), col ="blue", lty=3)
abline(v=mean(cluster2$disable), col="red", lty=1)
abline(v=median(cluster2$disable), col ="red", lty=3)

legend("topright",col = c("red", "red","blue", "blue"), lty=c(1,2), lwd=2,
       legend = c("����2 Mean","����2 Median", "����1 Mean", "����1 Median"))

#histogram
hist(cluster1$disable, main="����� �̿��� �� ������׷�",
     breaks = seq(0,700000,by=50000),ylim = c(0,40), 
     col=rgb(0,0,1,1/4), xlab = "����� �̿��� ��", ylab = "�� ����",
     cex.lab=1.5, cex.axis=1, cex.main=2, cex.sub=1.5)

hist(cluster2$disable, main="����2", breaks = seq(0,700000,by=50000),
     col=rgb(1,0,0,1/4), ylim = c(0,40),
     xlab = "����� �̿��� ��", ylab = "�� ����",
     cex.lab=1.5, cex.axis=1, cex.main=2, cex.sub=1.5, add = T)

#�������м�
summary(cluster1$disable)
summary(cluster2$disable)
describe(cluster1$disable)
describe(cluster2$disable)

