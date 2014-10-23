###############
#experiment  ##
###############
library(xtable)
library(ggplot2)
library(gridExtra)
library(cluster)
#read data
## read two images, one forecast and one observation
forecast.table = read.table(file="E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\coamps_ttl_prcp_g2_2005051100_f07.txt_nohead.gz", header = F)
observation.table = read.table(file="E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\coamps_ttl_prcp_g2_2005051100_f24.txt_nohead.gz", header = F)
colnames(forecast.table)<-c("lat","long","z")
colnames(observation.table)<-c("lat","long","z")
str(forecast.table)
summary(forecast.table)
mean(forecast.table$z!=0)#0.3145842

#plot
pred.plot<-ggplot(data=forecast.table,aes(x=long,y=lat,colour=log(z)))+
    geom_point()+
    scale_colour_gradient(low="green",high="red")+
    ggtitle("2005051100_f07 map")
	pred.plot
	
pdf<-pdf(file="E:\\UW\\autumn 2014\\RA\\report\\predict plot.pdf",width=10,height=8)
pred.plot
dev.off()

obs.plot<-ggplot(data=observation.table,aes(x=long,y=lat,colour=z))+
  geom_point()+
  scale_colour_gradient(low="white",high="blue")+
  ggtitle("2005051100_f24 map")
pdf<-pdf(file="E:\\UW\\autumn 2014\\RA\\report\\observation plot.pdf",width=10,height=8)
obs.plot
dev.off()
grid.arrange(pred.plot,obs.plot,ncol=2)

#cluster
#combine data together
wkdata<-data.frame(lat=c(forecast.table$lat,observation.table$lat),
                   long=c(forecast.table$long,observation.table$long),
                   z=c(forecast.table$z,observation.table$z),
                   group=rep(c("forecast","observation"),c(dim(forecast.table)[1],dim(observation.table)[1])))


 #kmeans
K=5
#change for binary data
wkdata$binary<-as.numeric(wkdata$z>0)*100
clusterk= kmeans(wkdata[,c(1,2,5)],centers=K)
clusterm= kmeans(cbind(wkdata[,1]*6,wkdata[,2],wkdata[,5]),centers=K)
clusterz=kmeans(wkdata[,3],centers=K)
sum(clusterz$cluster!=clusterk$cluster)
#cluster1 = wkdata[clusterk$cluster==1,]
#cluster2 = wkdata[clusterk$cluster==2,]
#cluster3 = wkdata[clusterk$cluster==3,]
#cluster4 = wkdata[clusterk$cluster==4,]

wkdata$cluster<-clusterm$cluster
cluster<-ggplot(data=wkdata,aes(x=long,y=lat,colour=cluster))+
  geom_point()
  
  +
  scale_colour_gradient(low="white",high="dark green")
  
  cluster

  ratio<-rep(0,K)
  for(i in 1:K){
	ratio[i]<-mean(wkdata[clusterk$cluster==i,5])
  }
 ratio
 
 n<-NULL;
 for(i in 1:K){
 n<-c(n,sum(wkdata$cluster==i))}
 n 
 
 wkdata$ratio<-rep(0,dim(wkdata)[1])
 for(i in 1:K){
 wkdata$ratio[wkdata$cluster==i]<-rep(ratio[i],n[i])
 }

 a<-which.max(ratio)
 
 test<-wkdata[clusterk$cluster==a,]
 
 #picture 
g<-ggplot(data=test[test[,3]>0,],aes(x=long,y=lat,colour=z))+
  geom_point()+
  scale_colour_gradient(low="white",high="blue")
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\test plot.pdf",width=10,height=8)  
g
dev.off()
#hierachical clustering
#average
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "euclidean",
                        stand = FALSE, method = "average")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
average<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))
#single
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "euclidean",
                        stand = FALSE, method = "single")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
single<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut)) 

#complete
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "euclidean",
                        stand = FALSE, method = "complete")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
complete<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))
complete

#wald's method
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "euclidean",
                        stand = FALSE, method = "ward")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
wald<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))
wald

#save plots
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\single.pdf",width=10,height=8)  
single
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\average.pdf",width=10,height=8)  
average
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\complete.pdf",width=10,height=8)  
complete
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\wald.pdf",width=10,height=8)  
wald
dev.off()

#measure
#L1

#average
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "manhattan",
                        stand = FALSE, method = "average")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
average<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))
#single
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "manhattan",
                        stand = FALSE, method = "single")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
single<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut)) 

#complete
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "manhattan",
                        stand = FALSE, method = "complete")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
complete<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))


#wald's method
result = agnes(test[test[,3]>0,-4:-5],diss = F, metric = "manhattan",
                        stand = FALSE, method = "ward")

temp.cutree = cutree(result, 1:4)
test$cut=rep(0,dim(test)[1])
test$cut[test[,3]>0]<-temp.cutree[,4]
test$cut<-as.factor(test$cut)
wald<-ggplot()+
  geom_point(data=test,aes(x=long,y=lat,colour=cut))
wald

#save plots
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\singleL1.pdf",width=10,height=8)  
single
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\averageL1.pdf",width=10,height=8)  
average
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\completeL1.pdf",width=10,height=8)  
complete
dev.off()
pdf<-pdf("E:\\UW\\autumn 2014\\RA\\report\\waldL1.pdf",width=10,height=8)  
wald
dev.off()
