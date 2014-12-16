#Gaussian Mixture Model
#library
library(mclust)           # load mclust library
#function definition
measure<-function(model,G=9){
para=model$parameters
variance=(para)$variance
center<-para$mean;
covariance=variance[[4]]
DD<-array(dim=c(1,2,9))
L<-array(dim=c(2,2,9))
for(index in 1:9){
	ch<-variance[[5]][,,index];
	dd<-diag(ch);
	L[,,index]<-t(ch/dd);
	DD[,,index]<-dd^2
}
return(list(mean=center,covariance=covariance,L=L,Diag=DD))
}


###########################################33
try<-cbind(wkdata$long[wkdata$z>20],wkdata$lat[wkdata$z>20],wkdata$z.mod[wkdata$z>20])
test<-try[sample(dim(try)[1], size=5000, replace = FALSE),1:2]
plot(test[,1:2])
default<-mclustBIC(test)
plot(default)
Customize <- mclustBIC(test, G = 1:20, x = default)
Model <- summary(Customize,data = test)
Model <-summary(default,data = test)
plot(Customize, G = 10:20, legendArgs = list(x = "bottomleft"))
mclust2Dplot(data = test[,1:2], what = "density", identify = TRUE, parameters = Model$parameters, z = Model$z)
summary(Customize,test)
wreathDefault <- mclustBIC(wreath)
wreathCustomize <- mclustBIC(wreath, G = 1:20, x = wreathDefault)
plot(Customize, G = 10:20, legendArgs = list(x = "bottomleft"))
summary(wreathCustomize, wreath)


dens<-densityMclust(test)
plot(dens)
plot(dens,type="persp",col=grey(.8))
plot(dens,type="image",col=topo.colors(50))
plot(Customize,test,col="grey",nlevels=10,drawlabels=FALSE)


########################
#example online
########################
library(mclust)
data(wreath)
wreathBIC <- mclustBIC(wreath)
wreathBIC <- mclustBIC(wreath, G = 1:20, x = wreathBIC)
wreathModel <- summary(wreathBIC, data = wreath)
mclust2Dplot(data = wreath, what = "density", identify = TRUE, parameters = wreathModel$parameters, z = wreathModel$z)



####################
#plot
####################
library(xtable)
library(ggplot2)
library(gridExtra)
library(cluster)
library(mclust) 
####################
#read data
####################
## read two images, one forecast and one observation
observation.table = read.table(file="E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\coamps_ttl_prcp_g2_2005051100_f24.txt_nohead.gz", header = F)
colnames(observation.table)<-c("lat","long","z")
##################
#candidates<-c(paste("f0",c(7:9),sep=""),paste("f0",c(10:15),sep=""))
#filename<-paste("E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\coamps_ttl_prcp_g2_2005051100_",candidates,".txt_nohead.gz",sep="")
#for(i in 1:length(candidates)){
#i=1
#forecast.table=read.table(file=filename[i],header=F)
#################
forecast.table = read.table(file="E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\coamps_ttl_prcp_g2_2005051100_f09.txt_nohead.gz", header = F)
colnames(forecast.table)<-c("lat","long","z")
#cluster
#combine data together
wkdata<-data.frame(lat=c(forecast.table$lat,observation.table$lat),
                   long=c(forecast.table$long,observation.table$long),
                   z=c(forecast.table$z,observation.table$z),
                   group=rep(c("forecast","observation"),c(dim(forecast.table)[1],dim(observation.table)[1])))

#introduce threshold
threshold=20
wkdata$z.mod<-wkdata$z
wkdata$z.mod[wkdata$z<threshold]=0
#modify data
wkdata$binary<-as.numeric(wkdata$z.mod>0)
####################
#plot raw
####################
raw<-ggplot(data=wkdata,aes(x=long,y=lat,colour=z.mod))+geom_point()
pdf<-pdf("raw_f14.pdf",width=8,height=6)
raw
dev.off()

#######################
#cluster test
#######################
try<-cbind(wkdata$long[wkdata$z>threshold],wkdata$lat[wkdata$z>threshold],wkdata$z.mod[wkdata$z>threshold])
set.seed(570)
test<-try[sample(dim(try)[1], size=25000, replace = FALSE),1:2]
#default<-mclustBIC(test)
#testsummary<-summary(default,test)
#pdf<-pdf("cluster_f14.pdf",width=8,height=6)
#mclust2Dplot(test,classification=testsummary$classification,parameters=testsummary$parameters)
#dev.off()
#}

#####
#get 95 quantile
#####
filenames <- list.files(path="E:\\UW\\autumn 2014\\RA\\code\\Data\\DATA_COAMPS\\", pattern="*nohead.gz", full.names=TRUE)
length(filenames)#98
temp<-NULL;
for(i in filenames){
temp<-c(temp,as.vector(t(read.table(file=i,colClasses=c("NULL","NULL","numeric"), header = F))))
}
quantile(temp,probs=0.95)#18.34836
quan<-NULL;
for(i in filenames){
temp<-as.vector(t(read.table(file=i,colClasses=c("NULL","NULL","numeric"), header = F)))
quan<-c(quan,quantile(temp,probs=0.95))
}
median(quan)# 17.30275

#####
#no prior
model1<-Mclust(data=test,G=9,modelNames="VVV")
plot(model1,what="classification")
measure(model1,G=9)
#prior
model2<-Mclust(data=test,G=9,modelNames="VVV",prior= priorControl())
measure(model2,G=9)
#no difference
model3<-Mclust(data=test,G=9,modelNames="VEV",prior= priorControl())
model4<-Mclust(data=test,G=9,modelNames="EEV",prior= priorControl())

pdf<-pdf("diff model.pdf",width=8,height=6)
plot(model1,what="classification")
plot(model2,what="classification")
plot(model3,what="classification")
plot(model4,what="classification")
dev.off()

