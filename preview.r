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
colnames(forecast.table)<-c("long","lat","y")
colnames(observation.table)<-c("long","lat","y")
str(forecast.table)
summary(forecast.table)
mean(forecast.table$V3!=0)#0.3145842

#plot
pred.plot<-ggplot(data=forecast.table,aes(x=lat,y=long,colour=y))+
    geom_point()+
    scale_colour_gradient(low="white",high="blue")+
    ggtitle("2005051100_f07 map")
pdf<-pdf(file="E:\\UW\\autumn 2014\\RA\\report\\predict plot.pdf",width=10,height=8)
pred.plot
dev.off()

obs.plot<-ggplot(data=forecast.table,aes(x=lat,y=long,colour=y))+
  geom_point()+
  scale_colour_gradient(low="white",high="blue")+
  ggtitle("2005051100_f24 map")
pdf<-pdf(file="E:\\UW\\autumn 2014\\RA\\report\\observation plot.pdf",width=10,height=8)
obs.plot
dev.off()
grid.arrange(pred.plot,obs.plot,ncol=2)

#cluster
#combine data together
wkdata<-data.frame(lat)
sampled.cluster = agnes(point.sample, diss = F, metric = "euclidean",
                        stand = FALSE, method = "average")
temp.cutree = cutree(sampled.cluster, 1:nbr.clusts)
point.count = tapply(source.lab, list(clust.assign, source.lab), length)
#kmeans









map<- ggplot()+
  geom_polygon(data=forecast.table, aes(x=lat, y=long))+
  scale_fill_manual(values=forecast.table$y,name="y")

map
+
  geom_path(data=circle.data50,aes(x=x,y=y,group=group),color="dark grey")+
  geom_point(data=NPP[index,],aes(x=long.km,y=lat.km,group=group),color="red",size=4)+
  geom_text(data = NPP[index,], x = NPP[index,"long.km"], y = NPP[index,"lat.km"]-5, label = NPP[index,"Name"])+
  ggtitle(paste(factor.interest,"mapping in",NPP[index,"State"]))