########ICP_geometric_data########
###require_library
require(lattice)
library(xtable)
library(ggplot2)
library(kernlab)
library(mclust)
library(cluster)
###function_definition
myimage<-function(data){
if(!("lattice"%in%loadedNamespaces())){require(lattice)}
x<-dim(data)[1]:1;
y<-1:dim(data)[2];
plotdata<-expand.grid(x=x,y=y);
plotdata$z<-as.vector(data);
levelplot(z~y*x,plotdata,col.regions=terrain.colors);
}
anlydata<-function(obsdata,foredata){
x<-dim(obsdata)[1]:1;
y<-1:dim(foredata)[2];
data.obs<-expand.grid(x=x,y=y);
data.fore<-data.obs;
data.obs$z<-as.vector(obsdata);
data.fore$z<-as.vector(foredata);
anly.data<-rbind(data.obs,data.fore);
anly.data$type<-factor(rep(c("obs","fore"),each=length(obsdata)));
select<-anly.data$z==0;
anly.data<-anly.data[!select,]
rownames(anly.data)<-1:dim(anly.data)[1]
return(anly.data)
}
downsamp<-function(data,gap){
rowsize=dim(data)[1];
colsize=dim(data)[2];
rowseq=seq(1,rowsize,gap);
colseq=seq(1,colsize,gap);
newdata=matrix(rep(NA,(length(rowseq)-1)*(length(colseq)-1)),nrow=(length(rowseq)-1));
for(i in 1:(length(rowseq)-1)){
	for(j in 1:(length(colseq)-1))
	newdata[i,j]<-mean(data[(gap*i-1):(gap*i),(gap*j-1):(gap*j)]);
}
return(newdata);
}

T.fun<-function(X,Y){
	n=dim(X)[1];
	m=dim(Y)[1];
	S=(cov(X)*n+cov(Y)*m)/(n+m-2);
	T=(n*m/(n+m))*(t(apply(X,2,mean)-apply(Y,2,mean))%*%solve(S)%*%(apply(X,2,mean)-apply(Y,2,mean)))
	return(T)
	}

#F-stat
F.fun<-function(X,Y){
	S.X=cov(X);
	S.Y=cov(Y);
	#S=(S.X*(n-1)+S.Y*(m-1))/(n+m-2)
	dd.X<-diag(S.X);
	dd.Y<-diag(S.Y);
	L.X<-t(S.X/dd.X);
	L.Y<-t(S.Y/dd.Y);
	return(c(dd.X/dd.Y,L.X[2,1]-L.Y[2,1]))
}

#measure for GMM
measure<-function(data,model,G=9){
label<-model$classification
result<-data.frame(data,label=as.factor(label))
#colnames(result)[3]<-c("label")

#ratio-stat
ratio.vector<-NULL;
for(index in 1:G){
	a<-mean(result[(result$label==as.character(index)),"type"]=="fore")
	ratio.vector<-cbind(ratio.vector,a)
}

#t-stat
T.result<-NULL;
for(index in 1:G){
	a<-result[result$label==as.character(index)&result$type=="fore",1:2]
	b<-result[result$label==as.character(index)&result$type=="obs",1:2]
	T.result<-c(T.result,T.fun(a,b));
}

#other statistics
F.result<-NULL;
for(index in 1:G){
	a<-result[result$label==as.character(index)&result$type=="fore",1:2]
	b<-result[result$label==as.character(index)&result$type=="obs",1:2]
	F.result<-cbind(F.result,F.fun(a,b));
}
result<-rbind(ratio.vector,T.result,F.result)
colnames(result)<-paste("group",1:G)
rownames(result)<-c("ratio","T-stat","x.ratio","y.ratio","rotation")
return(result)
}

###read_data
obs<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake000_g240.txt"))
#----#shift  3 pts right,  -5 pts up
fore001<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake001_g240.txt"))
#----#shift  6 pts right, -10 pts up
fore002<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake002_g240.txt"))
#----#shift 12 pts right, -20 pts up
fore003<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom003_g240.txt"))
#----#shift 24 pts right, -40 pts up
fore004<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake004_g240.txt"))
#----#shift 48 pts right, -80 pts up
fore005<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake005_g240.txt"))
#----#shift 12 pts right, -20 pts up, times 1.5
fore006<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake006_g240.txt"))
#----#shift 12 pts right, -20 pts up, minus 0.05 in.
fore007<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Perturbed\\fake007_g240.txt"))


###combine_data
###matrix_data_for_plot
matrix001<-obs+fore001;
matrix002<-obs+fore002;
matrix003<-obs+fore003;
matrix004<-obs+fore004;
matrix005<-obs+fore005;
matrix006<-obs+fore006;
matrix007<-obs+fore007;
###downsampling
d=8
newobs<-downsamp(obs,d);
newfore001<-downsamp(fore001,d);
newfore002<-downsamp(fore002,d);
newfore003<-downsamp(fore003,d);
newfore004<-downsamp(fore004,d);
newfore005<-downsamp(fore005,d);
newfore006<-downsamp(fore006,d);
newfore007<-downsamp(fore007,d);

###dataframe_for_cluster_analysis
test001<-anlydata(newobs,newfore001);
test002<-anlydata(newobs,newfore002);
test003<-anlydata(newobs,newfore003);
test004<-anlydata(newobs,newfore004);
test005<-anlydata(newobs,newfore005);
test006<-anlydata(newobs,newfore006);
test007<-anlydata(newobs,newfore007);

###plot_data
###plot_combine_data
pdf<-pdf(file="E:\\Dropbox\\proj\\2014WeatherVerification\\results\\ICP\\nongeo\\original.pdf",width=10,height=8)
myimage(obs);
myimage(fore001);
myimage(fore002);
myimage(fore003);
myimage(fore004);
myimage(fore005);
myimage(fore006);
dev.off()



###GNN_cluster
pdf<-pdf(file="E:\\Dropbox\\proj\\2014WeatherVerification\\results\\ICP\\nongeo\\nongeo01.pdf")
plot(test001$x,test001$y,col=c("red","green")[as.numeric(test001$type)],cex=1)
#gaussian
model1<-Mclust(data=test001[,1:2],G=1,modelNames="VVV")
plot(model1,what="classification")
model2<-Mclust(data=test001[,1:2],G=2,modelNames="VVV")
plot(model2,what="classification")
model3<-Mclust(data=test001[,1:2],G=3,modelNames="VVV")
plot(model3,what="classification")
model4<-Mclust(data=test001[,1:2],G=4,modelNames="VVV")
plot(model4,what="classification")
model5<-Mclust(data=test001[,1:2],G=5,modelNames="VVV")
plot(model5,what="classification")
model6<-Mclust(data=test001[,1:2],G=6,modelNames="VVV")
plot(model6,what="classification")
model7<-Mclust(data=test001[,1:2],G=7,modelNames="VVV")
plot(model7,what="classification")
model8<-Mclust(data=test001[,1:2],G=8,modelNames="VVV")
plot(model8,what="classification")
model9<-Mclust(data=test001[,1:2],G=9,modelNames="VVV")
plot(model9,what="classification")
dev.off()

###measure
result.mea1<-measure(test001,model1,G=1)
result.mea2<-measure(test001,model2,G=2)
result.mea3<-measure(test001,model3,G=3)
result.mea4<-measure(test001,model4,G=4)
result.mea5<-measure(test001,model5,G=5)
result.mea6<-measure(test001,model6,G=6)
result.mea7<-measure(test001,model7,G=7)
result.mea8<-measure(test001,model8,G=8)
result.mea9<-measure(test001,model9,G=9)

xtable(result.mea1)
xtable(result.mea2)
xtable(result.mea3)
xtable(result.mea4)
xtable(result.mea5)
xtable(result.mea6)
xtable(result.mea7)
xtable(result.mea8)
xtable(result.mea9)


