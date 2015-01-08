#####ICP_geometric_data########
###require_library
require(lattice)
###function_definition
myimage<-function(data){
if(!("lattice"%in%loadedNamespaces())){require(lattice)}
x<-dim(data)[1]:1;
y<-1:dim(data)[2];
plotdata<-expand.grid(x=x,y=y);
plotdata$z<-as.vector(data);
levelplot(z~y*x,plotdata, terrain.colors);
}
anlydata<-function(obs,fore){
x<-dim(obs)[1]:1;
y<-1:dim(fore)[2];
data.obs<-expand.grid(x=x,y=y);
data.fore<-data.obs;
data.obs$z<-as.vector(obs);
data.fore$z<-as.vector(fore);
anly.data<-rbind(data.obs,data.fore);
anly.data$type<-rep(c("obs","fore"),each=length(obs));
select<-anly.data$z==0;
anly.data<-anly.data[!select,]
return(anly.data)
}
###read data
obs<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom000_g240.txt"))
obs<-data.matrix(obs)

#----#50 pts. to the right
fore001<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom001_g240.txt"))
#----#200 pts. to the right
fore002<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom002_g240.txt"))
#----#125 pts. to the right and too big
fore003<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom003_g240.txt"))
#----#125 pts. to the right and wrong orientation
fore004<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom004_g240.txt"))
#----#125 pts. to the right and huge!
fore005<-data.matrix(read.table(file="E:\\Dropbox\\proj\\2014WeatherVerification\\data\\Cases20081023\\Cases\\Fake\\Geometric\\geom005_g240.txt"))

###combine_data
###matrix_data_for_plot
matrix001<-obs+fore001;
matrix002<-obs+fore002;
matrix003<-obs+fore003;
matrix004<-obs+fore004;
matrix005<-obs+fore005;
###dataframe_for_error_analysis
test001<-anlydata(obs,fore001);
test002<-anlydata(obs,fore002);
test003<-anlydata(obs,fore003);
test004<-anlydata(obs,fore004);
test005<-anlydata(obs,fore005);

###plot_data
###plot_combine_data
myimage(matrix001);
myimage(matrix002);
myimage(matrix003);
myimage(matrix004);
myimage(matrix005);

###error_analysis
#ratio-stat
ratio.result<-rep(0,5)
ratio.result[1]<-mean(test001[,"type"]=="fore")
ratio.result[2]<-mean(test002[,"type"]=="fore")
ratio.result[3]<-mean(test003[,"type"]=="fore")
ratio.result[4]<-mean(test004[,"type"]=="fore")
ratio.result[5]<-mean(test005[,"type"]=="fore")
#t-stat
T.fun<-function(X,Y){
	n=dim(X)[1];
	m=dim(Y)[1];
	S=(cov(X)*n+cov(Y)*m)/(n+m-2);
	T=(n*m/(n+m))*(t(apply(X,2,mean)-apply(Y,2,mean))%*%solve(S)%*%(apply(X,2,mean)-apply(Y,2,mean)))
	return(T)
	}
T.result<-rep(0,5)
T.result[1]<-T.fun(test001[test001$type=="fore",1:2],test001[test001$type=="obs",1:2])
T.result[2]<-T.fun(test002[test002$type=="fore",1:2],test002[test002$type=="obs",1:2])
T.result[3]<-T.fun(test003[test003$type=="fore",1:2],test003[test003$type=="obs",1:2])
T.result[4]<-T.fun(test004[test004$type=="fore",1:2],test004[test004$type=="obs",1:2])
T.result[5]<-T.fun(test005[test005$type=="fore",1:2],test005[test005$type=="obs",1:2])

#F-stat
F.fun<-function(X,Y){
	n=dim(X)[1];
	m=dim(Y)[1];
	S.X=cov(X);
	S.Y=cov(Y);
	S=(S.X*(n-1)+S.Y*(m-1))/(n+m-2)
	dd.X<-diag(S.X);
	dd.Y<-diag(S.Y);
	L.X<-t(S.X/dd.X);
	L.Y<-t(S.Y/dd.Y);
	return(c(dd.X/dd.Y,L.X[2,1]-L.Y[2,1]))
}
F.result<-matrix(rep(NA,15),nrow=5)
F.result[1,]<-F.fun(test001[test001$type=="fore",1:2],test001[test001$type=="obs",1:2])
F.result[2,]<-F.fun(test002[test002$type=="fore",1:2],test002[test002$type=="obs",1:2])
F.result[3,]<-F.fun(test003[test003$type=="fore",1:2],test003[test003$type=="obs",1:2])
F.result[4,]<-F.fun(test004[test004$type=="fore",1:2],test004[test004$type=="obs",1:2])
F.result[5,]<-F.fun(test005[test005$type=="fore",1:2],test005[test005$type=="obs",1:2])
F.result
library(int64)
.Machine$integer.max











a<-matrix(1:9,ncol=3)
image(x=rep(1:3,each=3),y=rep(1:3,each=3),z=a)
x<-dim(matrix001)[1]:1;
y<-1:dim(obs)[2];
data<-expand.grid(x=x, y=y)
data$z<-as.vector(test001[1:length(obs),3])
data
levelplot(z~y*x,data,col.regions =  terrain.colors)
