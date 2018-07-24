if (!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library ("cluster")
library("ggplot2")
install.packages("factoextra")
library("factoextra")
setwd("~/Desktop/Dr. Shao and Yang/MS Data/imzML/Data_after_using_other_codes/")
tSNEdata<-read.csv("t-SNE_results_on_whole_dataset.csv", header=FALSE)
opticsdata<-read.csv("optics_order_on_whole_dataset.csv", header=FALSE)
wholedataset<-read.csv("aligned_intensity_matrix(182_peaks).csv", header=TRUE)

head(tSNEdata, 5)
dim(tSNEdata)
md<-scale(tSNEdata)
wholedataset<-t(wholedataset)
dim(wholedataset)
wholemd<-scale(wholedataset)
head(md,10)
dist.md<-dist(md,method="euclidean")
dist.md
# as.matrix(dist.md)

# display the dissimilarity matrix:
fviz_dist(dist.md)
# dist.md.cor<-get_dist(md, method="pearson")
# fviz_dist(dist.md.cor)

# k-means clustering
fviz_nbclust(md, kmeans, method="wss")
km.res<-kmeans(md, 6, nstart=50)

print(km.res)
km.res$size
km.res$cluste
# add cluster to original dataset
mdd<-cbind(md, cluster=km.res$cluster)
head(mdd, n=5)
# visualize the result
fviz_cluster(km.res, data=md, ellipse.type="euclid", 
             star.plot=TRUE, repel=TRUE, ggtheme=theme_minimal() )

# CLARA in R

### PCA results analysis 01/03/2018
pca_score<-read.csv("PCA_Score_after_log_incomplete.csv",header=FALSE,sep=",")
dim(pca_score)
pca1<-scale(pca_score[,1:2])
fviz_nbclust(pca_score[,1:2],clara, method="silhouette")+theme_classic()
clara.res<-clara(pca_score[,1:2], 4, samples=50, pamLike = TRUE)
fviz_cluster(clara.res, ellipse.type = "euclid", pointsize = 1, geom = "point",
             palette=c("red", "darkgoldenrod", "green3","cyan2"),
             ggtheme=theme_classic() )



### 182 ions distribution 01/04/2018
ions_182<-read.csv("182_ions_with_intensity.csv",header=FALSE,sep=",")
dim(ions_182)
par(mar=c(2,2,2,2))
plot(as.numeric(ions_182[4,]))
# heatmap of 5 MCR components
MCR_components<-read.csv("MCR_components.csv",header=FALSE,sep=",")
dim(MCR_components)
a<-MCR_components^0.2
dim(a)
plot_ly(z=as.matrix(a), type="heatmap")



# determine the number of clusters
fviz_nbclust(wholemd, clara, method="silhouette")+theme_classic()
clara.res<-clara(md, 6, samples=50, pamLike = TRUE)
print(clara.res)

# add cluster # to the original dataset
mdd<-cbind(md, cluster=clara.res$cluster)
head(mdd, n=5)

# change color palette
palette()
colorRampPalette(c("red", "darkgoldenrod", "green3","cyan2","deepskyblue2", "violet"))
# visualize
fviz_cluster(clara.res, ellipse.type = "euclid", pointsize = 1, geom = "point",
             palette=c("red", "darkgoldenrod", "green3","cyan2","deepskyblue2", "violet"),
            ggtheme=theme_classic() )


# transfer cluster ID and plot physical map 
clusterID<-clara.res$clustering
length(clusterID)
library(plotly)
library(graphics)
tempdata<-data.frame(matrix(vector(),0,(188)))
dim(tempdata)
i=1;
j=1;
for (i in 1:188){
  for (j in 1:75){
    tempdata[76-j,i]=clusterID[75*(i-1)+j]
  }
} 
dim(tempdata)
tempdata<-as.matrix(tempdata)

plot_ly( z=tempdata, type="heatmap", 
         colors=colorRamp(c("red", "darkgoldenrod", "green3","cyan2","deepskyblue2", "violet")))


# DBSCAN

installed.packages("dbscan")
install.packages("fpc")
library(fpc)
library(dbscan)
dbs<-fpc::dbscan(md, eps=0.10, MinPts = 50)

fviz_cluster(dbs, data=md, stand=FALSE, ellipse = FALSE,
              palette=c("red", "darkgoldenrod", "yellow","yellow3",
                        "cyan2","magenta","green", "green3", "deepskyblue","gray",
                        "deepskyblue3", 
                        "gold", "violetred1"), 
             show.clust.cent = FALSE, geom="point", 
             ggtheme = theme_classic())

dbscan::kNNdistplot(md, k=50)
abline(h=0.1, lty=2)
# transfer cluster ID and plot physical map
dbclusterID<-dbs$cluster
for (i in 1:14100){
  dbclusterID[i]=dbclusterID[i]+1;
}
length(dbclusterID)
library(plotly)
library(graphics)
dbtempdata<-data.frame(matrix(vector(),0,(188)))
dim(dbtempdata)
i=1;
j=1;
for (i in 1:188){
  for (j in 1:75){
    dbtempdata[76-j,i]=dbclusterID[75*(i-1)+j]
  }
} 
dim(dbtempdata)
dbtempdata<-as.matrix(dbtempdata)
plot_ly( z=dbtempdata, type="heatmap", 
         colors=c("black","red", "darkgoldenrod", "yellow","yellow3",
                   "cyan2","magenta","green", "green3", "deepskyblue","gray",
                   "deepskyblue3", 
                   "gold", "violetred1"))

write.table(dbclusterID, file="dbclusterID2.csv",row.names=FALSE, col.names=TRUE, sep=",")
         











# code for mathing optics and t-SNE clusters

colnames(tSNEdata)<-c("x","y")

temptable<-data.frame(matrix(vector(),0,(1)));
j=1;
for (i in 1:14100){
  if (tSNEdata[i,1]>0 &&tSNEdata[i,1]<40 && tSNEdata[i,2]> (-30)&&tSNEdata[i,2]<40){
    temptable[j,1]=i
    j=j+1
  }
}

temptable2<-data.frame(matrix(vector(),0,(1)));
for (i in 1:length(temptable$matrix.vector....0...1..)){
    temptable2[i,1]=opticsdata[temptable[i,1],]
}
  
temptable<-cbind(temptable,temptable2)
temptable
