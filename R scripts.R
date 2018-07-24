
# install required packages and call the libraries:
install.packages("cluster")
if (!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")
library ("cluster")
library("ggplot2")
# set up working directory
setwd("~/Desktop/Dr. Shao and Yang/MS Data/imzML/Data_after_using_other_codes/")
getwd()
my_data<-read.csv("aligned_intensity_matrix.csv", header=TRUE)

head(my_data, 10)
MS_data<-t(my_data)


data(my_data$In_calss_Quiz_1)
head(my_data$In_calss_Quiz_1)
my_data$In_calss_Quiz_1
length(my_data$In_calss_Quiz_1)
my_data[,'In_calss_Quiz_1']
my_data['Alanna_Courts',]
my_data['2',]
my_data[my_data=="na"]<-0
head(my_data, 10)
my_data[is.na(my_data)]<-0
my_data<-scale(my_data)
head(my_data,10)
md<-my_data
md
dist.md<-dist(MS_data,method="euclidean")
dist.md
# as.matrix(dist.md)
# install.packages("factoextra")
library("factoextra")
fviz_dist(dist.md)
# dist.md.cor<-get_dist(md, method="pearson")
# fviz_dist(dist.md.cor)

# k-means clustering
fviz_nbclust(MS_data, kmeans, method="wss")
km.res<-kmeans(MS_data, 8, nstart=50)
print(km.res)
km.res$size
km.res$cluster
mdd<-cbind(md, cluster=km.res$cluster)
head(mdd, n=5)
MS_data_scaled=scale(MS_data)
# find which column are constand zero numbers, it returns the column number.
which(apply(MS_data, 2, var)==0)
fviz_cluster(km.res, data=MS_data, ellipse.type="euclid", 
             star.plot=TRUE, repel=TRUE, ggtheme=theme_minimal())




