setwd("/users/Genwei/Desktop/Dr. Shao and Yang/MS Data/imzML/Machine_learning/")
library(randomForest)
library(stats)
library(gplots)
library(ROCR)
library(ggplot2)
library(reshape2)
A20L<-read.csv("aligned_intensity_matrix_20_adjusted2.csv", header=TRUE, sep = ",")
head(A20L,10)
dim(A20L)
samp1<-sample(nrow(A20L), 0.9*nrow(A20L))
# samp2<-sample(nrow(F20L[-samp1,]),0.05*nrow(F20L[-samp1,]))
A20Ltrain<-A20L[samp1,]
A20Ltest<-A20L[-samp1,]
# F20Ltest<-F20L[samp2,]
dim(A20Ltrain)
dim(A20Ltest)
set.seed(123)
A20L.rf<-randomForest(Label~. -Label, data=A20Ltrain, proximity=TRUE, ntree=500, mtry=7)
print(A20L.rf)
pred=predict(A20L.rf, newdata = A20Ltest)
summary(pred)
head(pred, 10)
together<-cbind(A20Ltest$Label,pred)
colnames(together)=c("original","predicted")
together<-data.frame(together)
together
# plot(together$predicted, together$original,xlab="x/ predicted feature", 
    # ylab="y/ assigned feature", 
    # main="Results from a trained randomForest model")
tb<-table(together$predicted, together$original)
barplot(tb)
tb
tbcor<-cor(tb)
melttbcor<-melt(tbcor)
head(melttbcor)
ggplot(data = melttbcor, aes(x=Var1, y=Var2, fill=value),scale_fill_gradientn()) + geom_tile()


F20Lfull<-read.csv("aligned_intensity_matrix_20.csv", header=TRUE, sep = ",")
predf20=predict(F20L.rf, newdata = F20Lfull)
write.matrix(format(predf20, scientific=FALSE), 
             file = paste("machine_learning_generated_label.csv", sep="/"), sep=",") 

fulldata<-read.csv("aligned_intensity_matrix_copy.csv", header=TRUE, sep = ",")
dim(fulldata)
predfulldata=predict(A20L.rf, newdata = fulldata)
write.matrix(format(predfulldata, scientific=FALSE), 
             file = paste("machine_learning_generated_label_full_2.csv", sep="/"), sep=",") 



predfulldata<-read.csv("machine_learning_generated_label_full_2.csv", header=FALSE, sep = ",")
dim(predfulldata)
colorindex<-predfulldata[,2]
length(colorindex)
temptable<-matrix(vector(),0,(188))
i=1;
j=1;
for (i in 1:188){
  for (j in 1:75){
    tempdata[76-j,i]=colorindex[75*(i-1)+j]
  }
} 
dim(tempdata)
tempdata<-as.matrix(tempdata)
install.packages("plotly")
library(plotly)
library(graphics)
plot_ly( z=tempdata, type="heatmap", colors=colorRamp(c("red", "blue1", "green")))



