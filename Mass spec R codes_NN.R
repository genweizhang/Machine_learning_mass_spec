install.packages("neuralnet")
library(neuralnet)
# library(nnet)
install.packages('utils')
library(utils)
install.packages('ROCR')
library(ROCR)
#install.packages('plyr')
#library(plyr)
library(doParallel)
install.packages('foreach')
library(foreach)
# install.packages("NeuralNetTools")
# library('NeuralNetTools')
install.packages('gplots')
library(gplots)

cl<-makeCluster(detectCores()-1)
registerDoParallel(cl) 

setwd("/Users/Genwei/Desktop/Dr. Shao and Yang/MS Data/imzML/Machine_learning/")
A20L<-read.csv("aligned_intensity_matrix_20_adjusted2.csv", header=TRUE, sep = ",")
head(A20L,10)
dim(A20L)
samp<-sample(nrow(A20L), 0.9*nrow(A20L))

# samp
## Below is not a very good scaling method in this case!
#maxs<-apply(md2, 2, max)
#mins<-apply(md2, 2, min)
#scaledmd<-as.data.frame(scale(md2, center = mins, scale = maxs-mins))
#md22<-scale(md2)
## This scale method is better in this case, which is: [(x)-mean(x)]/sd(s)
scaledA20L<-as.data.frame(scale(A20L[,1:182], scale = TRUE))
scaledA20L[is.na(scaledA20L)]<-0
# head(scaledmdd,10)
# samp1<-sample(nrow(scaledmdd), 0.08*nrow(scaledmdd))
a<-as.data.frame(A20L$Label)
colnames(a)=c("label")
scaledA20L<-cbind(scaledA20L,a)
mdtrain<-A20L[samp,]
# samp2<-sample(nrow(scaledmdd[-samp1,]), 0.015*nrow(scaledmdd[-samp1,]))
mdtest<-scaledA20L[-samp,]
# mstrain<-as.matrix.data.frame(mdtrain)
dim(mdtrain)
dim(mdtest)
colnames(mdtrain)
mdtrain<-model.matrix(mdtrain)
# head(mdtrain, 10)
# head(mdtest,10)
# dim(mdtest)
#set.seed(1)
#md.nnet<-nnet(energy~., data=mdtrain, 
#              size=2, rang=0.1, decay=1.0e-4, maxit=1000000)
#md.nnet
# x<-as.data.frame(0.96028,0.98062,	1.97159,	2.9098,	3.40408,	1.54795,	2.47586,	
#                 3.34738,	3.93156,	2.46749,	3.33404,	3.91943,	0.94518,	1.54107,	0.97439)
#pred2<-predict(md.nnet, mdtest, type="raw")
#pred2
#mdtest$energy
# head(mdtest, 10)
# colnames(mdtrain)
# Train the neural network by calling the function: neuralnet()
# pbar<-create_progress_bar('text')
# pbar$init(4)
# r_square<-NULL
# error<-NULL
# for (i in 3:6) {
colnames(mdtrain)=c("label","X1","X2","X3")
set.seed(123)
md.net<-neuralnet(label~X1+X2+X3, data=mdtrain, hidden = c(5), threshold = 0.05, stepmax = 1e+08, 
                  rep = 1,act.fct="logistic", linear.output = FALSE )
# pbar$step()
# print(md.net)
# plotnet(md.net)
plot(md.net,rep="best")


# mlp() to evaluate the relative importance of input variables.
# install.packages('RSNNS')
# install.packages('Rcpp')
# library('RSNNS')
# data(mdtrain)
# x<-mdtrain[,c('r12','r13','r14','r15','r16','r23',
#                'r24','r25','r26','r34','r35','r36','r45','r46','r56')]
# y<-mdtrain[,'energy']
# rel_imp<-mlp(x,y, size=5, lin.Out =TRUE)
# use garson algorithm to tell input variable importance
# garson(md.net, bar_plot=TRUE, xlab='input variable', ylab='relative importance')
pred<-compute(md.net, mdtest[,-16])
# ls(pred)
# pred$neurons
p<-pred$net.result
#p_<-p*(max(md2$energy)-min(md2$energy))+min(md2$energy)
#mdtest_<-mdtest$energy*(max(md2$energy)-min(md2$energy))+min(md2$energy)
# head(p,10)
# head(mdtest[,16],10)
p_<-p*sd(mdd2$energy)+mean(mdd2$energy)

#length(p_)
mdtest_<-mdtest[,16]*sd(mdd2$energy)+mean(mdd2$energy)
#length(mdtest_)
s.pred<-summary(lm(mdtest_~p_))
plot(p_,mdtest_, xlab="x/ predicted energy (Hartree)", main=" Results from a trained neural network", ylab="y/ Given energy (Hartree)")
abline(lm(mdtest_~p_), col="blue")
s.pred$r.squared

#    text(-152.847, -152.836,"R^2=0.9811")
#    text(-152.847, -152.834,"y=1.01x+1.82")
#    text(-152.847, -152.838,"RMSD=0.3638 kcal/mol")

d<-p_-mdtest_
rmsd=sqrt(sum(d^2)/length(d))
RMSD=rmsd*627.509
RMSD
# head(mdtest,5)
# head(p,5)

stopCluster(cl)

# md.net$weights
