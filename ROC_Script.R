### This code is written for the purpose of cs4033/5033 class HW3 by Genwei Zhang, 11/08/2017.

## Question 1:

# Re-direct the working directory.
setwd("~/Desktop/Data Science degree coursework/Fall 2017/CS4033:5033 ML/Homework/HW5/")
# Read in the data file, saved in .csv format using read.csv fcuntion.
originaldata<-read.csv("cs5033_assign05_data copy.csv", header=TRUE, sep="," )
# check the dimension and column names of original file.
dim(originaldata)
colnames(originaldata)

# 1.a
# Create an empty variable that store deterministic forcasts, f*=0.5.
Deter_fore<-vector(mode="integer", length=nrow(originaldata))
count<-0;
for (i in 1:nrow(originaldata)){
  if (originaldata[i,2]>=0.5){
    Deter_fore[i]=1
  }
  else{
    Deter_fore[i]=0
  }
  if (Deter_fore[i]==originaldata[i,1]){
    count<-count+1
  }
}
# Compute the accuracy, report in percentage.
   Accuracy=count/nrow(originaldata)*100
   
# 1.b
# Create an empty variable that store all the accuracy values... 
# from different binarization thresholds.
Accuracytable<-vector(mode="integer", length = 1001)
prb<-txtProgressBar(min=0, max=1001, initial = 0, style = 3)
for (j in 1:1001){
# Create an empty variable that store deterministic forcasts.
   Deter_fore<-vector(mode="integer", length=nrow(originaldata))
   count<-0;
   for (i in 1:nrow(originaldata)){
     if (originaldata[i,2]>=(j-1)*0.001 ){
       Deter_fore[i]=1
     }
     else{
       Deter_fore[i]=0
     }
     if (Deter_fore[i]==originaldata[i,1]){
       count<-count+1
     }
   }
   # Compute the accuracy, report in percentage.
   Accuracytable[j]=count/nrow(originaldata)*100
   setTxtProgressBar(prb,j)
}
# Plot the graph
plot(Accuracytable, pch=16,cex=0.5, ylab="Accuracy (%)", xlab="Binarization threshold")
# Find the index that have the value equals to the max and then... 
# correspond to the binarization threshold.
(which (Accuracytable %in% max(Accuracytable))-1)/1000

## Question 2

# 2.a
# In order to calculate the POD, POFD, we need to generated the...
# contingency table, where:
#                       a is observed and predicted
#                       b is non-observed but predicted
#                       c is observed but non-predicted
#                       d is non-observed and non-predicted

# Create two empty variables that store all the POD and POFD values... 
# for different binarization thresholds.
PODtable<-vector(mode="integer", length = 1001)
POFDtable<-vector(mode="integer", length = 1001)
prb<-txtProgressBar(min=0, max=1001, initial = 0, style = 3)
for (j in 1:1001){
  # Create an empty variable that store deterministic forcasts.
  Deter_fore<-vector(mode="integer", length=nrow(originaldata))
  a<-0;
  b<-0;
  c<-0;
  d<-0;
  for (i in 1:nrow(originaldata)){
    if (originaldata[i,2]>=(j-1)*0.001 ){
      Deter_fore[i]=1
    }
    else{
      Deter_fore[i]=0
    }
    if (Deter_fore[i]==1 && originaldata[i,1]==1){
      a<-a+1
    }
    if (Deter_fore[i]==1 && originaldata[i,1]==0){
      b<-b+1
    }
    if (Deter_fore[i]==0 && originaldata[i,1]==1){
      c<-c+1
    }
    if (Deter_fore[i]==0 && originaldata[i,1]==0){
      d<-d+1
    }
  }
  # Compute the POD and POFD.
  PODtable[j]=a/(a+c)
  POFDtable[j]=b/(b+d)
  setTxtProgressBar(prb,j)
}
# Generate the ROC curve
plot(POFDtable,PODtable, xlab="POFD", ylab="POD", 
     main="ROC curve", pch=16, cex=0.7, col="red")
# Add a random predictor, show its ROC curve in a dashed grey line (x = y).
par(new=TRUE)
abline(0,1,lty=2, lwd=2)
dev.off()

# 2.b
# Create an empty variables that store all the polygon values.
polygon_area<-vector(mode="integer", length = 1000)
for (i in 1:1000){
 polygon_area[i]= (PODtable[i]+PODtable[i+1])*(POFDtable[i]-POFDtable[i+1])/2 
}
# AUC is approximately equals to the sum of all the polygon area.
AUC<-sum(polygon_area)

# 3.c
# Create an array of 1 million random labels (0 or 1) and random forecast (0-1). 
Randomobserved<-vector(mode="integer", length=1000000)
for (i in 1:1000000) {
  Randomobserved[i]<-sample(c(0,1),1)
}
Randomforecast<-runif(1000000,min=0, max=1)

# Then, repeat 2.a and 2.b

# Create two empty variables that store all the POD and POFD values... 
# for different binarization thresholds.
rand_PODtable<-vector(mode="integer", length = 1001)
rand_POFDtable<-vector(mode="integer", length = 1001)
prb<-txtProgressBar(min=0, max=1001, initial = 0, style = 3)
for (j in 1:1001){
  # Create an empty variable that store deterministic forcasts.
  Deter_fore<-vector(mode="integer", length=length(Randomobserved))
  a<-0;
  b<-0;
  c<-0;
  d<-0;
  for (i in 1:length(Randomforecast)){
    if (Randomforecast[i]>=(j-1)*0.001 ){
      Deter_fore[i]=1
    }
    else{
      Deter_fore[i]=0
    }
    if (Deter_fore[i]==1 && Randomobserved[i]==1){
      a<-a+1
    }
    if (Deter_fore[i]==1 && Randomobserved[i]==0){
      b<-b+1
    }
    if (Deter_fore[i]==0 && Randomobserved[i]==1){
      c<-c+1
    }
    if (Deter_fore[i]==0 && Randomobserved[i]==0){
      d<-d+1
    }
  }
  # Compute the POD and POFD.
  rand_PODtable[j]=a/(a+c)
  rand_POFDtable[j]=b/(b+d)
  setTxtProgressBar(prb,j)
}
# Generate the ROC curve
plot(rand_POFDtable,rand_PODtable, xlab=" Random_POFD", ylab="Random_POD", 
     main="ROC curve (Random)", pch=16, cex=0.3, col="grey")

# Create an empty variables that store all the polygon values.
rand_polygon_area<-vector(mode="integer", length = 1000)
for (i in 1:1000){
  rand_polygon_area[i]= (rand_PODtable[i]+rand_PODtable[i+1])*
    (rand_POFDtable[i]-rand_POFDtable[i+1])/2 
}
# AUC is approximately equals to the sum of all the polygon area.
rand_AUC<-sum(rand_polygon_area)


## Question 3

# 3.a
# Split all the 9819 examples into 10 bins.
bin1<-originaldata[which(originaldata[,2]<0.1&originaldata[,2]>=0),]
bin2<-originaldata[which(originaldata[,2]<0.2&originaldata[,2]>=0.1),]
bin3<-originaldata[which(originaldata[,2]<0.3&originaldata[,2]>=0.2),]
bin4<-originaldata[which(originaldata[,2]<0.4&originaldata[,2]>=0.3),]
bin5<-originaldata[which(originaldata[,2]<0.5&originaldata[,2]>=0.4),]
bin6<-originaldata[which(originaldata[,2]<0.6&originaldata[,2]>=0.5),]
bin7<-originaldata[which(originaldata[,2]<0.7&originaldata[,2]>=0.6),]
bin8<-originaldata[which(originaldata[,2]<0.8&originaldata[,2]>=0.7),]
bin9<-originaldata[which(originaldata[,2]<0.9&originaldata[,2]>=0.8),]
bin10<-originaldata[which(originaldata[,2]<=1&originaldata[,2]>=0.9),]

# Compute the mean forecast for each bin.
f=c(mean(bin1[,2]),mean(bin2[,2]),mean(bin3[,2]),mean(bin4[,2]),mean(bin5[,2]),
    mean(bin6[,2]),mean(bin7[,2]),mean(bin8[,2]),mean(bin9[,2]),mean(bin10[,2]))
# Compute the conditional event frequency.
y=c(sum(bin1[,1])/nrow(bin1), sum(bin2[,1])/nrow(bin2), sum(bin3[,1])/nrow(bin3),
    sum(bin4[,1])/nrow(bin4), sum(bin5[,1])/nrow(bin5), sum(bin6[,1])/nrow(bin6),
    sum(bin7[,1])/nrow(bin7), sum(bin8[,1])/nrow(bin8), sum(bin9[,1])/nrow(bin9), 
    sum(bin10[,1])/nrow(bin10))
# Plot the reliability curve and also add a perfect model.
plot(f, y, xlab="Mean forecast", ylab="Conditional event frequency",
     main="Reliability Curve", xlim=c(0,1), ylim=c(0,1),
     pch=16, col="red")
par(new=TRUE)
lines(f[order(f)], y[order(f)], col="red", lwd=2)
abline(0,1,lty=2, lwd=2)
dev.off()

# 3.c
# Using the REL equation, calculate the reliability of the severe-wind model.
REL<-sum(nrow(bin1)*((f[1]-y[1])^2),nrow(bin2)*((f[2]-y[2])^2),nrow(bin3)*((f[3]-y[3])^2),
         nrow(bin4)*((f[4]-y[4])^2),nrow(bin5)*((f[5]-y[5])^2),nrow(bin6)*((f[6]-y[6])^2),
         nrow(bin7)*((f[7]-y[7])^2),nrow(bin8)*((f[8]-y[8])^2),nrow(bin9)*((f[9]-y[9])^2),
         nrow(bin10)*((f[10]-y[10])^2))/nrow(originaldata)
# end.
