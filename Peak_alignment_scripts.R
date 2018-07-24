library("data.table")
library("stats")

setwd("~/Desktop/Dr. Shao and Yang/peak_alignment_script")
beforealign<-read.csv2("Con_TaxB.csv", header=TRUE, sep = ",", na.strings="")

setDT(beforealign)
melteddata<-melt(beforealign, variable.factor = TRUE,
                 measure=patterns("_mass$", "_abund$"), 
                 value.name = c("ms", "ab"), variable.name ="Con")

reloadmass<-as.data.frame(melteddata)
sortedmass<-reloadmass[order(round(as.numeric(reloadmass$ms), 4), na.last =NA),]

datatable<-data.frame(matrix(,ncol=1+max(as.numeric(sortedmass$Con))));
#bottom<-matrix(,ncol=3, nrow=100)
#colnames(bottom)<-c("Con","ms","ab")
#sortedmass<-rbind(sortedmass, bottom)
pb = txtProgressBar(min = 0, max = length(sortedmass$ms), initial = 0, style=3);
k<-0;
kk<-0;
index<-1;
for (i in 1:length(sortedmass$ms)){
  i<-i+kk;
  k<-0;
  if(as.numeric(sortedmass$ms[i])<=300){
    datatable[index,1+as.numeric(sortedmass$Con[i])]=as.numeric(sortedmass$ab[i])
  for (j in 1:max(as.numeric(sortedmass$Con))){
      if((as.numeric(sortedmass$ms[i+j])-as.numeric(sortedmass$ms[i]))<=0.003){
        k=k+1;
        datatable[index,1+as.numeric(sortedmass$Con[i+k])]=as.numeric(sortedmass$ab[i+k])
      }
  }
    datatable[index,1]=mean(as.numeric(sortedmass$ms[i:(i+k)]))
    kk=kk+k;
  }
  
  if(as.numeric(sortedmass$ms[i])>300&&as.numeric(sortedmass$ms[i])<=500){
    datatable[index,1+as.numeric(sortedmass$Con[i])]=as.numeric(sortedmass$ab[i])
    for (j in 1:max(as.numeric(sortedmass$Con))){
      if((as.numeric(sortedmass$ms[i+j])-as.numeric(sortedmass$ms[i]))<=0.005){
        k=k+1;
        datatable[index,1+as.numeric(sortedmass$Con[i+k])]=as.numeric(sortedmass$ab[i+k])
      }
    }
    datatable[index,1]=mean(as.numeric(sortedmass$ms[i:(i+k)]))
    kk=kk+k;
  }
  
  if(as.numeric(sortedmass$ms[i])>500&&as.numeric(sortedmass$ms[i])<=800){
    datatable[index,1+as.numeric(sortedmass$Con[i])]=as.numeric(sortedmass$ab[i])
    for (j in 1:max(as.numeric(sortedmass$Con))){
      if((as.numeric(sortedmass$ms[i+j])-as.numeric(sortedmass$ms[i]))<=0.008){
        k=k+1;
        datatable[index,1+as.numeric(sortedmass$Con[i+k])]=as.numeric(sortedmass$ab[i+k])
      }
    }
    datatable[index,1]=mean(as.numeric(sortedmass$ms[i:(i+k)]))
    kk=kk+k;
  }
  
  if(as.numeric(sortedmass$ms[i])>800&&as.numeric(sortedmass$ms[i])<=1200){
    datatable[index,1+as.numeric(sortedmass$Con[i])]=as.numeric(sortedmass$ab[i])
    for (j in 1:max(as.numeric(sortedmass$Con))){
      if((as.numeric(sortedmass$ms[i+j])-as.numeric(sortedmass$ms[i]))<=0.012){
        k=k+1;
        datatable[index,1+as.numeric(sortedmass$Con[i+k])]=as.numeric(sortedmass$ab[i+k])
      }
    }
    datatable[index,1]=mean(as.numeric(sortedmass$ms[i:(i+k)]))
    kk=kk+k;
  }
  
  if(as.numeric(sortedmass$ms[i])>1200){
    datatable[index,1+as.numeric(sortedmass$Con[i])]=as.numeric(sortedmass$ab[i])
    for (j in 1:max(as.numeric(sortedmass$Con))){
      if((as.numeric(sortedmass$ms[i+j])-as.numeric(sortedmass$ms[i]))<=0.02){
        k=k+1;
        datatable[index,1+as.numeric(sortedmass$Con[i+k])]=as.numeric(sortedmass$ab[i+k])
      }
    }
    datatable[index,1]=mean(as.numeric(sortedmass$ms[i:(i+k)]))
    kk=kk+k;
  }
  setTxtProgressBar(pb, i)
  index<-index+1;
}
write.table(datatable, file="final_aligned_mass.csv",sep=",")


