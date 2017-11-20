corr<-function(directory, threshold=0){
  source("pollutantmean.R")
  source("complete.R")
  corr<-vector('numeric')
  emp<-vector('numeric')
  sul<-vector('numeric')
  nit<-vector('numeric')
  
    for (i in 1:322){
      nod<-complete(directory,i)
      nod<-sum(nod[,2])
      if (nod>threshold){
      d<-formatC(i,width=3,flag="0")
      pth<-paste(directory,"/",d,".csv",sep="")
      pdata<-read.table(pth,header=TRUE,sep=",")
      sul<-pdata[!is.na(pdata[,2])&!is.na(pdata[,3]),2]
      nit<-pdata[!is.na(pdata[,2])&!is.na(pdata[,3]),3]
      corr<-c(corr,cor(sul,nit))
      }
      else{
        corr<-c(corr,emp)
      }
    }
  corr
  }
