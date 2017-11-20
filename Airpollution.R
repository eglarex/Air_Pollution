pollutantmean<-function(directory,pollutant,id){
  m<-0
  mc<-0
  for (i in id) {
    d<-formatC(i,width=3,flag="0")
    pth<-paste(directory,"/",d,".csv",sep="")
    pdata<-read.table(pth,header=TRUE,sep=",")
    mc<-length(pdata[!is.na(pdata[,pollutant]),pollutant])+mc
    m<-sum(pdata[!is.na(pdata[,pollutant]),pollutant])+m
  }
  pm<-m/mc
  pm
}



