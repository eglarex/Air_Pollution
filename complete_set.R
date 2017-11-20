complete<-function(directory, id){
  nod<-vector('numeric')
  for (i in id) {
    d<-formatC(i,width=3,flag="0")
    pth<-paste(directory,"/",d,".csv",sep="")
    pdata<-read.table(pth,header=TRUE,sep=",")
    mc<-length(pdata[!is.na(pdata[,2])&!is.na(pdata[,3]),2])
    #mc<-length(pdata[!is.na(pdata[,2]),2])
    nod<-c(nod,mc)
  }
  print(nod)
  print(id)
  id<-cbind(id,nod)
  id
}
