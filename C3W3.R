---
title: "R Notebook"
output: html_notebook
---
Subsetting and Sorting

```{r}
set.seed(13435)
X<-data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11：15))
X<-X[sample(1:5),];X$var2[c(1,3)]=NA
X
X[,1]
X[,"var1"]
X[1:2,"var2"]
X[(X$var1<=3& X$var3>11),]
X[(X$var1<=3 | X$var3>15)]
X[which(X$var2>8),]
sort(X$var1)
sort(X$var1,decreasing=TRUE)
sort(X$var2,na.last=TRUE)
X[order(X$var1,X$var3),]
library(plyr)
arrange(X,var1)
arrange(X,desc(var1))
X$var4<-rnorm(5)
X
Y<-cbind(X,rnorm(5))
Y
```

Summarizing Data

```{r}
if(!file.exists("./data")){dir.create("./data")}
fileUrl<-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="../restaurants.csv")
restData<-read.csv("../restaurants.csv")
head(restData,n=3)
tail(restData,n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict,na.rm=TRUE)
quantile(restData$councilDistrict,probs=c(0.5,0.75,0.9))
table(restData$zipCode,useNA="ifany")
table(restData$councilDistrict,restData$zipCode)
sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)
colSums(is.na(restData))
all(colSums(is.na(restData))==0)
table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))
restData[restData$zipCode %in% c("21212","21213"),]
data(UCBAdmissions)
DF=as.data.frame(UCBAdmissions)
summary(DF)
xt<-xtabs(Freq~Gender+Admit,data=DF)
xt
warpbreaks$replicate<-rep(1:9,len=54)
xt=xtabs(breaks~.,data=warpbreaks)
xt
ftable(xt)
fakeData=rnorm(1e5)
object.size(fakeData)
print(object.size(fakeData),units = "Mb")
```

Creating New Variables

```{r}
if(!file.exists("./data")){dir.create("./data")}
fileUrl<-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="../restaurants.csv")
restData<-read.csv("../restaurants.csv")
s1<-seq(1,10,by=2); s1
s2<-seq(1,10,length=3);s2
x<-c(1,3,8,25,100);seq(along=x)
restData$nearMe=restData$neighborhood %in% c("Roland Park", "Homeland")
table(restData$nearMe)
restData$zipWrong= ifelse(restData$zipCode<0,TRUE,FALSE)
table(restData$zipWrong,restData$zipCode<0)
restData$zipGroups=cut(restData$zipCode,breaks=quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode)
library(Hmisc)
restData$zipGroups=cut2(restData$zipCode,g=4)
table(restData$zipGroups)
restData$zcf<-factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)
yesno<-sample(c("yes","no"),size=10,replace=TRUE)
yesnofac=factor(yesno,levels=c("yes","no"))
relevel(yesnofac,ref="yes")
as.numeric(yesnofac)
library(Hmisc);library(plyr)
restData2=mutate(restData,zipGroups=cut2(zipCode,g=4))
table(restData2$zipGroups)
```

Reshaping Data

```{r}
library(reshape)
head(mtcars)
mtcars$carname<-rownames(mtcars)
carMelt<-melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
cylData<-dcast(carMelt, cyl~variable)
cylData
cylData<-dcast(carMelt, cyl~variable,mean)
cylData
head(InsectSprays)
tapply(InsectSprays$count,InsectSprays$spray,sum)
spIns=split(InsectSprays$count,InsectSprays$spray)
spIns
sprCount=lapply(spIns,sum)
sprCount
unlist(sprCount)
sapply(spIns,sum)
library(Hmisc)
library(plyr)
ddply(InsectSprays,.(spray),plyr::summarize,sum=sum(count))
spraySums<-ddply(InsectSprays,.(spray),plyr::summarize,sum=ave(count,FUN=sum))
dim(spraySums)
head(spraySums)
```

Managing Data Frmaes with dply - Basic Tools

```{r}
library(dplyr)
options(width=105)
chicago<-readRDS("chicago.rds")
dim(chicago)
str(chicago)
names(chicago) 
head(select(chicago,city:dptp))
head(select(chicago,-(city:dptp)))
i<-match("city",names(chicago))
j<-match("dptp",names(chicago))
head(chicago[,-(i:j)])
chic.f<-filter(chicago, pm25tmean2>30)
head(chic.f,10)
chic.f<-filter(chicago, pm25tmean2>30 & tmpd> 80)
head(chic.f)
chicago<-arrange(chicago,date)
head(chicago)
tail(chicago)
chicago<-arrange(chicago,desc(date))
head(chicago)
chicago<-rename(chicago,pm25=pm25tmean2,dewpoint=dptp)
head(chicago)
chicago<-mutate(chicago,pm25detrend=pm25-mean(pm25,na.rm=TRUE))
head(select(chicago,pm25,pm25detrend))
chicago<-mutate(chicago,tempcat=factor(1*(tmpd>80),labels=c("cold","hot")))
hotcold<-group_by(chicago,tempcat)
hotcold
summarize(hotcold,pm25=mean(pm25),o3=max(o3tmean2),no2=median(no2tmean2))
summarize(hotcold,pm25=mean(pm25,na.rm=TRUE),o3=max(o3tmean2),no2=median(no2tmean2))
chicago<-mutate(chicago,year=as.POSIXlt(date)$year+1900)
years<-group_by(chicago,year)
summarize(years,pm25=mean(pm25,na.rm=TRUE),o3=max(o3tmean2),no2=median(no2tmean2))
chicago %>% mutate(month=as.POSIXlt(date)$mon+1) %>% group_by(month) %>% summarize(pm25=mean(pm25, na.rm=TRUE),o3=max(o3tmean2),no2=median(no2tmean2))
```

Merging Data

```{r}
if(!file.exists("./data")){dir.create("./data")}
fileUrl1="http://www.sharecsv.com/dl/e70e9c289adc4b87c900fdf69093f996/reviews.csv"
fileUrl2="http://www.sharecsv.com/dl/0863fd2414355555be0260f46dbe937b/solutions.csv"
download.file(fileUrl1,destfile="../reviews.csv")
download.file(fileUrl2,destfile="../solutions.csv")
reviews=read.csv("../reviews.csv") 
solutions<-read.csv("../solutions.csv")
head(reviews)
head(solutions)
names(reviews)
names(solutions)
mergedData=merge(reviews,solutions,by.x="solution_id",by.y="id",all=TRUE)
head(mergedData)
intersect(names(solutions),names(reviews))
mergedData2=merge(reviews, solutions, all=TRUE)
head(mergedData2)
df1=data.frame(id=sample(1:10),x=rnorm(10))
df2=data.frame(id=sample(1:10),y=rnorm(10))
arrange(join(df1,df2),id)
df1=data.frame(id=sample(1:10),x=rnorm(10))
df2=data.frame(id=sample(1:10),y=rnorm(10))
df3=data.frame(id=sample(1:10),z=rnorm(10))
dfList=list(df1,df2,df3)
join_all(dfList)

```

Quiz

```{r}
fileUrl1="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl1,destfile="../6hid.csv")
hid=read.csv("../6hid.csv")
agricultureLogical<-c(hid$ACR==3&hid$AGS==6)
which(agricultureLogical)
result<-hid[hid[,"ACR"]==3&hid[,"AGS"]==6&!is.na(hid[,"AGS"])&!is.na(hid[,"ACR"]),]
head(result,n=3)

library(jpeg)
fileUrl1="https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(fileUrl1,destfile="../jf.jpg")
jf<-readJPEG("../jf.jpg",native=TRUE)
quantile(jf,probs=c(0.3,0.8))

fileUrl1="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileUrl2="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl1,destfile="../gdp.csv")
download.file(fileUrl2,destfile="../fed.csv")
gdp<-read.csv("../gdp.csv")
fed<-read.csv("../fed.csv")
names(gdp)<-c("CountryCode","Rank","NA","Econonmy","GDP")
sum(gdp$X%in%fed$CountryCode)
dim(fed)
dim(gdp)
names(fed)
head(fed)
head(gdp,n=33)

mean(as.numeric(gsub(",","",gdp[which(gdp$X%in%fed[which(fed$Income.Group=="High income: OECD"),"CountryCode"]),"X.3"])))

mean(as.numeric(gdp[which(gdp$CountryCode%in%fed[which(fed$Income.Group=="High income: nonOECD"),"CountryCode"]),"Rank"]))

```


Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.




which(gdp$X%in%fed[which(fed$Income.Group=="High income: OECD"),"CountryCode"])
