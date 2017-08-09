
##### 景钦隆，2014年8月26日撰写，广州市疾控中心  ##### 
### 此程序用于登革热疫点（以街道为空间统计单元）状态统计，可用于疫点状态信号探测
### 包括疫情始终日期、疫情持续时间、疫情波次、疫情状态（持续、结束）、疫情强度（暴发、散发）、疫情趋势（上升、下降、持平）
### 其中疫情状态、疫情强度、疫情趋势以15天（登革热最长潜伏期）为时间聚集性探测期限

setwd("D:/")
mdf <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)
##### 查找重名病例 ##########
mdf.cf <- mdf[duplicated(mdf$姓名),]
mdf.cf$姓名  ### 重名病例名单
mdf$区县 <- substr(mdf$现住详细地址,1,3)
mdf$街道 <- substr(mdf$现住详细地址,4,6)
mdf$cases <- 1
library(plyr)
ydfz <- function(x){ifelse(x>25,"结束","持续")}
ydzt <- ddply(mdf,c("区县","街道"),summarize,cumcases=length(发病日期),firstdate=min(as.Date(发病日期))
      ,lastdate=max(as.Date(发病日期))
      ,flinterval=lastdate-firstdate
      ,slinterval=Sys.Date()-lastdate
      ,ssinterval=Sys.Date()-firstdate
      ,ydfz=ydfz(slinterval))
ydzt.st <- ydzt[order(ydzt$区县,-ydzt$cumcases),]
count=ydzt.st[,2]
ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=rep(0,length(count))
for(i in 1:length(count))
{
case1=subset(mdf,街道==count[i])
case2=subset(case1,as.Date(case1[,7])>=(Sys.Date()-15))
ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"暴发","散发")
time1=case1[,7]
time2=sort(unique(as.Date(time1)))
time3=rep(0,length(time2)-1)
if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
if ((length(time3)>0)==FALSE) {time3=1}
ydzt.st$frequency[i]=length(time3[time3>=25])+1
case3=subset(case1,as.Date(case1[,7])>=(Sys.Date()-30)&as.Date(case1[,7])<(Sys.Date()-15))
ydzt.st$trend[i]=ifelse(nrow(case2)>nrow(case3),"上升",ifelse(nrow(case2)<nrow(case3),"下降","持平"))
}
ydzt.st <- ydzt.st[,c("区县","街道","cumcases","firstdate","lastdate","flinterval","slinterval","ssinterval","frequency","ydfz","outbreak","trend")]
ydzt.st$ssinterval[ydzt.st$ydfz=="结束"]<-"--"
ydzt.st$outbreak[ydzt.st$ydfz=="结束"]<-"--"
ydzt.st$trend[ydzt.st$ydfz=="结束"]<-"--"
names(ydzt.st)<- c("区县","街道","累计病例数","首发日期","末例日期","末首间隔","今末间隔","今首间隔","疫情波次","疫点状态","疫情强度","疫情趋势")
ydzt.st$首发日期 <- format(ydzt.st$首发日期,"%m-%d")
ydzt.st$末例日期 <- format(ydzt.st$末例日期,"%m-%d")
ydzt.st$今末间隔 <- as.numeric(ydzt.st$今末间隔)
ydzt.st$末首间隔 <- as.numeric(ydzt.st$末首间隔)
ydzt.st$今首间隔 <- as.numeric(ydzt.st$今首间隔) 
write.csv(ydzt.st,paste0("登革热疫点状态",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","登革热疫点状态",Sys.Date(),".csv"))

###### 函数 plyr 版，2014年9月9日
ydfz <- function(x){ifelse(x>25,"结束","持续")}
ydqd <- function(x){ifelse (length(x[x>=(Sys.Date()-15)])>=3,"暴发","散发")}
ydqs <- function(x){ifelse (length(x[x>=(Sys.Date()-15)])>length(x[(x>=(Sys.Date()-30))&(x<(Sys.Date()-15))]),"上升",
        ifelse(length(x[x>=(Sys.Date()-15)])<length(x[(x>=(Sys.Date()-30))&(x<(Sys.Date()-15))]),"下降","持平"))}
ydcs <- function(x)
{ y=sort(unique(x))
  jgts <- NULL
  if (length(y)>1) {for (i in 1:(length(y)-1)){jgts[i]=difftime(y[i+1],y[i])}}
  if ((length(y)>1)==FALSE) {jgts=1}
  ydcs = length(jgts[jgts>=25])+1
  ydcs
}

ydzt <- ddply(mdf,c("区县","街道"),summarize,cumcases=length(发病日期),firstdate=min(as.Date(发病日期))
      ,lastdate=max(as.Date(发病日期))
      ,flinterval=lastdate-firstdate
      ,slinterval=Sys.Date()-lastdate
      ,ssinterval=Sys.Date()-firstdate
      ,ydfz=ydfz(slinterval)
      ,ydqd=ydqd(as.Date(发病日期))
      ,ydqs=ydqs(as.Date(发病日期))
      ,ydcs=ydcs(as.Date(发病日期))
  )
ydzt.st <- ydzt[order(ydzt$区县,-ydzt$cumcases),]
write.csv(ydzt.st,paste0("登革热疫点状态",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","登革热疫点状态",Sys.Date(),".csv"))

#### 绘制疫点状态图，程序待调整
n <- nrow(ydzt.sort)
ydzt$首发日期 <- as.Date(ydzt$首发日期)
ydzt$末例日期 <- as.Date(ydzt$末例日期)
ydzt.sort <- ydzt.st[order(ydzt.st$首发日期),]
with(ydzt.sort,{plot(首发日期,1:n,col="red",pch=20,xlim=c(as.Date("2014-06-01"),as.Date(Sys.Date())),ylim=c(0,80),axes=FALSE,ann=FALSE)})
t1 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"days")
t2 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"weeks")
axis.Date(side=1,t1,at=t2,format="%m-%d",pos=0,las=3)
axis(side=2,seq(1,n,1),pos=as.Date("2014-06-01"),yaxs="i")
with(ydzt.sort,points(末例日期,1:n,col="blue",pch=20))
with(ydzt.sort,{segments(首发日期,1:n,末例日期,1:n,col="grey45")})
abline(v=Sys.Date()-25,col="red")##疫情结束参考线
text(x=Sys.Date()-27,y=70,pos=1,srt=270,labels="疫情结束参考线",col="blue")
text(x=ydzt.sort$首发日期,y=1:n,labels=ydzt.sort$街道,cex=0.4,pos=2)


##aggregate函数法
#######
ydzk <- function(sj)
{
    rq <- as.Date(sj)
    cumcases <- length(rq)
    firstdate <- min(rq)
    lastdate <- max(rq)
    flinterval <- lastdate-firstdate
    slinterval <- Sys.Date()-lastdate
    ssinterval <- Sys.Date()-firstdate
    ydfz <- ydfz(slinterval)
    c(cumcases,format(firstdate,"%m-%d"),format(lastdate,"%m-%d"),flinterval,slinterval,ssinterval,ydfz)
}
aa <- aggregate(mdf$发病日期,list(mdf$区县,mdf$街道),ydzk)


###tapply函数法
ydzk <- function(sj)
{
    rq <- as.Date(sj)
    cumcases <- length(rq)
    firstdate <- min(rq)
    lastdate <- max(rq)
    flinterval <- lastdate-firstdate
    slinterval <- Sys.Date ()-lastdate
    data.frame(累计病例数=cumcases,首发日期=firstdate,末例日期=lastdate,末首间隔=flinterval,今末间隔=slinterval)
}
bb <- tapply(mdf$发病日期,mdf$街道,ydzk)



