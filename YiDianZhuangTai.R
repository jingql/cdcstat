
##### ����¡��2014��8��26��׫д�������м�������  ##### 
### �˳������ڵǸ����ߵ㣨�Խֵ�Ϊ�ռ�ͳ�Ƶ�Ԫ��״̬ͳ�ƣ��������ߵ�״̬�ź�̽��
### ��������ʼ�����ڡ��������ʱ�䡢���鲨�Ρ�����״̬��������������������ǿ�ȣ�������ɢ�������������ƣ��������½�����ƽ��
### ��������״̬������ǿ�ȡ�����������15�죨�Ǹ����Ǳ���ڣ�Ϊʱ��ۼ���̽������

setwd("D:/")
mdf <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)
##### ������������ ##########
mdf.cf <- mdf[duplicated(mdf$����),]
mdf.cf$����  ### ������������
mdf$���� <- substr(mdf$��ס��ϸ��ַ,1,3)
mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,4,6)
mdf$cases <- 1
library(plyr)
ydfz <- function(x){ifelse(x>25,"����","����")}
ydzt <- ddply(mdf,c("����","�ֵ�"),summarize,cumcases=length(��������),firstdate=min(as.Date(��������))
      ,lastdate=max(as.Date(��������))
      ,flinterval=lastdate-firstdate
      ,slinterval=Sys.Date()-lastdate
      ,ssinterval=Sys.Date()-firstdate
      ,ydfz=ydfz(slinterval))
ydzt.st <- ydzt[order(ydzt$����,-ydzt$cumcases),]
count=ydzt.st[,2]
ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=rep(0,length(count))
for(i in 1:length(count))
{
case1=subset(mdf,�ֵ�==count[i])
case2=subset(case1,as.Date(case1[,7])>=(Sys.Date()-15))
ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"����","ɢ��")
time1=case1[,7]
time2=sort(unique(as.Date(time1)))
time3=rep(0,length(time2)-1)
if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
if ((length(time3)>0)==FALSE) {time3=1}
ydzt.st$frequency[i]=length(time3[time3>=25])+1
case3=subset(case1,as.Date(case1[,7])>=(Sys.Date()-30)&as.Date(case1[,7])<(Sys.Date()-15))
ydzt.st$trend[i]=ifelse(nrow(case2)>nrow(case3),"����",ifelse(nrow(case2)<nrow(case3),"�½�","��ƽ"))
}
ydzt.st <- ydzt.st[,c("����","�ֵ�","cumcases","firstdate","lastdate","flinterval","slinterval","ssinterval","frequency","ydfz","outbreak","trend")]
ydzt.st$ssinterval[ydzt.st$ydfz=="����"]<-"--"
ydzt.st$outbreak[ydzt.st$ydfz=="����"]<-"--"
ydzt.st$trend[ydzt.st$ydfz=="����"]<-"--"
names(ydzt.st)<- c("����","�ֵ�","�ۼƲ�����","�׷�����","ĩ������","ĩ�׼��","��ĩ���","���׼��","���鲨��","�ߵ�״̬","����ǿ��","��������")
ydzt.st$�׷����� <- format(ydzt.st$�׷�����,"%m-%d")
ydzt.st$ĩ������ <- format(ydzt.st$ĩ������,"%m-%d")
ydzt.st$��ĩ��� <- as.numeric(ydzt.st$��ĩ���)
ydzt.st$ĩ�׼�� <- as.numeric(ydzt.st$ĩ�׼��)
ydzt.st$���׼�� <- as.numeric(ydzt.st$���׼��) 
write.csv(ydzt.st,paste0("�Ǹ����ߵ�״̬",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","�Ǹ����ߵ�״̬",Sys.Date(),".csv"))

###### ���� plyr �棬2014��9��9��
ydfz <- function(x){ifelse(x>25,"����","����")}
ydqd <- function(x){ifelse (length(x[x>=(Sys.Date()-15)])>=3,"����","ɢ��")}
ydqs <- function(x){ifelse (length(x[x>=(Sys.Date()-15)])>length(x[(x>=(Sys.Date()-30))&(x<(Sys.Date()-15))]),"����",
        ifelse(length(x[x>=(Sys.Date()-15)])<length(x[(x>=(Sys.Date()-30))&(x<(Sys.Date()-15))]),"�½�","��ƽ"))}
ydcs <- function(x)
{ y=sort(unique(x))
  jgts <- NULL
  if (length(y)>1) {for (i in 1:(length(y)-1)){jgts[i]=difftime(y[i+1],y[i])}}
  if ((length(y)>1)==FALSE) {jgts=1}
  ydcs = length(jgts[jgts>=25])+1
  ydcs
}

ydzt <- ddply(mdf,c("����","�ֵ�"),summarize,cumcases=length(��������),firstdate=min(as.Date(��������))
      ,lastdate=max(as.Date(��������))
      ,flinterval=lastdate-firstdate
      ,slinterval=Sys.Date()-lastdate
      ,ssinterval=Sys.Date()-firstdate
      ,ydfz=ydfz(slinterval)
      ,ydqd=ydqd(as.Date(��������))
      ,ydqs=ydqs(as.Date(��������))
      ,ydcs=ydcs(as.Date(��������))
  )
ydzt.st <- ydzt[order(ydzt$����,-ydzt$cumcases),]
write.csv(ydzt.st,paste0("�Ǹ����ߵ�״̬",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","�Ǹ����ߵ�״̬",Sys.Date(),".csv"))

#### �����ߵ�״̬ͼ�����������
n <- nrow(ydzt.sort)
ydzt$�׷����� <- as.Date(ydzt$�׷�����)
ydzt$ĩ������ <- as.Date(ydzt$ĩ������)
ydzt.sort <- ydzt.st[order(ydzt.st$�׷�����),]
with(ydzt.sort,{plot(�׷�����,1:n,col="red",pch=20,xlim=c(as.Date("2014-06-01"),as.Date(Sys.Date())),ylim=c(0,80),axes=FALSE,ann=FALSE)})
t1 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"days")
t2 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"weeks")
axis.Date(side=1,t1,at=t2,format="%m-%d",pos=0,las=3)
axis(side=2,seq(1,n,1),pos=as.Date("2014-06-01"),yaxs="i")
with(ydzt.sort,points(ĩ������,1:n,col="blue",pch=20))
with(ydzt.sort,{segments(�׷�����,1:n,ĩ������,1:n,col="grey45")})
abline(v=Sys.Date()-25,col="red")##��������ο���
text(x=Sys.Date()-27,y=70,pos=1,srt=270,labels="��������ο���",col="blue")
text(x=ydzt.sort$�׷�����,y=1:n,labels=ydzt.sort$�ֵ�,cex=0.4,pos=2)


##aggregate������
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
aa <- aggregate(mdf$��������,list(mdf$����,mdf$�ֵ�),ydzk)


###tapply������
ydzk <- function(sj)
{
    rq <- as.Date(sj)
    cumcases <- length(rq)
    firstdate <- min(rq)
    lastdate <- max(rq)
    flinterval <- lastdate-firstdate
    slinterval <- Sys.Date ()-lastdate
    data.frame(�ۼƲ�����=cumcases,�׷�����=firstdate,ĩ������=lastdate,ĩ�׼��=flinterval,��ĩ���=slinterval)
}
bb <- tapply(mdf$��������,mdf$�ֵ�,ydzk)


