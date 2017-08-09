
# author： Qinlong Jing
# 

#分析登革热全市、区县、街镇病例与蚊媒密度的关系

library(plyr)
## 读入每日本地病例数据库
mdf.bl <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)
mdf.bl$区县 <- substr(mdf.bl$现住详细地址,1,3)
mdf.bl$街镇 <- substr(mdf.bl$现住详细地址,4,6)
mdf.bl$cases <- 1

## 读入每日蚊媒数据
mdf.01 <- read.csv(file=file.choose(),header=T,as.is=T)
mdf.wm <- mdf.01[as.Date(mdf.01$A10)>=as.Date("2014-09-20"),] ###分析9月20日以来的数据

qxy <- unique(mdf.bl$区县)
qx <- qxy[!is.na(qxy)]
jzy <- unique(mdf.bl$街镇)
jz <- jzy[!is.na(jzy)]

####全市每日发病、报告病例数与蚊媒密度(平均值)关系走势图
qswm.mean <- ddply(mdf.wm,.(as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))

opar <- par(no.readonly=TRUE)
win.graph(width=30, height=15, pointsize=12) 
with(mdf.bl,
    {
    date=seq(as.Date("2014/06/01"),Sys.Date()-1,"1 day")
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date))) 
    data.onset=table(mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    for(i in 1:length(date))
    {
    data.plot[i,2]=ifelse(date[i]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[i]],0)
    data.plot[i,3]=ifelse(date[i]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[i]],0)
    data.plot[i,4]=ifelse(date[i]%in%as.Date(qswm.mean[,1]),qswm.mean[,2][as.Date(qswm.mean[,1])==date[i]],NA)
    data.plot[i,5]=ifelse(date[i]%in%as.Date(qswm.mean[,1]),qswm.mean[,3][as.Date(qswm.mean[,1])==date[i]],NA)
    data.plot[i,6]=ifelse(date[i]%in%as.Date(qswm.mean[,1]),qswm.mean[,4][as.Date(qswm.mean[,1])==date[i]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=8,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",lty=1,col="deeppink",lwd=1)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16)
    datetick <- seq(as.Date("2014/06/01"),Sys.Date()-1,"2 day")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=1,lty=2,ylim=c(0,30),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=1)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=1)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19)
    axis(side=4,at=seq(0,30,2.5))
})
title(main="广州市本地感染登革热每日发病、报告病例数与蚊媒密度（平均值）关系走势图",xlab="日期",ylab="病例数")
legend("topleft",inset=0.05,legend=c("报告病例数","布雷图指数","标准间指数","成蚊密度"),col=c("deeppink","cyan","lawngreen","blueviolet"),lty=c(1,2,3,4),pch=c(16,17,18,19),lwd=c(1,1,1,1),bty="n")



####全市每日发病、报告病例数与蚊媒密度(中位数)关系走势图
qswm.median <- ddply(mdf.wm,.(as.Date(A10)),summarize
              ,布雷图指数中位数=median(as.numeric(A29),na.rm=T)
              ,标准间指数中位数=median(as.numeric(A35),na.rm=T)
              ,成蚊密度中位数=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))

opar <- par(no.readonly=TRUE)
win.graph(width=30, height=15, pointsize=12) 
with(mdf.bl,
    {
    date=seq(as.Date("2014/06/01"),Sys.Date()-1,"1 day")
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date))) 
    data.onset=table(mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    
    for(i in 1:length(date))
    {
    data.plot[i,2]=ifelse(date[i]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[i]],0)
    data.plot[i,3]=ifelse(date[i]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[i]],0)
    data.plot[i,4]=ifelse(date[i]%in%as.Date(qswm.median[,1]),qswm.median[,2][as.Date(qswm.median[,1])==date[i]],NA)
    data.plot[i,5]=ifelse(date[i]%in%as.Date(qswm.median[,1]),qswm.median[,3][as.Date(qswm.median[,1])==date[i]],NA)
    data.plot[i,6]=ifelse(date[i]%in%as.Date(qswm.median[,1]),qswm.median[,4][as.Date(qswm.median[,1])==date[i]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=8,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",lty=1,col="deeppink",lwd=1)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16)
    datetick <- seq(as.Date("2014/06/01"),Sys.Date()-1,"2 day")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=1,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=1)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=1)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19)
    axis(side=4,at=seq(0,20,2.5))
})
title(main="广州市本地感染登革热每日发病、报告病例数与蚊媒密度（中位数）关系走势图",xlab="日期",ylab="病例数")
legend("topleft",inset=0.05,legend=c("报告病例数","布雷图指数","标准间指数","成蚊密度"),col=c("deeppink","cyan","lawngreen","blueviolet"),lty=c(1,2,3,4),pch=c(16,17,18,19),lwd=c(1,1,1,1),bty="n")



####十二个区县每日发病、报告病例数与蚊媒密度(平均数)关系走势图
qxwm.mean <- ddply(mdf.wm,.(A2,as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))

opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
win.graph(width=30, height=15, pointsize=12) 
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i],] #提取区县病例数据
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],] #提取区县蚊媒数据
with(qx.mdf.bl,
    {
    date=seq(as.Date("2014/06/01"),Sys.Date()-1,"1 day")
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    qxwm.mean1 <- qxwm.mean[qxwm.mean[,1]==qx[i],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(qxwm.mean1[,2]),qxwm.mean1[,3][as.Date(qxwm.mean1[,2])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(qxwm.mean1[,2]),qxwm.mean1[,4][as.Date(qxwm.mean1[,2])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(qxwm.mean1[,2]),qxwm.mean1[,5][as.Date(qxwm.mean1[,2])==date[k]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=8,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",lty=1,col="deeppink",lwd=1)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16)
    datetick <- seq(as.Date("2014/06/01"),Sys.Date()-1,"2 day")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=1,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=1)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=1)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19)
    axis(side=4,at=seq(0,20,2.5))
})
title(main=paste0(qx[i],"本地感染登革热每日发病、报告病例数与蚊媒密度（平均数）关系走势图"),xlab="日期",ylab="病例数")
legend("topleft",inset=0.05,legend=c("报告病例数","布雷图指数","标准间指数","成蚊密度"),col=c("deeppink","cyan","lawngreen","blueviolet"),lty=c(1,2,3,4),pch=c(16,17,18,19),lwd=c(1,1,1,1),bty="n")
}
par(opar)


####十二个区县每日发病、报告病例数与蚊媒密度(中位数)关系走势图
qxwm.median <- ddply(mdf.wm,.(A2,as.Date(A10)),summarize
              ,布雷图指数中位数=median(as.numeric(A29),na.rm=T)
              ,标准间指数中位数=median(as.numeric(A35),na.rm=T)
              ,成蚊密度中位数=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))

opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
win.graph(width=30, height=15, pointsize=12) 
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i],] #提取区县病例数据
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],] #提取区县蚊媒数据
with(qx.mdf.bl,
    {
    date=seq(as.Date("2014/06/01"),Sys.Date()-1,"1 day")
     data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    qxwm.median1 <- qxwm.median[qxwm.median[,1]==qx[i],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(qxwm.median1[,2]),qxwm.median1[,3][as.Date(qxwm.median1[,2])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(qxwm.median1[,2]),qxwm.median1[,4][as.Date(qxwm.median1[,2])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(qxwm.median1[,2]),qxwm.median1[,5][as.Date(qxwm.median1[,2])==date[k]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=8,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",lty=1,col="deeppink",lwd=1)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16)
    datetick <- seq(as.Date("2014/06/01"),Sys.Date()-1,"2 day")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=1,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=1)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=1)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19)
    axis(side=4,at=seq(0,20,2.5))
})
title(main=paste0(qx[i],"本地感染登革热每日发病、报告病例数与蚊媒密度（中位数）关系走势图"),xlab="日期",ylab="病例数")
legend("topleft",inset=0.05,legend=c("报告病例数","布雷图指数","标准间指数","成蚊密度"),col=c("deeppink","cyan","lawngreen","blueviolet"),lty=c(1,2,3,4),pch=c(16,17,18,19),lwd=c(1,1,1,1),bty="n")
}
par(opar)






library(plyr)
## 计算蚊媒指数平均数
jzwm.mean <- ddply(mdf.wm,.(A2,A4,as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T)
)

###街镇流行曲线分区县街镇一张表,ifelse版20141015版
opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
dev.new()
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i],] ##提取区县病例数据
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],]
par(mfrow=c(6,4),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
jzsly <- unique(qx.mdf.bl$街镇)
jzsl <- jzsly[jzsly!="不详乡"]
for (j in (1:length(jzsl)))
{
jz.qx.mdf.bl <- qx.mdf.bl[qx.mdf.bl$街镇==jzsl[j],]
jz.qx.mdf.wm <- qx.mdf.wm[qx.mdf.wm$街镇==jzsl[j],]
with(jz.qx.mdf.bl,
    {
    date=seq(as.Date("2014/06/01"),Sys.Date()-1,"1 day")
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(jz.qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(jz.qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    jzwm.mean1 <- jzwm.mean[jzwm.mean[,1]==qx[i]&jzwm.mean[,2]==jzsl[j],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,4][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,5][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,6][as.Date(jzwm.mean1[,3])==date[k]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=4,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",col="deeppink",lwd=1)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16,cex=0.5)
    datetick <- seq(as.Date("2014/06/01"),Sys.Date()-1,"7 days")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=0.5,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17,cex=0.5)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=0.5)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18,cex=0.5)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=0.5)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19,cex=0.5)
    axis(side=4,at=seq(0,20,2.5))
  })
title(main=paste0(qx[i],jzsl[j]),xlab="",ylab="病例数")
legend("topleft",inset=0.05,legend="报告曲线",col="deeppink",lty=1,pch=16,lwd=1,bty="n")
}
}

###街镇流行曲线街镇单张表,ifelse版20141015版
###用i区区县后再进行街镇画图，以免不能画出所有图
### 病例数据和蚊媒数据取9月20日以来的数据
opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
dev.new()
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i]&mdf.bl$发病日期>=as.Date("2014-09-20"),] ##提取区县病例数据,9月20日之后
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],]
jzsly <- unique(qx.mdf.bl$街镇)
jzsl <- jzsly[jzsly!="不详乡"]
for (j in (1:length(jzsl)))
{
dev.new()
jz.qx.mdf.bl <- qx.mdf.bl[qx.mdf.bl$街镇==jzsl[j],]
jz.qx.mdf.wm <- qx.mdf.wm[qx.mdf.wm$街镇==jzsl[j],]
with(jz.qx.mdf.bl,
    {
    date=seq(as.Date("2014/09/20"),Sys.Date()-1,"1 day")###日期改为9月20日起的数据
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(jz.qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(jz.qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    jzwm.mean1 <- jzwm.mean[jzwm.mean[,1]==qx[i]&jzwm.mean[,2]==jzsl[j],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,4][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,5][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,6][as.Date(jzwm.mean1[,3])==date[k]],NA)
    }
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=35,col="grey70",axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="l",col="deeppink",lwd=2)
    points(as.Date(data.plot$date),data.plot$rtnum,col="deeppink",pch=16,cex=2)
    datetick <- seq(as.Date("2014/09/20"),Sys.Date()-1,"1 days")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="cyan",lwd=2,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="cyan",pch=17,cex=2)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=2)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18,cex=2)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=2)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19,cex=2)
    axis(side=4,at=seq(0,20,2.5))
  })
title(main=paste0(qx[i],jzsl[j]),xlab="",ylab="病例数")
legend("topright",inset=0.05,legend=c("报告病例数","布雷图指数（平均数）","标准间指数（平均数）","成蚊密度（平均数）"),col=c("deeppink","cyan","lawngreen","blueviolet"),lty=c(1,2,3,4),pt.cex=c(2,2,2,2),pch=c(16,17,18,19),lwd=c(2,2,2,2),bty="n")
}
}
par(opar)


###街镇流行曲线街镇单张表,ifelse版20141018版
###用i区区县后再进行街镇画图，以免不能画出所有图
### 病例数据和蚊媒数据取9月20日以来的数据
### 发病曲线和报告曲线均以半透明直方图表示
opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
dev.new()
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i]&mdf.bl$发病日期>=as.Date("2014-09-20"),] ##提取区县病例数据,9月20日之后
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],]
jzsly <- unique(qx.mdf.bl$街镇)
jzsl <- jzsly[jzsly!="不详乡"]
for (j in (1:length(jzsl)))
{
win.graph(width=30, height=15, pointsize=12) 
jz.qx.mdf.bl <- qx.mdf.bl[qx.mdf.bl$街镇==jzsl[j],]
jz.qx.mdf.wm <- qx.mdf.wm[qx.mdf.wm$街镇==jzsl[j],]
with(jz.qx.mdf.bl,
    {
    date=seq(as.Date("2014/09/20"),Sys.Date()-1,"1 day")###日期改为9月20日起的数据
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(jz.qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(jz.qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    jzwm.mean1 <- jzwm.mean[jzwm.mean[,1]==qx[i]&jzwm.mean[,2]==jzsl[j],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,4][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,5][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,6][as.Date(jzwm.mean1[,3])==date[k]],NA)
    }
    mycolor <- rainbow(2,alpha=0.5)    
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=33,col=mycolor[1],axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="h",lwd=33,col=mycolor[2],lend=1)
    datetick <- seq(as.Date("2014/09/20"),Sys.Date()-1,"1 days")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    legend("topleft",inset=0.05,legend=c("发病病例数","报告病例数"),col=c(mycolor[1],mycolor[2]),fill=c(mycolor[1],mycolor[2]),border=FALSE,bty="n")

    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="deeppink",lwd=2,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="deeppink",pch=17,cex=2)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=2)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18,cex=2)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=2)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19,cex=2)
    axis(side=4,at=seq(0,20,2.5))
  })
title(main=paste0(qx[i],jzsl[j]),xlab="日期",ylab="病例数")
mtext("蚊媒密度（平均数）",side=4,line=-1.5)
legend("topright",inset=0.05,legend=c("布雷图指数（平均数）","标准间指数（平均数）","成蚊密度（平均数）"),col=c("deeppink","lawngreen","blueviolet"),lty=c(2,3,4),pt.cex=c(2,2,2),pch=c(17,18,19),lwd=c(2,2,2),bty="n")
}
}
par(opar)

###街镇流行曲线街镇单张表,ifelse版20141018版
###用i区区县后再进行街镇画图，以免不能画出所有图
### 病例数据和蚊媒数据取9月20日以来的数据
### 发病曲线和报告曲线均以半透明直方图表示
### 产生PNG图片版
dir.create(paste0("D:/","JZMRYQ",Sys.Date()))
setwd(paste0("D:/","JZMRYQ",Sys.Date()))

## 计算蚊媒指数平均数
jzwm.mean <- ddply(mdf.wm,.(A2,A4,as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T)
)

opar <- par(no.readonly=TRUE)
for (i in (1:length(qx)))
{
qx.mdf.bl <- mdf.bl[mdf.bl$区县==qx[i]&mdf.bl$发病日期>=as.Date("2014-09-20"),] ##提取区县病例数据,9月20日之后
qx.mdf.wm <- mdf.wm[mdf.wm$区县==qx[i],]
jzsly <- unique(qx.mdf.bl$街镇)
jzsl <- jzsly[jzsly!="不详乡"]
for (j in (1:length(jzsl)))
{
png(paste0(qx[i],jzsl[j],".png"),width=28,height=13,units="cm",res=300)
jz.qx.mdf.bl <- qx.mdf.bl[qx.mdf.bl$街镇==jzsl[j],]
jz.qx.mdf.wm <- qx.mdf.wm[qx.mdf.wm$街镇==jzsl[j],]
with(jz.qx.mdf.bl,
    {
    date=seq(as.Date("2014/09/20"),Sys.Date()-1,"1 day")###日期改为9月20日起的数据
    data.plot=data.frame(date=date,otnum=rep(NA,length(date)),rtnum=rep(NA,length(date)),BImean=rep(NA,length(date)),SSImean=rep(NA,length(date)),ADImean=rep(NA,length(date)))
    data.onset=table(jz.qx.mdf.bl$发病日期)
    data.onset1=as.data.frame(data.onset)
    data.report=table(jz.qx.mdf.bl$网络报告时间)
    data.report1=as.data.frame(data.report)
    jzwm.mean1 <- jzwm.mean[jzwm.mean[,1]==qx[i]&jzwm.mean[,2]==jzsl[j],]
    for(k in 1:length(date))
    {
    data.plot[k,2]=ifelse(date[k]%in%as.Date(data.onset1[,1]),data.onset1[,2][as.Date(data.onset1[,1])==date[k]],0)
    data.plot[k,3]=ifelse(date[k]%in%as.Date(data.report1[,1]),data.report1[,2][as.Date(data.report1[,1])==date[k]],0)
    data.plot[k,4]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,4][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,5]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,5][as.Date(jzwm.mean1[,3])==date[k]],NA)
    data.plot[k,6]=ifelse(date[k]%in%as.Date(jzwm.mean1[,3]),jzwm.mean1[,6][as.Date(jzwm.mean1[,3])==date[k]],NA)
    }
    mycolor <- rainbow(2,alpha=0.5)    
    plot(as.Date(data.plot$date),data.plot$otnum,type="h",lwd=33,col=mycolor[1],axes=FALSE,ann=FALSE,lend=1,ylim=c(0,max(c(data.plot$otnum,data.plot$rtnum))+10))
    lines(as.Date(data.plot$date),data.plot$rtnum,type="h",lwd=33,col=mycolor[2],lend=1)
    datetick <- seq(as.Date("2014/09/20"),Sys.Date()-1,"1 days")
    axis.Date(side=1,date,at=datetick,"%m-%d",las=3,pos=0)
    axis(side=2)
    legend("topleft",inset=0.05,legend=c("发病病例数","报告病例数"),col=c(mycolor[1],mycolor[2]),fill=c(mycolor[1],mycolor[2]),border=FALSE,bty="n")

    par(new=T)
    plot(as.Date(data.plot$date),data.plot[,4],type="l",col="deeppink",lwd=2,lty=2,ylim=c(0,20),axes=FALSE,ann=FALSE)
    points(as.Date(data.plot$date),data.plot[,4],col="deeppink",pch=17,cex=2)
    lines(as.Date(data.plot$date),data.plot[,5],type="l",col="lawngreen",lty=3,lwd=2)
    points(as.Date(data.plot$date),data.plot[,5],col="lawngreen",pch=18,cex=2)
    lines(as.Date(data.plot$date),data.plot[,6],type="l",col="blueviolet",lty=4,lwd=2)
    points(as.Date(data.plot$date),data.plot[,6],col="blueviolet",pch=19,cex=2)
    axis(side=4,at=seq(0,20,2.5))
  })
title(main=paste0(qx[i],jzsl[j]),xlab="日期",ylab="病例数")
mtext("蚊媒密度（平均数）",side=4,line=-1.5)
legend("topright",inset=0.05,legend=c("布雷图指数（平均数）","标准间指数（平均数）","成蚊密度（平均数）"),col=c("deeppink","lawngreen","blueviolet"),lty=c(2,3,4),pt.cex=c(2,2,2),pch=c(17,18,19),lwd=c(2,2,2),bty="n")
dev.off()
}
}
par(opar)

