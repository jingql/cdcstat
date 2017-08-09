
####用于每日蚊媒数据画图
##全市平均数作图

names(qswm.mean)[1]<- "监测日期"
dev.new()
opar <- par(no.readonly=TRUE)
par(oma=c(1,1,1,1),mar=c(4,4,2,4),mgp=c(3,1,0))
with(qswm.mean,{plot(as.Date(监测日期),布雷图指数平均数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,14),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数平均数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度平均数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,14,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main="广州市每日布雷图指数平均数、标准间指数平均数、成蚊密度平均数走势图",xlab="监测日期",ylab="平均值")
                legend("topright",inset=0.05,c("布雷图指数平均数","标准间指数平均数","成蚊密度平均数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
par(opar)

##全市中位数画图
names(qswm.median)[1]<- "监测日期"
dev.new()
opar <- par(no.readonly=TRUE)
par(oma=c(1,1,1,1),mar=c(4,4,2,4),mgp=c(3,1,0))
with(qswm.median,{plot(as.Date(监测日期),布雷图指数中位数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,8),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数中位数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度中位数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,8,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main="广州市每日布雷图指数中位数、标准间指数中位数、成蚊密度中位数走势图",xlab="监测日期",ylab="中位数")
                legend("topright",inset=0.05,c("布雷图指数中位数","标准间指数中位数","成蚊密度中位数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
par(opar)


##全市达标率画图
names(qswm.hgl)[1]<- "监测日期"
dev.new()
opar <- par(no.readonly=TRUE)
par(oma=c(1,1,1,1),mar=c(4,4,2,4),mgp=c(3,1,0))
with(qswm.hgl,{plot(as.Date(监测日期),布雷图指数达标率,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,100),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数达标率,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度达标率,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)
                lines(as.Date(监测日期),总体达标率,type="b",bty="l",pch=15,col="red",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,100,10),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main="广州市每日总体达标率、布雷图指数达标率、标准间指数达标率、成蚊密度达标率走势图",xlab="监测日期",ylab="达标率（%）")
                legend("topright",inset=0.05,c("布雷图指数达标率","标准间指数达标率","成蚊密度达标率","总体达标率"),pch=c(16,17,18,15),lty=c(1,2,3,1),col=c("darkorange","dodgerblue2","purple4","red"),box.col="white",cex=0.8)
})
par(opar)

#### 十二区县单张图
### 区县平均数一张图
names(qxwm.mean)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.mean$区县)
qx <- qxy[!is.na(qxy)]
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(4,3),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmean <- NULL
for (i in 1:length(qx))
{
qxmean =subset(qxwm.mean,区县==qx[i])
with(qxmean,{plot(as.Date(监测日期),布雷图指数平均数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数平均数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度平均数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx[i],xlab="",ylab="平均数")
                legend("topright",inset=0.05,c("布雷图指数平均数","标准间指数平均数","成蚊密度平均数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

###区县中位数一张图
names(qxwm.median)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.median$区县)
qx <- qxy[!is.na(qxy)]
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(4,3),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmedian <- NULL
for (i in 1:length(qx))
{
qxmedian =subset(qxwm.median,区县==qx[i])
with(qxmedian,{plot(as.Date(监测日期),布雷图指数中位数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数中位数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度中位数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx[i],xlab="",ylab="中位数")
                legend("topright",inset=0.05,c("布雷图指数中位数","标准间指数中位数","成蚊密度中位数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

### 区县达标率一张图
names(qxwm.hgl)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.hgl$区县)
qx <- qxy[!is.na(qxy)]
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(4,3),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxhgl <- NULL
for (i in 1:length(qx))
{
qxhgl =subset(qxwm.hgl,区县==qx[i])
with(qxhgl,{plot(as.Date(监测日期),布雷图指数达标率,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,100),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数达标率,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度达标率,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)
                lines(as.Date(监测日期),总体达标率,type="b",bty="l",pch=15,col="red",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,100,10),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx[i],xlab="",ylab="达标率（%）")
                legend("topright",inset=0.05,c("布雷图指数达标率","标准间指数达标率","成蚊密度达标率","总体达标率"),pch=c(16,17,18,15),lty=c(1,2,3,1),col=c("darkorange","dodgerblue2","purple4","red"),box.col="white",cex=0.8)
})
}
par(opar)

####十二区县每个指标2张图
##区县平均数画两张图
names(qxwm.mean)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.mean$区县)
qx <- qxy[!is.na(qxy)]
qx1 <- qx[1:6]  # "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
qx2 <- qx[7:12] # "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"

#### "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmean <- NULL
for (i in 1:length(qx1))
{
qxmean =subset(qxwm.mean,区县==qx1[i])
with(qxmean,{plot(as.Date(监测日期),布雷图指数平均数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数平均数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度平均数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx1[i],xlab="",ylab="平均值")
                legend("topright",inset=0.05,c("布雷图指数平均数","标准间指数平均数","成蚊密度平均数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

## "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmean <- NULL
for (i in 1:length(qx2))
{
qxmean =subset(qxwm.mean,区县==qx2[i])
with(qxmean,{plot(as.Date(监测日期),布雷图指数平均数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数平均数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度平均数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx2[i],xlab="",ylab="平均值")
                legend("topright",inset=0.05,c("布雷图指数平均数","标准间指数平均数","成蚊密度平均数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

###区县中位数画两张图
names(qxwm.median)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.median$区县)
qx <- qxy[!is.na(qxy)]
qx1 <- qx[1:6]  # "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
qx2 <- qx[7:12] # "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"

#### "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmedian <- NULL
for (i in 1:length(qx1))
{
qxmedian =subset(qxwm.median,区县==qx1[i])
with(qxmedian,{plot(as.Date(监测日期),布雷图指数中位数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数中位数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度中位数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx1[i],xlab="",ylab="中位数")
                legend("topright",inset=0.05,c("布雷图指数中位数","标准间指数中位数","成蚊密度中位数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

# "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxmedian <- NULL
for (i in 1:length(qx2))
{
qxmedian =subset(qxwm.median,区县==qx2[i])
with(qxmedian,{plot(as.Date(监测日期),布雷图指数中位数,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,40),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数中位数,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度中位数,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,40,2),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx2[i],xlab="",ylab="中位数")
                legend("topright",inset=0.05,c("布雷图指数中位数","标准间指数中位数","成蚊密度中位数"),pch=c(16,17,18),lty=c(1,2,3),col=c("darkorange","dodgerblue2","purple4"),box.col="white",cex=0.9)
})
}
par(opar)

### 区县达标率画两张图
names(qxwm.hgl)[1:2]<- c("区县","监测日期")
qxy <- unique(qxwm.hgl$区县)
qx <- qxy[!is.na(qxy)]
qx1 <- qx[1:6]  # "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
qx2 <- qx[7:12] # "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"

#### "白云区" "从化市" "番禺区" "海珠区" "花都区" "黄埔区"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxhgl <- NULL
for (i in 1:length(qx1))
{
qxhgl =subset(qxwm.hgl,区县==qx1[i])
with(qxhgl,{plot(as.Date(监测日期),布雷图指数达标率,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,100),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数达标率,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度达标率,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)
                lines(as.Date(监测日期),总体达标率,type="b",bty="l",pch=15,col="red",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,100,10),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx1[i],xlab="",ylab="达标率（%）")
                legend("topright",inset=0.05,c("布雷图指数达标率","标准间指数达标率","成蚊密度达标率","总体达标率"),pch=c(16,17,18,15),lty=c(1,2,3,1),col=c("darkorange","dodgerblue2","purple4","red"),box.col="white",cex=0.8)
})
}
par(opar)

# "荔湾区" "萝岗区" "南沙区" "天河区" "越秀区" "增城市"
dev.new()
opar <- par(no.readonly=TRUE)
par(mfrow=c(3,2),oma=c(1,1,1,1),mar=c(3,4,2,4),mgp=c(2,1,0))
qxhgl <- NULL
for (i in 1:length(qx2))
{
qxhgl =subset(qxwm.hgl,区县==qx2[i])
with(qxhgl,{plot(as.Date(监测日期),布雷图指数达标率,type="b",bty="l",pch=16,col="darkorange",ann=FALSE,axes=FALSE,ylim=c(0,100),cex=1.5,lwd=2,lty=1)
                lines(as.Date(监测日期),标准间指数达标率,type="b",bty="l",pch=17,col="dodgerblue2",cex=1.5,lwd=2,lty=2)
                lines(as.Date(监测日期),成蚊密度达标率,type="b",bty="l",pch=18,col="purple4",cex=1.5,lwd=2,lty=3)
                lines(as.Date(监测日期),总体达标率,type="b",bty="l",pch=15,col="red",cex=1.5,lwd=2,lty=3)                
                t1 <- seq(min(as.Date(监测日期),na.rm=T)-5,Sys.Date()-1,"days")
                axis.Date(side=1,t1,at=t1,"%m-%d",las=3,xaxs="i",pos=0)
                axis(side=2,at=seq(0,100,10),pos=min(as.Date(监测日期),na.rm=T)-1)
                title(main=qx2[i],xlab="",ylab="达标率（%）")
                legend("topright",inset=0.05,c("布雷图指数达标率","标准间指数达标率","成蚊密度达标率","总体达标率"),pch=c(16,17,18,15),lty=c(1,2,3,1),col=c("darkorange","dodgerblue2","purple4","red"),box.col="white",cex=0.8)
})
}
par(opar)

############
library(reshape2)
library(ggplot2)
qxwm.mean$group=ifelse(qxwm.mean[,1]%in%unique(qxwm.mean[,1])[1:6],1,2)

data=melt(qxwm.mean,id=c("区县","监测日期","group"),measure=c("布雷图指数平均数", "标准间指数平均数","成蚊密度平均数"))
ggplot(data,aes(监测日期,value,colour=variable))+geom_point()+geom_line()+facet_grid(区县~group)
names(data)
data1=subset(data,group==1)
data2=subset(data,group==2)
ggplot(data1,aes(监测日期,value,colour=variable))+geom_point()+geom_line()+
facet_grid(区县~.,scales="free_y")+scale_x_date(breaks=seq(as.Date("2014-9-1"),
as.Date("2014-10-7"),"1 day"))+ theme(axis.text.x=element_text(angle=90))+
scale_colour_hue("")+ylab("平均数")
