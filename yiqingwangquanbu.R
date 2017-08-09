##### 疫情网下载登革热数据统计，report.csv,报告地区，全部包括已删除卡###########3
##### 景钦隆，2014年7月27日编写,广州市疾控中心 #########

library(reshape)

setwd("D:/")
#  读入疫情网数据，report.csv,报告地区，全部包括已删除卡
mdf <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 

#  读入自行统计本地感染一览表数据，每日登革热日报本地感染病例一览表数据
mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
#  读入自行统计输入病例一览表数据，每日登革热日报输入病例一览表数据
mdf.df.sr <- mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE)

## 增加 省、市、区、街道（镇）字段
mdf$省 <- substr(mdf$现住详细地址,1,3)
mdf$地市 <- substr(mdf$现住详细地址,4,6)
mdf$区县 <- substr(mdf$现住详细地址,7,9)
mdf$街道 <- substr(mdf$现住详细地址,10,12)
mdf$cases <- 1
today <- as.Date(Sys.Date(),"%Y-%m-%d")
table(mdf$审核状态)

## 已删除卡统计 
ysck <- mdf[mdf$审核状态=="已删除卡",]
today.ysck <- subset(ysck,as.Date(删除时间)==(today-1))
### 前一日删除病例名单 ###########
if (nrow(today.ysck)!=0) today.ysck$患者姓名 else "前一日无删除病例" 
##排除当天报告当天删除病例后的删除病例。
today.ysck.t <- today.ysck[today.ysck$报告卡录入时间<(today-1),]
if (nrow(today.ysck.t)!=0) today.ysck.t[,c("患者姓名","区县","街道","删除原因","备注")] else "前一日无删除病例" 

## 未审核卡统计 
wshk <- mdf[mdf$审核状态=="未审核卡",]
wshk$患者姓名
today.wshk <- subset(wshk,as.Date(wshk$报告卡录入时间)==(today-1))
### 前一日未审核病例名单 ###########
if (nrow(today.wshk)!=0) today.wshk$患者姓名 else "前一日无未审核卡病例" 

## 已终审卡统计 
yzsk <- mdf[mdf$审核状态=="已终审卡",]
### 查找已终审卡重名病例 
yzsk.cf <- yzsk[duplicated(yzsk$患者姓名),]
### 已终审卡重名病例名单 ######
if (nrow(yzsk.cf)!=0) yzsk.cf$患者姓名 else "已终审卡无重名病例" 

### 已终审卡前一日订正报告统计
dingzheng.yzsk <- subset(yzsk,as.Date(yzsk$订正报告时间)==(today-1))
####  前一日订正病例名单
if (nrow(dingzheng.yzsk)!=0) dingzheng.yzsk$患者姓名 else "前一日无订正病例" 

##### 统计已终审卡前一天新增报告病例数，包括本地和输入病例
xz.yzsk <- yzsk[as.Date(yzsk$报告卡录入时间)==(today-1),]
if (nrow(xz.yzsk )!=0) aggregate(cases~地市+区县+街道,length,data=xz.yzsk) else "前一日无新增报告病例"
#####  已终审卡前一日新增报告病例名单
if (nrow(xz.yzsk )!=0) 
{
    xz.yzsk$患者姓名
    xz.md <- xz.yzsk[c("患者姓名","性别","年龄","现住详细地址","职业","发病日期","报告卡录入时间","病例分类","报告单位","备注")]
    write.csv(xz.md,paste0("前一日新增报告病例一览表",Sys.Date(),".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","前一日新增报告病例一览表",Sys.Date(),".csv"))
} else {"前一日无新增报告病例"}



# 从已终审卡中排除输入病例，得到疫情网本地病例一览表数据,并统计日报数据（数据统计截至前一日）
##该项统计不包括外地报本地病例
### yqw--疫情网,bd--本地,rb--日报
mdf.bd.yqw <- subset(yzsk,!(yzsk$患者姓名 %in% mdf.df.sr$姓名))
hz <- aggregate(cases~区县+街道,length,data=mdf.bd.yqw)
mdf.rq <- mdf.bd.yqw[c("区县","街道","性别","职业","发病日期","报告卡录入时间","病例分类","报告单位","cases")]
mdf.rh <- melt(mdf.rq,id=c("区县","街道","性别","职业","发病日期","报告卡录入时间","病例分类","报告单位"))
hz.r <- cast(mdf.rh,区县+街道~ 病例分类,length)
hz.r$累计病例 <- hz.r$临床诊断病例+hz.r$实验室确诊病例+hz.r$疑似病例
hz.r.sort <- hz.r[order(hz.r$区县,-hz.r$累计病例),]
hz.r.sort$当日新增病例 <- 0
today <- as.Date(Sys.Date(),"%Y-%m-%d")
if ((today-1) %in% as.Date(mdf.bd.yqw$报告卡录入时间))
{
xz.mdf <- mdf.bd.yqw[as.Date(mdf.bd.yqw$报告卡录入时间)==(today-1),]
xz.hz <- aggregate(cases~区县+街道,length,data=xz.mdf)
for (i in 1:nrow(xz.hz))
   { for (j in 1:nrow(hz.r.sort))
        {
         if (hz.r.sort$街道[j]==xz.hz$街道[i]) hz.r.sort$当日新增病例[j]=xz.hz$cases[i]
         }
    }
}
qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,临床诊断病例,实验室确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
qx.xz.hz$街道 <- " "
final <- rbind(hz.r.sort,qx.xz.hz)
final.sort <- final[order(final$区县,-final$累计病例),c("区县","街道","累计病例","当日新增病例","临床诊断病例","实验室确诊病例","疑似病例")]
colsum <- colSums(qx.xz.hz[,2:6])
colsum.df <- as.data.frame(t(colsum))
colsum.df$街道 <- " "
colsum.df$区县 <- "合计"
final.final <- rbind(final.sort,colsum.df)
write.csv(final.final,paste0("登革热日报表",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","登革热日报表",Sys.Date(),".csv"))


### 导出从疫情网统计的本地病例一览表
rb.mdf.bd.yqw <- mdf.bd.yqw[,c("患者姓名","性别","年龄","现住详细地址","职业","发病日期","报告卡录入时间","病例分类","报告单位")]
rb.mdf.bd.yqw$现住详细地址 <- substr(rb.mdf.bd.yqw$现住详细地址,7,40)
rb.mdf.bd.yqw$报告卡录入时间 <- as.Date(rb.mdf.bd.yqw$报告卡录入时间)
rb.mdf.bd.yqw$报告卡录入时间 <- format(rb.mdf.bd.yqw$报告卡录入时间,"%m-%d")
rb.mdf.bd.yqw$发病日期 <- as.Date(rb.mdf.bd.yqw$发病日期)
rb.mdf.bd.yqw$发病日期 <- format(rb.mdf.bd.yqw$发病日期,"%m-%d")
rb.mdf.bd.yqw$序号 <- 1:nrow(rb.mdf.bd.yqw)
write.csv(rb.mdf.bd.yqw,paste0("登革热本地病例一览表",Sys.Date(),".csv"),row.names=F)
shell.exec(paste0("D:/","登革热本地病例一览表",Sys.Date(),".csv"))


## 日报短信内容编写
###   新增报告
xz.hz.sort <- xz.hz[order(xz.hz$区县,-xz.hz$cases),]
xz.hz.qx <- aggregate(cases~区县,sum,data=xz.hz.sort)
xz.hz.qx <- xz.hz.qx[order(-xz.hz.qx$cases),]
aa4 <- NULL
for (i in 1:nrow(xz.hz.qx))
{  
 aa <- subset(xz.hz.sort,xz.hz.sort$区县== xz.hz.qx$区县[i])
 aa1 <- paste0(aa$街道,aa$cases,seq="例、")
 aa2 <- paste0(xz.hz.qx$区县[i],xz.hz.qx$cases[i],"例",",")
 aa3 <- c(aa2,"包括",aa1,";")
 aa4 <- c(aa4,aa3)
}
aa5 <- paste0(Sys.Date()-1,"，我市新增报告登革热",colsum.df$当日新增病例,"例，","其中")
###   累计报告
qx.xz.hz.sort <- qx.xz.hz[order(-qx.xz.hz$累计病例),]
aa6 <- paste0("截至",Sys.Date()-1,"24时",",我市共报告本地感染登革热",colsum.df$累计病例,"例，")
aa7 <- paste0(qx.xz.hz.sort$区县,qx.xz.hz.sort$累计病例,"例",seq="、")

### 最终格式和内容
cat(aa5,aa4)
cat(aa6,"分别为",aa7)
paste(cat(aa5,aa4),cat(aa6,"分别为",aa7))


# 本地病例统计
## 绘制流行曲线 
mdf.bd.yqw$ot <- as.Date(mdf.bd.yqw$发病日期)
mdf.cf <- mdf[duplicated(mdf.bd.yqw$姓名),]
library(scales)
library(ggplot2)
p <- ggplot(data=mdf.bd.yqw,aes(x=ot),xlim=c(as.Date("2014-06-01"),as.Date(Sys.Date())))
p1 <- p + geom_histogram(aes(fill=区县),binwidth=1)
p2 <- p1 + scale_x_date(labels=date_format("%m-%d"),breaks=date_breaks("2 days")) 
p3 <- p2 + theme(axis.text.x=element_text(angle=90)) 
p4 <- p3 + xlab("发病日期")+ylab("发病人数")
p4

##流行曲线区县分面+病例分类填充
p5 <- p4 + facet_grid(区县~.)
p5 <- p4 + facet_grid(区县~.)+aes(colour=病例分类)
p5 <- p4 + facet_grid(区县~.)+geom_histogram(aes(fill=病例分类),binwidth=1)

## 发病时间和报告时间间隔 
mdf.bd.yqw$rt.mdf <- as.Date(mdf.bd.yqw$报告卡录入时间)
mdf.bd.yqw$ot.mdf <- as.Date(mdf.bd.yqw$报告卡录入时间)
mdf.iv <- mdf[order(mdf.bd.yqw$ot.mdf),]
n <- nrow(mdf.bd.yqw)
with(mdf.iv,{plot(ot.mdf,1:n,col="red",pch=20,xlim=c(as.Date("2014-06-01"),as.Date(Sys.Date())),ylim=c(0,80),axes=FALSE,ann=FALSE)})
t1 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"days")
t2 <- seq(as.Date("2014-06-01"),as.Date(Sys.Date()),"weeks")
axis.Date(side=1,t1,at=t2,format="%m-%d",pos=0,las=3)
axis(side=2,seq(0,80,10),pos=as.Date("2014-06-01"),yaxs="i")
with(mdf.iv,{points(rt.mdf,1:n,col="blue",pch=20)})
with(mdf.iv,{segments(ot.mdf,1:n,rt.mdf,1:n,col="grey45")})
time.interval <- mdf.bd.yqw$rt.mdf-mdf.bd.yqw$ot.mdf
text(x=as.Date("2014-06-10"),y=70,labels=paste("最长天数:",max(time.interval),"\n","最短天数:",min(time.interval),"\n","中位数:",median(time.interval)),pos=1,adj=0.5)




# 寻找疫情网report已终审卡和自行统计表中病例分类有变动的名单及病例分类 
ggb1.yzsk <- NULL
for (i in 1:nrow(mdf.df))
{
   for (j in 1:nrow(yzsk))
     {
       if (mdf.df$姓名[i] == yzsk$患者姓名[j])
        {
         if (mdf.df$病例分类[i] != yzsk$病例分类[j]) 
            {ggb1.yzsk <- rbind(ggb1.yzsk,c(mdf.df$姓名[i],mdf.df$病例分类[i],yzsk$患者姓名[j],yzsk$病例分类[j]))}
        }
     }
}
cnames <- c("自统计表姓名","自统计表病例分类","疫情网姓名","疫情网病例分类")
ggb1.yzsk <- as.data.frame(ggb1.yzsk)
names(ggb1.yzsk)<-cnames
ggb1.yzsk



# 寻找疫情网report全部和自行统计表中病例分类有变动的名单及病例分类 
ggbl <- NULL
for (i in 1:nrow(mdf.df))
{
   for (j in 1:nrow(mdf))
     {
       if (mdf.df$姓名[i] == mdf$患者姓名[j])
        {
         if (mdf.df$病例分类[i] != mdf$病例分类[j]) 
            {ggbl <- rbind(ggbl,c(mdf.df$姓名[i],mdf.df$病例分类[i],mdf$患者姓名[j],mdf$病例分类[j]))}
        }
     }
}
cnames <- c("自统计表姓名","自统计表病例分类","疫情网姓名","疫情网病例分类")
ggb1 <- as.data.frame(ggbl)
names(ggb1)<-cnames
ggb1



##### 寻找疫情网截至前天report已终审卡本地病例和昨天report已终审卡本地病例中病例分类有变动的名单及病例分类 #########
###  读入疫情网数据，report.csv,报告地区，全部包括已删除卡
### qr.rb.mdf.bd.yqw--前一日统计的本地病例数据库（rb.mdf.bd.yqw为昨日，如“登革热本地病例一览表2014-08-08”）
qr.rb.mdf.bd.yqw <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
#### 昨日本地病例一览表和今日本地病例一览表中不匹配病例(bppmd.zj--不匹配名单)
bppmd.zj <- subset(qr.rb.mdf.bd.yqw,!(qr.rb.mdf.bd.yqw$患者姓名 %in% rb.mdf.bd.yqw$患者姓名))
bppmd.zj$患者姓名

#### 今日统计本地病例一览表和昨日本地病例一览表中不匹配病例(bppmd.jz--不匹配名单)
rb.mdf.bd.yqw.jy <- subset(rb.mdf.bd.yqw,as.Date(rb.mdf.bd.yqw$报告卡录入时间,"%m-%d")!=(today-1))
bppmd.jz <- subset(rb.mdf.bd.yqw.jy,!(rb.mdf.bd.yqw.jy$患者姓名 %in% qr.rb.mdf.bd.yqw$患者姓名))
bppmd.jz$患者姓名

### 今日统计已删除卡是否在昨日本地病例名单中
ysck.bd.sf <- subset(today.ysck,today.ysck$患者姓名 %in% qr.rb.mdf.bd.yqw$患者姓名) 
ysck.bd.sf$姓名

### 姓名相同，街道有更改病例名单 ### 程序尚需调整
qr.rb.mdf.bd.yqw$街道 <- substr(qr.rb.mdf.bd.yqw$现住详细地址,4,6)
rb.mdf.bd.yqw$街道 <- substr(rb.mdf.bd.yqw$现住详细地址,4,6)
### 今日报表地址变化
bh.jr <- subset(rb.mdf.bd.yqw,(rb.mdf.bd.yqw$患者姓名 %in% qr.rb.mdf.bd.yqw$患者姓名)&!(rb.mdf.bd.yqw$街道 %in% qr.rb.mdf.bd.yqw$街道))
### 昨日报表地址变化
bh.zr <- subset(qr.rb.mdf.bd.yqw,(qr.rb.mdf.bd.yqw$患者姓名 %in% rb.mdf.bd.yqw$患者姓名)&!(qr.rb.mdf.bd.yqw$街道 %in% rb.mdf.bd.yqw$街道))


# 寻找今日报表rb.mdf.bd.yqw和昨日报表qr.rb.mdf.bd.yqw中病例分类有变动的名单及病例分类(bt.bd-本地病例变动名单) 
bt.bd <- NULL
for (i in 1:nrow(rb.mdf.bd.yqw))
{
   for (j in 1:nrow(qr.rb.mdf.bd.yqw))
     {
       if (rb.mdf.bd.yqw$患者姓名[i] == qr.rb.mdf.bd.yqw$患者姓名[j])
        {
         if (rb.mdf.bd.yqw$病例分类[i] != qr.rb.mdf.bd.yqw$病例分类[j]) 
            {bt.bd <- rbind(bt.bd,c(rb.mdf.bd.yqw$患者姓名[i],rb.mdf.bd.yqw$病例分类[i],qr.rb.mdf.bd.yqw$患者姓名[j],qr.rb.mdf.bd.yqw$病例分类[j]))}
        }
     }
}
cnames <- c("昨日姓名","昨日病例分类","前日姓名","前日病例分类")
bt.bd <- as.data.frame(bt.bd)
names(bt.bd)<-cnames
bt.bd

# 寻找今日报表rb.mdf.bd.yqw和昨日报表qr.rb.mdf.bd.yqw中病例分类有变动的名单及街道(bt.bd-本地病例变动名单) 
bt.bd <- NULL
for (i in 1:nrow(rb.mdf.bd.yqw))
{
   for (j in 1:nrow(qr.rb.mdf.bd.yqw))
     {
       if (rb.mdf.bd.yqw$患者姓名[i] == qr.rb.mdf.bd.yqw$患者姓名[j])
        {
         if (rb.mdf.bd.yqw$街道[i] != qr.rb.mdf.bd.yqw$街道[j]) 
            {bt.bd <- rbind(bt.bd,c(rb.mdf.bd.yqw$患者姓名[i],rb.mdf.bd.yqw$街道[i],qr.rb.mdf.bd.yqw$患者姓名[j],qr.rb.mdf.bd.yqw$街道[j]))}
        }
     }
}
cnames <- c("昨日姓名","昨日街道","前日姓名","前日街道")
bt.bd <- as.data.frame(bt.bd)
names(bt.bd)<-cnames
bt.bd

## 今天和昨天的登革热日报表比对
qr.final.final <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
a <- final.final[final.final$街道!=" ",] ##今天统计表
b <- qr.final.final[qr.final.final$街道!=" ",] ##昨天统计表
a1 <- subset(a,!(a$街道 %in% b$街道)) ## 今天新增街道   
a2 <- subset(b,!(b$街道 %in% a$街道)) ## 今天排除街道

