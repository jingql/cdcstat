##### 此程序用于截至前一日本地感染登革热病例情况 #####
##### 景钦隆，2014年7月26日撰写，广州市疾控中心  #####
### 2014年9月10日，修订today为日为最后数字，for循环中改为区县和街道一致后才添加当日新增。###
### 2014年10月4日，增加每日区县新增病例类型统计
library(reshape)
setwd("D:/")
if (file.exists(paste0("D:/","登革热日报表",Sys.Date(),".csv")))
{file.remove(paste0("D:/","登革热日报表",Sys.Date(),".csv"))}
mdf <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)
mdf$区县 <- substr(mdf$现住详细地址,1,3)
mdf$街道 <- substr(mdf$现住详细地址,4,6)
mdf$cases <- 1
hz <- aggregate(cases~区县+街道,length,data=mdf)
mdf.rq <- mdf[c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位","cases")]
mdf.rh <- melt(mdf.rq,id=c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位"))
hz.r <- cast(mdf.rh,区县+街道~病例分类,length)
hz.r$累计病例 <- hz.r$临床诊断病例+hz.r$实验室确诊病例+hz.r$疑似病例
hz.r.sort <- hz.r[order(hz.r$区县,-hz.r$累计病例),]
hz.r.sort$当日新增病例 <- 0
today <- as.Date(as.character(as.Date(Sys.Date(),"%Y-%m-%d")))
if ((today-1) %in% as.Date(mdf$网络报告时间))
{
xz.mdf <- mdf[as.Date(mdf$网络报告时间)==(today-1),]
xz.hz <- aggregate(cases~区县+街道,length,data=xz.mdf)
for (i in 1:nrow(xz.hz))
   { for (j in 1:nrow(hz.r.sort))
        {
         if ((hz.r.sort$区县[j]==xz.hz$区县[i])&(hz.r.sort$街道[j]==xz.hz$街道[i])) hz.r.sort$当日新增病例[j]=xz.hz$cases[i]
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

#每日区县新增病例类型统计
xz.mdf <- mdf[as.Date(mdf$网络报告时间)==(today-1),]
xz.mdf.rq <- xz.mdf[c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位","cases")]
xz.mdf.melt <- melt(xz.mdf.rq,id=c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位"))
xz.hz.r <- cast(xz.mdf.melt,区县~病例分类,length)
xz.hz.r$确诊 <- xz.hz.r$临床诊断病例+xz.hz.r$实验室确诊病例
names(xz.hz.r)[4]<- "疑似"
xz.qx.hz <- xz.hz.r[,c("区县","确诊","疑似")]
xz.qx.hz.colsum <- colSums(xz.qx.hz[2:3])
xz.qx.hz.colsum.df <- as.data.frame(t(xz.qx.hz.colsum))
xz.qx.hz.colsum.df$区县 <- "合计"
names(xz.qx.hz.colsum.df)[1:3] <- c("确诊","疑似","区县")
xz.qx.hz.final <- rbind(xz.qx.hz,xz.qx.hz.colsum.df)
write.csv(xz.qx.hz.final,paste0("区县每日新增病例分类",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","区县每日新增病例分类",Sys.Date(),".csv"))

