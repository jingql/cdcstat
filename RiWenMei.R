##### 此程序用于用于统计每日蚊媒数据 #####
##### 景钦隆，2014年9月28日撰写，广州市疾控中心  #####
## 2014年10月10日,移交科室其他统计人员，9月20日为统计起始日期
##2014年10月9日，修订成蚊密度平均值、中位数和达标率计算方法，疫点100m范围和400m范围合起来计算三个指标。
setwd("D:/")
library(plyr)
mdf.01 <- read.csv(file=file.choose(),header=T,as.is=T)
mdf <- mdf.01[as.Date(mdf.01$A10)>=as.Date("2014-09-20"),]
qxwm.mean <- ddply(mdf,.(A2,as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qxwm.median <- ddply(mdf,.(A2,as.Date(A10)),summarize
              ,布雷图指数中位数=median(as.numeric(A29),na.rm=T)
              ,标准间指数中位数=median(as.numeric(A35),na.rm=T)
              ,成蚊密度中位数=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qswm.mean <- ddply(mdf,.(as.Date(A10)),summarize
              ,布雷图指数平均数=mean(as.numeric(A29),na.rm=T)
              ,标准间指数平均数=mean(as.numeric(A35),na.rm=T)
              ,成蚊密度平均数=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qswm.median <- ddply(mdf,.(as.Date(A10)),summarize
              ,布雷图指数中位数=median(as.numeric(A29),na.rm=T)
              ,标准间指数中位数=median(as.numeric(A35),na.rm=T)
              ,成蚊密度中位数=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
jcd <- function(x1,x2,x3,x4){ifelse((!is.na(as.numeric(x1))|!is.na(as.numeric(x2))|!is.na(as.numeric(x3))|!is.na(as.numeric(x4))),1,0)}
bdbs <- function(x1,x2,x3,x4){ifelse((as.numeric(x1)>=5 | as.numeric(x2)>=1 | as.numeric(x3)>=2 | as.numeric(x4)>=2),1,0)}
mdf$jcd <- jcd(mdf$A29,mdf$A35,mdf$A36,mdf$A37)
mdf$bdbs <- bdbs(mdf$A29,mdf$A35,mdf$A36,mdf$A37)
jcd.BISSI <- function(x){ifelse(!is.na(as.numeric(x)),1,0)}
jcd.cw <- function(x1,x2){ifelse((!is.na(as.numeric(x1)))|(!is.na(as.numeric(x2))),1,0)}
bdbs.BI <- function(x){ifelse((as.numeric(x)>=5),1,0)}
bdbs.SSI <- function(x){ifelse((as.numeric(x)>=1),1,0)}
bdbs.ADI <- function(x1,x2){ifelse((as.numeric(x1)>=2 | as.numeric(x2)>=2),1,0)}
mdf$BIjcd <- jcd.BISSI(mdf$A29)
mdf$BIbdbs <- bdbs.BI(mdf$A29)
mdf$SSIjcd <- jcd.BISSI(mdf$A35)
mdf$SSIbdbs <- bdbs.SSI(mdf$A35)
mdf$ADIjcd <- jcd.cw(mdf$A36,mdf$A37)
mdf$ADIbdbs <- bdbs.ADI(mdf$A36,mdf$A37)
qswm.hgl <- ddply(mdf,.(as.Date(A10)),summarize
                  ,总体监测点数=sum(jcd)
                  ,总体达标数=总体监测点数-sum(bdbs,na.rm=T)
                  ,布雷图指数监测点数=sum(BIjcd)
                  ,布雷图指数达标数=布雷图指数监测点数-sum(BIbdbs,na.rm=T)
                  ,标准间指数监测点数=sum(SSIjcd)
                  ,标准间指数达标数=标准间指数监测点数-sum(SSIbdbs,na.rm=T)
                  ,成蚊密度监测点数=sum(ADIjcd)
                  ,成蚊密度达标数=成蚊密度监测点数-sum(ADIbdbs,na.rm=T)
                  ,总体达标率=round(总体达标数/总体监测点数*100,2)
                  ,布雷图指数达标率=round(布雷图指数达标数/布雷图指数监测点数*100,2)
                  ,标准间指数达标率=round(标准间指数达标数/标准间指数监测点数*100,2)
                  ,成蚊密度达标率=round(成蚊密度达标数/成蚊密度监测点数*100,2))
qxwm.hgl <- ddply(mdf,.(A2,as.Date(A10)),summarize
                  ,总体监测点数=sum(jcd)
                  ,总体达标数=总体监测点数-sum(bdbs,na.rm=T)
                  ,布雷图指数监测点数=sum(BIjcd)
                  ,布雷图指数达标数=布雷图指数监测点数-sum(BIbdbs,na.rm=T)
                  ,标准间指数监测点数=sum(SSIjcd)
                  ,标准间指数达标数=标准间指数监测点数-sum(SSIbdbs,na.rm=T)
                  ,成蚊密度监测点数=sum(ADIjcd)
                  ,成蚊密度达标数=成蚊密度监测点数-sum(ADIbdbs,na.rm=T)
                  ,总体达标率=round(总体达标数/总体监测点数*100,2)
                  ,布雷图指数达标率=round(布雷图指数达标数/布雷图指数监测点数*100,2)
                  ,标准间指数达标率=round(标准间指数达标数/标准间指数监测点数*100,2)
                  ,成蚊密度达标率=round(成蚊密度达标数/成蚊密度监测点数*100,2))           
write.csv(qxwm.mean,paste0("平均数区县每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("平均数区县每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"))
write.csv(qxwm.median,paste0("中位数区县每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("中位数区县每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"))
write.csv(qswm.mean,paste0("平均数全市每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("平均数全市每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"))
write.csv(qswm.median,paste0("中位数全市每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("中位数全市每日布雷图指数、标准间指数、成蚊密度走势图",Sys.Date(),".csv"))
write.csv(qswm.hgl,paste0("达标率全市每日走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("达标率全市每日走势图",Sys.Date(),".csv"))
write.csv(qxwm.hgl,paste0("区县达标率每日走势图",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("区县达标率每日走势图",Sys.Date(),".csv"))

