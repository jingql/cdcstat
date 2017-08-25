
mdf <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)

library(plyr)
library(reshape)

ydzt <- function(mdf=mdf,currentdate=Sys.Date()){ #疫点状态统计函数
    ##### 景钦隆，2014年8月26日撰写，广州市疾控中心  ##### 
    ### 2017年8月25日修订，case1[,7]--case1$发病日期，增加此15天和上15天病例数两个变量
    ### 此程序用于登革热疫点（以街道为空间统计单元）状态统计，可用于疫点状态信号探测
    ### 包括疫情始终日期、疫情持续时间、疫情波次、疫情状态（持续、结束）、疫情强度（暴发、散发）、疫情趋势（上升、下降、持平）
    ### 其中疫情状态、疫情强度、疫情趋势以15天（登革热最长潜伏期）为时间聚集性探测期限
    setwd("D:/")
    mdf.cf <- mdf[duplicated(mdf$姓名),]
    ##### 查找重名病例 ##########
    mdf.cf$姓名  ### 重名病例名单
    mdf$区县 <- substr(mdf$现住详细地址,1,3)
    mdf$街道 <- substr(mdf$现住详细地址,4,6)
    mdf$cases <- 1
    ydfz <- function(x){ifelse(x>25,"结束","持续")}
    ydzt <- ddply(mdf,c("区县","街道"),summarize,cumcases=length(发病日期),firstdate=min(as.Date(发病日期))
          ,lastdate=max(as.Date(发病日期))
          ,flinterval=lastdate-firstdate
          ,slinterval=currentdate-lastdate
          ,ssinterval=currentdate-firstdate
          ,ydfz=ydfz(slinterval))
    ydzt.st <- ydzt[order(ydzt$区县,-ydzt$cumcases),]
    count=ydzt.st[,2]
    ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=rep(0,length(count))
    for(i in 1:length(count)){
        case1=subset(mdf,街道==count[i])
        case2=subset(case1,as.Date(case1$网络报告时间)>=(currentdate-15)) #发病日期与网络报告时间变换
        ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"暴发","散发")
        time1=case1$发病日期
        time2=sort(unique(as.Date(time1)))
        time3=rep(0,length(time2)-1)
        if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
        if ((length(time3)>0)==FALSE) {time3=1}
        ydzt.st$frequency[i]=length(time3[time3>=25])+1
        case3=subset(case1,as.Date(case1$网络报告时间)>=(currentdate-30)&as.Date(case1$网络报告时间)<(currentdate-15)) # 发病日期与网络报告时间变换
        ydzt.st$trend[i]=ifelse(nrow(case2)>nrow(case3),"上升",ifelse(nrow(case2)<nrow(case3),"下降","持平"))
        ydzt.st$curr15days[i] = nrow(case2)
        ydzt.st$past15days[i] = nrow(case3)
    }
    ydzt.st <- ydzt.st[,c("区县","街道","cumcases","firstdate","lastdate","flinterval","slinterval","ssinterval","frequency","ydfz","outbreak","trend","curr15days","past15days")]
    ydzt.st$outbreak[ydzt.st$ydfz=="结束"]<-"--"
    ydzt.st$trend[ydzt.st$ydfz=="结束"]<-"--"
    names(ydzt.st)<- c("区县","街道","累计病例数","首发日期","末例日期","末首间隔","今末间隔","今首间隔","疫情波次","疫点状态","疫情强度","疫情趋势","此15天病例数","上15天病例数")
    ydzt.st$首发日期 <- format(ydzt.st$首发日期,"%m-%d")
    ydzt.st$末例日期 <- format(ydzt.st$末例日期,"%m-%d")
    ydzt.st$今末间隔 <- as.numeric(ydzt.st$今末间隔)
    ydzt.st$末首间隔 <- as.numeric(ydzt.st$末首间隔)
    ydzt.st$今首间隔 <- as.numeric(ydzt.st$今首间隔) 
    write.csv(ydzt.st,paste0("D:/","登革热疫点状态",currentdate,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","登革热疫点状态",currentdate,".csv"))
    return(ydzt.st)
}

blrb <- function(mdf=mdf,today=Sys.Date()){ #病例日报统计函数
    ##### 此程序用于截至前一日本地感染登革热病例情况 #####
    ##### 景钦隆，2014年7月26日撰写，广州市疾控中心  #####
    mdf$区县 <- substr(mdf$现住详细地址,1,3)
    mdf$街道 <- substr(mdf$现住详细地址,4,6)
    mdf$cases <- 1
    hz <- aggregate(cases~区县+街道,length,data=mdf)
    mdf.rq <- mdf[c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位","cases")]
    mdf.rh <- melt(mdf.rq,id=c("区县","街道","性别","职业","发病日期","网络报告时间","病例分类","报告单位"))
    hz.r <- cast(mdf.rh,区县+街道~病例分类,length)
    # hz.r$累计病例 <- hz.r$临床诊断病例+hz.r$实验室确诊病例+hz.r$疑似病例
    hz.r$累计病例 <- hz.r$确诊病例+hz.r$疑似病例
    hz.r.sort <- hz.r[order(hz.r$区县,-hz.r$累计病例),]
    hz.r.sort$当日新增病例 <- 0
    #today <- as.Date(as.character(as.Date(Sys.Date(),"%Y-%m-%d")))
    if ((today-1) %in% as.Date(mdf$网络报告时间)){
      xz.mdf <- mdf[as.Date(mdf$网络报告时间)==(today-1),]
      xz.hz <- aggregate(cases~区县+街道,length,data=xz.mdf)
      for (i in 1:nrow(xz.hz)){
       for (j in 1:nrow(hz.r.sort)){
         if ((hz.r.sort$区县[j]==xz.hz$区县[i])&(hz.r.sort$街道[j]==xz.hz$街道[i])) hz.r.sort$当日新增病例[j]=xz.hz$cases[i]
         }}
    }
    # qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,临床诊断病例,实验室确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    # qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,临床诊断病例,实验室确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(累计病例,当日新增病例,确诊病例,疑似病例)~区县,sum,data=hz.r.sort)
    qx.xz.hz$街道 <- " "
    final <- rbind(hz.r.sort,qx.xz.hz)
    final.sort <- final[order(final$区县,-final$累计病例),c("区县","街道","累计病例","当日新增病例","确诊病例","疑似病例")]
    colsum <- colSums(qx.xz.hz[,2:5])
    colsum.df <- as.data.frame(t(colsum))
    colsum.df$街道 <- " "
    colsum.df$区县 <- "合计"
    final.final <- rbind(final.sort,colsum.df)
    write.csv(final.final,paste0("登革热日报表",today,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","登革热日报表",today,".csv"))
    return(final.final)
}


# 计算疫点状态表
ydzt <- ydzt(mdf=mdf,currentdate=Sys.Date())
ydzt

# 计算每日新增病例
blrb <- blrb(mdf=mdf,today=Sys.Date())
blrb


