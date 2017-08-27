
### �Ǹ����ߵ�״̬��������߶�ͳ�ƣ�ͳ�ƺ���
### ����¡��2014��8��26��׫д�������м�������  ##### 
### 2017��8��25���޶���case1[,7]--case1$�������ڣ����Ӵ�15�����15�첡������������
### �˳������ڵǸ����ߵ㣨�Խֵ�Ϊ�ռ�ͳ�Ƶ�Ԫ��״̬ͳ�ƣ��������ߵ�״̬�ź�̽��
### ��������ʼ�����ڡ��������ʱ�䡢���鲨�Σ����������ڼ��㣩������״̬�����������������������ڼ��㣩������ǿ�ȣ�������ɢ�������������ڼ��㣩���������ƣ��������½�����ƽ�����������ڼ��㣩
### ��������״̬������ǿ�ȡ�����������15�죨�Ǹ����Ǳ���ڣ�Ϊʱ��ۼ���̽������
### "������"����Ϊ�����Ƿ����ﵽ���������׼�����������ڼ��㣬15���ڷ�������3����

### �÷���ydzt <- df_ydzt(mdf=mdf,currentdate=Sys.Date())

library(plyr)
df_ydzt <- function(mdf=mdf,currentdate=Sys.Date()){ #�ߵ�״̬ͳ�ƺ���
    mdf.cf <- mdf[duplicated(mdf$����),]
    ##### ������������ ##########
    mdf.cf$����  ### ������������
    mdf$���� <- substr(mdf$��ס��ϸ��ַ,1,3)
    mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,4,6)
    mdf$cases <- 1
    ydfz <- function(x){ifelse(x>25,"����","����")}    
    ydzt <- ddply(mdf,c("����","�ֵ�"),summarize,cumcases=length(��������)
          ,firstdate=min(as.Date(��������)),lastdate=max(as.Date(��������)),flinterval=lastdate-firstdate
	    ,firstreportdate=min(as.Date(���籨��ʱ��)),lastreportdate=max(as.Date(���籨��ʱ��)),flreportinterval=lastreportdate-firstreportdate)
    ydzt$slinterval=currentdate-ydzt$lastdate
    ydzt$ssinterval=currentdate-ydzt$firstdate
    ydzt$ydfz <- ydfz(ydzt$slinterval)
    ydzt.st <- ydzt[order(ydzt$����,-ydzt$cumcases),]
    count=ydzt.st[,2]
    ydzt.st$outbreak=ydzt.st$frequency=ydzt.st$trend=rep(0,length(count))
    bfd <- function(jzcases=jzcases){ # �Ƿ��������Ǳ������жϺ���
                    jzcasesaggr <- aggregate(���~��������,data=jzcases,length)
                    jzcasesaggr <- jzcasesaggr[order(as.Date(jzcasesaggr$��������)),]
			  if (nrow(jzcasesaggr)>1){
                        ybfd <- "" 
                        for (i in 1:(nrow(jzcasesaggr)-1)){
                             if (ybfd=="��") break
                             for (j in (i+1):nrow(jzcasesaggr)){
                             intervalcases <- subset(jzcasesaggr,(as.Date(��������)>=as.Date(jzcasesaggr[i,1])) & (as.Date(��������)<=as.Date(jzcasesaggr[j,1])))
                             if ((nrow(intervalcases)>=3) & ((as.Date(jzcasesaggr[j,1])-as.Date(jzcasesaggr[i,1]))<=15)){
                                 ybfd <- "��"
                                 break
                             } else {
                                  ybfd <- "��"
                             }}
                         }
                      }
                      if(nrow(jzcasesaggr)<=1 & jzcasesaggr[1,2] < 3) {ybfd <- "��"}
                      if(nrow(jzcasesaggr)<=1 & jzcasesaggr[1,2] >= 3 ) {ybfd <- "��"}
                      return(ybfd) 
             }

    for(i in 1:length(count)){
        case1=subset(mdf,�ֵ�==count[i])
        case2=subset(case1,as.Date(case1$���籨��ʱ��)>=(currentdate-15)) #�������������籨��ʱ��任
        ydzt.st$outbreak[i]=ifelse(nrow(case2)>=3,"����","ɢ��")
        time1=case1$��������
        time2=sort(unique(as.Date(time1)))
        time3=rep(0,length(time2)-1)
        if (length(time3)>0) {for(j in 1:(length(time2)-1)){time3[j]=difftime(time2[j+1],time2[j])}}
        if ((length(time3)>0)==FALSE) {time3=1}
        ydzt.st$frequency[i]=length(time3[time3>=25])+1
        case3=subset(case1,as.Date(case1$���籨��ʱ��)>=(currentdate-30)&as.Date(case1$���籨��ʱ��)<(currentdate-15)) # �������������籨��ʱ��任
        ydzt.st$trend[i]=ifelse(nrow(case2)>nrow(case3),"����",ifelse(nrow(case2)<nrow(case3),"�½�","��ƽ"))
        ydzt.st$curr15days[i] = nrow(case2)
        ydzt.st$past15days[i] = nrow(case3)
        ydzt.st$bfd[i] = bfd(case1)
    }
    ydzt.st <- ydzt.st[,c("����","�ֵ�","cumcases","firstdate","lastdate","bfd","flinterval","slinterval","ssinterval",
                          "frequency","ydfz","firstreportdate","lastreportdate","flreportinterval","outbreak","trend","curr15days","past15days")]
    ydzt.st$outbreak[ydzt.st$ydfz=="����"]<-"--"
    ydzt.st$trend[ydzt.st$ydfz=="����"]<-"--"
    ydzt.st$casechange <- ydzt.st$curr15days-ydzt.st$past15days
    names(ydzt.st)<- c("����","�ֵ�","�ۼƲ�����","�׷�����","ĩ������","������","ĩ�׼��","��ĩ���","���׼��","���鲨��"
                       ,"�ߵ�״̬","�ױ�����","ĩ������","ĩ�ױ����","����ǿ��","��������","��15�첡����","��15�첡����","����������")
    ydzt.st$�׷����� <- format(ydzt.st$�׷�����,"%m-%d")
    ydzt.st$ĩ������ <- format(ydzt.st$ĩ������,"%m-%d")
    ydzt.st$��ĩ��� <- as.numeric(ydzt.st$��ĩ���)
    ydzt.st$ĩ�׼�� <- as.numeric(ydzt.st$ĩ�׼��)
    ydzt.st$���׼�� <- as.numeric(ydzt.st$���׼��) 
    ydzt.st$�ױ����� <- format(ydzt.st$�ױ�����,"%m-%d")
    ydzt.st$ĩ������ <- format(ydzt.st$ĩ������,"%m-%d")
    ydzt.st$ĩ�ױ���� <- as.numeric(ydzt.st$ĩ�ױ����) 
    write.csv(ydzt.st,paste0("D:/","�Ǹ����ߵ�״̬",currentdate,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","�Ǹ����ߵ�״̬",currentdate,".csv"))
    return(ydzt.st)
}

