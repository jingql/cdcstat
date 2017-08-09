##### �˳�����������ͳ��ÿ����ý���� #####
##### ����¡��2014��9��28��׫д�������м�������  #####
## 2014��10��10��,�ƽ���������ͳ����Ա��9��20��Ϊͳ����ʼ����
##2014��10��9�գ��޶������ܶ�ƽ��ֵ����λ���ʹ���ʼ��㷽�����ߵ�100m��Χ��400m��Χ��������������ָ�ꡣ
setwd("D:/")
library(plyr)
mdf.01 <- read.csv(file=file.choose(),header=T,as.is=T)
mdf <- mdf.01[as.Date(mdf.01$A10)>=as.Date("2014-09-20"),]
qxwm.mean <- ddply(mdf,.(A2,as.Date(A10)),summarize
              ,����ͼָ��ƽ����=mean(as.numeric(A29),na.rm=T)
              ,��׼��ָ��ƽ����=mean(as.numeric(A35),na.rm=T)
              ,�����ܶ�ƽ����=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qxwm.median <- ddply(mdf,.(A2,as.Date(A10)),summarize
              ,����ͼָ����λ��=median(as.numeric(A29),na.rm=T)
              ,��׼��ָ����λ��=median(as.numeric(A35),na.rm=T)
              ,�����ܶ���λ��=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qswm.mean <- ddply(mdf,.(as.Date(A10)),summarize
              ,����ͼָ��ƽ����=mean(as.numeric(A29),na.rm=T)
              ,��׼��ָ��ƽ����=mean(as.numeric(A35),na.rm=T)
              ,�����ܶ�ƽ����=mean(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
qswm.median <- ddply(mdf,.(as.Date(A10)),summarize
              ,����ͼָ����λ��=median(as.numeric(A29),na.rm=T)
              ,��׼��ָ����λ��=median(as.numeric(A35),na.rm=T)
              ,�����ܶ���λ��=median(c(as.numeric(A36),as.numeric(A37)),na.rm=T))
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
                  ,���������=sum(jcd)
                  ,��������=���������-sum(bdbs,na.rm=T)
                  ,����ͼָ��������=sum(BIjcd)
                  ,����ͼָ�������=����ͼָ��������-sum(BIbdbs,na.rm=T)
                  ,��׼��ָ��������=sum(SSIjcd)
                  ,��׼��ָ�������=��׼��ָ��������-sum(SSIbdbs,na.rm=T)
                  ,�����ܶȼ�����=sum(ADIjcd)
                  ,�����ܶȴ����=�����ܶȼ�����-sum(ADIbdbs,na.rm=T)
                  ,��������=round(��������/���������*100,2)
                  ,����ͼָ�������=round(����ͼָ�������/����ͼָ��������*100,2)
                  ,��׼��ָ�������=round(��׼��ָ�������/��׼��ָ��������*100,2)
                  ,�����ܶȴ����=round(�����ܶȴ����/�����ܶȼ�����*100,2))
qxwm.hgl <- ddply(mdf,.(A2,as.Date(A10)),summarize
                  ,���������=sum(jcd)
                  ,��������=���������-sum(bdbs,na.rm=T)
                  ,����ͼָ��������=sum(BIjcd)
                  ,����ͼָ�������=����ͼָ��������-sum(BIbdbs,na.rm=T)
                  ,��׼��ָ��������=sum(SSIjcd)
                  ,��׼��ָ�������=��׼��ָ��������-sum(SSIbdbs,na.rm=T)
                  ,�����ܶȼ�����=sum(ADIjcd)
                  ,�����ܶȴ����=�����ܶȼ�����-sum(ADIbdbs,na.rm=T)
                  ,��������=round(��������/���������*100,2)
                  ,����ͼָ�������=round(����ͼָ�������/����ͼָ��������*100,2)
                  ,��׼��ָ�������=round(��׼��ָ�������/��׼��ָ��������*100,2)
                  ,�����ܶȴ����=round(�����ܶȴ����/�����ܶȼ�����*100,2))           
write.csv(qxwm.mean,paste0("ƽ��������ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("ƽ��������ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"))
write.csv(qxwm.median,paste0("��λ������ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("��λ������ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"))
write.csv(qswm.mean,paste0("ƽ����ȫ��ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("ƽ����ȫ��ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"))
write.csv(qswm.median,paste0("��λ��ȫ��ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("��λ��ȫ��ÿ�ղ���ͼָ������׼��ָ���������ܶ�����ͼ",Sys.Date(),".csv"))
write.csv(qswm.hgl,paste0("�����ȫ��ÿ������ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("�����ȫ��ÿ������ͼ",Sys.Date(),".csv"))
write.csv(qxwm.hgl,paste0("���ش����ÿ������ͼ",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("���ش����ÿ������ͼ",Sys.Date(),".csv"))
