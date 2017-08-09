##### �˳������ڽ���ǰһ�ձ��ظ�Ⱦ�Ǹ��Ȳ������ #####
##### ����¡��2014��7��26��׫д�������м�������  #####
### 2014��9��10�գ��޶�todayΪ��Ϊ������֣�forѭ���и�Ϊ���غͽֵ�һ�º�����ӵ���������###
### 2014��10��4�գ�����ÿ������������������ͳ��
library(reshape)
setwd("D:/")
if (file.exists(paste0("D:/","�Ǹ����ձ���",Sys.Date(),".csv")))
{file.remove(paste0("D:/","�Ǹ����ձ���",Sys.Date(),".csv"))}
mdf <- read.csv(file=file.choose(),header=TRUE,as.is=TRUE)
mdf$���� <- substr(mdf$��ס��ϸ��ַ,1,3)
mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,4,6)
mdf$cases <- 1
hz <- aggregate(cases~����+�ֵ�,length,data=mdf)
mdf.rq <- mdf[c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ","cases")]
mdf.rh <- melt(mdf.rq,id=c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ"))
hz.r <- cast(mdf.rh,����+�ֵ�~��������,length)
hz.r$�ۼƲ��� <- hz.r$�ٴ���ϲ���+hz.r$ʵ����ȷ�ﲡ��+hz.r$���Ʋ���
hz.r.sort <- hz.r[order(hz.r$����,-hz.r$�ۼƲ���),]
hz.r.sort$������������ <- 0
today <- as.Date(as.character(as.Date(Sys.Date(),"%Y-%m-%d")))
if ((today-1) %in% as.Date(mdf$���籨��ʱ��))
{
xz.mdf <- mdf[as.Date(mdf$���籨��ʱ��)==(today-1),]
xz.hz <- aggregate(cases~����+�ֵ�,length,data=xz.mdf)
for (i in 1:nrow(xz.hz))
   { for (j in 1:nrow(hz.r.sort))
        {
         if ((hz.r.sort$����[j]==xz.hz$����[i])&(hz.r.sort$�ֵ�[j]==xz.hz$�ֵ�[i])) hz.r.sort$������������[j]=xz.hz$cases[i]
         }
    }
}
qx.xz.hz <- aggregate(cbind(�ۼƲ���,������������,�ٴ���ϲ���,ʵ����ȷ�ﲡ��,���Ʋ���)~����,sum,data=hz.r.sort)
qx.xz.hz$�ֵ� <- " "
final <- rbind(hz.r.sort,qx.xz.hz)
final.sort <- final[order(final$����,-final$�ۼƲ���),c("����","�ֵ�","�ۼƲ���","������������","�ٴ���ϲ���","ʵ����ȷ�ﲡ��","���Ʋ���")]
colsum <- colSums(qx.xz.hz[,2:6])
colsum.df <- as.data.frame(t(colsum))
colsum.df$�ֵ� <- " "
colsum.df$���� <- "�ϼ�"
final.final <- rbind(final.sort,colsum.df)
write.csv(final.final,paste0("�Ǹ����ձ���",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","�Ǹ����ձ���",Sys.Date(),".csv"))

#ÿ������������������ͳ��
xz.mdf <- mdf[as.Date(mdf$���籨��ʱ��)==(today-1),]
xz.mdf.rq <- xz.mdf[c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ","cases")]
xz.mdf.melt <- melt(xz.mdf.rq,id=c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ"))
xz.hz.r <- cast(xz.mdf.melt,����~��������,length)
xz.hz.r$ȷ�� <- xz.hz.r$�ٴ���ϲ���+xz.hz.r$ʵ����ȷ�ﲡ��
names(xz.hz.r)[4]<- "����"
xz.qx.hz <- xz.hz.r[,c("����","ȷ��","����")]
xz.qx.hz.colsum <- colSums(xz.qx.hz[2:3])
xz.qx.hz.colsum.df <- as.data.frame(t(xz.qx.hz.colsum))
xz.qx.hz.colsum.df$���� <- "�ϼ�"
names(xz.qx.hz.colsum.df)[1:3] <- c("ȷ��","����","����")
xz.qx.hz.final <- rbind(xz.qx.hz,xz.qx.hz.colsum.df)
write.csv(xz.qx.hz.final,paste0("����ÿ��������������",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","����ÿ��������������",Sys.Date(),".csv"))
