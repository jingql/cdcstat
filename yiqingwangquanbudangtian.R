##### ���������صǸ�������ͳ�ƣ�report.csv,���������ȫ��������ɾ����###########3
##### ����¡��2014��7��27�ձ�д,�����м������� #########
library(reshape)
setwd("D:/")
#  ��������������
mdf <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 

#  ��������ͳ�����벡��һ�������ݣ�ÿ�յǸ����ձ����벡��һ��������
mdf.df.sr <- mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE)


###  ��������ͳ�Ʊ��ظ�Ⱦһ��������
mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 

###### ���� ʡ���С������ֵ������ֶ� #######
mdf$ʡ <- substr(mdf$��ס��ϸ��ַ,1,3)
mdf$���� <- substr(mdf$��ס��ϸ��ַ,4,6)
mdf$���� <- substr(mdf$��ס��ϸ��ַ,7,9)
mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,10,12)
mdf$cases <- 1
today <- as.Date(Sys.Date(),"%Y-%m-%d")

#### ��ɾ����ͳ�� ##########
ysck <- mdf[mdf$���״̬=="��ɾ����",]
today.ysck <- subset(ysck,as.Date(ɾ��ʱ��)==today)
### ����ɾ���������� ###########
if (nrow(today.ysck)!=0) today.ysck$�������� else "ǰһ����ɾ������" 

#### ������ͳ��  ##########
yzsk <- mdf[mdf$���״̬=="������",]
##### ������������������ ##########
yzsk.cf <- yzsk[duplicated(yzsk$����),]
### ������������������ ######
if (nrow(yzsk.cf)!=0) yzsk.cf$���� else "����������������" 

#### �����󿨵��ն�������ͳ�� ######
dingzheng.yzsk <- subset(yzsk,as.Date(��������ʱ��)==today)
#######  ���ն�����������  ######
if (nrow(dingzheng.yzsk)!=0) dingzheng.yzsk$�������� else "ǰһ���޶�������" 

######### ͳ�������󿨵����������没���� ##############
xz.yzsk <- yzsk[as.Date(yzsk$���濨¼��ʱ��)==today,]
if (nrow(xz.yzsk )!=0) aggregate(cases~����+����+�ֵ�,length,data=xz.yzsk) else "ǰһ�����������没��"
#####  ������ǰһ���������没������ ############
if (nrow(xz.yzsk )!=0) 
{
    xz.yzsk$��������
    xz.md <- xz.yzsk[c("��������","�Ա�","����","��ס��ϸ��ַ","ְҵ","��������","���濨¼��ʱ��","��������","���浥λ","��ע")]
    write.csv(xz.md,paste0("�����������没��һ����",Sys.Date(),".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","�����������没��һ����",Sys.Date(),".csv"))
} else {"�������������没��"}

# �����������ų����벡�����õ����������ز���һ��������,��ͳ���ձ����ݣ�����ͳ�ƽ���ǰһ�գ�
##����ͳ�Ʋ�������ر����ز���
### yqw--������,bd--����,rb--�ձ�
mdf.bd.yqw <- subset(yzsk,!(yzsk$�������� %in% mdf.df.sr$����))
hz <- aggregate(cases~����+�ֵ�,length,data=mdf.bd.yqw)
mdf.rq <- mdf.bd.yqw[c("����","�ֵ�","�Ա�","ְҵ","��������","���濨¼��ʱ��","��������","���浥λ","cases")]
mdf.rh <- melt(mdf.rq,id=c("����","�ֵ�","�Ա�","ְҵ","��������","���濨¼��ʱ��","��������","���浥λ"))
hz.r <- cast(mdf.rh,����+�ֵ�~ ��������,length)
hz.r$�ۼƲ��� <- hz.r$�ٴ���ϲ���+hz.r$ʵ����ȷ�ﲡ��+hz.r$���Ʋ���
hz.r.sort <- hz.r[order(hz.r$����,-hz.r$�ۼƲ���),]
hz.r.sort$������������ <- 0
today <- as.Date(Sys.Date(),"%Y-%m-%d")
if (today %in% as.Date(mdf.bd.yqw$���濨¼��ʱ��))
{
xz.mdf <- mdf.bd.yqw[as.Date(mdf.bd.yqw$���濨¼��ʱ��)==today,]
xz.hz <- aggregate(cases~����+�ֵ�,length,data=xz.mdf)
for (i in 1:nrow(xz.hz))
   { for (j in 1:nrow(hz.r.sort))
        {
         if (hz.r.sort$�ֵ�[j]==xz.hz$�ֵ�[i]) hz.r.sort$������������[j]=xz.hz$cases[i]
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
write.csv(final.final,paste0("�Ǹ������ձ���",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","�Ǹ������ձ���",Sys.Date(),".csv"))


### ������������ͳ�Ƶı��ز���һ����
rb.mdf.bd.yqw <- mdf.bd.yqw[,c("��������","�Ա�","����","��ס��ϸ��ַ","ְҵ","��������","���濨¼��ʱ��","��������","���浥λ")]
rb.mdf.bd.yqw$��ס��ϸ��ַ <- substr(rb.mdf.bd.yqw$��ס��ϸ��ַ,7,40)
rb.mdf.bd.yqw$���濨¼��ʱ�� <- as.Date(rb.mdf.bd.yqw$���濨¼��ʱ��)
rb.mdf.bd.yqw$���濨¼��ʱ�� <- format(rb.mdf.bd.yqw$���濨¼��ʱ��,"%m-%d")
rb.mdf.bd.yqw$�������� <- as.Date(rb.mdf.bd.yqw$��������)
rb.mdf.bd.yqw$�������� <- format(rb.mdf.bd.yqw$��������,"%m-%d")
rb.mdf.bd.yqw$��� <- 1:nrow(rb.mdf.bd.yqw)
write.csv(rb.mdf.bd.yqw,paste0("�Ǹ��ȱ��ز���һ����",Sys.Date(),".csv"),row.names=F)
shell.exec(paste0("D:/","�Ǹ��ȱ��ز���һ����",Sys.Date(),".csv"))


## �ձ��������ݱ�д
###   ��������
xz.hz.sort <- xz.hz[order(xz.hz$����,-xz.hz$cases),]
xz.hz.qx <- aggregate(cases~����,sum,data=xz.hz.sort)
xz.hz.qx <- xz.hz.qx[order(-xz.hz.qx$cases),]
aa4 <- NULL
for (i in 1:nrow(xz.hz.qx))
{  
 aa <- subset(xz.hz.sort,xz.hz.sort$����== xz.hz.qx$����[i])
 aa1 <- paste0(aa$�ֵ�,aa$cases,seq="����")
 aa2 <- paste0(xz.hz.qx$����[i],xz.hz.qx$cases[i],"��",",")
 aa3 <- c(aa2,"����",aa1,";")
 aa4 <- c(aa4,aa3)
}
aa5 <- paste0(Sys.Date(),"��������������Ǹ���",colsum.df$������������,"����","����")
###   �ۼƱ���
qx.xz.hz.sort <- qx.xz.hz[order(-qx.xz.hz$�ۼƲ���),]
aa6 <- paste0("����",Sys.Date(),"24ʱ",",���й����汾�ظ�Ⱦ�Ǹ���",colsum.df$�ۼƲ���,"����")
aa7 <- paste0(qx.xz.hz.sort$����,qx.xz.hz.sort$�ۼƲ���,"��",seq="��")

### ���ո�ʽ������
cat(aa5,aa4)
cat(aa6,"�ֱ�Ϊ",aa7)
paste(cat(aa5,aa4),cat(aa6,"�ֱ�Ϊ",aa7))




##### Ѱ��������report�����󿨺�����ͳ�Ʊ��в��������б䶯���������������� #########
ggb1.yzsk <- NULL
for (i in 1:nrow(mdf.df))
{
   for (j in 1:nrow(yzsk))
     {
       if (mdf.df$����[i] == yzsk$��������[j])
        {
         if (mdf.df$��������[i] != yzsk$��������[j]) 
            {ggb1.yzsk <- rbind(ggb1.yzsk,c(mdf.df$����[i],mdf.df$��������[i],yzsk$��������[j],yzsk$��������[j]))}
        }
     }
}
cnames <- c("��ͳ�Ʊ�����","��ͳ�Ʊ���������","����������","��������������")
ggb1.yzsk <- as.data.frame(ggb1.yzsk)
names(ggb1.yzsk)<-cnames
ggb1.yzsk



##### Ѱ��������reportȫ��������ͳ�Ʊ��в��������б䶯���������������� #########
ggbl <- NULL
for (i in 1:nrow(mdf.df))
{
   for (j in 1:nrow(mdf))
     {
       if (mdf.df$����[i] == mdf$��������[j])
        {
         if (mdf.df$��������[i] != mdf$��������[j]) 
            {ggbl <- rbind(ggbl,c(mdf.df$����[i],mdf.df$��������[i],mdf$��������[j],mdf$��������[j]))}
        }
     }
}
cnames <- c("��ͳ�Ʊ�����","��ͳ�Ʊ���������","����������","��������������")
ggb1 <- as.data.frame(ggbl)
names(ggb1)<-cnames
ggb1
