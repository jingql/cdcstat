##### ���������صǸ�������ͳ�ƣ�report.csv,���������ȫ��������ɾ����###########3
##### ����¡��2014��7��27�ձ�д,�����м������� #########

library(reshape)

setwd("D:/")
#  �������������ݣ�report.csv,���������ȫ��������ɾ����
mdf <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 

#  ��������ͳ�Ʊ��ظ�Ⱦһ�������ݣ�ÿ�յǸ����ձ����ظ�Ⱦ����һ��������
mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
#  ��������ͳ�����벡��һ�������ݣ�ÿ�յǸ����ձ����벡��һ��������
mdf.df.sr <- mdf.df <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE)

## ���� ʡ���С������ֵ������ֶ�
mdf$ʡ <- substr(mdf$��ס��ϸ��ַ,1,3)
mdf$���� <- substr(mdf$��ס��ϸ��ַ,4,6)
mdf$���� <- substr(mdf$��ס��ϸ��ַ,7,9)
mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,10,12)
mdf$cases <- 1
today <- as.Date(Sys.Date(),"%Y-%m-%d")
table(mdf$���״̬)

## ��ɾ����ͳ�� 
ysck <- mdf[mdf$���״̬=="��ɾ����",]
today.ysck <- subset(ysck,as.Date(ɾ��ʱ��)==(today-1))
### ǰһ��ɾ���������� ###########
if (nrow(today.ysck)!=0) today.ysck$�������� else "ǰһ����ɾ������" 
##�ų����챨�浱��ɾ���������ɾ��������
today.ysck.t <- today.ysck[today.ysck$���濨¼��ʱ��<(today-1),]
if (nrow(today.ysck.t)!=0) today.ysck.t[,c("��������","����","�ֵ�","ɾ��ԭ��","��ע")] else "ǰһ����ɾ������" 

## δ��˿�ͳ�� 
wshk <- mdf[mdf$���״̬=="δ��˿�",]
wshk$��������
today.wshk <- subset(wshk,as.Date(wshk$���濨¼��ʱ��)==(today-1))
### ǰһ��δ��˲������� ###########
if (nrow(today.wshk)!=0) today.wshk$�������� else "ǰһ����δ��˿�����" 

## ������ͳ�� 
yzsk <- mdf[mdf$���״̬=="������",]
### ������������������ 
yzsk.cf <- yzsk[duplicated(yzsk$��������),]
### ������������������ ######
if (nrow(yzsk.cf)!=0) yzsk.cf$�������� else "����������������" 

### ������ǰһ�ն�������ͳ��
dingzheng.yzsk <- subset(yzsk,as.Date(yzsk$��������ʱ��)==(today-1))
####  ǰһ�ն�����������
if (nrow(dingzheng.yzsk)!=0) dingzheng.yzsk$�������� else "ǰһ���޶�������" 

##### ͳ��������ǰһ���������没�������������غ����벡��
xz.yzsk <- yzsk[as.Date(yzsk$���濨¼��ʱ��)==(today-1),]
if (nrow(xz.yzsk )!=0) aggregate(cases~����+����+�ֵ�,length,data=xz.yzsk) else "ǰһ�����������没��"
#####  ������ǰһ���������没������
if (nrow(xz.yzsk )!=0) 
{
    xz.yzsk$��������
    xz.md <- xz.yzsk[c("��������","�Ա�","����","��ס��ϸ��ַ","ְҵ","��������","���濨¼��ʱ��","��������","���浥λ","��ע")]
    write.csv(xz.md,paste0("ǰһ���������没��һ����",Sys.Date(),".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","ǰһ���������没��һ����",Sys.Date(),".csv"))
} else {"ǰһ�����������没��"}



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
if ((today-1) %in% as.Date(mdf.bd.yqw$���濨¼��ʱ��))
{
xz.mdf <- mdf.bd.yqw[as.Date(mdf.bd.yqw$���濨¼��ʱ��)==(today-1),]
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
write.csv(final.final,paste0("�Ǹ����ձ���",Sys.Date(),".csv"),row.names = FALSE)
shell.exec(paste0("D:/","�Ǹ����ձ���",Sys.Date(),".csv"))


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
aa5 <- paste0(Sys.Date()-1,"��������������Ǹ���",colsum.df$������������,"����","����")
###   �ۼƱ���
qx.xz.hz.sort <- qx.xz.hz[order(-qx.xz.hz$�ۼƲ���),]
aa6 <- paste0("����",Sys.Date()-1,"24ʱ",",���й����汾�ظ�Ⱦ�Ǹ���",colsum.df$�ۼƲ���,"����")
aa7 <- paste0(qx.xz.hz.sort$����,qx.xz.hz.sort$�ۼƲ���,"��",seq="��")

### ���ո�ʽ������
cat(aa5,aa4)
cat(aa6,"�ֱ�Ϊ",aa7)
paste(cat(aa5,aa4),cat(aa6,"�ֱ�Ϊ",aa7))


# ���ز���ͳ��
## ������������ 
mdf.bd.yqw$ot <- as.Date(mdf.bd.yqw$��������)
mdf.cf <- mdf[duplicated(mdf.bd.yqw$����),]
library(scales)
library(ggplot2)
p <- ggplot(data=mdf.bd.yqw,aes(x=ot),xlim=c(as.Date("2014-06-01"),as.Date(Sys.Date())))
p1 <- p + geom_histogram(aes(fill=����),binwidth=1)
p2 <- p1 + scale_x_date(labels=date_format("%m-%d"),breaks=date_breaks("2 days")) 
p3 <- p2 + theme(axis.text.x=element_text(angle=90)) 
p4 <- p3 + xlab("��������")+ylab("��������")
p4

##�����������ط���+�����������
p5 <- p4 + facet_grid(����~.)
p5 <- p4 + facet_grid(����~.)+aes(colour=��������)
p5 <- p4 + facet_grid(����~.)+geom_histogram(aes(fill=��������),binwidth=1)

## ����ʱ��ͱ���ʱ���� 
mdf.bd.yqw$rt.mdf <- as.Date(mdf.bd.yqw$���濨¼��ʱ��)
mdf.bd.yqw$ot.mdf <- as.Date(mdf.bd.yqw$���濨¼��ʱ��)
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
text(x=as.Date("2014-06-10"),y=70,labels=paste("�����:",max(time.interval),"\n","�������:",min(time.interval),"\n","��λ��:",median(time.interval)),pos=1,adj=0.5)




# Ѱ��������report�����󿨺�����ͳ�Ʊ��в��������б䶯���������������� 
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



# Ѱ��������reportȫ��������ͳ�Ʊ��в��������б䶯���������������� 
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



##### Ѱ������������ǰ��report�����󿨱��ز���������report�����󿨱��ز����в��������б䶯���������������� #########
###  �������������ݣ�report.csv,���������ȫ��������ɾ����
### qr.rb.mdf.bd.yqw--ǰһ��ͳ�Ƶı��ز������ݿ⣨rb.mdf.bd.yqwΪ���գ��硰�Ǹ��ȱ��ز���һ����2014-08-08����
qr.rb.mdf.bd.yqw <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
#### ���ձ��ز���һ�����ͽ��ձ��ز���һ�����в�ƥ�䲡��(bppmd.zj--��ƥ������)
bppmd.zj <- subset(qr.rb.mdf.bd.yqw,!(qr.rb.mdf.bd.yqw$�������� %in% rb.mdf.bd.yqw$��������))
bppmd.zj$��������

#### ����ͳ�Ʊ��ز���һ���������ձ��ز���һ�����в�ƥ�䲡��(bppmd.jz--��ƥ������)
rb.mdf.bd.yqw.jy <- subset(rb.mdf.bd.yqw,as.Date(rb.mdf.bd.yqw$���濨¼��ʱ��,"%m-%d")!=(today-1))
bppmd.jz <- subset(rb.mdf.bd.yqw.jy,!(rb.mdf.bd.yqw.jy$�������� %in% qr.rb.mdf.bd.yqw$��������))
bppmd.jz$��������

### ����ͳ����ɾ�����Ƿ������ձ��ز���������
ysck.bd.sf <- subset(today.ysck,today.ysck$�������� %in% qr.rb.mdf.bd.yqw$��������) 
ysck.bd.sf$����

### ������ͬ���ֵ��и��Ĳ������� ### �����������
qr.rb.mdf.bd.yqw$�ֵ� <- substr(qr.rb.mdf.bd.yqw$��ס��ϸ��ַ,4,6)
rb.mdf.bd.yqw$�ֵ� <- substr(rb.mdf.bd.yqw$��ס��ϸ��ַ,4,6)
### ���ձ�����ַ�仯
bh.jr <- subset(rb.mdf.bd.yqw,(rb.mdf.bd.yqw$�������� %in% qr.rb.mdf.bd.yqw$��������)&!(rb.mdf.bd.yqw$�ֵ� %in% qr.rb.mdf.bd.yqw$�ֵ�))
### ���ձ�����ַ�仯
bh.zr <- subset(qr.rb.mdf.bd.yqw,(qr.rb.mdf.bd.yqw$�������� %in% rb.mdf.bd.yqw$��������)&!(qr.rb.mdf.bd.yqw$�ֵ� %in% rb.mdf.bd.yqw$�ֵ�))


# Ѱ�ҽ��ձ���rb.mdf.bd.yqw�����ձ���qr.rb.mdf.bd.yqw�в��������б䶯����������������(bt.bd-���ز����䶯����) 
bt.bd <- NULL
for (i in 1:nrow(rb.mdf.bd.yqw))
{
   for (j in 1:nrow(qr.rb.mdf.bd.yqw))
     {
       if (rb.mdf.bd.yqw$��������[i] == qr.rb.mdf.bd.yqw$��������[j])
        {
         if (rb.mdf.bd.yqw$��������[i] != qr.rb.mdf.bd.yqw$��������[j]) 
            {bt.bd <- rbind(bt.bd,c(rb.mdf.bd.yqw$��������[i],rb.mdf.bd.yqw$��������[i],qr.rb.mdf.bd.yqw$��������[j],qr.rb.mdf.bd.yqw$��������[j]))}
        }
     }
}
cnames <- c("��������","���ղ�������","ǰ������","ǰ�ղ�������")
bt.bd <- as.data.frame(bt.bd)
names(bt.bd)<-cnames
bt.bd

# Ѱ�ҽ��ձ���rb.mdf.bd.yqw�����ձ���qr.rb.mdf.bd.yqw�в��������б䶯���������ֵ�(bt.bd-���ز����䶯����) 
bt.bd <- NULL
for (i in 1:nrow(rb.mdf.bd.yqw))
{
   for (j in 1:nrow(qr.rb.mdf.bd.yqw))
     {
       if (rb.mdf.bd.yqw$��������[i] == qr.rb.mdf.bd.yqw$��������[j])
        {
         if (rb.mdf.bd.yqw$�ֵ�[i] != qr.rb.mdf.bd.yqw$�ֵ�[j]) 
            {bt.bd <- rbind(bt.bd,c(rb.mdf.bd.yqw$��������[i],rb.mdf.bd.yqw$�ֵ�[i],qr.rb.mdf.bd.yqw$��������[j],qr.rb.mdf.bd.yqw$�ֵ�[j]))}
        }
     }
}
cnames <- c("��������","���սֵ�","ǰ������","ǰ�սֵ�")
bt.bd <- as.data.frame(bt.bd)
names(bt.bd)<-cnames
bt.bd

## ���������ĵǸ����ձ����ȶ�
qr.final.final <- read.csv(file=file.choose(),header=TRUE,stringsAsFactors=FALSE) 
a <- final.final[final.final$�ֵ�!=" ",] ##����ͳ�Ʊ�
b <- qr.final.final[qr.final.final$�ֵ�!=" ",] ##����ͳ�Ʊ�
a1 <- subset(a,!(a$�ֵ� %in% b$�ֵ�)) ## ���������ֵ�   
a2 <- subset(b,!(b$�ֵ� %in% a$�ֵ�)) ## �����ų��ֵ�
