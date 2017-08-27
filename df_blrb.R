
##### ����¡��2017��8��25��׫д�������м�������  ##### 
##### �˳������ڵǸ���ÿ�������������ۼ�ͳ��
##### �÷�: blrb <- df_blrb(mdf=mdf,today=Sys.Date())

library(reshape)
df_blrb <- function(mdf=mdf,today=Sys.Date()){ #�����ձ�ͳ�ƺ���
    ##### �˳������ڽ���ǰһ�ձ��ظ�Ⱦ�Ǹ��Ȳ������ #####
    ##### ����¡��2014��7��26��׫д�������м�������  #####
    mdf$���� <- substr(mdf$��ס��ϸ��ַ,1,3)
    mdf$�ֵ� <- substr(mdf$��ס��ϸ��ַ,4,6)
    mdf$cases <- 1
    hz <- aggregate(cases~����+�ֵ�,length,data=mdf)
    mdf.rq <- mdf[c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ","cases")]
    mdf.rh <- melt(mdf.rq,id=c("����","�ֵ�","�Ա�","ְҵ","��������","���籨��ʱ��","��������","���浥λ"))
    hz.r <- cast(mdf.rh,����+�ֵ�~��������,length)
    # hz.r$�ۼƲ��� <- hz.r$�ٴ���ϲ���+hz.r$ʵ����ȷ�ﲡ��+hz.r$���Ʋ���
    hz.r$�ۼƲ��� <- hz.r$ȷ�ﲡ��+hz.r$���Ʋ���
    hz.r.sort <- hz.r[order(hz.r$����,-hz.r$�ۼƲ���),]
    hz.r.sort$������������ <- 0
    #today <- as.Date(as.character(as.Date(Sys.Date(),"%Y-%m-%d")))
    if ((today-1) %in% as.Date(mdf$���籨��ʱ��)){
      xz.mdf <- mdf[as.Date(mdf$���籨��ʱ��)==(today-1),]
      xz.hz <- aggregate(cases~����+�ֵ�,length,data=xz.mdf)
      for (i in 1:nrow(xz.hz)){
       for (j in 1:nrow(hz.r.sort)){
         if ((hz.r.sort$����[j]==xz.hz$����[i])&(hz.r.sort$�ֵ�[j]==xz.hz$�ֵ�[i])) hz.r.sort$������������[j]=xz.hz$cases[i]
         }}
    }
    # qx.xz.hz <- aggregate(cbind(�ۼƲ���,������������,�ٴ���ϲ���,ʵ����ȷ�ﲡ��,���Ʋ���)~����,sum,data=hz.r.sort)
    # qx.xz.hz <- aggregate(cbind(�ۼƲ���,������������,�ٴ���ϲ���,ʵ����ȷ�ﲡ��,���Ʋ���)~����,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(�ۼƲ���,������������,ȷ�ﲡ��,���Ʋ���)~����,sum,data=hz.r.sort)
    qx.xz.hz <- aggregate(cbind(�ۼƲ���,������������,ȷ�ﲡ��,���Ʋ���)~����,sum,data=hz.r.sort)
    qx.xz.hz$�ֵ� <- " "
    final <- rbind(hz.r.sort,qx.xz.hz)
    final.sort <- final[order(final$����,-final$�ۼƲ���),c("����","�ֵ�","�ۼƲ���","������������","ȷ�ﲡ��","���Ʋ���")]
    colsum <- colSums(qx.xz.hz[,2:5])
    colsum.df <- as.data.frame(t(colsum))
    colsum.df$�ֵ� <- " "
    colsum.df$���� <- "�ϼ�"
    final.final <- rbind(final.sort,colsum.df)
    write.csv(final.final,paste0("�Ǹ����ձ���",today,".csv"),row.names = FALSE)
    shell.exec(paste0("D:/","�Ǹ����ձ���",today,".csv"))
    return(final.final)
}

