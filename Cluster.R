setwd("~/Documents/Lab/AMC APC TDJ/Data")

tdjv<-read.csv("tdjv_completers_2016-03-04.csv")
tdja<-read.csv("tdja_completers_2016-03-04.csv")
apc<-read.csv("apc_completers_2016-02-19.csv")
amc<-read.csv("amc_completers_2016-02-19.csv")

clus<-read.csv("cluster.csv")

amc$final_ratio_cube<-amc$final_ratio^3
apc$final_ratio_sqr<-apc$final_ratio^2

d_amc<-merge(clus[,c("STUDYID","threegpcluster_withCC")],amc[,c("studyid","final_ratio","final_ratio_cube")], by.x="STUDYID",by.y="studyid")

d_apc<-merge(clus[,c("STUDYID","threegpcluster_withCC")], apc[,c("studyid","final_ratio","final_ratio_sqr")],by.x="STUDYID",by.y="studyid")

d_tdja<-merge(clus[,c("STUDYID","threegpcluster_withCC")], tdja[,c("studyid","final_ratio")],by.x="STUDYID",by.y="studyid")

d_tdjv<-merge(clus[,c("STUDYID","threegpcluster_withCC")], tdjv[,c("studyid","final_ratio")],by.x="STUDYID",by.y="studyid")

colnames(d_amc)<-c("studyid","cluster","amc_final_ratio","amc_cube")

colnames(d_apc)<-c("studyid","cluster","apc_final_ratio","apc_square")

colnames(d_tdja)<-c("studyid","cluster","tdja_final_ratio")

colnames(d_tdjv)<-c("studyid","cluster","tdjv_final_ratio")

library(car)

d_amc$cluster<-factor(d_amc$cluster)
d_apc$cluster<-factor(d_apc$cluster)
d_tdja$cluster<-factor(d_tdja$cluster)
d_tdjv$cluster<-factor(d_tdjv$cluster)

Anova(lm(amc_final_ratio~cluster, data=d_amc))
Anova(lm(amc_cube~cluster, data=d_amc))
Anova(lm(apc_final_ratio~cluster, data=d_apc))
Anova(lm(apc_square~cluster, data=d_apc))
Anova(lm(tdja_final_ratio~cluster, data=d_tdja))
Anova(lm(tdjv_final_ratio~cluster, data=d_tdjv))

#only tdjv is trending at p=0.06612

pairwise.t.test(d_amc$amc_final_ratio, d_amc$cluster, p.adjust.method="none")
pairwise.t.test(d_amc$amc_final_ratio, d_amc$cluster, p.adjust.method="fdr")
pairwise.t.test(d_amc$amc_cube, d_amc$cluster, p.adjust.method="none")
pairwise.t.test(d_amc$amc_cube, d_amc$cluster, p.adjust.method="fdr")

pairwise.t.test(d_apc$apc_final_ratio, d_apc$cluster, p.adjust.method="none")
pairwise.t.test(d_apc$apc_final_ratio, d_apc$cluster, p.adjust.method="fdr")
pairwise.t.test(d_apc$apc_square, d_apc$cluster, p.adjust.method="none")
pairwise.t.test(d_apc$apc_square, d_apc$cluster, p.adjust.method="fdr")

pairwise.t.test(d_tdja$tdja_final_ratio, d_tdja$cluster, p.adjust.method="none")
pairwise.t.test(d_tdja$tdja_final_ratio, d_tdja$cluster, p.adjust.method="fdr")

pairwise.t.test(d_tdjv$tdjv_final_ratio, d_tdjv$cluster, p.adjust.method="none")
pairwise.t.test(d_tdjv$tdjv_final_ratio, d_tdjv$cluster, p.adjust.method="fdr")

write.csv(d_amc, "cluster_data_amc.csv",row.names=F)
write.csv(d_apc, "cluster_data_apc.csv",row.names=F)
write.csv(d_tdja, "cluster_data_tdja.csv",row.names=F)
write.csv(d_tdjv, "cluster_data_tdjv.csv",row.names=F)