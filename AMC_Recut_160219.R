setwd("~/Documents/Lab/AMC APC TDJ/Data")
amc<-read.csv("amc_data_20150702.csv")

###Split Data into Completers and Non-Completers


###Non-Completers: Develop a description of who's in this group. Are their data just random noise? Or after the first failure are they continuously climbing the "green tree brances"
level0<-amc[which(amc$level==0),]
noncompleters<-level0[which(level0$error<0.75),"studyid"]
nc_data<-lapply(noncompleters, function(d){
	return(amc[which(amc$studyid == d),])
})
fun<-function(d){
	s<-d[[1]]
	for (i in 2:(length(d))){
		s<- rbind(s,d[[i]])
	}
	return(s)
}
nc_data<-fun(nc_data)

###now that non-completers are identified, describe their patterns of behavior... random walk versus climbing the green tree.

nc_part<-seq(from = 1, to = dim(nc_data)[[1]]-5, by = 6)
avg_err<-data.frame()
for(i in nc_part){
	all<-nc_data[(i+1):(i+5),]
	me<-all[1,]
	avg_acc<-mean(all$error)
	me<-data.frame(me, avg_acc)
	avg_err<-rbind(avg_err, me)
}

###Completers: 

completers<-level0[which(level0$error>0.5), "studyid"]
c_data<-lapply(completers, function(d){
	return(amc[which(amc$studyid==d),])
})
c_data<-fun(c_data)


#Identify max level for each participant where accuracy is 0.75 or greater
#Could use a for loop that loops through 1,7, 13, 19, ... , 985

part <- seq(from = 1, to = dim(c_data)[[1]]-5, by = 6)
max_lev<-data.frame()
for(i in part){
	all <- c_data[i:(i+5),]
	all_75 <- all[which(all$error > 0.5),]
	me<-all_75[dim(all_75)[[1]],]
	max_lev<-rbind(max_lev, me)
}
max_lev$final_ratio<-max_lev$size/226

max_lev$DX2<-max_lev$DX

levels(max_lev$DX2)<-c("22q","1td","xxx","xxy")
max_lev$DX2<-as.character(max_lev$DX2)
max_lev$DX2<-factor(max_lev$DX2)

###output CSVs###
#write.csv(max_lev[,c("studyid","gender","DX","age","final_ratio")], paste("amc_completers_",as.character(Sys.Date()),".csv",sep=""), row.names=F)
#write.csv(nc_data, paste("amc_noncompleters_",as.character(Sys.Date()),".csv",sep=""),row.names=F)

###Statistics/Machine Learning###

max_lev$age_c<-max_lev$age - mean(max_lev$age)
max_lev$final_ratio_sqr<-(max_lev$final_ratio)^2
max_lev$final_ratio_cub<-(max_lev$final_ratio)^3

max_lev$DX3<-max_lev$DX2
levels(max_lev$DX3)<-c("1td","22q","xxx","xxy","sca")
max_lev[which(max_lev$DX3=="xxx"),"DX3"]<-"sca"
max_lev[which(max_lev$DX3=="xxy"),"DX3"]<-"sca"

library(psych)

fit1<-lm(final_ratio~DX2*gender*age_c, data=max_lev)
fit2<-lm(final_ratio_cub~DX2*gender*age_c, data=max_lev)
fit3<-lm(final_ratio_cub~DX2 + age_c, data=max_lev)
fit4<-lm(final_ratio~DX2*age_c, data=max_lev)

fit5<-lm(final_ratio~DX3*gender*age_c, data=max_lev)
fit6<-lm(final_ratio_cub~DX3*gender*age_c, data=max_lev)

fit7<-lm(final_ratio~DX3*age_c, data=max_lev)
fit8<-lm(final_ratio_cub~DX3, data=max_lev)
#Save and output a matrix with this inoformation

###chi-square Exact Test of Non-Completers
nc_table<-table(nc_data$DX)/6
chisq.test(nc_table)

###For table###
library(psych)
describeBy(max_lev$final_ratio, group=max_lev$DX3)

###Kruskal-Wallis Test###
kruskal.test(final_ratio~DX2, data=max_lev)
t.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="22q"),"final_ratio"])
t.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="xxx"),"final_ratio"])
t.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="xxy"),"final_ratio"])

wilcox.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="22q"),"final_ratio"])
wilcox.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="xxx"),"final_ratio"])
wilcox.test(max_lev[which(max_lev$DX=="td"),"final_ratio"],max_lev[which(max_lev$DX=="xxy"),"final_ratio"])

kruskal.test(final_ratio~DX3, data=max_lev)
t.test(max_lev[which(max_lev$DX3=="1td"),"final_ratio"],max_lev[which(max_lev$DX3=="22q"),"final_ratio"])
t.test(max_lev[which(max_lev$DX3=="1td"),"final_ratio"],max_lev[which(max_lev$DX3=="sca"),"final_ratio"])

wilcox.test(max_lev[which(max_lev$DX3=="1td"),"final_ratio"],max_lev[which(max_lev$DX3=="22q"),"final_ratio"])
wilcox.test(max_lev[which(max_lev$DX3=="1td"),"final_ratio"],max_lev[which(max_lev$DX3=="sca"),"final_ratio"])

#Output
hist(max_lev$final_ratio, main = "Histogram of Final Ratios Achieved by Completers", col="gray", ylab = "Number achieving of 152 total completers", xlab="ratio achieved")
legend(0.5, 50, c("22q n=60", "td n=49", "xxx n=18", "xxy n=25"))

setwd("~/Documents/Lab/AMC APC TDJ/NewTables")
library(stargazer)
stargazer(summary(fit8)[4], summary=F, digits=2, out="amc_sca.html")
#stargazer(summary(fit3)[4], summary=F, digits=2, out="fit3.html")
#stargazer(summary(fit4)[4], summary=F, digits=2, out="fit4.html")
library(car)
#stargazer(Anova(fit3), summary=F, digits=2, out="amc_Anova_cube.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="none")[3], summary=F, digits=2, out="amc_cube_none.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="fdr")[3], summary=F, digits=2, out="amc_cube_fdr.html")
#stargazer(Anova(fit4), summary=F, digits=2, out="amc_Anova_skew.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="none")[3], summary=F, digits=2, out="amc_skew_none.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="fdr")[3], summary=F, digits=2, out="amc_skew_fdr.html")

library(ggplot2)

ggplot(max_lev, aes(DX, final_ratio, fill=DX)) + geom_violin() + scale_fill_manual(values=c("hotpink","blue","orange","forestgreen"))+labs(title="Final Ratios (Uncorrected)", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz()
ggplot(max_lev, aes(DX, final_ratio_cub, fill=DX)) + geom_violin() + scale_fill_manual(values=c("hotpink","blue","orange","forestgreen"))+labs(title="Final Ratios Cubed", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz()
ggplot(max_lev, aes(DX3, final_ratio, fill=DX3)) + geom_violin() + scale_fill_manual(values=c("blue","hotpink","brown"))+labs(title="Final Ratios (Uncorrected)", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz()
ggplot(max_lev, aes(DX3, final_ratio_cub, fill=DX3)) + geom_violin() + scale_fill_manual(values=c("blue","hotpink","brown"))+labs(title="Final Ratios Cubed", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

###Pairwise T-Tests between SCA and 22q for Kathryn###
pairt<-function(max_lev_df, column){
	return(
	list(t.test(max_lev_df[which(max_lev_df$DX3=="22q"),column],max_lev_df[which(max_lev_df$DX3=="sca"),column]),
wilcox.test(max_lev_df[which(max_lev_df$DX3=="22q"),column],max_lev_df[which(max_lev_df$DX3=="sca"),column]))
	)
}
pairt(max_lev, "final_ratio")
pairt(max_lev, "final_ratio_cub")


###separage good at it kids from bad at it kids for Courtney###
max_lev$gb<- ifelse(max_lev$final_ratio_cub < 0.58, "bad","good")
summary(lm(final_ratio_cub~age_c, data=max_lev[which(max_lev$gb=="good"),]))
summary(lm(final_ratio_cub~age_c, data=max_lev[which(max_lev$gb=="bad"),]))

summary(lm(final_ratio_cub~age_c+DX3, data=max_lev[which(max_lev$gb=="good"),]))
summary(lm(final_ratio_cub~age_c+DX3, data=max_lev[which(max_lev$gb=="bad"),]))