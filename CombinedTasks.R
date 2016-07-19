###rbind all the completers, preserving which task it was by adding a column for "task"
###mixed effects? repeated measures?
fun<-function(d){
	s<-d[[1]]
	for (i in 2:(length(d))){
		s<- rbind(s,d[[i]])
	}
	return(s)
}
###AMC###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
amc<-read.csv("amc_data_20150702.csv")

###Completers: 
level0<-amc[which(amc$level==0),]
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
levels(max_lev$DX2)<-c("22q","1td","sca","xxx","xxy")
max_lev[which(max_lev$DX2=="xxx"),"DX2"]<-"sca"
max_lev[which(max_lev$DX2=="xxy"),"DX2"]<-"sca"
max_lev$DX2<-as.character(max_lev$DX2)
max_lev$DX2<-factor(max_lev$DX2)
max_lev$age_c<-max_lev$age - mean(max_lev$age)
max_lev$final_ratio_sqr<-(max_lev$final_ratio)^2
max_lev$final_ratio_cub<-(max_lev$final_ratio)^3

amc_proc<-max_lev

###APC###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
apc<-read.table("apc_data_20150702.txt", header=T)

###Completers: 
level0<-apc[which(apc$level==0),]
completers<-level0[which(level0$error>0.5), "studyid"]
c_data<-lapply(completers, function(d){
	return(apc[which(apc$studyid==d),])
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

max_lev$final_ratio<-max_lev$size/2000

max_lev$DX2<-max_lev$DX
levels(max_lev$DX2)<-c("22q","1td","sca","xxx","xxy")
max_lev[which(max_lev$DX2=="xxx"),"DX2"]<-"sca"
max_lev[which(max_lev$DX2=="xxy"),"DX2"]<-"sca"
max_lev$DX2<-as.character(max_lev$DX2)
max_lev$DX2<-factor(max_lev$DX2)
max_lev$age_c<-max_lev$age - mean(max_lev$age)
max_lev$final_ratio_sqr<-(max_lev$final_ratio)^2

apc_proc<-max_lev

###TDJ-a###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdja<-read.csv("tdj-a_data_20150702.csv", header=T)

level0<-tdja[which(tdja$level==0),]
completers<-level0[which(level0$error>0.5), "studyid"]
c_data<-lapply(completers, function(d){
	return(tdja[which(tdja$studyid==d),])
})
c_data<-fun(c_data)

#Identify max level for each participant where accuracy is 0.75 or greater
#Could use a for loop that loops through 1,7, 13, 19, ... , 985

part <- seq(from = 1, to = dim(c_data)[[1]]-8, by = 9)
max_lev<-data.frame()
for(i in part){
	all <- c_data[i:(i+8),]
	all_75 <- all[which(all$error > 0.5),]
	me<-all_75[dim(all_75)[[1]],]
	max_lev<-rbind(max_lev, me)
}

max_lev$final_ratio<-456/max_lev$duration

max_lev$DX2<-max_lev$DX
levels(max_lev$DX2)<-c("22q","1td","sca","xxx","xxy")
max_lev[which(max_lev$DX2=="xxx"),"DX2"]<-"sca"
max_lev[which(max_lev$DX2=="xxy"),"DX2"]<-"sca"
max_lev$DX2<-as.character(max_lev$DX2)
max_lev$DX2<-factor(max_lev$DX2)
max_lev$age_c<-max_lev$age - mean(max_lev$age)

tdja_proc<-max_lev

###TDJ-v###
setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdjv<-read.csv("tdj-v_data_20150702.txt", header=T)

###Completers: 
level0<-tdjv[which(tdjv$level==0),]
completers<-level0[which(level0$error>0.5), "studyid"]
c_data<-lapply(completers, function(d){
	return(tdjv[which(tdjv$studyid==d),])
})
c_data<-fun(c_data)

#Identify max level for each participant where accuracy is 0.75 or greater
#Could use a for loop that loops through 1,7, 13, 19, ... , 985

part <- seq(from = 1, to = dim(c_data)[[1]]-8, by = 9)
max_lev<-data.frame()
for(i in part){
	all <- c_data[i:(i+8),]
	all_75 <- all[which(all$error > 0.5),]
	me<-all_75[dim(all_75)[[1]],]
	max_lev<-rbind(max_lev, me)
}

max_lev$final_ratio<-456/max_lev$duration

max_lev$DX2<-max_lev$DX
levels(max_lev$DX2)<-c("22q","1td","sca","xxx","xxy")
max_lev[which(max_lev$DX2=="xxx"),"DX2"]<-"sca"
max_lev[which(max_lev$DX2=="xxy"),"DX2"]<-"sca"
max_lev$DX2<-as.character(max_lev$DX2)
max_lev$DX2<-factor(max_lev$DX2)
max_lev$age_c<-max_lev$age - mean(max_lev$age)

tdjv_proc<-max_lev

tdja_proc[which(tdja_proc$final_ratio > 1), "final_ratio"]<- 1
tdjv_proc[which(tdjv_proc$final_ratio > 1), "final_ratio"]<-1

###what we really care about are

mycols<-c("studyid","gender","DX","age","final_ratio","DX2")

amc_proc<-amc_proc[,mycols]
apc_proc<-apc_proc[,mycols]
tdja_proc<-tdja_proc[,mycols]
tdjv_proc<-tdjv_proc[,mycols]

amc_proc$task<-"amc"
apc_proc$task<-"apc"
tdjv_proc$task<-"tdjv"
tdja_proc$task<-"tdja"

proc<-rbind(amc_proc,apc_proc,tdjv_proc,tdja_proc)
proc$task<-factor(proc$task)
proc$age_c<-proc$age-mean(proc$age)

###need a wide dataframe for the cor test, to see if participants scores are actually correlated with each other
proc_wide<-merge(amc_proc, apc_proc, by="studyid")
proc_wide<-proc_wide[,c("studyid","gender.x","DX.x","age.x","final_ratio.x","final_ratio.y")]
colnames(proc_wide)<-c("studyid","gender","DX","age","amc","apc")
proc_wide<-merge(proc_wide, tdjv_proc, by="studyid")
proc_wide<-proc_wide[,c("studyid","gender.x","DX.x","age.x","amc","apc","final_ratio")]
colnames(proc_wide)<-c("studyid","gender","DX","age","amc","apc", "tdjv")
proc_wide<-merge(proc_wide, tdja_proc, by="studyid")
proc_wide<-proc_wide[,c("studyid","gender.x","DX.x","age.x","amc","apc", "tdjv","final_ratio")]
colnames(proc_wide)<-c("studyid","gender","DX","age","amc","apc", "tdjv","tdja")
cor(proc_wide[,c("amc","apc","tdjv","tdja")])
cor.test(proc_wide$amc, proc_wide$apc)
cor.test(proc_wide$amc, proc_wide$tdja)
cor.test(proc_wide$amc, proc_wide$tdjv)
cor.test(proc_wide$apc, proc_wide$tdja)
cor.test(proc_wide$apc, proc_wide$tdjv)
cor.test(proc_wide$tdjv, proc_wide$tdja)

library(stargazer)
stargazer(cor(proc_wide[,c("amc","apc","tdjv","tdja")]), summary=F, digits=2,out="cortable_collapsed.html")


###Correlations By Diagnostic Group
proc_wide$DX2<-proc_wide$DX
levels(proc_wide$DX2)<-c(levels(proc_wide$DX),"sca")
proc_wide[which(proc_wide$DX2=="xxx"), "DX2"]<-"sca"
proc_wide[which(proc_wide$DX2=="xxy"), "DX2"]<-"sca"

stargazer(cor(proc_wide[which(proc_wide$DX2=="td"),c("amc","apc","tdjv","tdja")]), summary=F, digits=2,out="cortable_td.html")
stargazer(cor(proc_wide[which(proc_wide$DX2=="22q"),c("amc","apc","tdjv","tdja")]), summary=F, digits=2,out="cortable_22q.html")
stargazer(cor(proc_wide[which(proc_wide$DX2=="sca"),c("amc","apc","tdjv","tdja")]), summary=F, digits=2,out="cortable_sca.html")

cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"apc"])
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"apc"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"apc"], proc_wide[which(proc_wide$DX2=="22q"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"tdjv"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])

cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"apc"])
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"apc"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"apc"], proc_wide[which(proc_wide$DX2=="sca"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"tdjv"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])

cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"apc"])
cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="td"),"apc"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])
cor.test(proc_wide[which(proc_wide$DX2=="td"),"apc"], proc_wide[which(proc_wide$DX2=="td"),"tdjv"])
cor.test(proc_wide[which(proc_wide$DX2=="td"),"tdjv"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])

p.adjust(
c(
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"apc"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"amc"], proc_wide[which(proc_wide$DX2=="22q"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"apc"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"apc"], proc_wide[which(proc_wide$DX2=="22q"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="22q"),"tdjv"], proc_wide[which(proc_wide$DX2=="22q"),"tdja"])$p.value,

cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"apc"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"amc"], proc_wide[which(proc_wide$DX2=="sca"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"apc"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"apc"], proc_wide[which(proc_wide$DX2=="sca"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="sca"),"tdjv"], proc_wide[which(proc_wide$DX2=="sca"),"tdja"])$p.value,

cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"apc"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="td"),"amc"], proc_wide[which(proc_wide$DX2=="td"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="td"),"apc"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="td"),"apc"], proc_wide[which(proc_wide$DX2=="td"),"tdjv"])$p.value,
cor.test(proc_wide[which(proc_wide$DX2=="td"),"tdjv"], proc_wide[which(proc_wide$DX2=="td"),"tdja"])$p.value
), method="fdr"
)

###we now have multiple measures from each participant, so we need to do a nonlinear model###

hist(proc$final_ratio, main = "Histogram of Final Ratios Achieved by Completers", col="gray", ylab = "Number achieving of 163 total completers", xlab="ratio achieved", xlim=c(0.4,1.2))

library(nlme)
fit<-lme(final_ratio~task, random=~1|studyid, data=proc)

proc$task2<-proc$task
levels(proc$task2)<-c("amc","1apc","tdja","tdjv")
proc$task2<-factor(as.character(proc$task2))

proc$task3<-proc$task
levels(proc$task3)<-c("amc","apc","1tdja","tdjv")
proc$task3<-factor(as.character(proc$task3))

proc$task4<-proc$task
levels(proc$task4)<-c("amc","apc","tdja","1tdjv")
proc$task4<-factor(as.character(proc$task4))

fit2<-lme(final_ratio~task2, random=~1|studyid, data=proc)
fit3<-lme(final_ratio~task3, random=~1|studyid, data=proc)
fit4<-lme(final_ratio~task4, random=~1|studyid, data=proc)

library(psych)
describeBy(proc[which(proc$DX2=="1td"),"final_ratio"], group=proc[which(proc$DX2=="1td"),"task"])
describeBy(proc$final_ratio, group=proc$task)

###ANOVA###
library(reshape)
proc_long<-melt(proc_wide, id=c("studyid","gender","DX","age"))
colnames(proc_long)<-c("studyid","gender","DX","age","task","final_ratio")

library(ez)

theanova<-ezANOVA(proc_long, wid=studyid, within=task, dv=final_ratio)