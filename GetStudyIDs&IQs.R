###Find Age and Gender Details###

task_ag<-function(d){
	d<-d[,c("studyid","DX", "gender","age")]
	d$studyid <- as.character(d$studyid)
	d$DX2<-as.character(d$DX)
	d[which(d$DX2=="xxx"),"DX2"]<-"sca"
	d[which(d$DX2=="xxy"),"DX2"]<-"sca"
	d$DX2<-factor(d$DX2)
	return(unique(d))
}

setwd("~/Documents/Lab/AMC APC TDJ/Data")
amc<-read.csv("amc_data_20150702.csv")
amc<-task_ag(amc)


setwd("~/Documents/Lab/AMC APC TDJ/Data")
apc<-read.table("apc_data_20150702.txt", header=T)
apc<- task_ag(apc)


setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdjv<-read.csv("tdj-v_data_20150702.txt", header=T)
tdjv<- task_ag(tdjv)


setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdja<-read.csv("tdj-a_data_20150702.csv", header=T)
tdja<-task_ag(tdja)


all_tasks<-rbind(amc, apc, tdjv, tdja)

all_tasks<-unique(all_tasks)

fit<-lm(age~DX2, data=all_tasks)
library(car)
Anova(fit)
chisq.test(table(all_tasks$DX2, all_tasks$gender))

library(psych)

age_desc<-function(d){
	desc1<-describeBy(d$age, group=d$DX)
	desc2<-describeBy(d$age, group=d$DX2)
	d22q<-unlist(desc1$`22q`[,c("n","mean","sd","se")])
	dTD<-unlist(desc1$`td`[,c("n","mean","sd","se")])
	dXXX<-unlist(desc1$`xxx`[,c("n","mean","sd","se")])
	dXXY<-unlist(desc1$`xxy`[,c("n","mean","sd","se")])
	dSCA<-unlist(desc2$`sca`[,c("n","mean","sd","se")])
	out<-data.frame(d22q,dTD,dXXX,dXXY,dSCA)
	return(t(out))
}
amc_age<-age_desc(amc)
apc_age<-age_desc(apc)
tdja_age<-age_desc(tdja)
tdjv_age<-age_desc(tdjv)
all_age<-age_desc(all_tasks)

gender_desc<-function(d){
	return(table(d$DX, d$gender))
}

amc_gender<-gender_desc(amc)
apc_gender<-gender_desc(apc)
tdja_gender<-gender_desc(tdja)
tdjv_gender<-gender_desc(tdjv)
all_gender<-gender_desc(all_tasks)

library(stargazer)
stargazer(amc_age, summary=F, digits=2, out="amc_age.html")
stargazer(apc_age, summary=F, digits=2, out="apc_age.html")
stargazer(tdja_age, summary=F, digits=2, out="tdja_age.html")
stargazer(tdjv_age, summary=F, digits=2, out="tdjv_age.html")
stargazer(all_age, summary=F, digits=2, out="all_age.html")

stargazer(as.data.frame.matrix(amc_gender), summary=F, digits=0, out="amc_gender.html")
stargazer(as.data.frame.matrix(apc_gender), summary=F, digits=0, out="apc_gender.html")
stargazer(as.data.frame.matrix(tdja_gender), summary=F, digits=0, out="tdja_gender.html")
stargazer(as.data.frame.matrix(tdjv_gender), summary=F, digits=0, out="tdjv_gender.html")
stargazer(as.data.frame.matrix(all_gender), summary=F, digits=0, out="all_gender.html")


###List All Participants###

task<-function(d){
	d<-d[,c("studyid","DX")]
	d$studyid <- as.character(d$studyid)
	return(d)
}

setwd("~/Documents/Lab/AMC APC TDJ/Data")
amc<-read.csv("amc_data_20150702.csv")
amc<-task(amc)

setwd("~/Documents/Lab/AMC APC TDJ/Data")
apc<-read.table("apc_data_20150702.txt", header=T)
apc<- task(apc)

setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdjv<-read.csv("tdj-v_data_20150702.txt", header=T)
tdjv<- task(tdjv)

setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdja<-read.csv("tdj-a_data_20150702.csv", header=T)
tdja<-task(tdja)

all_tasks<-rbind(amc, apc, tdjv, tdja)

all_tasks<-unique(all_tasks)

###Who is missing IQ###
WASI<-read.csv("WASI.csv")
WASI.II<-read.csv("WASI-II.csv")
WISC.III<-read.csv("WISC-III.csv")
WISC.IV<-read.csv("WISC-IV.csv")

WASI<-WASI[,c("Study_ID","WASI_Verb_IQ","WASI_Perf_IQ","WASI_Full4_IQ")]
WASI.II<-WASI.II[,c("Study_ID","WASI_II_Verbal_Comp_IQ","WASI_II_Perc_Rsng_IQ","WASI_II_Full4_IQ")]
WISC.III<-WISC.III[,c("Study_ID","WISCIII_VIQ","WISCIII_PIQ","WISCIII_FSIQ")]
WISC.IV<-WISC.IV[,c("Study_ID","WISCIV_VerbalComprehension_C","WISCIV_PerceptualReasoning_C","WISCIV_FullScale_C")]

WASI$test<-"WASI"
WASI.II$test<-"WASI-II"
WISC.III$test<-"WISC-III"
WISC.IV$test<-"WISC-IV"

colnames(WASI)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WASI.II)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.III)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.IV)<-c("studyid","VIQ","PIQ","FSIQ","test")

IQ<-rbind(WASI,WASI.II,WISC.III,WISC.IV)

all_IQ<-merge(all_tasks, IQ, all.x=T)

missingFSIQ<-all_IQ[which(is.na(all_IQ$FSIQ)),"studyid"]
missingVIQ<-all_IQ[which(is.na(all_IQ$VIQ)),"studyid"]
missingPIQ<-all_IQ[which(is.na(all_IQ$PIQ)),"studyid"]

###bring in CABILIDs###
cabil<-read.csv("CABILID.csv")
cabil<-cabil[,c("STUDY..OldCABILID","STUDY..__kp_StudyID")]
colnames(cabil)<-c("cabil","studyid")
cabil$cabil2<-as.character(cabil$cabil)
cabil$cabil2<-substr(cabil$cabil2,start=1,stop=3)

cabil_all<-merge(all_tasks, cabil, all.x=T)

###Never had IQ, i.e, merge on cabilid###
WASI<-read.csv("WASI.csv")
WASI.II<-read.csv("WASI-II.csv")
WISC.III<-read.csv("WISC-III.csv")
WISC.IV<-read.csv("WISC-IV.csv")

WASI<-WASI[,c("CABIL_ID","Study_ID","WASI_Verb_IQ","WASI_Perf_IQ","WASI_Full4_IQ")]
WASI.II<-WASI.II[,c("CABIL_ID","Study_ID","WASI_II_Verbal_Comp_IQ","WASI_II_Perc_Rsng_IQ","WASI_II_Full4_IQ")]
WISC.III<-WISC.III[,c("CABIL_ID","Study_ID","WISCIII_VIQ","WISCIII_PIQ","WISCIII_FSIQ")]
WISC.IV<-WISC.IV[,c("CABIL_ID","Study_ID","WISCIV_VerbalComprehension_C","WISCIV_PerceptualReasoning_C","WISCIV_FullScale_C")]

WASI$test<-"WASI"
WASI.II$test<-"WASI-II"
WISC.III$test<-"WISC-III"
WISC.IV$test<-"WISC-IV"

colnames(WASI)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WASI.II)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.III)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.IV)<-c("cabil","studyid","VIQ","PIQ","FSIQ","test")

IQ<-rbind(WASI,WASI.II,WISC.III,WISC.IV)
all_IQ2<-merge(cabil_all, IQ, all.x=T, by.x="cabil2", by.y="cabil")

neverFSIQ<-all_IQ2[which(is.na(all_IQ2$FSIQ)),"studyid.x"]
neverVIQ<-all_IQ2[which(is.na(all_IQ2$FSIQ)),"studyid.x"]
neverPIQ<-all_IQ2[which(is.na(all_IQ2$FSIQ)),"studyid.x"]

neverFSIQ<-data.frame(neverFSIQ)
colnames(neverFSIQ)<-"studyid"
neverFSIQ<-merge(neverFSIQ, cabil_all, by="studyid")

missingFSIQ<-data.frame(missingFSIQ)
colnames(missingFSIQ)<-"studyid"
missingFSIQ<-merge(missingFSIQ, cabil_all, by="studyid")

###Output###
write.csv(cabil_all,"all_amcapctdj_studyids.csv",row.names=F)
write.csv(missingFSIQ, "amcapctdj_studyids_missingIQfromCycle2.csv",row.names=F)
write.csv(neverFSIQ,"amcapctdj_studyids_noIQ.csv",row.names=F)