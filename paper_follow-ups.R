setwd("~/Documents/Lab/AMC APC TDJ/Data")

#####################
## EFFECT SIZE AMC ##
#####################

amc<-read.csv("amc_completers_2016-02-19.csv")
#amc requires cube transform
amc$final_ratio_cube<-amc$final_ratio^3

DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

amc$DX2<-DX2sca(amc$DX)

###effect size###
#cohen's d for t-tests
library(psych)
describe(amc$final_ratio_cube)
describeBy(amc$final_ratio_cube, group=amc$DX2)
#td vs 22q
(0.82 - 0.68)/0.19
#td vs SCA
(0.82 - 0.79)/0.19
#SCA vs 22q
(0.79 - 0.68)/0.19

#R^2 for F tests
summary(lm(final_ratio_cube~DX2, data=amc))

#df for t-tests
t.test(amc[which(amc$DX2=="1td"),"final_ratio_cube"],amc[which(amc$DX2=="22q"), "final_ratio_cube"])
t.test(amc[which(amc$DX2=="1td"),"final_ratio_cube"],amc[which(amc$DX2=="sca"), "final_ratio_cube"])


#######################
## EFFECT SIZE TDJ-V ##
#######################

tdjv<-read.csv("tdjv_completers_2016-02-19.csv")

tdjv$DX2<-DX2sca(tdjv$DX)

###effect size###
#cohen's d for t-tests

describe(tdjv$final_ratio)
describeBy(tdjv$final_ratio, group=tdjv$DX2)
#td vs 22q
(0.83 - 0.7)/0.15
#td vs SCA
(0.83 - 0.78)/0.15
#SCA vs 22q
(0.78 - 0.7)/0.15

#R^2 for F tests
summary(lm(final_ratio~DX2, data=tdjv))

#df for t-tests
t.test(tdjv[which(tdjv$DX2=="1td"),"final_ratio"],tdjv[which(tdjv$DX2=="22q"), "final_ratio"])
t.test(tdjv[which(tdjv$DX2=="1td"),"final_ratio"],tdjv[which(tdjv$DX2=="sca"), "final_ratio"])


#######################
## EFFECT SIZE TDJ-A ##
#######################

tdja<-read.csv("tdja_completers_2016-02-19.csv")

tdja$DX2<-DX2sca(tdja$DX)

###effect size###
#cohen's d for t-tests

describe(tdja$final_ratio)
describeBy(tdja$final_ratio, group=tdja$DX2)
#td vs 22q
(0.94 - 0.78)/0.18
#td vs SCA
(0.94 - 0.89)/0.18
#SCA vs 22q
(0.89 - 0.78)/0.18

#R^2 for F tests
summary(lm(final_ratio~DX2, data=tdja))

#df for t-tests
t.test(tdja[which(tdja$DX2=="1td"),"final_ratio"],tdja[which(tdja$DX2=="22q"), "final_ratio"])
t.test(tdja[which(tdja$DX2=="1td"),"final_ratio"],tdja[which(tdja$DX2=="sca"), "final_ratio"])

#####################
## EFFECT SIZE APC ##
#####################

apc<-read.csv("apc_completers_2016-02-19.csv")

apc$DX2<-DX2sca(apc$DX)
apc$final_ratio_sqr<-apc$final_ratio^2

###effect size###
#cohen's d for t-tests

describe(apc$final_ratio_sqr)
describeBy(apc$final_ratio_sqr, group=apc$DX2)
#td vs 22q
(0.66 - 0.64)/0.16
#td vs SCA
(0.66 - 0.69)/0.16
#SCA vs 22q
(0.69 - 0.64)/0.16

#R^2 for F tests
summary(lm(final_ratio_sqr~DX2, data=apc))

#df for t-tests
t.test(apc[which(apc$DX2=="1td"),"final_ratio_sqr"],apc[which(apc$DX2=="22q"), "final_ratio_sqr"])
t.test(apc[which(apc$DX2=="1td"),"final_ratio_sqr"],apc[which(apc$DX2=="sca"), "final_ratio_sqr"])


#did excluded participants differ on age/gender/IQ

###get IQ data###
WASI<-read.csv("WASI.csv")
WASI.II<-read.csv("WASI-II.csv")
WISC.III<-read.csv("WISC-III.csv")
WISC.IV<-read.csv("WISC-IV.csv", na.strings="-999")
missing<-read.csv("amcapctdj_studyids_noIQ_filled.csv", na.strings="XXXX")

WASI<-WASI[,c("Study_ID","WASI_Verb_IQ","WASI_Perf_IQ","WASI_Full4_IQ")]
WASI.II<-WASI.II[,c("Study_ID","WASI_II_Verbal_Comp_IQ","WASI_II_Perc_Rsng_IQ","WASI_II_Full4_IQ")]
WISC.III<-WISC.III[,c("Study_ID","WISCIII_VIQ","WISCIII_PIQ","WISCIII_FSIQ")]
WISC.IV<-WISC.IV[,c("Study_ID","WISCIV_VerbalComprehension_C","WISCIV_PerceptualReasoning_C","WISCIV_FullScale_C")]
missing<-missing[,c("studyid","VCI","PRI","FSIQ")]

###for WASI no PSI or WMI from file maker###

WASI$test<-"WASI"
WASI.II$test<-"WASI-II"
WISC.III$test<-"WISC-III"
WISC.IV$test<-"WISC-IV"
missing$test<-"unknown"

colnames(WASI)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WASI.II)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.III)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(WISC.IV)<-c("studyid","VIQ","PIQ","FSIQ","test")
colnames(missing)<-c("studyid","VIQ","PIQ","FSIQ","test")

IQ<-rbind(WASI,WASI.II,WISC.III,WISC.IV, missing)

###get noncomps###

amcn<-read.csv("amc_noncompleters_2016-02-19.csv")
tdjvn<-read.csv("tdjv_noncompleters_2016-02-19.csv")
tdjan<-read.csv("tdja_noncompleters_2016-02-19.csv")
apcn<-read.csv("apc_noncompleters_2016-02-19.csv")

amcn$task<-"amc"
apcn$task<-"apc"
tdjvn$task<-"tdjv"
tdjan$task<-"tdja"

amcn$comp<-"n"
apcn$comp<-"n"
tdjvn$comp<-"n"
tdjan$comp<-"n"

amcn$task_comp<-"amcn"
apcn$task_comp<-"apcn"
tdjvn$task_comp<-"tdjvn"
tdjan$task_comp<-"tdjan"

amcn$final_ratio<-"NA"
apcn$final_ratio<-"NA"
tdjvn$final_ratio<-"NA"
tdjan$final_ratio<-"NA"

amc$task<-"amc"
apc$task<-"apc"
tdjv$task<-"tdjv"
tdja$task<-"tdja"

amc$comp<-"y"
apc$comp<-"y"
tdjv$comp<-"y"
tdja$comp<-"y"

amc$task_comp<-"amcy"
apc$task_comp<-"apcy"
tdjv$task_comp<-"tdjvy"
tdja$task_comp<-"tdjay"

mycols<-c("studyid","gender", "DX", "age","task","comp","task_comp", "final_ratio")

all<-rbind(amcn[which(amcn$level == 0),mycols], apcn[which(apcn$level == 0),mycols], tdjvn[which(tdjvn$level == 0),mycols], tdjan[which(tdjan$level == 0),mycols], 
amc[,mycols], apc[,mycols],tdja[,mycols],tdjv[,mycols])

all$comp<-as.factor(all$comp)

###age###
t.test(amcn[which(amcn$level == 0),"age"], amc$age)
t.test(apcn[which(apcn$level == 0), "age"], apc$age)
t.test(tdjvn[which(tdjvn$level == 0), "age"], tdjv$age)
t.test(tdjan[which(tdjan$level == 0), "age"], tdja$age)

###can't use lme with factors, come back to it with internet###
table(all$comp, all$gender, all$task)

#amc
chisq.test(rbind(c(5,8),c(69,83)))

#apc
chisq.test(rbind(c(14,5),c(59,86)))

#tdja
chisq.test(rbind(c(9,7), c(43,54)))

#tdjv
chisq.test(rbind(c(16,10), c(38,51)))

###IQ###
all<-merge(all, IQ, all.x=T, all.y=F, by="studyid")

#amc
#t.test(all[which(all$task == "amc" & all$comp == "n"), "FSIQ"], all[which(all$task == "amc" & all$comp == "y"), "FSIQ"])

#FSIQ
t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "FSIQ"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "FSIQ"])

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "FSIQ"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "FSIQ"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "FSIQ"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "FSIQ"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "FSIQ"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "FSIQ"])

#VIQ
t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "VIQ"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "VIQ"])

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "VIQ"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "VIQ"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "VIQ"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "VIQ"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "VIQ"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "VIQ"])

#PIQ
t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "PIQ"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "PIQ"])

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "PIQ"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "PIQ"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "PIQ"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "PIQ"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "PIQ"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "PIQ"])

#same people every task?

amcm<-amc[,c("studyid","final_ratio_cube")]
colnames(amcm)<-c("studyid","amc_fr")

all<-merge(all, amcm, all.x=T, by="studyid")

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "amc_fr"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "amc_fr"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "amc_fr"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "amc_fr"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "amc_fr"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "amc_fr"])

apcm<-apc[,c("studyid","final_ratio_sqr")]
colnames(apcm)<-c("studyid","apc_fr")

all<-merge(all, apcm, all.x=T, by="studyid")

t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "apc_fr"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "apc_fr"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "apc_fr"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "apc_fr"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "apc_fr"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "apc_fr"])

tdjam<-tdja[,c("studyid","final_ratio")]
colnames(tdjam)<-c("studyid","tdja_fr")

all<-merge(all, tdjam, all.x=T, by="studyid")

t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "tdja_fr"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "tdja_fr"])

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "tdja_fr"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "tdja_fr"])

t.test(all[which(all$task == "tdjv" & all$comp == "n" & all$DX =="22q"), "tdja_fr"], all[which(all$task == "tdjv" & all$comp == "y" & all$DX =="22q"), "tdja_fr"])

tdjvm<-tdjv[,c("studyid","final_ratio")]
colnames(tdjvm)<-c("studyid","tdjv_fr")

all<-merge(all, tdjvm, all.x=T, by="studyid")

t.test(all[which(all$task == "amc" & all$comp == "n" & all$DX =="22q"), "tdjv_fr"], all[which(all$task == "amc" & all$comp == "y" & all$DX =="22q"), "tdjv_fr"])

t.test(all[which(all$task == "apc" & all$comp == "n" & all$DX =="22q"), "tdjv_fr"], all[which(all$task == "apc" & all$comp == "y" & all$DX =="22q"), "tdjv_fr"])

t.test(all[which(all$task == "tdja" & all$comp == "n" & all$DX =="22q"), "tdjv_fr"], all[which(all$task == "tdja" & all$comp == "y" & all$DX =="22q"), "tdjv_fr"])

all22q<-all[which(all$DX == "22q"),]

failure_rate<-table(all$studyid, all$task_comp)

###Wide Format for Failure Rate###

mycols<-c("studyid","gender", "DX", "age","task","comp","task_comp", "final_ratio")

amc_wide<-rbind(amcn[which(amcn$level == 0),mycols], amc[,mycols])

apc_wide<-rbind(apcn[which(apcn$level == 0),mycols], apc[,mycols])

tdjv_wide<-rbind(tdjvn[which(tdjvn$level == 0),mycols], tdjv[,mycols])

tdja_wide<-rbind(tdjan[which(tdjan$level == 0),mycols], tdja[,mycols])

colnames(amc_wide)<-c("studyid","gender","DX", "age", "task", "comp_amc","task_comp","final_ratio_amc")

colnames(apc_wide)<-c("studyid","gender","DX", "age", "task", "comp_apc","task_comp","final_ratio_apc")

colnames(tdjv_wide)<-c("studyid","gender","DX", "age", "task", "comp_tdjv","task_comp","final_ratio_tdjv")

colnames(tdja_wide)<-c("studyid","gender","DX", "age", "task", "comp_tdja","task_comp","final_ratio_tdja")

all_wide<-merge(amc_wide, apc_wide[,c("studyid","comp_apc","final_ratio_apc")], by = "studyid",all.x=T, all.y=T)
all_wide<-merge(all_wide, tdjv_wide[,c("studyid","comp_tdjv","final_ratio_tdjv")], by = "studyid",all.x=T, all.y=T)
all_wide<-merge(all_wide, tdja_wide[,c("studyid","comp_tdja","final_ratio_tdja")], by = "studyid",all.x=T, all.y=T)

chisq.test(table(all_wide$comp_amc, all_wide$comp_apc))
chisq.test(table(all_wide$comp_amc, all_wide$comp_tdja))
chisq.test(table(all_wide$comp_amc, all_wide$comp_tdjv))

chisq.test(table(all_wide$comp_apc, all_wide$comp_tdja))
chisq.test(table(all_wide$comp_apc, all_wide$comp_tdjv))

chisq.test(table(all_wide$comp_tdja, all_wide$comp_tdjv))

###z score lmes###
zcols<-c("studyid","gender","DX", "age","task", "final_ratio_amc","final_ratio_apc","final_ratio_tdjv","final_ratio_tdja")

all_wide<-all_wide[,zcols]
all_wide$final_ratio_amc<-as.numeric(all_wide$final_ratio_amc)
all_wide$final_ratio_apc<-as.numeric(all_wide$final_ratio_apc)
all_wide$final_ratio_tdja<-as.numeric(all_wide$final_ratio_tdja)
all_wide$final_ratio_tdjv<-as.numeric(all_wide$final_ratio_tdjv)

all_wide$amc_z<-scale(all_wide$final_ratio_amc)
all_wide$apc_z<-scale(all_wide$final_ratio_apc)
all_wide$tdja_z<-scale(all_wide$final_ratio_tdja)
all_wide$tdjv_z<-scale(all_wide$final_ratio_tdjv)
all_wide$DX2<-DX2sca(all_wide$DX)

library(nlme)

summary(lme(amc_z~apc_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))
summary(lme(amc_z~tdja_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))
summary(lme(amc_z~tdjv_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))

summary(lme(apc_z~tdja_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))
summary(lme(apc_z~tdjv_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))

summary(lme(tdja_z~tdjv_z*DX2, random = ~1|studyid, data=all_wide, na.action=na.omit))