###Import IQ###

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

all_IQ<-merge(all_tasks, IQ, all.x=T)

###AMC - oi geez, huge effect of IQ, I was not prepared for that...###

amc_ml<-read.csv("amc_completers_2016-02-19.csv")

amc_ml_iq<-merge(amc_ml, all_IQ)

DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

amc_ml_iq$DX2<-DX2sca(amc_ml_iq$DX)

amc_fit1<-lm(final_ratio^3 ~ DX2*FSIQ*gender*age, data=amc_ml_iq)
amc_fit2<-lm(final_ratio^3 ~ DX2*FSIQ, data=amc_ml_iq)
amc_fit3 <- lm(final_ratio^3 ~ DX2*VIQ, data=amc_ml_iq)
amc_fit4 <- lm(final_ratio^3 ~ DX2*PIQ, data=amc_ml_iq)

###APC - oi geez, huge effect of IQ, I was not prepared for that...###

apc_ml<-read.csv("apc_completers_2016-02-19.csv")

apc_ml_iq<-merge(apc_ml, all_IQ)

DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

apc_ml_iq$DX2<-DX2sca(apc_ml_iq$DX)

apc_fit1<-lm(final_ratio^2 ~ DX2*FSIQ*gender*age, data=apc_ml_iq)
apc_fit2<-lm(final_ratio^2 ~ DX2*FSIQ, data=apc_ml_iq)
apc_fit3 <- lm(final_ratio^2 ~ DX2*VIQ, data=apc_ml_iq)
apc_fit4 <- lm(final_ratio^2 ~ DX2*PIQ, data=apc_ml_iq)

###TDJ-V - oi geez, huge effect of IQ, I was not prepared for that...###

tdjv_ml<-read.csv("tdjv_completers_2016-02-19.csv")

tdjv_ml_iq<-merge(tdjv_ml, all_IQ)

DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

tdjv_ml_iq$DX2<-DX2sca(tdjv_ml_iq$DX)

tdjv_fit1<-lm(final_ratio ~ DX2*FSIQ*gender*age, data=tdjv_ml_iq)
tdjv_fit2<-lm(final_ratio ~ DX2*FSIQ, data=tdjv_ml_iq)
tdjv_fit3 <- lm(final_ratio ~ DX2*VIQ, data=tdjv_ml_iq)
tdjv_fit4 <- lm(final_ratio ~ DX2*PIQ, data=tdjv_ml_iq)

###TDJ-A - oi geez, huge effect of IQ, I was not prepared for that...###

tdja_ml<-read.csv("tdja_completers_2016-02-19.csv")

tdja_ml_iq<-merge(tdja_ml, all_IQ)

DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

tdja_ml_iq$DX2<-DX2sca(tdja_ml_iq$DX)

tdja_fit1<-lm(final_ratio ~ DX2*FSIQ*gender*age, data=tdja_ml_iq)
tdja_fit2<-lm(final_ratio ~ DX2*FSIQ, data=tdja_ml_iq)
tdja_fit3 <- lm(final_ratio ~ DX2*VIQ, data=tdja_ml_iq)
tdja_fit4 <- lm(final_ratio ~ DX2*PIQ, data=tdja_ml_iq)

summary(lm(FSIQ~DX2, data=all_IQ))
summary(lm(VIQ~DX2, data=all_IQ))
summary(lm(PIQ~DX2, data=all_IQ))

t.test(all_IQ[which(all_IQ$DX2=="22q"),"FSIQ"], all_IQ[which(all_IQ$DX2=="sca"),"FSIQ"])
t.test(all_IQ[which(all_IQ$DX2=="22q"),"VIQ"], all_IQ[which(all_IQ$DX2=="sca"),"VIQ"])
t.test(all_IQ[which(all_IQ$DX2=="22q"),"PIQ"], all_IQ[which(all_IQ$DX2=="sca"),"PIQ"])

wilcox.test(all_IQ[which(all_IQ$DX2=="22q"),"FSIQ"], all_IQ[which(all_IQ$DX2=="sca"),"FSIQ"])
wilcox.test(all_IQ[which(all_IQ$DX2=="22q"),"VIQ"], all_IQ[which(all_IQ$DX2=="sca"),"VIQ"])
wilcox.test(all_IQ[which(all_IQ$DX2=="22q"),"PIQ"], all_IQ[which(all_IQ$DX2=="sca"),"PIQ"])

###plot based on amc_ml_iq etc., but clean out that weird 200 first!

###AMC
amc_ml_iq[which(amc_ml_iq$PIQ>150),"PIQ"]<-NA

plot(amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), "PIQ"], amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), "final_ratio"], main = "AMC Final Ratio vs PRI", xlab="PRI", ylab="Max Ratio", col="blue", pch=19, xlim=c(40, 150), ylim = c(0.50, 1.0))
points(amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), "PIQ"], amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), "PIQ"], amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~PIQ, data=amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~PIQ, data=amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~PIQ, data=amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~PIQ, data=amc_ml_iq), col="black", lty=2)

quartz()
plot(amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), "age"], amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), "final_ratio"], main = "AMC Final Ratio vs Age", xlab="Age (months)", ylab="Max Ratio", col="blue", pch=19, xlim=c(80, 190), ylim = c(0.5, 1.0))
points(amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), "age"], amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19,xlim=c(80, 190), ylim = c(0.45, 1.05))
points(amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), "age"], amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~age, data=amc_ml_iq[which(amc_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~age, data=amc_ml_iq[which(amc_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~age, data=amc_ml_iq[which(amc_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~age, data=amc_ml_iq), col="black", lty=2)

###APC
apc_ml_iq[which(apc_ml_iq$PIQ>150),"PIQ"]<-NA

quartz()
plot(apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), "PIQ"], apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), "final_ratio"], main = "APC Final Ratio vs PRI", xlab="PRI", ylab="Max Ratio", col="blue", pch=19, xlim=c(40, 150), ylim = c(0.5, 1.0))
points(apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), "PIQ"], apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), "PIQ"], apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~PIQ, data=apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~PIQ, data=apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~PIQ, data=apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~PIQ, data=apc_ml_iq), col="black", lty=2)

quartz()
plot(apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), "age"], apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), "final_ratio"], main = "APC Final Ratio vs Age", xlab="Age (months)", ylab="Max Ratio", col="blue", pch=19, xlim=c(80, 190), ylim = c(0.45, 1.05))
points(apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), "age"], apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19, xlim=c(80, 190), ylim = c(0.5, 1.0))
points(apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), "age"], apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~age, data=apc_ml_iq[which(apc_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~age, data=apc_ml_iq[which(apc_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~age, data=apc_ml_iq[which(apc_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~age, data=apc_ml_iq), col="black", lty=2)

###TDJ-A
tdja_ml_iq[which(tdja_ml_iq$PIQ>150),"PIQ"]<-NA

quartz()
plot(tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), "PIQ"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), "final_ratio"], main = "TDJ-A Final Ratio vs PRI", xlab="PRI", ylab="Max Ratio", col="blue", pch=19, xlim=c(40, 150), ylim = c(0.45, 1.05))
points(tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), "PIQ"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), "PIQ"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~PIQ, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~PIQ, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~PIQ, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~PIQ, data=tdja_ml_iq), col="black", lty=2)

quartz()
plot(tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), "age"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), "final_ratio"], main = "TDJ-A Final Ratio vs Age", xlab="Age (months)", ylab="Max Ratio", col="blue", pch=19, xlim=c(80, 190), ylim = c(0.45, 1.05))
points(tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), "age"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), "age"], tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~age, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~age, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~age, data=tdja_ml_iq[which(tdja_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~age, data=tdja_ml_iq), col="black", lty=2)

###TDJ-V
tdjv_ml_iq[which(tdjv_ml_iq$PIQ>150),"PIQ"]<-NA

quartz()
plot(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), "PIQ"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), "final_ratio"], main = "TDJ-V Final Ratio vs PRI", xlab="PRI", ylab="Max Ratio", col="blue", pch=19, xlim=c(40, 155), ylim = c(0.45, 1.05))
points(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), "PIQ"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), "PIQ"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~PIQ, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~PIQ, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~PIQ, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~PIQ, data=tdjv_ml_iq), col="black", lty=2)

quartz()
plot(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), "age"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), "final_ratio"], main = "TDJ-V Final Ratio vs Age", xlab="Age (months)", ylab="Max Ratio", col="blue", pch=19, xlim=c(80, 190), ylim = c(0.45, 1.05))
points(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), "age"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), "final_ratio"], col="hotpink", pch=19)
points(tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), "age"], tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), "final_ratio"], col="darkgreen", pch=19)
abline(lm(final_ratio~age, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "1td"), ]), col="blue")
abline(lm(final_ratio~age, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "22q"), ]), col="hotpink")
abline(lm(final_ratio~age, data=tdjv_ml_iq[which(tdjv_ml_iq$DX2 == "sca"), ]), col="darkgreen")
abline(lm(final_ratio~age, data=tdjv_ml_iq), col="black", lty=2)

##SCA IQ test
t.test(all_IQ[which(all_IQ$DX == "xxx"),"FSIQ"], all_IQ[which(all_IQ$DX == "xxy"), "FSIQ"])