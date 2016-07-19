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

#RTM#
rtm<-read.csv("rtm_correlation_rm.outlier_20141030.txt")

all_tasks$studyid<-as.factor(all_tasks$studyid)

d<-merge(all_tasks, rtm, by=c("studyid","DX"))

d$DX1<-d$DX
levels(d$DX1)<-c("22q","1td","xxx","xxy")
d$DX1<-factor(as.character(d$DX1))

fit<-lm(mean~DX1, data=d)

d$DX2<-d$DX1
levels(d$DX2)<-c("1td","22q","xxx","xxy","sca")
d[which(d$DX2=="xxx"),"DX2"]<-"sca"
d[which(d$DX2=="xxy"),"DX2"]<-"sca"

fit1<-lm(mean~DX2, data=d)

