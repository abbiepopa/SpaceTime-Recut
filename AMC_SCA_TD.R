
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

###Compare Age and Gender###
amc_sca<-amc[which(amc$DX != "22q"),]

fun_DX2<-function(d){
	d[which(d == "xxx")]<-"sca"
	d[which(d == "xxy")]<-"sca"
	return(d)
}

amc_sca$DX2<-as.factor(fun_DX2(as.character(amc_sca$DX)))

amc_sca_1lev<-amc_sca[which(amc_sca$level == 1),]

t.test(amc_sca_1lev[which(amc_sca_1lev$DX2=="sca"),"age"],amc_sca_1lev[which(amc_sca_1lev$DX2=="td"),"age"])

###age and gender between groups?###
summary(lm(age~as.factor(DX), data=amc_sca_1lev))
chisq.test(table(amc_sca_1lev$DX2, amc_sca_1lev$gender))

###skew and kurtosis?###
max_lev_sca<-max_lev[which(max_lev$DX != "22q"),]
max_lev_sca$DX<-as.factor(as.character(max_lev_sca$DX))
max_lev_sca$DX2<-as.factor(as.character(max_lev_sca$DX2))
max_lev_sca$DX2<-as.factor(fun_DX2(as.character(max_lev_sca$DX2)))

library(psych)

describe(max_lev_sca)

###completion rate###
table(nc_data$DX)
table(max_lev_sca$DX)
chisq.test(data.frame(td<-c(0,49), xxx<-c(6,18), xxy<-c(12, 25)))
chisq.test(data.frame(td=c(0,49), sca<-c(18, 43)))

###actual ml###
summary(lm(final_ratio^2~DX2*age, data=max_lev_sca))
summary(lm(final_ratio^2~DX*age, data=max_lev_sca))
summary(lm(final_ratio^2~DX2*age + DX2*gender, data=max_lev_sca))

summary(lm(final_ratio^2~DX2, data=max_lev_sca))
summary(lm(final_ratio^2~DX, data=max_lev_sca))

t.test((max_lev_sca[which(max_lev_sca$DX=="xxx"),"final_ratio"])^2, (max_lev_sca[which(max_lev_sca$DX=="xxy"),"final_ratio"])^2)

###girls like girls and boys###
max_lev_sca$frs<-max_lev_sca$final_ratio^2
t.test(max_lev_sca[which(max_lev_sca$DX=="td" & max_lev_sca$gender == "male"),"frs"], max_lev_sca[which(max_lev_sca$DX=="xxy" & max_lev_sca$gender == "male"),"frs"])


t.test(max_lev_sca[which(max_lev_sca$DX=="td" & max_lev_sca$gender == "female"),"frs"], max_lev_sca[which(max_lev_sca$DX=="xxx" & max_lev_sca$gender == "female"),"frs"])