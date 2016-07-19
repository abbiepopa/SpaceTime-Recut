setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdjv<-read.csv("tdj-v_data_20150702.txt", header=T)

###Split Data into Completers and Non-Completers


###Non-Completers: Develop a description of who's in this group. Are their data just random noise? Or after the first failure are they continuously climbing the "green tree brances"
level0<-tdjv[which(tdjv$level==0),]
noncompleters<-level0[which(level0$error<0.75),"studyid"]
nc_data<-lapply(noncompleters, function(d){
	return(tdjv[which(tdjv$studyid == d),])
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

nc_part<-seq(from = 1, to = dim(nc_data)[[1]]-8, by = 9)
avg_err<-data.frame()
for(i in nc_part){
	all<-nc_data[(i+1):(i+8),]
	me<-all[1,]
	avg_acc<-mean(all$error)
	me<-data.frame(me, avg_acc)
	avg_err<-rbind(avg_err, me)
}

###dang it, didn't save histogram stuff, do that here###
table(avg_err$DX, avg_err$avg_acc)

###Completers: 

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


###skew and kurtosis?###
max_lev_sca<-max_lev[which(max_lev$DX != "22q"),]
max_lev_sca$DX<-as.factor(as.character(max_lev_sca$DX))
max_lev_sca$DX2<-as.factor(as.character(max_lev_sca$DX2))
max_lev_sca$DX2<-as.factor(fun_DX2(as.character(max_lev_sca$DX2)))

library(psych)

describe(max_lev_sca)

###completion rate###
table(nc_data$DX)/9
table(max_lev_sca$DX)
chisq.test(data.frame(td<-c(1,30), xxx<-c(3,11), xxy<-c(4, 12)))
chisq.test(data.frame(td=c(1,30), sca<-c(7, 23)))

###actual ml###
summary(lm(final_ratio~DX2*age, data=max_lev_sca))
summary(lm(final_ratio~DX*age, data=max_lev_sca))

summary(lm(final_ratio~DX2*age + DX2*gender, data=max_lev_sca))

summary(lm(final_ratio~DX2, data=max_lev_sca))
summary(lm(final_ratio~DX, data=max_lev_sca))

t.test((max_lev_sca[which(max_lev_sca$DX=="xxx"),"final_ratio"]), (max_lev_sca[which(max_lev_sca$DX=="xxy"),"final_ratio"]))


###girls like girls and boys###
max_lev_sca$frs<-max_lev_sca$final_ratio^2
t.test(max_lev_sca[which(max_lev_sca$DX=="td" & max_lev_sca$gender == "male"),"final_ratio"], max_lev_sca[which(max_lev_sca$DX=="xxy" & max_lev_sca$gender == "male"),"final_ratio"])


t.test(max_lev_sca[which(max_lev_sca$DX=="td" & max_lev_sca$gender == "female"),"final_ratio"], max_lev_sca[which(max_lev_sca$DX=="xxx" & max_lev_sca$gender == "female"),"final_ratio"])