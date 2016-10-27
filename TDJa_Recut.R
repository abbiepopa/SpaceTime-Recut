setwd("~/Documents/Lab/AMC APC TDJ/Data")
tdja<-read.csv("tdj-a_data_20150702.csv", header=T)

###Split Data into Completers and Non-Completers


###Non-Completers: Develop a description of who's in this group. Are their data just random noise? Or after the first failure are they continuously climbing the "green tree brances"
level0<-tdja[which(tdja$level==0),]
noncompleters<-level0[which(level0$error<0.75),"studyid"]
nc_data<-lapply(noncompleters, function(d){
	return(tdja[which(tdja$studyid == d),])
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
hist(avg_err$avg_acc, breaks=c(0,0.5,0.75,1), main="How often do non-completers succeed or fail \n on the rest of the 24 trials?", ylab="Total Participants in Category", xlab="Average Accuracy", freq=T, col="gray")

###Completers: 

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

###output data files###
#write.csv(max_lev[,c("studyid","gender","DX","age","final_ratio")], paste("tdja_completers_",as.character(Sys.Date()),".csv",sep=""), row.names=F)
#write.csv(nc_data, paste("tdja_noncompleters_",as.character(Sys.Date()),".csv",sep=""),row.names=F)

###Statistics/Machine Learning###

max_lev$age_c<-max_lev$age - mean(max_lev$age)

fit1<-lm(final_ratio~DX2*gender*age_c, data=max_lev)
fit2<-lm(final_ratio~DX2 + age, data=max_lev)

max_lev$final_ratio2<-max_lev$final_ratio
max_lev[which(max_lev$final_ratio2>1), "final_ratio2"]<-1

fit3<-lm(final_ratio2~DX2*gender*age_c, data=max_lev)
fit4<-lm(final_ratio2~DX2, data=max_lev)

###chi-square###
nc_table<-table(nc_data$DX)/9
chisq.test(nc_table)

###for desc table###
library(psych)
describeBy(max_lev$final_ratio2, group=max_lev$DX2)
describeBy(max_lev$final_ratio2, group=max_lev$DX)

#Output
hist(max_lev$final_ratio, main = "Histogram of Final Ratios Achieved by Completers", col="gray", ylab = "Number achieving of 97 total completers", xlab="ratio achieved")
legend(0.5, 25, c("22q n=40", "td n=30", "xxx n=13", "xxy n=14", "sca n=27"))

setwd("~/Documents/Lab/AMC APC TDJ/NewTables")
library(stargazer)
library(car)
stargazer(summary(fit4)[4], summary=F, digits=2, out="fit2a_collapsed.html")
#stargazer(Anova(fit2), summary=F, digits=2, out="tdja_Anova.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="none")[3], summary=F, digits=2, out="tdja_none.html")
#stargazer(pairwise.t.test(max_lev$final_ratio, max_lev$DX2, p.adj="fdr")[3], summary=F, digits=2, out="tdja_fdr.html")

library(ggplot2)

ggplot(max_lev, aes(DX, final_ratio, fill=DX)) + geom_violin() + scale_fill_manual(values=c("hotpink","blue","orange","forestgreen"))+labs(title="Final Ratios", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz()
ggplot(max_lev, aes(DX2, final_ratio, fill=DX2)) + geom_violin() + scale_fill_manual(values=c("hotpink","blue","brown"))+labs(title="Final Ratios", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz
ggplot(max_lev, aes(DX, final_ratio2, fill=DX)) + geom_violin() + scale_fill_manual(values=c("hotpink","blue","orange","forestgreen"))+labs(title="Final Ratios", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")

quartz()
ggplot(max_lev, aes(DX2, final_ratio2, fill=DX2)) + geom_violin() + scale_fill_manual(values=c("blue","hotpink","brown"))+labs(title="Final Ratios", x="Diagnostic Group", y="Final Ratio")+geom_boxplot(width=0.05,col="gray", fill="black")


###Pairwise T-Tests between SCA and 22q for Kathryn###
pairt<-function(max_lev_df, column){
	return(
	list(t.test(max_lev_df[which(max_lev_df$DX2=="22q"),column],max_lev_df[which(max_lev_df$DX2=="sca"),column]),
wilcox.test(max_lev_df[which(max_lev_df$DX2=="22q"),column],max_lev_df[which(max_lev_df$DX2=="sca"),column]))
	)
}
pairt(max_lev, "final_ratio")
