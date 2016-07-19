#########################################################################################
###Caveat, in general outlier means "someone who is >2 SD from the sample mean" 
###Thus outliers shouldn't be different for age and Dx than they are for just the straight up DX
###It's possible that Tony want's a list of people who are outside a certain confidence interval, which is what I've calculated here

###It's also very possible Tony just wants you to eye-ball the scatter plots...
#########################################################################################

##########################################################
###Grab AMC data so we have an example set to work with###
##########################################################

setwd("~/Documents/Lab/AMC APC TDJ/Data")
amc<-read.csv("amc_completers_2016-02-19.csv")

###function for converting Dx###
DX2sca<-function(d){
	dx_char<-as.character(d)
	dx_char[which(dx_char == "xxx")]<-"sca"
	dx_char[which(dx_char == "xxy")]<-"sca"
	dx_char[which(dx_char == "td")]<-"1td"
	return(factor(dx_char))
}

###make the new DX column
amc$Dx2<-DX2sca(amc$DX)

###################################################
###Calculate Outliers Using Built In R Functions###
###################################################

###need a linear model to work on###
amc.lm_ageDx<-lm(final_ratio~age*Dx2, data=amc)

###need a prediction matrix###
amc.pred_ageDx<-predict(amc.lm_ageDx, interval=c("prediction"), level=0.95)

amc$lower<-amc.pred_ageDx[,2]
amc$upper<-amc.pred_ageDx[,3]

amc$outlier<-0

amc[which(amc$lower > amc$final_ratio),"outlier"]<-1
amc[which(amc$upper < amc$final_ratio),"outlier"]<-1

outliers<-amc[which(amc$outlier ==1), "studyid"]

outliers

######################################################################################
###Or You can import (copy and paste them to your workspace) these functions I made###
######################################################################################

###or use this function for outliers based on age and DX###
out_ageDx<-function(d){
	d.lm<-lm(final_ratio~age*Dx2, data=d)
	d.pred<-predict(d.lm, interval=c("prediction"),level=0.95)
	d$lower<-d.pred[,2]
	d$upper<-d.pred[,3]
	d$outlier<-0
	d[which(d$lower > d$final_ratio),"outlier"]<-1
	d[which(d$upper < d$final_ratio),"outlier"]<-1
	out<-d[which(d$outlier ==1),"studyid"]
	return(out)
}

###and this function for outliers based on age only###
out_age<-function(d){
	d.lm<-lm(final_ratio~age, data=d)
	d.pred<-predict(d.lm, interval=c("prediction"),level=0.95)
	d$lower<-d.pred[,2]
	d$upper<-d.pred[,3]
	d$outlier<-0
	d[which(d$lower > d$final_ratio),"outlier"]<-1
	d[which(d$upper < d$final_ratio),"outlier"]<-1
	out<-d[which(d$outlier ==1),"studyid"]
	return(out)
}

###this function works the same as the previous, but automatically separates by Dx

out_age_byDx<-function(d){
	q22<-out_age(amc[which(amc$Dx2 == "22q"),])
	td<-out_age(amc[which(amc$Dx2 == "1td"),])
	sca<-out_age(amc[which(amc$Dx2 == "sca"),])
	results<-list("These are the outliers from those with 22q",q22,"These are the outliers from those who are TD",td, "These are the outliers from those with SCA", sca)
	return(results)
}

###TO USE THE FUNCTIONS

###name of function (name of dataset)

###you can change "final_ratio," "studyid," and "age" if your columns have different names

###you can change 0.95 if you want a different number of SDs

##example

out_age_byDx(amc)