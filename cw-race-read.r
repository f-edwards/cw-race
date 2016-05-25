rm(list=ls())

library(plyr)
library(TTR)
library(dplyr)
library(tidyr)
library(Amelia)

##############################################################
### Read Foster Care Entry Data - State-year panel produced from AFCARS
### produced with FC_AFCARS_indTOstate.r in this repository

source("~/Dropbox/cw-race/cw-race-functions.r")
setwd("~/Dropbox/data/fc-race")

#IPUMS ACS 2000-2011 - File created using CensusTransform.r in this repository
pop<-read.csv("popdat.csv", head=TRUE)[,-1]

### Create three-year moving averages for 2000-2002, 2001-2003, 2002-2004, 2003-2005, 2004-2006 
### for comparability with 3 yr ACS for 2007 onward
states<-unique(pop$state)

temp<-pop[pop$year<2007,]
mov.ave<-matrix(ncol=ncol(pop))
for(s in 1:length(states)){
	s.temp<-temp[temp$state==states[s],]
	s.temp<-s.temp[with(s.temp, order(year)),]
	s.ma<-matrix(nrow=7, ncol=ncol(s.temp))
	s.ma[,1]<-s.temp$state
	s.ma[,2]<-s.temp$year

	for(c in 3:ncol(s.temp)){
		s.ma[,c]<-SMA(s.temp[,c], 3)
	}
	mov.ave<-rbind(mov.ave, s.ma)
}

mov.ave<-as.data.frame(mov.ave)
names(mov.ave)<-names(pop)
mov.ave<-na.omit(mov.ave)
popmerge<-rbind(pop[pop$year>2006, ], mov.ave, pop[pop$year==2000, ])


### Berry et al. Citizen and Govt Ideology data: https://rcfording.wordpress.com/state-ideology-data/
### Converted by author from .xlsx into .csv

ideo<-read.csv("ideo6010.csv")
pol<-cleanpol(ideo)
pol$year<-pol$year+1

### NATIONAL PRISONER STATISTICS http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34540
incartemp<-read.delim("34540-0001-Data.tsv", head=TRUE)
s.inc<-incartemp$STATE
incartemp<-incartemp[,-3]
incartemp[incartemp<0]<-NA
incartemp<-cbind(s.inc, incartemp)

incar<-(data.frame(state=incartemp$STATEID, year=incartemp$YEAR,
  incartot=incartemp$CWPRIVM+incartemp$CWPRIVF,
  new.incar=incartemp$ADTOTM+incartemp$ADTOTF))  

### FBI UCR Data - http://www.icpsr.umich.edu/icpsrweb/ICPSR/series/57/studies?sortBy=7&archive=ICPSR&q=allocated+state&searchSource=revise
### Crimes reported to police
cr11<-read.ucr("34582-0004-Data.txt", 2011)
cr10<-read.ucr("33523-0004-Data.txt", 2010)
cr09<-read.ucr("30763-0004-Data.txt", 2009)
cr08<-read.ucr("27644-0004-Data.txt", 2008)
cr07<-read.ucr("25114-0004-Data.txt", 2007)
cr06<-read.ucr("23780-0004-Data.txt", 2006)
cr05<-read.ucr("04717-0004-Data.txt", 2005)
cr04<-read.ucr("04466-0004-Data.txt", 2004)
cr03<-read.ucr("04360-0004-Data.txt", 2003)
cr02<-read.ucr("04009-0004-Data.txt", 2002)
cr01<-read.ucr("03721-0004-Data.txt", 2001)
cr00<-read.ucr("03451-0004-Data.txt", 2000)
crime<-rbind(cr11,cr10,cr09,cr08, cr07, cr06, cr05, cr04, cr03, cr02, cr01, cr00)


### University of Kentucky Center for Poverty research data - http://www.ukcpr.org/data
pov<-read.csv("UKCPR_National_Welfare_Data_Set_090814.csv", na.strings=c("-", ""))
pov<-pov[pov$year>1999,]
names(pov)[1]<-"stname"
keeps<-c("state","year","AFDC.TANF.Recipients", "Food.Stamp.SNAP.Recipients", "AFDC.TANF.Benefit.for.3.person.family",
	"FS.SNAP.Benefit.for.3.person.family", "AFDC.TANF_FS.3.Person.Benefit", "Total.SSI",
	"NSLP.Total.Participation", "SBP.Total.Participation", "WIC.participation",
	"Number.of.Poor..thousands.", "Food.Insecure", 
	"Gross.State.Product",
	"Medicaid.beneficiaries", "State.EITC.Rate")
pov<-cleanpol(pov)

pov<-pov[,names(pov)%in%keeps]

names(pov)<-c("state" ,"year","food.insec", "GSP", "AFDCRec", "SNAPRec", "AFDCBen3",
	"SNAPBen3", "AFDCFS3Ben", "Total.SSI", "npoor", "eitc.st","medicaidrec",
	 "WIC.par", "NSLP.Total", "SBP.Total")
pov$food.insec<-pov$food.insec/100 ## rescale to [0,1]

#### State and local govt employment data from Annual Survey of Public Employment and Payroll 
### https://www.census.gov//govs/apes/

emp<-read.csv("emp-dat.csv", head=TRUE)
emp<-emp[-(which(emp$state=="US")),]
emp$gov.function<-ifelse(
	(emp$gov.function=="Police with power of arrest")|
	(emp$gov.function=="Persons with power of arrest ")|
	(emp$gov.function=="Police Protection - Officers")|
	(emp$gov.function=="Police Officers Only"),
	"Police Officers", as.character(emp$gov.function))

police<-emp[which(emp$gov.function=="Police Officers"),]
names(police)<-c("year", "state", "drop", "police.ft.emp", "police.tot.pay")
welfare<-emp[which(emp$gov.function=="Public Welfare"),]
names(welfare)<-c("year", "state", "drop", "welfare.ft.emp", "welfare.tot.pay")
corrections<-emp[which(emp$gov.function=="Correction"),]
names(corrections)<-c("year", "state", "drop", "cor.ft.emp", "cor.tot.pay")
total<-emp[which(emp$gov.function=="Total"),]
names(total)<-c("year", "state", "drop", "tot.ft.emp", "tot.tot.pay")
edu<-emp[which(emp$gov.function=="Education Total"),]
names(edu)<-c("year", "state", "drop", "edu.ft.emp", "edu.tot.pay")



spend<-join_all(list(police, welfare, corrections, edu, total), by=c("state", "year"))
spend<-spend[-(which(spend$state=="DC")),]
names(spend)[2]<-"stname"
spend<-spend[,-(grep("drop", names(spend)))]


### Death sentences from DPIC
death<-read.csv("dpic-sentences.csv")
names(death)[1]<-"stname"
d.g<-gather(death, "stname","year", 2:39)
names(d.g)<-c("stname", "year", "death.sent")
d.g$year<-as.numeric(substring(d.g$year,2))

sp.pol<-join_all(list(d.g,spend, pol),  
	by=c("stname", "year"))

sp.pol$death.pen<-!(is.na(sp.pol$death.sent))
sp.pol[which(is.na(sp.pol$death.sent)), "death.sent"]<-0

### Regional Price Parity Index from BEA 
### http://www.bea.gov/iTable/iTableHtml.cfm?reqid=70&step=30&isuri=1&7022=101&7023=8&7024=non-industry&7033=-1&7025=0&7026=xx&7027=-1&7001=8101&7028=1&7031=0&7040=-1&7083=levels&7029=101&7090=70

rpp<-read.csv("rpp.csv", head=TRUE)
### Construct average Regional Price Parity for 2008=2013 by state

rpp$rpp<-apply(rpp[,3:8], MARGIN=1, FUN=mean)
rpp$rpp<-rpp$rpp/100
names(rpp)[1]<-"state"
rp.out<-rpp[,c(1,9)]

fc<-join_all(list(popmerge, incar, 
	sp.pol, crime, pov), by=c("state", "year"))

fc<-join_all(list(fc, rp.out), by="state")

fc$stname<-NA
fc<-stnames(fc)

fc.new<-read.csv("fc-race-state.csv")
names(fc.new)[1:2]<-c("stname", "year")
fc<-left_join(fc.new, fc, by=c("stname", "year"))

fc<-fc[fc$year!=2001,]
fc<-fc[fc$year!=2000,]