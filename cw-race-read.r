library(plyr)
library(TTR)
library(dplyr)
library(tidyr)
library(Amelia)

##############################################################
### Read Foster Care Entry Data - State-year panel produced from AFCARS
### produced with FC_AFCARS_indTOstate.r in this repository


#IPUMS ACS 2000-2011 - File created using CensusTransform.r in this repository
pop<-read.csv("pop-race-2000-2014.csv", head=TRUE)

### Berry et al. Citizen and Govt Ideology data: https://rcfording.wordpress.com/state-ideology-data/
### Converted by author from .xlsx into .csv

ideo<-read.csv("ideo6014.csv")
ideo$state<-NULL
pol<-cleanpol2(ideo)
pol$year<-pol$year

### NATIONAL PRISONER STATISTICS http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34540
incartemp<-read.delim("36281-0001-Data.tsv", head=TRUE)
s.inc<-incartemp$STATE
incartemp<-incartemp[,-3]
incartemp[incartemp<0]<-NA
incartemp<-cbind(s.inc, incartemp)

incar<-(data.frame(state=incartemp$STATEID, year=incartemp$YEAR,
  incartot=incartemp$CWPRIVM+incartemp$CWPRIVF,
  new.incar=incartemp$ADTOTM+incartemp$ADTOTF))  

incar<-cbind(incar, incartemp[,53:80])

### FBI UCR Data - http://www.icpsr.umich.edu/icpsrweb/ICPSR/series/57/studies?sortBy=7&archive=ICPSR&q=allocated+state&searchSource=revise
### Crimes reported to police
# cr12<-read.ucr("35019-0004-Data.txt", 2012)
# cr11<-read.ucr("34582-0004-Data.txt", 2011)
# cr10<-read.ucr("33523-0004-Data.txt", 2010)
# cr09<-read.ucr("30763-0004-Data.txt", 2009)
# cr08<-read.ucr("27644-0004-Data.txt", 2008)
# cr07<-read.ucr("25114-0004-Data.txt", 2007)
# cr06<-read.ucr("23780-0004-Data.txt", 2006)
# cr05<-read.ucr("04717-0004-Data.txt", 2005)
# cr04<-read.ucr("04466-0004-Data.txt", 2004)
# cr03<-read.ucr("04360-0004-Data.txt", 2003)
# cr02<-read.ucr("04009-0004-Data.txt", 2002)
# cr01<-read.ucr("03721-0004-Data.txt", 2001)
# cr00<-read.ucr("03451-0004-Data.txt", 2000)
# crime<-rbind(cr12,cr11,cr10,cr09,cr08, cr07, cr06, cr05, cr04, cr03, cr02, cr01, cr00)


### University of Kentucky Center for Poverty research data - http://www.ukcpr.org/data
pov<-read.csv("UKCPR_National_Welfare_Data_01202016_0.csv", na.strings=c("-", ""))
names(pov)[1]<-"stname"
keeps<-c("stname","year","AFDC.TANF.Recipients", "Food.Stamp.SNAP.Recipients", "AFDC.TANF.Benefit.for.3.person.family",
	"FS.SNAP.Benefit.for.3.person.family", "AFDC.TANF_FS.3.Person.Benefit", "Total.SSI",
	"NSLP.Total.Participation", "SBP.Total.Participation", "WIC.participation",
	"Number.of.Poor..thousands.", "Food.Insecure",
	"Gross.State.Product",
	"Medicaid.beneficiaries", "State.EITC.Rate")

pov<-pov[,names(pov)%in%keeps]

names(pov)<-c("stname" ,"year","food.insec", "GSP", "AFDCRec", "SNAPRec", "AFDCBen3",
	"SNAPBen3", "AFDCFS3Ben", "Total.SSI", "npoor", "eitc.st","medicaidrec",
	 "WIC.par", "NSLP.Total", "SBP.Total")
pov$food.insec<-pov$food.insec/100 ## rescale to [0,1]

#### State and local govt employment data from Annual Survey of Public Employment and Payroll 
### https://www.census.gov//govs/apes/

# emp<-read.csv("emp-dat.csv", head=TRUE)
# emp<-emp[-(which(emp$state=="US")),]
# emp$gov.function<-ifelse(
# 	(emp$gov.function=="Police with power of arrest")|
# 	(emp$gov.function=="Persons with power of arrest ")|
# 	(emp$gov.function=="Police Protection - Officers")|
# 	(emp$gov.function=="Police Officers Only"),
# 	"Police Officers", as.character(emp$gov.function))
# 
# police<-emp[which(emp$gov.function=="Police Officers"),]
# names(police)<-c("year", "state", "drop", "police.ft.emp", "police.tot.pay")
# welfare<-emp[which(emp$gov.function=="Public Welfare"),]
# names(welfare)<-c("year", "state", "drop", "welfare.ft.emp", "welfare.tot.pay")
# corrections<-emp[which(emp$gov.function=="Correction"),]
# names(corrections)<-c("year", "state", "drop", "cor.ft.emp", "cor.tot.pay")
# total<-emp[which(emp$gov.function=="Total"),]
# names(total)<-c("year", "state", "drop", "tot.ft.emp", "tot.tot.pay")
# edu<-emp[which(emp$gov.function=="Education Total"),]
# names(edu)<-c("year", "state", "drop", "edu.ft.emp", "edu.tot.pay")
# 
# 
# 
# spend<-join_all(list(police, welfare, corrections, edu, total), by=c("state", "year"))
# spend<-spend[-(which(spend$state=="DC")),]
# names(spend)[2]<-"stname"
# spend<-spend[,-(grep("drop", names(spend)))]


# ### Death sentences from DPIC
# death<-read.csv("dpic-sentences.csv")
# names(death)[1]<-"stname"
# d.g<-gather(death, "stname","year", 2:39)
# names(d.g)<-c("stname", "year", "death.sent")
# d.g$year<-as.numeric(substring(d.g$year,2))
# 
# sp.pol<-join_all(list(d.g,spend, pol),  
# 	by=c("stname", "year"))
# 
# sp.pol$death.pen<-!(is.na(sp.pol$death.sent))
# sp.pol[which(is.na(sp.pol$death.sent)), "death.sent"]<-0

### Regional Price Parity Index from BEA 
### http://www.bea.gov/iTable/iTableHtml.cfm?reqid=70&step=30&isuri=1&7022=101&7023=8&7024=non-industry&7033=-1&7025=0&7026=xx&7027=-1&7001=8101&7028=1&7031=0&7040=-1&7083=levels&7029=101&7090=70
# 
# rpp<-read.csv("rpp.csv", head=TRUE)
# ### Construct average Regional Price Parity for 2008=2013 by state
# 
# rpp$rpp<-apply(rpp[,3:8], MARGIN=1, FUN=mean)
# rpp$rpp<-rpp$rpp/100
# names(rpp)[1]<-"state"
# rp.out<-rpp[,c(1,9)]

names(pop)[1:2]<-c("state", "year")

fc<-join_all(list(pop, incar), by=c("state", "year"))

fc$stname<-NA
fc<-stnames(fc)

fc.new<-read.csv("fc-race-state.csv")
names(fc.new)[1:2]<-c("stname", "year")
fc<-left_join(fc.new, fc, by=c("stname", "year"))
fc<-left_join(fc, pov, by=c("stname", "year"))
fc<-left_join(fc, pol, by=c("state", "year"))

### CREATE VARS
fc<-fc%>%mutate(cl.wht.pc=cl.white/wht.child, cl.blk.pc=cl.blk/blk.child, 
                cl.amind.pc=cl.nat.am/amind.child, cl.lat.pc=cl.latino/latino.child,
                ent.wht.pc=ent.white/wht.child, ent.blk.pc=ent.blk/blk.child,
                ent.amind.pc=ent.nat.am/amind.child, ent.lat.pc=ent.latino/latino.child,
                chpov.wht.pc=wht.child.pov/wht.child, chpov.blk.pc=blk.child.pov/blk.child,
                chpov.amind.pc=amind.child.pov/amind.child, chpov.latino.pc=latino.child.pov/latino.child
                )%>%
  filter(year>2006)%>%filter(stname!=("PR"))%>%filter(stname!="DC")

