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

#AK 2013 is missing race data
index<-which(incar$state==2 & incar$year==2013)
incar[index,5:ncol(incar)]<-NA

ucr<-read.csv("ucr2014.csv", head=TRUE, stringsAsFactors = FALSE)
names(ucr)[1]<-"stname"

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

names(pop)[1:2]<-c("state", "year")

fc<-left_join(pop, incar, by=c("state", "year"))

fc$stname<-NA
fc<-stnames(fc)

fc.new<-read.csv("fc-race-state.csv")
names(fc.new)[1:2]<-c("stname", "year")
fc<-left_join(fc.new, fc, by=c("stname", "year"))
fc<-left_join(fc, pov, by=c("stname", "year"))
fc<-left_join(fc, pol, by=c("state", "year"))
fc<-left_join(fc, ucr, by=c("stname", "year"))

fc[which(fc$amind.child.pov==0), "amind.child.pov"]<-NA
fc[which(fc$stname=="HI" & fc$year==2010), "amind.child"]<-NA

### CREATE VARS
fc<-fc%>%mutate(cl.wht.pc=cl.white/wht.child, cl.blk.pc=cl.blk/blk.child, 
                cl.amind.pc=cl.nat.am/amind.child, cl.lat.pc=cl.latino/latino.child,
                ent.wht.pc=ent.white/wht.child, ent.blk.pc=ent.blk/blk.child,
                ent.amind.pc=ent.nat.am/amind.child, ent.lat.pc=ent.latino/latino.child,
                chpov.wht.pc=wht.child.pov/wht.child, chpov.blk.pc=blk.child.pov/blk.child,
                chpov.amind.pc=amind.child.pov/amind.child, chpov.latino.pc=latino.child.pov/latino.child)%>%
                filter(stname!="PR")%>%filter(stname!="DC")

