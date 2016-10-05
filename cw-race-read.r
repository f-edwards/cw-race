library(TTR)
library(dplyr)
library(tidyr)
library(Amelia)

setwd("U:/cw-race/data")
source("U:/cw-race/cw-race-functions.r")
##############################################################
### Read Foster Care Entry Data - State-year panel produced from AFCARS
### produced with FC_AFCARS_indTOstate.r in this repository


#IPUMS ACS 2000-2011 - File created using CensusTransform.r in this repository
pop<-read.csv("pop-race-2000-2014.csv", head=TRUE)
names(pop)[1:2]<-c("state", "year")

temp<-pop[pop$year<2007,]
states<-unique(pop$state)

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
mov.ave<-mov.ave%>%filter(year>2001)
popmerge<-rbind(pop[pop$year>2006, ], mov.ave, pop[pop$year<2002, ])
# popmerge[popmerge==0]<-NA
pop<-popmerge

### Berry et al. Citizen and Govt Ideology data: https://rcfording.wordpress.com/state-ideology-data/
### Converted by author from .xlsx into .csv

ideo<-read.csv("ideo6014.csv")
ideo$state<-NULL
pol<-cleanpol2(ideo)
pol$year<-pol$year
pol<-pol%>%select(statename, year, inst6014_nom, St, state)

### NATIONAL PRISONER STATISTICS http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/34540
incartemp<-read.delim("36281-0001-Data.tsv", head=TRUE)
s.inc<-incartemp$STATE
incartemp<-incartemp[,-3]
# incartemp[incartemp<0]<-NA
incartemp<-cbind(s.inc, incartemp)

incar<-(data.frame(state=incartemp$STATEID, year=incartemp$YEAR,
  incartot=incartemp$CWPRIVM+incartemp$CWPRIVF,
  new.incar=incartemp$ADTOTM+incartemp$ADTOTF))  

incar<-cbind(incar, incartemp[,c(53:56, 59:60)])

#AK 2013 is missing race data
index<-which(incar$state==2 & incar$year==2013)
incar[index,5:ncol(incar)]<-NA

incar[incar<0]<-NA

ucr<-read.csv("ucr2014.csv", head=TRUE, stringsAsFactors = FALSE)
names(ucr)[1]<-"stname"

read.ucr<-function(x, year){
  if(year%in%c(2000,2001,2002,2003,2004,2005,2006)){
    out<-read.fwf(x, widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4),
                  col.names=c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST", "FIPS_CTY", "CPOPARST", 
                              "CPOCRIM","AG_ARRST", "AG_OFF", "COVIND", "INDEX",
                              "MODINDX", "MURDER", "RAPE", "ROBBERY",
                              "AGASLT","BURGLRY", "LARCENY","MVTHEFT", "ARSON"))
  }
  
  out$year<-year
  crime<-crimestate(out)
  return(crime)
}

crimestate<-function(x){
  states<-unique(x$FIPS_ST)
  cout<-data.frame("state"=states, "year"=unique(x$year), "crime"=NA, "viol"=NA)
  for (s in 1:length(states)){
    cout[s, "crime"]<-sum(x[x$FIPS_ST==states[s],"INDEX"])
    cout[s, "viol"]<-sum(x[x$FIPS_ST==states[s],"MURDER"])+
      sum(x[x$FIPS_ST==states[s],"RAPE"])+
      sum(x[x$FIPS_ST==states[s],"ROBBERY"])+
      sum(x[x$FIPS_ST==states[s],"AGASLT"])
  }
  return(cout)
}

cr06<-read.ucr("23780-0004-Data.txt", 2006)
cr05<-read.ucr("04717-0004-Data.txt", 2005)
cr04<-read.ucr("04466-0004-Data.txt", 2004)
cr03<-read.ucr("04360-0004-Data.txt", 2003)
cr02<-read.ucr("04009-0004-Data.txt", 2002)
cr01<-read.ucr("03721-0004-Data.txt", 2001)
cr00<-read.ucr("03451-0004-Data.txt", 2000)
crime<-rbind(cr06, cr05, cr04, cr03, cr02, cr01, cr00)

crime.temp<-left_join(crime, pop%>%select(state, year, tot))
crime.temp$v.crime.rt<-crime.temp$viol/crime.temp$tot*100000



###historical pop data
hist<-read.csv("hist-pop.csv")

hist<-hist%>%select(state, pctblk1930, pctimm1930, pctami1930)

###boarding school data
board<-read.csv("boardingschool.csv")
board$board<-board$boarding.n>0

fc<-left_join(pop, incar, by=c("state", "year"))

crime.temp$stname<-NA
crime.temp<-stnames(crime.temp)
crime.temp<-crime.temp%>%select(stname, year, v.crime.rt)
crime<-bind_rows(ucr, crime.temp)

fc$stname<-NA
fc<-stnames(fc)

fc.new<-read.csv("fc-race-state.csv")
fc.new[is.na(fc.new)]<-0
names(fc.new)[1:2]<-c("stname", "year")
fc.new<-fc.new%>%select(stname, year, cl.blk, ent.blk, cl.white, ent.white, cl.nat.am, ent.nat.am, cl, ent)

##MICHIGAN 2000,2001 data is terrible, treating as missing
fc.new[which((fc.new$stname=="MI")&(fc.new$year<=2001)),3:ncol(fc.new)]<-NA

fc<-left_join(fc.new, fc, by=c("stname", "year"))
fc<-left_join(fc, pol, by=c("state", "year"))
fc<-left_join(fc, crime, by=c("stname", "year"))
fc<-left_join(fc, hist, by="state")
fc<-left_join(fc, board, by="stname")


# fc[which(fc$amind.child.pov==0), "amind.child.pov"]<-NA
# fc[which(fc$stname=="HI" & fc$year==2010), "amind.child"]<-NA

fc<-fc%>%filter(stname!="DC")%>%filter(stname!="PR")

write.csv(fc, "fc.csv", row.names=FALSE)
