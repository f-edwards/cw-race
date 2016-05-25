---
title: "Race and child welfare EDA"
author: "Frank Edwards"
date: "May 25, 2016"
output: html_document
---

```{r}
require(dplyr)
require(ggplot2)
require(maps)
require(data.table)
require(tidyr)

source("~/Dropbox/cw-race/cw-race-read.r")
source("state-county-choropleths/map-functions.r")



#nc11<-fread("/data/child2011.csv", data.table=FALSE)
pop1<-fread("data/nhgis0018_ds207_20145_2014_state.csv", data.table=FALSE)
pop2<-fread("data/nhgis0018_ds206_20145_2014_state.csv", data.table=FALSE)

pop3<-fread("/census/census-race-1880-2014.csv")
pop3<-pop3%>%
  filter(YEAR==1880|YEAR==1910|YEAR==1970)

pop3$pct.imm<-pop3$foreign/pop3$tot
pop3$pct.blk<-pop3$blk/pop3$tot
pop3$pct.ami<-pop3$amind/pop3$tot
names(pop3)[(which(names(pop3)=="STATEFIP"))]<-"state"


pop.imm<-pop3%>%
  select(state, YEAR, pct.imm)%>%
  spread(YEAR, pct.imm)

names(pop.imm)[2:ncol(pop.imm)]<-paste("pop.imm", names(pop.imm)[2:ncol(pop.imm)], sep=".")

pop.blk<-pop3%>%
  select(state, YEAR, pct.blk)%>%
  spread(YEAR, pct.blk)

names(pop.blk)[2:ncol(pop.blk)]<-paste("pop.blk", names(pop.blk)[2:ncol(pop.blk)], sep=".")

pop.ami<-pop3%>%
  select(state, YEAR, pct.ami)%>%
  spread(YEAR, pct.ami)

names(pop.ami)[2:ncol(pop.ami)]<-paste("pop.ami", names(pop.ami)[2:ncol(pop.ami)], sep=".")

pop.tot<-pop2%>%
  rename(tot.pop=ABA2E001, wht.pop=ABA2E002, blk.pop=ABA2E003, ami.pop=ABA2E004, lat.pop=ABBAE012)%>%
  dplyr::select(STATE, STATEA, contains("pop"))


# This poverty data is crap - counts are way to low, come back later
# pop.pov<-pop1%>%
#   rename(tot.pov=ABTBE002)%>%
#   mutate(child.pov=ABTBM003+ABTBM004+ABTBM005,
#          child.pop=child.pov+ABTBM011+ABTBM012+ABTBM013,
#          blk.child.pov=ABTDM003+ABTDM004+ABTDM005,
#          blk.child.pop=blk.child.pov+ABTDM011+ABTDM012+ABTDM013,
#          ami.child.pov=ABTEM003+ABTEM004+ABTEM005,
#          ami.child.pop=ami.child.pov+ABTEM011+ABTEM012+ABTEM013,
#          lat.child.pov=ABTKM003+ABTKM004+ABTKM005,
#          lat.child.pop=lat.child.pov+ABTKM011+ABTKM012+ABTKM013)%>%
#   dplyr::select(STATE, STATEA, contains("child"))
# 
# pop<-full_join(pop.tot, pop.pov, by=c("STATE", "STATEA"))

pop.tot$state<-as.numeric(pop.tot$STATEA)



fc.st<-fc11%>%
  group_by(state)%>%
  summarise(ami.fc=sum(amiakn==1), blk.fc=sum(blkafram==1),
            wht.fc=sum(white==1), lat.fc=sum(hisorgin==1))

pop.tot$YEAR<-2014

fc.pop<-full_join(fc.st, pop.tot, by="state")
fc.pop<-full_join(fc.pop, pop.imm, by="state")
fc.pop<-full_join(fc.pop, pop.ami, by="state")
fc.pop<-full_join(fc.pop, pop.blk, by="state")

pop.out<-fc.pop[,c(1, 6, 7, 15:22)]
write.csv(fc.pop, "")

```

```{r}

fc.pop$ami.fc.pc<-fc.pop$ami.fc/fc.pop$ami.pop
fc.pop$blk.fc.pc<-fc.pop$blk.fc/fc.pop$blk.pop
fc.pop$lat.fc.pc<-fc.pop$lat.fc/fc.pop$lat.pop
fc.pop$wht.fc.pc<-fc.pop$wht.fc/fc.pop$wht.pop


fc.pop$pctblk<-fc.pop$blk.pop/fc.pop$tot.pop
fc.pop$pctami<-fc.pop$ami.pop/fc.pop$tot.pop


fc.pop$state<-fc.pop$STATE
map.race.pc<-statemap(fc.pop, 
                      list(fc.pop$ami.fc.pc, fc.pop$blk.fc.pc, fc.pop$lat.fc.pc, fc.pop$wht.fc.pc), 
                      8, c("ami fc pc", "blk fc pc", "lat fc pc", "wht fc pc")) + 
  ggtitle("FC Contact per capita")

map.race.pc

map.disp<-statemap(fc.pop, 
                   list(fc.pop$ami.fc.pc/fc.pop$wht.fc.pc, 
                        fc.pop$blk.fc.pc/fc.pop$wht.fc.pc, 
                        fc.pop$lat.fc.pc/fc.pop$wht.fc.pc), 
                   8, c("ami fc disp", "blk fc disp", "lat fc disp")) +
  ggtitle("Racial Disproportion in FC Contact")

map.disp

### map out predictions, check hypotheses

m1.blk<-lm(log(blk.fc.pc)~pop.imm.1910+pop.blk.1910+pop.ami.1910, data=fc.pop)

# fc.splom<-fc.pop%>%
#   mutate(pct.wht=wht.pop/tot.pop, pct.blk=blk.pop/tot.pop, pct.ami=ami.pop/tot.pop,
#          pct.lat=lat.pop/tot.pop)%>%
#   dplyr::select(ami.fc.pc,blk.fc.pc, lat.fc.pc,wht.fc.pc, pct.ami, pct.blk, pct.lat, pct.wht)
# 
# require(lattice) 
# require(pander)
# pander(cor(fc.splom))
## do plots of pct pop by entry for group - maybe overlay with fit line from other groups?

```

I should also look into other outcomes - placement type, length of lifetime in FC, removal reason

Also, bring in NCANDS

Then do county level, mapping prob less useful

figure out how to build out county predictors - maybe census restricted, CDC mortality, others - esp need to come up w/ something like poverty exposure

Requested pums data on pop by race - will take some doing to clean