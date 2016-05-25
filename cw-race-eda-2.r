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
source("~/Dropbox/cw-race/map-functions.r")


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