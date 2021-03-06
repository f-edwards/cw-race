#########
rm(list=ls())
set.seed(1)
library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(MASS)
# library(rstanarm, options(mc.cores = parallel::detectCores()))
# library(rstan)
# library(multiwayvcov)
# library(lmtest)
# library(sandwich)
# library(plm)

# 
# setwd("~/sync/cw-race/")
# source("~/sync/cw-race/cw-race-functions.r")
# fc<-read.csv("~/sync/cw-race/data/fc.csv")

# setwd("~/sync/cw-race/")
# source("~/sync/cw-race/cw-race-functions.r")
# fc<-read.csv("~/sync/cw-race/data/fc.csv")

# for laptop
setwd("C:/sync/cw-race/")
source("C:/sync/cw-race/cw-race-functions.r")
fc<-read.csv("C:/sync/cw-race/data/fc.csv", stringsAsFactors = FALSE)

###robustness checks
# mort<-read.csv("~/sync/cw-race/data/infmort.csv")
# names(mort)[1:3]<-c("state", "year", "nonwht.inf.mort")
# mort$mortdisp<-mort[,3]/mort[,4]
# fc<-left_join(fc, mort, by=c("state", "year"))
arrest<-read.csv("C:/sync/cw-race/data/ucr-race.csv")
names(arrest)[2]<-"year"
###Treat meas. error as missing for imputation
missing<-matrix(c(55, 2000, 55, 2001, 51, 2013, 51, 2014, 1, 2011, 1, 2012, 1, 2013, 1, 2014,
                21, 2000, 21, 2001, 21, 2002, 21, 2003, 21, 2004, 21, 2005, 21, 2006, 21, 2007, 21, 2008,
                30, 2004), ncol=2, byrow=TRUE)
cols<-which(names(arrest)%in%c("w.arrest", "b.arrest", "ai.arrest"))

for(i in 1:nrow(missing)){
  arrest[which((arrest$state==missing[i,1]) & arrest$year==missing[i,2]), cols]<-NA
}



welfare<-read.csv("C:/sync/cw-race/data/UKCPR_National_Welfare_Data_12062016.csv", stringsAsFactors = FALSE)
names(welfare)[1]<-"stname"
welfare<-welfare%>%dplyr::filter(year>1999)%>%dplyr::select(stname, year, AFDC.TANF.Recipients, Food.Stamp.SNAP.Recipients, AFDC.TANF.Benefit.for.3.person.family, Medicaid.beneficiaries)
fc<-left_join(fc, welfare, by=c("stname", "year"))
rpp<-read.csv("C:/sync/cw-race/data/rpp.csv")
rpp$rpp<-rpp$rpp/100
for(s in rpp$state){
  fc[which(fc$state==s), "AFDC.TANF.Benefit.for.3.person.family"]<-fc[which(fc$state==s), "AFDC.TANF.Benefit.for.3.person.family"]*rpp[which(rpp$state==s), "rpp"]
}
inflate<-cbind(c(2000:2014), c(1.37,1.34, 1.32, 1.29, 1.25, 1.21, 1.17, 1.14,1.10, 1.10, 1.09,1.05, 1.03,1.02, 1 ))
for(y in inflate[,1]){
  fc[which(fc$year==y), "AFDC.TANF.Benefit.for.3.person.family"]<-fc[which(fc$year==y), "AFDC.TANF.Benefit.for.3.person.family"]*inflate[which(inflate[,1]==y), 2]
}

source("cw-imputation.r", echo=TRUE)


# ### MAKE INCAR AND FC FIGURE
# fcrt<-fc%>%group_by(stname)%>%summarise(Bcrt=sum(cl.blk)/sum(blk.child), Acrt=sum(cl.nat.am)/sum(amind.child))
# fc14<-fcrt%>%filter(year==2014)
# 
# fcd<-fc.imp$imputations[[1]]%>%filter(year==2014)%>%dplyr::select(c(bw.disp, ami.disp, b.incardisp, a.incardisp))
# 
## FILTER OUT THOSE W/NO BLACK, NA CASELOADS for REUN OFFSET
fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.blk>1)
  fcn.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.nat.am>1)
}

##### INEQ TS PLOTS
# plotdat<-fc.imp$imputations[[1]]%>%group_by(year)%>%summarise(bw.disp=(sum(cl.blk)/sum(blk.child))/(sum(cl.white)/sum(wht.child)),
#                                                               na.disp=(sum(cl.nat.am)/sum(amind.child))/(sum(cl.white)/sum(wht.child)))
# dispts<-ggplot(fc.imp$imputations[[1]]%>%filter(year>2000), aes(x=year))+
#   geom_line(aes(y=ami.disp))+geom_line(aes(y=bw.disp), lty=2)+xlab("Year")+
#   ylab("FC Caseload ineq. Nat.Am./White solid, Afr.Am./White dashed")+
#   coord_cartesian(ylim=c(0,15))+
#   facet_wrap(~stname)
# tsstate<-ggplot(fc.imp$imputations[[1]], aes(x=year, y=cl/child))+geom_line()+facet_wrap(~stname)+xlab("Year")+ylab("Caseload per cap.")
# 
# ggsave("~/sync/sync/cw-race/figures/dispts.pdf", dispts, height=6, width=8)
# ggsave("~/sync/sync/cw-race/figures/statets.pdf", tsstate, height=8, width=10)
# 
# z<-ggplot(fc.imp$imputations[[1]])+geom_line(aes(x=year, y=log(bw.disp)))+
#   geom_line(aes(y=log(b.incardisp), x=year), lty=2)+facet_wrap(~stname, nrow=5)+
#   ggtitle("African American disparity in FC caseloads (solid) and incarceration (dashed)")+
#   ylab("Log disparity")+scale_x_continuous(breaks=c(2000,2008))+xlab("Year")
# 
# ggsave(z, file="b-inc-fc-ts.pdf", width=8, height=6)
# 
# z<-ggplot(fc.imp$imputations[[1]])+geom_line(aes(x=year, y=log(ami.disp)))+
#   geom_line(aes(y=log(a.incardisp), x=year), lty=2)+facet_wrap(~stname, nrow=5)+
#   ggtitle("Native American disparity in FC caseloads (solid) and incarceration (dashed)")+
#   ylab("Log disparity")+scale_x_continuous(breaks=c(2000,2008))+coord_cartesian(ylim=c(-0.5, 4))+xlab("Year")
# 
# ggsave(z, file="a-inc-fc-ts.pdf", width=8, height=6)


######## MAIN STATE FE MODEL RESULTS
#source("state-models.r")

######################################
## Main models
source("regime-models.r", echo=TRUE)

# RE Model tables
#source("~/sync/cw-race/cw-tables.r", echo=TRUE)

#source("sim.R", echo=TRUE)

#source("~/sync/cw-race/cw-forest.r", echo=TRUE)
## BAYESIAN MODELS
#source("bayes-models.r", echo=TRUE)

#######################
## Sim visuals
# setwd("~/sync/sync/cw-race/figures")
# source("~/sync/sync/cw-race/sim.R")

###Between and within-state variances
vardat<-fc.imp$imputations[[1]]

within.bw<-summarise(vardat%>%group_by(stname)%>%summarise(within.var=var(bw.disp)), mean.within=median(within.var))
between.bw<-summarise(vardat%>%group_by(year.c)%>%summarise(between.var=var(bw.disp)), mean.between=median(between.var))

within.aw<-summarise(vardat%>%group_by(stname)%>%summarise(within.var=var(ami.disp)), mean.within=median(within.var))
between.aw<-summarise(vardat%>%group_by(year.c)%>%summarise(between.var=var(ami.disp)), mean.between=median(between.var))

vardat<-fc.imp$imputations[[1]]

state.cl<-summarise(vardat%>%group_by(stname)%>%summarise(within.var=var(bw.disp)), mean.within=median(within.var))
between.bw<-summarise(vardat%>%group_by(year.c)%>%summarise(between.var=var(bw.disp)), mean.between=median(between.var))

within.aw<-summarise(vardat%>%group_by(stname)%>%summarise(within.var=var(ami.disp)), mean.within=median(within.var))
between.aw<-summarise(vardat%>%group_by(year.c)%>%summarise(between.var=var(ami.disp)), mean.between=median(between.var))

###Mins and maxes
bw<-vardat%>%group_by(stname)%>%summarise(bw=mean(bw.disp))
aw<-vardat%>%group_by(stname)%>%summarise(aw=mean(ami.disp))
