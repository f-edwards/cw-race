#########
rm(list=ls())
set.seed(1)
library(merTools)
library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(MASS)

# setwd("C:/Users/kilgore/Dropbox/cw-race/data/")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-functions.r")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-read.r")


## for laptop
setwd("~/Dropbox/cw-race/data/")
source("~/Dropbox/cw-race/cw-race-functions.r")
source("~/Dropbox/cw-race/cw-race-read.r")

##for output
# setwd("~/Dropbox/cw-race-paper/")

# ### for libra
# setwd("~/cw-race/data/")
# source("~/cw-race/cw-race-functions.r")
# source("~/cw-race/cw-race-read.r")
# setwd("~/cw-race/")

##unstable TS vars, amind.lessHS, amind.child, amind.f, blk.child (WV, HI, OR, UT, VT) 

##EVERYTHING IN ACS IS SUSPECT FOR 

ggplot(fc)+geom_line(aes(x=year, y=log(amind.child.pov)))+facet_wrap(~stname)

fc<-fc%>%filter(stname!="DC")%>%filter(stname!="PR")


###imputations on FC raw data - drop unused vars

drops<-c("inst6013_adacope", "citi6013", "welf", "wht.welf", "blk.welf", "amind.welf", "latino.welf",
  "ASIANM", "ASIANF", "NHPIM", "NHPIF", "APIM", "APIF","TWORACEM", 
         "TWORACEF", "ADDRACEM", "ADDRACEF", "UNKRACEM", "UNKRACEF",
  "NOTHISPM", "NOTHISPF", "UNKHISPM", "UNKHISPF",
          "TOTHCATM", "TOTHCATF", 
  "pctimm1900", "pctami1900", "pctblk1900", "HISPF", "HISPM", 
  "cl.latino", "ent.latino","lifelos.latino", "grp.inst.latino", "cl.asian","ent.asian","lifelos.asian","grp.inst.asian",  "cl.unk",       
   "ent.unk", "lifelos.unk","grp.inst.unk", "cl.haw.pi", "ent.haw.pi","lifelos.haw.pi","grp.inst.haw.pi",
  "St",
  "l.adult", "latino.unemp", "latino.emp", "latino", "latino.child", "latino.lessHS", "l.adult")

##DROP UNUSED VARIABLES
fc<-fc[, -which(names(fc)%in%(drops))]

ratios<-which(names(fc)%in%c("blk.lessHS", "wht.lessHS", "amind.lessHS"))


bounds<-cbind(1:ncol(fc),
              rep(0, ncol(fc)),
              rep(Inf, ncol(fc)))

bounds[ratios, 3]<-1

#### TRY LOG TRANSFORMATION TO IMPROVE IMPUTATION MODEL
colClass<-sapply(fc, class)
colClass[2]<-"year"
colClass[66:73]<-"hist"
colClass[36:39]<-"adult"
colClass[15]<-"state"

numeric.vals<-NULL
for(i in 1:ncol(fc)){
  if(colClass[i]%in%c("integer", "numeric")){
    numeric.vals<-c(numeric.vals, i)
  }
}

m<-5

# m=ceiling(max(apply(fc.ineq, 2, function(x){sum(is.na(x))}))/nrow(fc.ineq)*100)
blk.acs<-which(colnames(fc)%in%c("blk.child", "blk.child.pov", "blk.lessHS", "blk.unemp", "blk.singpar"))
amind.acs<-which(colnames(fc)%in%c("amind.child", "amind.child.pov", "amind.lessHS", "amind.unemp", "amind.singpar"))

strong<-c(2000,2007:2014)
amind.thresh<-which((fc$amind.child<100000)&(!(fc$year%in%strong)))
blk.thresh<-which((fc$blk.child<100000)&(!(fc$year%in%strong)))

amind.oi<-matrix(nrow=length(amind.acs)*length(amind.thresh), ncol=2)
amind.oi[,2]<-rep(amind.acs, length(amind.thresh))

for(i in 1:length(amind.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(amind.acs)-1
  amind.oi[oi.min:oi.max,1]<-rep(amind.thresh[i], length(amind.acs))
  oi.min<-oi.max+1
}

blk.oi<-matrix(nrow=length(blk.acs)*length(blk.thresh), ncol=2)
blk.oi[,2]<-rep(blk.acs, length(blk.thresh))

for(i in 1:length(blk.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(blk.acs)-1
  blk.oi[oi.min:oi.max,1]<-rep(blk.thresh[i], length(blk.acs))
  oi.min<-oi.max+1
}

oi<-rbind(amind.oi, blk.oi)

oi.priors<-matrix(nrow=nrow(oi), ncol=4)
oi.priors[,1:2]<-oi[,1:2]

create.prior<-function(x){
  ##function receives row,column pair to set prior based on strong observation years, compute mean and sd based on observed strong periods
  st<-fc[oi[x,1], "stname"]
  var<-oi[x,2]
  year<-fc[oi[x,1], "year"]
  strong.2000<-fc[which((fc$year==2000)&(fc$stname==st)), var]
  strong.2009<-fc[which((fc$year==2009)&(fc$stname==st)), var]
  ###WEIGHT ON PROXIMITY TO OBS, with obs - Weight is 0.1 on obs, 0.75 on 2000 at 2001, linear to 0.15 at 2009
  pr.E<-(0.1*fc[oi[x,1], var])+((0.75-(year-2001)*0.12))*strong.2000+(.9-(0.75-(year-2001)*0.12))*strong.2009
  all.set<-fc[fc$stname==st,var]
  pr.SD<-sd(all.set)/sqrt(15)
  return(c(pr.E, pr.SD))
}

for(i in 1:nrow(oi.priors)){
  oi.priors[i,3:4]<-create.prior(i)
}


###MISSING NATIVE AMERICAN DATA IN VT, DE

##OI SETUP
#calculate prioportion of proxys observed variance due to measurement error
#\rho=\sigma^2_u/sigma^2_w - w is proxy variance, \sigma^2_u=\rho\sigma^2_w

##imputation model
m<-15
fc.imp<-amelia(fc, m=m,
               ts="year", cs="stname", polytime=1, 
               bounds=bounds, overimp=oi,  
               priors=oi.priors,
               idvars=c("state", "statename","adult", "w.adult", "b.adult", "a.adult", 
                        "pctblk1930", "pctimm1930", "pctami1930", "pctblk1960", "pctami1960", 
                        "pctimm1960", "boarding.n", "board"))

# # ##ts structure
# # fc.imp.TS<-amelia(fc, m=m,
# #                ts="year", cs="stname", polytime=1, 
# #                bounds=bounds, overimp=oi, 
# #                #priors=oi.priors,
# #                idvars=c("state", "statename","adult", "w.adult", "b.adult", "a.adult", 
# #                         "pctblk1930", "pctimm1930", "pctami1930", "pctblk1960", "pctami1960", 
# #                         "pctimm1960", "boarding.n", "board"))
# tscsPlot(fc.imp, var="blk.child", cs="MT")

for(i in 1:m){
fc.imp$imputations[[i]]<-
  fc.imp$imputations[[i]]%>%mutate(obs=1:nrow(fc),
  cl.wht.pc=cl.white/wht.child, 
  cl.blk.pc=cl.blk/blk.child, 
  cl.amind.pc=cl.nat.am/amind.child, 
  ent.wht.pc=ent.white/wht.child, 
  ent.blk.pc=ent.blk/blk.child,
  ent.amind.pc=ent.nat.am/amind.child, 
  chpov.wht.pc=wht.child.pov/wht.child, 
  chpov.blk.pc=blk.child.pov/blk.child,
  chpov.amind.pc=amind.child.pov/amind.child, 
  chpovrt=child.pov/child, 
  pctblk=blk/tot,
  pctami=amind/tot,
  pctwht=wht/tot,
  incarrt=(TOTRACEM+TOTRACEF)/adult,
  b.incarrt=(BLACKM+BLACKF)/b.adult,
  b.m.incarrt=(BLACKM)/(b.adult-blk.f),
  b.f.incarrt=(BLACKF)/blk.f,
  a.incarrt=(AIANM+AIANF)/(a.adult),
  a.m.incarrt=(AIANM)/(a.adult-amind.f),
  a.f.incarrt=(AIANF)/amind.f,
  w.incarrt=(WHITEM+WHITEF)/w.adult,
  b.incardisp=b.incarrt/w.incarrt,
  a.incardisp=a.incarrt/w.incarrt,
  bdisp.chpov=(blk.child.pov/blk.child)/(wht.child.pov/wht.child),
  adisp.chpov=(chpov.amind.pc/chpov.wht.pc),
  w.unemp.rt=wht.unemp/(wht.emp+wht.unemp),
  b.unemp.rt=blk.unemp/(blk.emp+blk.unemp),
  a.unemp.rt=amind.unemp/(amind.unemp+amind.emp),
  w.singpar.rt=wht.singpar/wht.child,
  b.singpar.rt=blk.singpar/blk.child,
  a.singpar.rt=amind.singpar/amind.child,
  bw.disp=cl.blk.pc/cl.wht.pc,
  ami.disp=cl.amind.pc/cl.wht.pc,
  year.c=year-2000)
}


ggplot(fc.imp$imputations[[1]])+geom_line(aes(x=year, y=log(cl.amind.pc)))+facet_wrap(~stname)


#### TO SET INTEGERS IF DOING COUNT MODELS
# for(i in 1:m){
#   for(j in 1:ncol(fc)){
#     if(colClass[j]=="integer"){
#       fc.imp$imputations[[i]][,j]<-ceiling(fc.imp$imputations[[i]][,j])
#     }
#   }
# }


###lags and leads
for(i in 1:m){
  fc.imp$imputations[[i]]<-fc.imp$imputations[[i]]%>%group_by(stname)%>%arrange(year.c)%>%
  mutate(incarrt.lag=lag(incarrt), b.incarrt.lag=lag(b.incarrt), a.incarrt.lag=lag(a.incarrt),
         b.m.incarrt.lag=lag(b.m.incarrt), b.f.incarrt.lag=lag(b.f.incarrt),
         a.m.incarrt.lag=lag(a.m.incarrt), a.f.incarrt.lag=lag(a.f.incarrt),
         cl.blk.lag=lag(cl.blk), cl.nat.am.lag=lag(cl.nat.am),
         clrt.blk.lag=lag(cl.blk)/lag(blk.child), clrt.nat.am.lag=lag(cl.nat.am)/lag(amind.child),
         incarrt.lead=lead(incarrt), b.incarrt.lead=lead(b.incarrt), a.incarrt.lead=lead(a.incarrt),
         b.m.incarrt.lead=lead(b.m.incarrt), b.f.incarrt.lead=lead(b.m.incarrt),
         a.m.incarrt.lead=lead(a.m.incarrt), a.f.incarrt.lead=lead(a.f.incarrt),
         cl.blk.lead=lead(cl.blk), cl.nat.am.lead=lead(cl.nat.am),
         clrt.blk.lead=lead(cl.blk)/lead(blk.child), cl.nat.am.lead=lead(cl.nat.am)/lead(amind.child))
}
# ###SANS IMPUTED DATA
# 
# 
# merge.mi.nb<-function(model.imp){
#   beta<-do.call(rbind, lapply(model.imp, function(d) coef(d)))
#   se<-do.call(rbind, lapply(model.imp, function(d) sqrt(diag(vcov(d)))))
#   meld<-mi.meld(q=beta, se=se)
#   out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
#   out$z<-out[,1]/out[,2]
#   out$p<-2*pnorm(-abs(out$z))
#   out<-rbind(out, NA); row.names(out)[nrow(out)]<-"theta"
# 
#   theta.out<-data.frame(matrix(nrow=m, ncol=2))
#   for(i in 1:m){
#     theta.out[i,1]<-model.imp[[i]]$theta
#     theta.out[i,2]<-model.imp[[i]]$SE.theta
#   }
#   
#   theta<-t(mi.meld(q=as.matrix(theta.out[,1]),
#                         se=as.matrix(theta.out[,2])))
# 
#   out<-list(out, theta)
#   
#   return(out)
# }
# 
# # b.cl.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.blk~-1+scale(b.incarrt)+
# #                       scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
# #                       scale(chpov.blk.pc)+scale(pctblk)+
# #                       scale(inst6014_nom)+scale(v.crime.rt)+
# #                       year.c+factor(stname)+offset(log(blk.child)),
# #                     data=d))
# # 
# # b.res<-merge.mi.nb(b.cl.fe)
# 
library(plm)
library(lmtest)

merge.mi<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) d[,1]))
  se<-do.call(rbind, lapply(model.imp, function(d) d[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$z<-out[,1]/out[,2]
  out$p<-round(2*pnorm(-abs(out$z)),3)
  names(out)[1:2]<-c("Beta", "SE")
  
  out<-list(out)
  
  return(out)
}


###PLM CL FE models, PCSE (Beck and Katz)

b.plm<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+log(b.incarrt.lag)+
             log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
             log(chpov.blk.pc)+log(pctblk)+
             inst6014_nom+log(v.crime.rt),
           index=c("stname", "year.c"),
           effect="twoways",
           model="within",
           data=d))
b.plm.pcse<-lapply(b.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.fe<-merge.mi(b.plm.pcse)

a.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="twoways",
                                                  model="within",
                                                  data=d))
a.plm.pcse<-lapply(a.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.fe<-merge.mi(a.plm.pcse)

b.m.plm<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+log(b.m.incarrt.lag)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  
                                                  model="within",
                                                  data=d))
b.m.plm.pcse<-lapply(b.m.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.m.fe<-merge.mi(b.m.plm.pcse)

a.m.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  
                                                  model="within",
                                                  data=d))
a.m.plm.pcse<-lapply(a.m.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.m.fe<-merge.mi(a.m.plm.pcse)

b.f.plm<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  
                                                  model="within",
                                                  data=d))
b.f.plm.pcse<-lapply(b.f.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.f.fe<-merge.mi(b.f.plm.pcse)

a.f.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  
                                                  model="within",
                                                  data=d))
a.f.plm.pcse<-lapply(a.f.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.f.fe<-merge.mi(a.f.plm.pcse)


#####
##LAGGED DV
#####

b.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+
                 log(b.incarrt.lag)+log(b.unemp.rt)+
                 log(b.singpar.rt)+log(blk.lessHS)+
                 log(chpov.blk.pc)+log(pctblk)+
                 inst6014_nom+log(v.crime.rt)+
            log(clrt.blk.lag),
            index=c("stname", "year.c"),
            model="pooling",
            effect=NULL,
            data=d))
b.plm.lag.pcse<-lapply(b.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.lag<-merge.mi(b.plm.lag.pcse)

b.m.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+
                                                        log(b.m.incarrt.lag)+log(b.unemp.rt)+
                                                        log(b.singpar.rt)+log(blk.lessHS)+
                                                        log(chpov.blk.pc)+log(pctblk)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        log(clrt.blk.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      effect=NULL,
                                                      data=d))
b.m.plm.lag.pcse<-lapply(b.m.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.m.lag<-merge.mi(b.m.plm.lag.pcse)

b.f.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+
                                                        sqrt(b.f.incarrt.lag)+log(b.unemp.rt)+
                                                        log(b.singpar.rt)+log(blk.lessHS)+
                                                        log(chpov.blk.pc)+log(pctblk)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        log(clrt.blk.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      effect=NULL,
                                                      data=d))
b.f.plm.lag.pcse<-lapply(b.f.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.f.lag<-merge.mi(b.f.plm.lag.pcse)

a.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                 log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                 sqrt(chpov.amind.pc)+log(pctami)+
                 inst6014_nom+log(v.crime.rt)+
                 sqrt(clrt.nat.am.lag),
               index=c("stname", "year.c"),
               model="pooling",
               data=d))
a.plm.lag.pcse<-lapply(a.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.lag<-merge.mi(a.plm.lag.pcse)

a.m.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
                                                        log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                        sqrt(chpov.amind.pc)+log(pctami)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        sqrt(clrt.nat.am.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      data=d))
a.m.plm.lag.pcse<-lapply(a.m.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.m.lag<-merge.mi(a.m.plm.lag.pcse)

a.f.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
                                                        log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                        sqrt(chpov.amind.pc)+log(pctami)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        sqrt(clrt.nat.am.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      data=d))
a.f.plm.lag.pcse<-lapply(a.f.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.f.lag<-merge.mi(a.f.plm.lag.pcse)

###################################################
###PLM ENTRIES FE models, PCSE (Beck and Katz)
###################################################

b.plm<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+log(b.incarrt.lag)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="twoways",
                                                  model="within",
                                                  data=d))
b.plm.pcse<-lapply(b.plm, function(d) coeftest(d, vcov=vcovHC,cluster=c("stname", "year.c")))
b.ent.fe<-merge.mi(b.plm.pcse)

a.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="twoways",
                                                  model="within",
                                                  data=d))
a.plm.pcse<-lapply(a.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.ent.fe<-merge.mi(a.plm.pcse)

b.m.plm<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+log(b.m.incarrt.lag)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    
                                                    model="within",
                                                    data=d))
b.m.plm.pcse<-lapply(b.m.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.m.ent.fe<-merge.mi(b.m.plm.pcse)

a.m.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    
                                                    model="within",
                                                    data=d))
a.m.plm.pcse<-lapply(a.m.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.m.ent.fe<-merge.mi(a.m.plm.pcse)

b.f.plm<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    
                                                    model="within",
                                                    data=d))
b.f.plm.pcse<-lapply(b.f.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.f.ent.fe<-merge.mi(b.f.plm.pcse)

a.f.plm<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    
                                                    model="within",
                                                    data=d))
a.f.plm.pcse<-lapply(a.f.plm, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
a.f.ent.fe<-merge.mi(a.f.plm.pcse)


# 
# ###LAGGED DV
# 
b.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+
                                                        log(b.incarrt.lag)+log(b.unemp.rt)+
                                                        log(b.singpar.rt)+log(blk.lessHS)+
                                                        log(chpov.blk.pc)+log(pctblk)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        log(clrt.blk.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      effect=NULL,
                                                      data=d))
b.plm.lag.pcse<-lapply(b.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.ent.lag<-merge.mi(b.plm.lag.pcse)



b.m.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+
                                                          log(b.m.incarrt.lag)+log(b.unemp.rt)+
                                                          log(b.singpar.rt)+log(blk.lessHS)+
                                                          log(chpov.blk.pc)+log(pctblk)+
                                                          inst6014_nom+log(v.crime.rt)+
                                                          log(clrt.blk.lag),
                                                        index=c("stname", "year.c"),
                                                        model="pooling",
                                                        effect=NULL,
                                                        data=d))
b.m.plm.lag.pcse<-lapply(b.m.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.m.ent.lag<-merge.mi(b.m.plm.lag.pcse)

b.f.plm.lag<-lapply(fc.imp$imputations, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+
                                                          sqrt(b.f.incarrt.lag)+log(b.unemp.rt)+
                                                          log(b.singpar.rt)+log(blk.lessHS)+
                                                          log(chpov.blk.pc)+log(pctblk)+
                                                          inst6014_nom+log(v.crime.rt)+
                                                          log(clrt.blk.lag),
                                                        index=c("stname", "year.c"),
                                                        model="pooling",
                                                        effect=NULL,
                                                        data=d))
b.f.plm.lag.pcse<-lapply(b.f.plm.lag, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))
b.f.ent.lag<-merge.mi(b.f.plm.lag.pcse)

a.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                                                        log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                        sqrt(chpov.amind.pc)+log(pctami)+
                                                        inst6014_nom+log(v.crime.rt)+
                                                        sqrt(clrt.nat.am.lag),
                                                      index=c("stname", "year.c"),
                                                      model="pooling",
                                                      data=d))
a.plm.lag.pcse<-lapply(a.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.ent.lag<-merge.mi(a.plm.lag.pcse)

a.plm.fd<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                                                        log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                        sqrt(chpov.amind.pc)+log(pctami)+
                                                        inst6014_nom+log(v.crime.rt),
                                                      index=c("stname", "year.c"),
                                                      model="fd",
                                                      data=d))
a.plm.fd.pcse<-lapply(a.plm.fd, function(d) coeftest(d, vcov=vcovBK))
a.ent.fd<-merge.mi(a.plm.fd.pcse)


a.m.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
                                                          log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                          sqrt(chpov.amind.pc)+log(pctami)+
                                                          inst6014_nom+log(v.crime.rt)+
                                                          sqrt(clrt.nat.am.lag),
                                                        index=c("stname", "year.c"),
                                                        model="pooling",
                                                        data=d))
a.m.plm.lag.pcse<-lapply(a.m.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.m.ent.lag<-merge.mi(a.m.plm.lag.pcse)

a.f.plm.lag<-lapply(fc.imp$imputations, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
                                                          log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                          sqrt(chpov.amind.pc)+log(pctami)+
                                                          inst6014_nom+log(v.crime.rt)+
                                                          sqrt(clrt.nat.am.lag),
                                                        index=c("stname", "year.c"),
                                                        model="pooling",
                                                        data=d))
a.f.plm.lag.pcse<-lapply(a.f.plm.lag, function(d) coeftest(d, vcov=vcovBK))
a.f.ent.lag<-merge.mi(a.f.plm.lag.pcse)
