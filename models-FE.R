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
library(rstanarm)
library(xtable)
library(stargazer)

# setwd("C:/Users/kilgore/Dropbox/cw-race/data/")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-functions.r")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-read.r")
# setwd("C:/Users/kilgore/Dropbox/cw-race-paper/")

# ## for laptop
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

fc$bw.disp<-fc$cl.blk.pc/fc$cl.wht.pc
fc$ami.disp<-fc$cl.amind.pc/fc$cl.wht.pc

fc2014<-fc%>%filter(year==2014)%>%summarise(blkcl=sum(cl.blk), whtcl=sum(cl.white),
                                            amicl=sum(cl.nat.am, na.rm=TRUE), latcl=sum(cl.latino),
                                            blkch=sum(blk.child), whtch=sum(wht.child),
                                            amich=sum(amind.child, na.rm=TRUE), latch=sum(latino.child))%>%
  mutate(blkcl.pc=blkcl/blkch, whtcl.pc=whtcl/whtch, amicl.pc=amicl/amich, latcl.pc=latcl/latch)

fc<-fc%>%mutate(obs=1:nrow(fc), 
 chpovrt=child.pov/child, 
 pctblk=blk/tot,
 pctami=amind/tot,
 pctwht=wht/tot,
 incarrt=(TOTRACEM+TOTRACEF)/adult,
 b.incarrt=(BLACKM+BLACKF)/b.adult,
 b.m.incarrt=(BLACKM)/b.adult,
 b.f.incarrt=(BLACKF)/b.adult,
 a.incarrt=(AIANM+AIANF)/a.adult,
 a.m.incarrt=(AIANM)/a.adult, ###WRONG - THIS NEEDS POP BY GENDER
 a.f.incarrt=(AIANF)/a.adult,
 w.incarrt=(WHITEM+WHITEF)/w.adult,
 b.incardisp=b.incarrt/w.incarrt,
 a.incardisp=a.incarrt/w.incarrt,
 bdisp.chpov=(blk.child.pov/blk.child)/(wht.child.pov/wht.child),
 adisp.chpov=(chpov.amind.pc/chpov.wht.pc),
 b.welf.incl=blk.welf/blk.child.pov,
 a.welf.incl=amind.welf/amind.child.pov,
 w.welf.incl=wht.welf/wht.child.pov,
 w.unemp.rt=wht.unemp/(wht.emp+wht.unemp),
 b.unemp.rt=blk.unemp/(blk.emp+blk.unemp),
 a.unemp.rt=amind.unemp/(amind.unemp+amind.emp),
 w.singpar.rt=wht.singpar/wht.child,
 b.singpar.rt=blk.singpar/blk.child,
 a.singpar.rt=amind.singpar/amind.child,
 year.c=year-2000
 )

fc.ineq<-fc

fc.ineq<-fc.ineq%>%
  dplyr::select(cl.blk, b.incarrt, chpov.blk.pc, chpovrt, 
         incarrt, 
         child.pov, 
         inst6014_nom, year.c, pctblk, stname, obs, 
         blk.child, cl.nat.am, 
         a.incarrt, chpov.amind.pc, amind.child, pctami,
         w.incarrt, b.incardisp,  a.incardisp, 
         bdisp.chpov, adisp.chpov, bw.disp, 
         ami.disp, 
         w.unemp.rt, b.unemp.rt, a.unemp.rt,  w.singpar.rt, b.singpar.rt, a.singpar.rt, 
         wht.lessHS, blk.lessHS, amind.lessHS, 
         v.crime.rt,
         cl.white, wht.child, chpov.wht.pc, pctwht,
         b.m.incarrt, b.f.incarrt, a.m.incarrt, a.f.incarrt,
         board, pctblk1930, pctblk1960,  pctami1930, pctami1960, 
         pctimm1930, pctimm1960)%>%
  filter(year.c>2)



colClass<-sapply(fc.ineq, class)

bounds<-cbind(1:ncol(fc.ineq),
              rep(0.00001, ncol(fc.ineq)),
              rep(Inf, ncol(fc.ineq)))

# m=ceiling(max(apply(fc.ineq, 2, function(x){sum(is.na(x))}))/nrow(fc.ineq)*100)
oi.names<-c("chpov.amind.pc", "amind.child", "pctami",
            "adisp.chpov", "a.singpar.rt", "amind.lessHS")

oi.index<-which(colnames(fc.ineq)%in%oi.names)

oi.thresh<-which(fc.ineq$amind.child<10000)

oi<-matrix(nrow=length(oi.index)*length(oi.thresh), ncol=2)
oi[,2]<-rep(oi.index, length(oi.thresh))

for(i in 1:length(oi.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(oi.index)-1
  oi[oi.min:oi.max,1]<-rep(oi.thresh[i], length(oi.index))
  oi.min<-oi.max+1
}


m<-5

fc.imp<-amelia(fc.ineq, m=m,
         ts="year.c", cs="stname", polytime=1, bounds=bounds, p2s=0, #overimp=oi,
         idvars=c("b.f.incarrt", "a.f.incarrt"))

# OItest<-overimpute(fc.imp, "l.incarrt")


for(i in 1:m){
  for(j in 1:ncol(fc.ineq)){
    if(colClass[j]=="integer"){
      fc.imp$imputations[[i]][,j]<-ceiling(fc.imp$imputations[[i]][,j])
    }
  }
}

for(i in 1:m){
  fc.imp$imputations[[i]]<-fc.imp$imputations[[i]]%>%group_by(stname)%>%arrange(year.c)%>%
  mutate(incarrt.lag=lag(incarrt), b.incarrt.lag=lag(b.incarrt), a.incarrt.lag=lag(a.incarrt),
         b.m.incarrt.lag=lag(b.m.incarrt), b.f.incarrt.lag=lag(b.m.incarrt),
         a.m.incarrt.lag=lag(a.m.incarrt), a.f.incarrt.lag=lag(a.f.incarrt),
         cl.blk.lag=lag(cl.blk), cl.nat.am.lag=lag(cl.nat.am),
         clrt.blk.lag=lag(cl.blk)/lag(blk.child), clrt.nat.am.lag=lag(cl.nat.am)/lag(amind.child),
         incarrt.lead=lead(incarrt), b.incarrt.lead=lead(b.incarrt), a.incarrt.lead=lead(a.incarrt),
         b.m.incarrt.lead=lead(b.m.incarrt), b.f.incarrt.lead=lead(b.m.incarrt),
         a.m.incarrt.lead=lead(a.m.incarrt), a.f.incarrt.lead=lead(a.f.incarrt),
         cl.blk.lead=lead(cl.blk), cl.nat.am.lead=lead(cl.nat.am),
         clrt.blk.lead=lead(cl.blk)/lead(blk.child), cl.nat.am.lead=lead(cl.nat.am)/lead(amind.child))
}
###SANS IMPUTED DATA

library(MASS)

merge.mi.nb<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) coef(d)))
  se<-do.call(rbind, lapply(model.imp, function(d) sqrt(diag(vcov(d)))))
  meld<-mi.meld(q=beta, se=se)
  out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$z<-out[,1]/out[,2]
  out$p<-2*pnorm(-abs(out$z))
  out<-rbind(out, NA); row.names(out)[nrow(out)]<-"theta"

  theta.out<-data.frame(matrix(nrow=m, ncol=2))
  for(i in 1:m){
    theta.out[i,1]<-model.imp[[i]]$theta
    theta.out[i,2]<-model.imp[[i]]$SE.theta
  }
  
  theta<-t(mi.meld(q=as.matrix(theta.out[,1]),
                        se=as.matrix(theta.out[,2])))

  out<-list(out, theta)
  
  return(out)
}

# b.cl.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.blk~-1+scale(b.incarrt)+
#                       scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
#                       scale(chpov.blk.pc)+scale(pctblk)+
#                       scale(inst6014_nom)+scale(v.crime.rt)+
#                       year.c+factor(stname)+offset(log(blk.child)),
#                     data=d))
# 
# b.res<-merge.mi.nb(b.cl.fe)

library(plm)
library(lmtest)


###PLM FE models, PCSE (Beck and Katz)
pdat<-as.data.frame(fc.imp$imputations[[1]])

b.plm<-plm(as.numeric(scale(cl.blk/blk.child))~as.numeric(scale(b.incarrt))+as.numeric(scale(b.incarrt.lag))+
             as.numeric(scale(b.unemp.rt))+as.numeric(scale(b.singpar.rt))+as.numeric(scale(blk.lessHS))+
             as.numeric(scale(chpov.blk.pc))+as.numeric(scale(pctblk))+
             as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
           index=c("stname", "year.c"),
           effect="individual",
           model="within",
           data=pdat)
b.plm.pcse<-coeftest(b.plm, vcov=vcovBK,cluster=c("stname", "year.c"))

b.m.plm<-plm(as.numeric(scale(cl.blk/blk.child))~as.numeric(scale(b.m.incarrt))+as.numeric(scale(b.m.incarrt.lag))+
            as.numeric(scale(b.unemp.rt))+as.numeric(scale(b.singpar.rt))+as.numeric(scale(blk.lessHS))+
             as.numeric(scale(chpov.blk.pc))+as.numeric(scale(pctblk))+
             inst6014_nom+as.numeric(scale(v.crime.rt)), 
           index=c("stname", "year.c"),
           effect="individual",
           model="within",
           data=pdat)
b.m.plm.pcse<-coeftest(b.m.plm, vcov=vcovBK)

b.f.plm<-plm(as.numeric(scale(cl.blk/blk.child))~as.numeric(scale(b.f.incarrt))+as.numeric(scale(b.f.incarrt.lag))+
               as.numeric(scale(b.unemp.rt))+as.numeric(scale(b.singpar.rt))+as.numeric(scale(blk.lessHS))+
               as.numeric(scale(chpov.blk.pc))+as.numeric(scale(pctblk))+
               as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
             index=c("stname", "year.c"),
             effect="individual",
             model="within",
             data=pdat)
b.f.plm.pcse<-coeftest(b.f.plm, vcov=vcovBK)

a.plm<-plm(as.numeric(scale(cl.nat.am/amind.child))~as.numeric(scale(a.incarrt))+
             as.numeric(scale(a.incarrt.lag))+
             as.numeric(scale(a.unemp.rt))+
             as.numeric(scale(a.singpar.rt))+as.numeric(scale(amind.lessHS))+
             as.numeric(scale(chpov.amind.pc))+as.numeric(scale(pctami))+
             as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
           index=c("stname", "year.c"),
           effect="individual",
           model="within",
           data=pdat)

a.plm.pcse<-coeftest(a.plm, vcov=vcovBK)

pdat.filt<-pdat%>%filter(year.c>6)
a.plm1<-plm(as.numeric(scale(cl.nat.am/amind.child))~as.numeric(scale(a.incarrt))+
             as.numeric(scale(a.incarrt.lag))+
             as.numeric(scale(a.unemp.rt))+
             as.numeric(scale(a.singpar.rt))+as.numeric(scale(amind.lessHS))+
             as.numeric(scale(chpov.amind.pc))+as.numeric(scale(pctami))+
             as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
           index=c("stname", "year.c"),
           effect="individual",
           model="within",
           data=pdat.filt)

a.plm.pcse<-coeftest(a.plm, vcov=vcovBK)

a.m.plm<-plm(as.numeric(scale(cl.nat.am/amind.child))~as.numeric(scale(a.m.incarrt))+as.numeric(scale(a.m.incarrt.lag))+
               as.numeric(scale(a.unemp.rt))+as.numeric(scale(a.singpar.rt))+as.numeric(scale(amind.lessHS))+
               as.numeric(scale(chpov.amind.pc))+as.numeric(scale(pctami))+
               as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
             index=c("stname", "year.c"),
             model="within",
             effect="individual",
             data=pdat)

a.f.plm<-plm(as.numeric(scale(cl.nat.am/amind.child))~as.numeric(scale(a.f.incarrt))+as.numeric(scale(a.f.incarrt.lag))+
               as.numeric(scale(a.unemp.rt))+as.numeric(scale(a.singpar.rt))+as.numeric(scale(amind.lessHS))+
               as.numeric(scale(chpov.amind.pc))+as.numeric(scale(pctami))+
               as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt)), 
             index=c("stname", "year.c"),
             model="within",
             effect="individual",
             data=pdat)

a.f.plm.pcse<-coeftest(a.f.plm, vcov=vcovBK)

###LAGGED DV, LEAD INCAR - WEIRD SHIT BETWEEN LM AND PLM

b.plm.lag<-plm(as.numeric(scale(cl.blk/blk.child))~as.numeric(scale(b.incarrt))+
                 as.numeric(scale(b.incarrt.lag))+as.numeric(scale(b.incarrt.lead))+
                 as.numeric(scale(b.unemp.rt))+
                 as.numeric(scale(b.singpar.rt))+
                 as.numeric(scale(blk.lessHS))+
                 as.numeric(scale(chpov.blk.pc))+as.numeric(scale(pctblk))+
                 as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt))+
            as.numeric(scale(clrt.blk.lag)),
            index=c("stname", "year.c"),
            model="pooling",
            effect=NULL, 
            data=pdat)

b.plm.lag.pcse<-coeftest(b.plm.lag, vcov=vcovBK)

a.plm.lag<-plm(as.numeric(scale(cl.blk/blk.child))~as.numeric(scale(a.incarrt))+as.numeric(scale(a.incarrt.lag))+
                 as.numeric(scale(a.incarrt.lead))+
                 as.numeric(scale(b.unemp.rt))+as.numeric(scale(b.singpar.rt))+as.numeric(scale(blk.lessHS))+
                 as.numeric(scale(chpov.blk.pc))+as.numeric(scale(pctblk))+
                 as.numeric(scale(inst6014_nom))+as.numeric(scale(v.crime.rt))+
                 as.numeric(scale(clrt.nat.am.lag)),
               index=c("stname", "year.c"),
               model="pooling",
               data=pdat)
a.plm.lag.pcse<-coeftest(a.plm.lag, vcov=vcovBK)


###PLACEBO FE



b.cl.fe1<-lapply(fc.imp$imputations, function(d) glm.nb(cl.blk~-1+scale(b.incarrt)+scale(b.incarrt.lag)+
                                                         scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
                                                         scale(chpov.blk.pc)+scale(pctblk)+
                                                         scale(inst6014_nom)+scale(v.crime.rt)+
                                                         year.c+factor(stname)+offset(log(blk.child)),
                                                       data=d))

b.res1<-merge.mi.nb(b.cl.fe)

b.cl.fe2<-lapply(fc.imp$imputations, function(d) glm.nb(cl.blk~-1+scale(b.incarrt)+scale(b.incarrt.lag)+scale(cl.blk.lag)+
                                                          scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
                                                          scale(chpov.blk.pc)+scale(pctblk)+
                                                          scale(inst6014_nom)+scale(v.crime.rt)+
                                                          year.c+factor(stname)+offset(log(blk.child)),
                                                        data=d))

b.res2<-merge.mi.nb(b.cl.fe)


b.cl.m.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.blk~-1+scale(b.m.incarrt)+
               scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
               scale(chpov.blk.pc)+scale(pctblk)+
               scale(inst6014_nom)+scale(v.crime.rt)+
               year.c+factor(stname)+offset(log(blk.child)), 
             data=d))

b.m.res<-merge.mi.nb(b.cl.m.fe)

b.cl.f.fe<-lapply(fc.imp$imputations, function(d) 
  glm.nb(cl.blk~-1+scale(b.f.incarrt)+
  scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
    scale(chpov.blk.pc)+scale(pctblk)+
    scale(inst6014_nom)+scale(v.crime.rt)+
                 year.c+factor(stname)+offset(log(blk.child)), 
               data=d))

b.f.res<-merge.mi.nb(b.cl.f.fe)


a.cl.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.nat.am~-1+scale(a.incarrt)+
              scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
                scale(chpov.amind.pc)+scale(pctami)+
                scale(inst6014_nom)+scale(v.crime.rt)+
              year.c+factor(stname)+offset(log(amind.child)), 
            data=d))

a.res<-merge.mi.nb(a.cl.fe)

a.cl.fe1<-lapply(fc.imp$imputations, function(d) glm.nb(cl.nat.am~-1+scale(a.incarrt)+scale(a.incarrt.lag)+
                                                         scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
                                                         scale(chpov.amind.pc)+scale(pctami)+
                                                         scale(inst6014_nom)+scale(v.crime.rt)+
                                                         year.c+factor(stname)+offset(log(amind.child)), 
                                                       data=d))

a.res1<-merge.mi.nb(a.cl.fe1)

a.cl.fe2<-lapply(fc.imp$imputations, function(d) glm.nb(cl.nat.am~-1+scale(a.incarrt)+scale(a.incarrt.lag)+scale(cl.nat.am.lag)+
                                                         scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
                                                         scale(chpov.amind.pc)+scale(pctami)+
                                                         scale(inst6014_nom)+scale(v.crime.rt)+
                                                         year.c+factor(stname)+offset(log(amind.child)), 
                                                       data=d))

a.res2<-merge.mi.nb(a.cl.fe2)

a.cl.m.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.nat.am~-1+scale(a.m.incarrt)+
                scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
                scale(chpov.amind.pc)+scale(pctami)+
                scale(inst6014_nom)+scale(v.crime.rt)+
                year.c+factor(stname)+offset(log(amind.child)),
              data=d))

a.m.res<-merge.mi.nb(a.cl.m.fe)

a.cl.f.fe<-lapply(fc.imp$imputations, function(d) glm.nb(cl.nat.am~-1+scale(a.f.incarrt)+
               scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
               scale(chpov.amind.pc)+scale(pctami)+
               scale(inst6014_nom)+scale(v.crime.rt)+
              year.c+factor(stname)+offset(log(amind.child)), 
            data=d))

a.f.res<-merge.mi.nb(a.cl.f.fe)


###RE DISP MODELS
b.disp<-lapply(fc.imp$imputations, function(d) lmer(log(bw.disp)~-1+scale(b.incardisp)+
                                                      scale(bdisp.chpov)+
                                                      scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
                                                      scale(I(blk.lessHS/wht.lessHS))+
                                                      scale(pctblk)+
                                                      scale(inst6014_nom)+scale(v.crime.rt)+
                                                      year.c+
                                                      (1|stname),
                                                    data=d))

a.disp<-lapply(fc.imp$imputations, function(d) lmer(log(ami.disp)~-1+scale(a.incardisp)+
                                                      scale(adisp.chpov)+
                                                      scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
                                                      scale(I(amind.lessHS/wht.lessHS))+
                                                      scale(pctami)+
                                                      scale(inst6014_nom)+scale(v.crime.rt)+
                                                      year.c+
                                                      (1|stname),
                                                    data=d))

###MAKE TABLES
library(texreg)

b.tab<-htmlreg(list(b.cl.fe[[1]], b.cl.fe1[[1]], b.cl.fe2[[1]]),
              caption="Black foster care caseloads, 
              negative binomial models, state fixed effects,
              Model 2 + lagged incarceration, Model 3 + lagged FC caseload", caption.above=TRUE)

b.gender.tab<-htmlreg(list(b.cl.m.fe[[1]], b.cl.f.fe[[1]]),caption="Black foster care caseloads, incarceration by gender,
              negative binomial models, state fixed effects,
              Model 2 + lagged incarceration, Model 3 + lagged FC caseload", caption.above=TRUE)

a.tab<-htmlreg(list(a.cl.fe[[1]], a.cl.fe1[[1]], a.cl.fe2[[1]]),
               caption="Native American foster care caseloads, 
              negative binomial models, state fixed effects,
              Model 2 + lagged incarceration, Model 3 + lagged FC caseload", caption.above=TRUE)

a.gender.tab<-htmlreg(list(a.cl.m.fe[[1]], a.cl.f.fe[[1]]),
                      caption="Native American foster care caseloads, incarceration by gender,
              negative binomial models, state fixed effects,
                      Model 2 + lagged incarceration, Model 3 + lagged FC caseload", caption.above=TRUE)

b.re.tab<-htmlreg(b.disp[[1]], caption="log(Black/White foster care caseload disproportion), 
              multilevel linear model, state random effects", caption.above=TRUE)
a.re.tab<-htmlreg(a.disp[[1]],caption="log(Native American/White foster care caseload disproportion), 
              multilevel linear model, state random effects")



