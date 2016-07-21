#########
# These models replicate models from "Saving Children" for racial inequalities. Expanding to Nat. Am., Latino and working through separate outcomes (i.e LOS, Nplacement, placement setting, casegoal) at the individual level is the next step
rm(list=ls())
set.seed(1)
library(plyr)
library(merTools)
library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(rstanarm)
library(xtable)

# setwd("C:/Users/kilgore/Dropbox/cw-race/data/")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-functions.r")
# source("C:/Users/kilgore/Dropbox/cw-race/cw-race-read.r")
# setwd("C:/Users/kilgore/Dropbox/cw-race-paper/")
# 
### for libra
setwd("~/cw-race/data/")
source("~/cw-race/cw-race-functions.r")
source("~/cw-race/cw-race-read.r")


# setwd("H:/cw-race/data/")
# source("H:/cw-race/cw-race-functions.r")
# source("H:/cw-race/cw-race-read.r")

fc$bw.disp<-fc$cl.blk.pc/fc$cl.wht.pc
fc$lw.disp<-fc$cl.lat.pc/fc$cl.wht.pc
fc$ami.disp<-fc$cl.amind.pc/fc$cl.wht.pc

###big dip in HI 2010, unrealistic, impute it
fc[which(fc$stname=="MI"), c("HISPM", "HISPF") ]<-0
index<-names(fc)[grep("amind", names(fc))]
fc[which(fc$stname=="HI"), index]
##DON'T TRUST THE ZEROES, NA


##PROBLEMATIC LATINO INCAR DATA:
##MI under, MD no data
###FIX DROPPED HAWAII

###HI IS WEIRD, IMPUTE NATAM VALUES
# fc.ineq[fc.ineq$stname=="HI", c(grep("ami", colnames(fc.ineq)))]<-NA
# fc.ineq[fc.ineq$stname=="HI", "cl.nat.am"]<-NA

fc2014<-fc%>%filter(year==2014)%>%summarise(blkcl=sum(cl.blk), whtcl=sum(cl.white),
                                            amicl=sum(cl.nat.am, na.rm=TRUE), latcl=sum(cl.latino),
                                            blkch=sum(blk.child), whtch=sum(wht.child),
                                            amich=sum(amind.child, na.rm=TRUE), latch=sum(latino.child))%>%
  mutate(blkcl.pc=blkcl/blkch, whtcl.pc=whtcl/whtch, amicl.pc=amicl/amich, latcl.pc=latcl/latch)

### BOARDING SCHOOL DATA
### FROM http://www.archives.gov/research/native-americans/bia-guide/schools.html
### SAVING CHILDREN VARS
fc<-fc%>%mutate(obs=1:nrow(fc), 
 chpovrt=child.pov/child, 
 pctblk=blk/tot,
 pctlat=latino/tot,
 pctami=amind/tot,
 pctwht=wht/tot,
 incarrt=ifelse(TOTRACEM>0, (TOTRACEM+TOTRACEF)/adult, NA),
 b.incarrt=ifelse(BLACKM>0, (BLACKM+BLACKF)/b.adult, NA),
 l.incarrt=ifelse(HISPM>0, (HISPM+HISPF)/l.adult,NA),
 a.incarrt=ifelse(AIANM>0, (AIANM+AIANF)/a.adult,NA),
 w.incarrt=ifelse(WHITEM>0, (WHITEM+WHITEF)/w.adult,NA),
 b.incardisp=ifelse(BLACKM>0, b.incarrt/w.incarrt,NA),
 l.incardisp=ifelse(HISPM>0, l.incarrt/w.incarrt,NA),
 a.incardisp=ifelse(AIANM>0, a.incarrt/w.incarrt,NA),
 bdisp.chpov=(blk.child.pov/blk.child)/(wht.child.pov/wht.child),
 ldisp.chpov=(chpov.latino.pc/chpov.wht.pc),
 adisp.chpov=(chpov.amind.pc/chpov.wht.pc),
 b.welf.incl=blk.welf/blk.child.pov,
 l.welf.incl=latino.welf/latino.child.pov,
 a.welf.incl=amind.welf/amind.child.pov,
 w.welf.incl=wht.welf/wht.child.pov,
 # b.snap.incl=blk.snap/blk.child.pov,
 # l.snap.incl=latino.snap/latino.child.pov,
 # a.snap.incl=amind.snap/amind.child.pov,
 # w.snap.incl=wht.snap/wht.child.pov,
 w.unemp.rt=wht.unemp/(wht.emp+wht.unemp),
 b.unemp.rt=blk.unemp/(blk.emp+blk.unemp),
 a.unemp.rt=amind.unemp/(amind.unemp+amind.emp),
 l.unemp.rt=latino.unemp/(latino.emp+latino.unemp),
 w.singpar.rt=wht.singpar/wht.child,
 b.singpar.rt=blk.singpar/blk.child,
 a.singpar.rt=amind.singpar/amind.child,
 l.singpar.rt=latino.singpar/latino.child,
 year.c=year-2007
 )

fc.ineq<-fc

### SOME MISSING INCARCERATION DATA REPORTED AS 0 - AK in 2013
fc$year.p<-fc$year-2000
##TS PLOTS



race.pc.ts<-ggplot(data=fc, aes(x=year.p, y=cl.blk.pc))+geom_line(aes(color="Black"))+
  geom_line(aes(y=cl.amind.pc, color="Native Am"))+
  geom_line(aes(y=cl.lat.pc, color="Latino"))+
  xlab("Year")+ylab("Caseload per cap")+
  ylim(0, 0.20)+
  facet_wrap(~stname)

welf.pc.ts<-ggplot(data=fc, aes(x=year, y=b.welf.incl))+geom_line(aes(color="Black"))+
  geom_line(aes(y=a.welf.incl, color="Native Am"))+
  geom_line(aes(y=l.welf.incl, color="Latino"))+
  xlab("Year")+ylab("Cash welfare recipients per children in poverty")+
  ylim(0, 0.5)+
  facet_wrap(~stname)

welf.disp.ts<-ggplot(data=fc, aes(x=year, y=b.welf.incl/w.welf.incl))+geom_line(aes(color="B/W"))+
  geom_line(aes(y=a.welf.incl/w.welf.incl, color="Native Am/W"))+
  geom_line(aes(y=l.welf.incl/w.welf.incl, color="Latino/W"))+
  xlab("Year")+ylab("Welfare inequality")+
  ylim(0, 1)+
  facet_wrap(~stname)

ggsave("race-pc-ts.pdf", race.pc.ts, width=11, height=8)

disp.ts<-ggplot(data=fc, aes(x=year.p, y=bw.disp))+geom_line(aes(color="B/W"))+
  geom_line(aes(y=lw.disp, color="L/W"))+
  geom_line(aes(y=ami.disp, color="Nat.Am./W"))+
  xlab("Year")+ylab("Caseload disproportion")+
  ylim(0,15)+
  facet_wrap(~stname)

ggsave("race-disp-ts.pdf", disp.ts, width=11, height=8)


disp.incar.ts<-ggplot(data=fc, aes(x=year.p, y=b.incardisp))+geom_line(aes(color="B/W"))+
  geom_line(aes(y=l.incardisp, color="L/W"))+
  geom_line(aes(y=a.incardisp, color="Nat.Am./W"))+
  xlab("Year")+ylab("Incarceration disproportion")+
  facet_wrap(~stname)
ggsave("race-incardisp-ts.pdf", disp.incar.ts, width=11, height=8)


incar.ts<-ggplot(data=fc, aes(x=year.p, y=b.incarrt))+geom_line(aes(color="Black"))+
  geom_line(aes(y=l.incarrt, color="Latino"))+
  geom_line(aes(y=a.incarrt, color="Native Am."))+
  xlab("Year")+ylab("Incarceration rates")+
  facet_wrap(~stname)
  #scale_x_continuous(breaks=c(2008,2013))
ggsave("race-incarpc-ts.pdf", incar.ts, width=11, height=8)


### HAWAII COUNTS AMIND DIFFERENTLY IN AFCARS / CENSUS. MAYBE NATIVE HAWAIIAN ARE INCLUDED IN AFCARS, NOT CENSUS COUNTS? DROPPING FOR NOW
###MI
###select vars to include

fc.ineq<-fc.ineq%>%
  dplyr::select(cl.blk, b.incarrt, chpov.blk.pc, chpovrt, 
         incarrt, 
         eitc.st, 
         child.pov, 
         inst6014_nom, year.c, pctblk, stname, obs, 
         blk.child, cl.latino, l.incarrt, 
         chpov.latino.pc, pctlat, latino.child, cl.nat.am, 
         a.incarrt, chpov.amind.pc, amind.child, pctami,
         w.incarrt, b.incardisp, l.incardisp, a.incardisp, 
         bdisp.chpov, ldisp.chpov, adisp.chpov, bw.disp, 
         lw.disp, ami.disp, b.welf.incl, a.welf.incl, l.welf.incl, 
         w.welf.incl, 
         #a.snap.incl, b.snap.incl, l.snap.incl, w.snap.incl,
         w.unemp.rt, b.unemp.rt, a.unemp.rt, l.unemp.rt, w.singpar.rt, b.singpar.rt, a.singpar.rt, 
         l.singpar.rt, wht.lessHS, blk.lessHS, amind.lessHS, latino.lessHS,
         v.crime.rt,
         cl.white, wht.child, chpov.wht.pc, pctwht)

### Descriptive table
fc.desc<-fc.ineq%>%select(-stname, -obs)
Mean<-round(sapply(na.omit(fc.desc), mean),3)
SD<-round(sapply(na.omit(fc.desc), sd),3)
Minimum<-round(sapply(na.omit(fc.desc), min),3)
Maximum<-round(sapply(na.omit(fc.desc), max),3)
descriptives<-xtable(cbind(Mean, SD, Minimum, Maximum), caption="Descriptive Statistics")
print(descriptives, file="desctable.tex")


colClass<-sapply(fc.ineq, class)

bounds<-cbind(1:ncol(fc.ineq),
              rep(0.001, ncol(fc.ineq)),
              rep(Inf, ncol(fc.ineq)))

m=ceiling(max(apply(fc.ineq, 2, function(x){sum(is.na(x))}))/nrow(fc.ineq)*100)

fc.imp<-amelia(fc.ineq, m=m,
         ts="year.c", cs="stname", polytime=1, bounds=bounds, p2s=0)

# OItest<-overimpute(fc.imp, "l.incarrt")


for(i in 1:m){
  for(j in 1:ncol(fc.ineq)){
    if(colClass[j]=="integer"){
      fc.imp$imputations[[i]][,j]<-ceiling(fc.imp$imputations[[i]][,j])
    }
  }
}



b.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.blk~scale(b.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(blk.child),
      data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

w.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.white~scale(w.incarrt)+scale(incarrt)+
          scale(w.unemp.rt)+scale(w.singpar.rt)+scale(wht.lessHS)+
          scale(chpov.wht.pc)+scale(pctwht)+scale(w.welf.incl)+
          scale(inst6014_nom)+scale(v.crime.rt)+
          year.c+
          (1|stname) + (1|obs), family=poisson, offset=log(wht.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

l.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.latino~scale(l.incarrt)+
        scale(l.unemp.rt)+scale(l.singpar.rt)+scale(latino.lessHS)+
        scale(chpov.latino.pc)+scale(pctlat)+scale(l.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(latino.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

a.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.nat.am~scale(a.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(amind.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

## Disproportion models
b.disp<-lapply(fc.imp$imputations, function(d) lmer(log(bw.disp)~scale(b.incardisp)+
        scale(bdisp.chpov)+
        scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
        scale(I(blk.lessHS/wht.lessHS))+scale(I(b.welf.incl/w.welf.incl))+
        scale(pctblk)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

a.disp<-lapply(fc.imp$imputations, function(d) lmer(log(ami.disp)~scale(a.incardisp)+
        scale(adisp.chpov)+
        scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
        scale(I(amind.lessHS/wht.lessHS))+scale(I(a.welf.incl/w.welf.incl))+
        scale(pctami)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

l.disp<-lapply(fc.imp$imputations, function(d) lmer(log(lw.disp)~scale(l.incardisp)+
        scale(ldisp.chpov)+
        scale(I(l.unemp.rt/w.unemp.rt))+scale(I(l.singpar.rt/w.singpar.rt))+
        scale(I(latino.lessHS/wht.lessHS))+scale(I(l.welf.incl/w.welf.incl))+
        scale(pctlat)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

# b.ineq.mi.fe<-mi.meld(ldply(b.ineq, fixef)[,-1], ldply(b.ineq, se.fixef)[,-1])
# a.ineq.mi.fe<-mi.meld(ldply(a.ineq, fixef)[,-1], ldply(a.ineq, se.fixef)[,-1])
# l.ineq.mi.fe<-mi.meld(ldply(l.ineq, fixef)[,-1], ldply(l.ineq, se.fixef)[,-1])
# 
# a.disp.mi.fe<-mi.meld(ldply(a.disp, fixef)[,-1], ldply(a.disp, se.fixef)[,-1])
# b.disp.mi.fe<-mi.meld(ldply(b.disp, fixef)[,-1], ldply(b.disp, se.fixef)[,-1])
# l.disp.mi.fe<-mi.meld(ldply(l.disp, fixef)[,-1], ldply(l.disp, se.fixef)[,-1])


### Merges imputation results according to Rubin for beta, se
### constructs parameter estimates from posterior sims with arm::sim()

makeMIRegTab<-function(x){
  r<-mi.meld(ldply(x, fixef)[,-1], ldply(x, se.fixef)[,-1])
  beta<-t(r[[1]])
  se<-t(r[[2]])
  fixef.list<-NULL
  for(i in 1:length(x)){
    newsim<-sim(x[[i]], n.sims=1000)
    fixef.list<-rbind(fixef.list, fixef(newsim))
  }
  beta.sim<-round(colMeans(fixef.list),3)
  se.sim<-round(apply(fixef.list, 2, sd),3)
  ci.sim<-round(as.data.frame(t(apply(fixef.list, 2, function(x)quantile(x, c(0.025, 0.975))))),3)
  results<-as.data.frame(cbind(beta.sim, se.sim, ci.sim))
  names(results)<-c("Beta", "SE", "95 percent CI, lower", "95 percent CI, upper")
  return(results)
}

b.c.tab<-makeMIRegTab(b.ineq)
row.names(b.c.tab)<- c("Intercept", "Black incarceration rate", "Black unemployment rate", "Black single parent rate",
                       "Black adults w/o HS rate", "Black child poverty rate", "Percent Black population",
                       "Black welfare enrollment per child poverty",
                       "Leg. ideology", "Violent crime rate", "Year")
print(xtable(b.c.tab, caption="Black foster care caseloads. Poisson multilevel regression", label="b.c.tab", digits=3),
      type="latex", file="b_c_tab.tex")

a.c.tab<-makeMIRegTab(a.ineq)
row.names(a.c.tab)<- c("Intercept", "Native Am. incarceration rate", "Native Am. unemployment rate", "Native Am. single parent rate",
                       "Native Am. adults w/o HS rate", "Native Am. child poverty rate", "Percent Native Am. population",
                       "Native Am. welfare enrollment per child poverty",
                       "Leg. ideology", "Violent crime rate", "Year")
print(xtable(a.c.tab, caption="Native American foster care caseloads. Poisson multilevel regression", label="a.c.tab", digits=3),
      type="latex", file="a_c_tab.tex")

l.c.tab<-makeMIRegTab(l.ineq)
print(xtable(l.c.tab, caption="Latino foster care caseloads. Poisson multilevel regression", label="l.c.tab", digits=3),
      type="latex", file="l_c_tab.tex")
row.names(l.c.tab)<- c("Intercept", "Latino Incarceration rate", "Latino Unemployment rate", "Latino Single parent rate",
                       "Latino Adults w/o HS rate", "Latino Child poverty rate", "Percent Latino population",
                       "Latino welfare enrollment per child poverty",
                       "Leg. Ideology", "Violent crime rate", "Year")


c.names<-c("Incarceration rate", "Unemployment rate", "Single parent rate",
           "Adults w/o HS rate", "Child poverty rate", "Percent of population",
           "Leg. Ideology", "Violent crime rate", "Year")

row.names(b.c.tab)<-row.names(a.c.tab)<-row.names(l.c.tab)<-c.names

count.tab<-rbind(b.c.tab,a.c.tab, l.c.tab)

b.d.tab<-makeMIRegTab(b.disp)
row.names(b.d.tab)<- c("Intercept", "Black/White Incarceration rate", "Black/White Unemployment rate", "Black/White Single parent rate",
                       "Black/White Adults w/o HS rate", "Black/White Child poverty rate", "Black/White welfare enroll. per child pov.",
                       "Percent Black population",
                       "Leg. Ideology", "Violent crime rate", "Year")
print(xtable(b.d.tab, caption="Black/White foster care caseload disproportion. Multilevel linear regression", label="b.d.tab", digits=3),
      type="latex", file="b_d_tab.tex")

a.d.tab<-makeMIRegTab(a.disp)
row.names(a.d.tab)<- c("Intercept", "Native Am./White Incarceration rate", "Native Am./White Unemployment rate", "Native Am./White Single parent rate",
                       "Native Am./White Adults w/o HS rate", "Native Am./White Child poverty rate", "Native Am./White welfare enroll. per child pov.",
                       "Percent Native Am.",
                       "Leg. Ideology", "Violent crime rate", "Year")
print(xtable(a.d.tab, caption="Native American/White foster care caseload disproportion. Multilevel linear regression", label="a.d.tab", digits=3),
      type="latex", file="a_d_tab.tex")

l.d.tab<-makeMIRegTab(l.disp)
row.names(l.d.tab)<- c("Intercept", "Latino/White incarceration rate", "Latino/White unemployment rate", "Latino/White single parent rate",
                       "Latino/White adults w/o HS rate", "Latino/White child poverty rate", "Latino/White welfare enroll. per child pov.",
                       "Percent Latino population",
                       "Leg. Ideology", "Violent crime rate", "Year")
print(xtable(l.d.tab, caption="Latino/White foster care caseload disproportion. Multilevel linear regression", label="l.c.tab", digits=3),
      type="latex", file="l_d_tab.tex")

### MAKE TABLE by var for all groups


### BY COMPONENT, need to make individual lists for RE components

# b.ineq.mi.re<-mi.meld(ldply(b.ineq, ranef, ldply(b.ineq, se.ranef)[,-1])
# a.ineq.mi.re<-mi.meld(ldply(a.ineq, ranef)[,-1], ldply(a.ineq, se.ranef)[,-1])
# l.ineq.mi.re<-mi.meld(ldply(l.ineq, ranef)[,-1], ldply(l.ineq, se.ranef)[,-1])
# 
# a.disp.mi.re<-mi.meld(ldply(a.disp, ranef)[,-1], ldply(a.disp, se.ranef)[,-1])
# b.disp.mi.re<-mi.meld(ldply(b.disp, ranef)[,-1], ldply(b.disp, se.ranef)[,-1])
# l.disp.mi.re<-mi.meld(ldply(l.disp, ranef)[,-1], ldply(l.disp, se.ranef)[,-1])


## Bayesian multilevel for robustness
# # library(rstanarm)
# fit<-stan_lmer(cl.nat.am~scale(a.incarrt)+
#              scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
#              scale(chpov.amind.pc)+scale(pctami)+
#              scale(inst6014_nom)+scale(v.crime.rt)+
#              year.c+
#              (1+year.c|stname) +(1|obs), family=poisson, offset=log(amind.child),
#            data=fc.imp$imputations[[1]])

# FOLLOWING http://stats.stackexchange.com/questions/117605/lmer-with-multiply-imputed-data
# imputeFEs<-ldply(l.disp, FEsim, n.sims=1000)
# imputeREs <- ldply(l.disp, REsim, nsims = 1000)
# modelFixedEff() does this now - not sure I trust it, taking sample means across imps, not Rubin's method 


# names.m1<-c("Black incarceration", "Black unemployment", "Black single parent", "Black less than HS", "Black child poverty", "Percent Black pop", "State leg. ideology", "Violent crime per cap", "Year", "Latino incarceration", "Latino unemployment", "Latino single parent", "Latino less than HS",  "Latino child poverty", "Percent Latino pop","Deportations per foreign born", "Native American incarceration",  "Native American unemployment", "Native American single parent", "Native American less than HS", "Native American child poverty", "Percent Nat. Am. pop")
# 
# texreg(list(b.ineq.glmer, l.ineq.glmer, a.ineq.glmer), file="count-models.tex",
#        caption="Caseloads by race, state intercepts, state|year slopes", 
#        custom.model.names =c("Black", "Latino", "Native American"), custom.coef.names=names.m1, caption.above=TRUE)
# 
# names.m2<-c("B/W incarceration", "B/W child poverty", "B/W unemployment", "B/W single parent", "B/W less than HS", "Percent Black pop", "State leg. ideology", "Violent crime per cap", "Year", "L/W incarceration", "L/W child poverty","Deportations per foreign born", "L/W unemployment", "L/W single parent", "L/W less than HS", "Percent Latino pop", "N.A./W incarceration", "N.A./W child poverty", "N.A./W unemployment", "N.A./W single parent", "N.A./W less than HS", "Percent Nat. Am. pop")
# 
# texreg(list(b.disp, l.disp, a.disp), file="disp-models.tex",
#         caption="Caseload disproportion by race, state intercepts, state|year slopes", 
#         custom.model.names = c("Black", "Latino", "Native American"), custom.coef.names = names.m2, caption.above=TRUE)
# 

### FE MODELS
# 
# b.count.fe<-lapply(fc.imp$imputations, function(d) glm(cl.blk~-scale(b.incarrt)+
#                                                        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
#                                                        scale(chpov.blk.pc)+scale(pctblk)+
#                                                        scale(inst6014_nom)+scale(v.crime.rt)+
#                                                        year.c+
#                                                        factor(stname), family=quasipoisson, offset=log(blk.child),
#                                                      data=d))
# # 
# l.count.fe<-lapply(fc.imp$imputations, function(d) glm(cl.latino~scale(l.incarrt)+
#                                                        scale(l.unemp.rt)+scale(l.singpar.rt)+scale(latino.lessHS)+
#                                                        scale(chpov.latino.pc)+scale(pctlat)+
#                                                        scale(inst6014_nom)+scale(v.crime.rt)+
#                                                        year.c+
#                                                        factor(stname) , family=quasipoisson, offset=log(latino.child),
#                                                      data=d))
# # 
# a.count.fe<-lapply(fc.imp$imputations, function(d) glm(cl.nat.am~scale(a.incarrt)+
#                                                        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
#                                                        scale(chpov.amind.pc)+scale(pctami)+
#                                                        scale(inst6014_nom)+scale(v.crime.rt)+
#                                                        year.c+
#                                                        factor(stname), family=quasipoisson, offset=log(amind.child),
#                                                      data=d))
# # 
# # ## Disproportion models
# b.disp.fe<-lapply(fc.imp$imputations, function(d) lm(log(bw.disp)~scale(b.incardisp)+
#                                                       scale(bdisp.chpov)+
#                                                       scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
#                                                       scale(I(blk.lessHS/wht.lessHS))+
#                                                       scale(pctblk)+
#                                                       scale(inst6014_nom)+scale(v.crime.rt)+
#                                                       year.c+
#                                                       factor(stname),
#                                                     data=d))
# # 
# a.disp.fe<-lapply(fc.imp$imputations, function(d) lm(log(ami.disp)~scale(a.incardisp)+
#                                                       scale(adisp.chpov)+
#                                                       scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
#                                                       scale(I(amind.lessHS/wht.lessHS))+
#                                                       scale(pctami)+scale(pctblk)+
#                                                       scale(inst6014_nom)+scale(v.crime.rt)+
#                                                       year.c+
#                                                       factor(stname),
#                                                     data=d))
# # 
# l.disp.fe<-lapply(fc.imp$imputations, function(d) lm(log(lw.disp)~scale(l.incardisp)+
#                                                       scale(ldisp.chpov)+
#                                                       scale(I(l.unemp.rt/w.unemp.rt))+scale(I(l.singpar.rt/w.singpar.rt))+
#                                          mary()             scale(I(latino.lessHS/wht.lessHS))+
#                                                       scale(pctlat)+scale(pctblk)+
#                                                       scale(inst6014_nom)+scale(v.crime.rt)+
#                                                       year.c+
#                                                       factor(stname),
#                                                     data=d))

## FE RESUlTS MIMIC DIFF IN DIFF - IF LOW VARIATION IN INCAR, CAN'T GET TRACTION ON FC (LIKELY)
# 
# ## BAYESIAN
# blk.count.bayes<-stan_glmer(cl.blk~scale(b.incarrt)+
#                              scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
#                              scale(chpov.blk.pc)+scale(pctblk)+
#                              scale(inst6014_nom)+scale(v.crime.rt)+
#                              year.c+
#                              (1|stname) + (1|obs), family=poisson, offset=log(blk.child),
#                           data=fc.imp$imputations[[1]])
# 
# lat.count.bayes<-stan_glmer(cl.latino~scale(l.incarrt)+
#                              scale(l.unemp.rt)+scale(l.singpar.rt)+scale(latino.lessHS)+
#                              scale(chpov.latino.pc)+scale(pctlat)+
#                              scale(inst6014_nom)+scale(v.crime.rt)+
#                              year.c+
#                              (1|stname) + (1|obs), family=poisson, offset=log(latino.child),
#                           data=fc.imp$imputations[[1]])
# 
# am.count.bayes<-stan_glmer(cl.nat.am~scale(a.incarrt)+
#                  scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
#                  scale(chpov.amind.pc)+scale(pctami)+
#                  scale(inst6014_nom)+scale(v.crime.rt)+
#                  year.c+
#                  (1+year.c|stname) +(1|obs), family=poisson, offset=log(amind.child),
#                data=fc.imp$imputations[[1]])
# 
# b.disp.bayes<-stan_lmer(log(bw.disp)~scale(b.incardisp)+
#         scale(bdisp.chpov)+
#         scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
#         scale(I(blk.lessHS/wht.lessHS))+
#         scale(pctblk)+
#         scale(inst6014_nom)+scale(v.crime.rt)+
#         year.c+
#         (1|stname),
#         data=fc.imp$imputations[[1]])
# 
# l.disp.bayes<-stan_lmer(log(lw.disp)~scale(l.incardisp)+
#         scale(ldisp.chpov)+
#         scale(I(l.unemp.rt/w.unemp.rt))+scale(I(l.singpar.rt/w.singpar.rt))+
#         scale(I(latino.lessHS/wht.lessHS))+
#         scale(pctlat)+
#         scale(inst6014_nom)+scale(v.crime.rt)+
#         year.c+
#         (1|stname),
#         data=fc.imp$imputations[[1]])
# 
# a.disp.bayes<-stan_lmer(log(bw.disp)~scale(a.incardisp)+
#         scale(adisp.chpov)+
#         scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
#         scale(I(amind.lessHS/wht.lessHS))+
#         scale(pctami)+
#         scale(inst6014_nom)+scale(v.crime.rt)+
#         year.c+
#         (1|stname),
#         data=fc.imp$imputations[[1]])

# warnings()
# save.image(file="cw-race-models-incar.R", safe=TRUE)
# #quit("no")