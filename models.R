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

# ## for laptop
setwd("~/Dropbox/cw-race/data/")
source("~/Dropbox/cw-race/cw-race-functions.r")
source("~/Dropbox/cw-race/cw-race-read.r")
setwd("~/Dropbox/cw-race/")

# ### for libra
# setwd("~/cw-race/data/")
# source("~/cw-race/cw-race-functions.r")
# source("~/cw-race/cw-race-read.r")
# setwd("~/cw-race/")

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
# fc.desc1<-fc.ineq%>%select(-stname, -obs, -chpovrt, -incarrt, -eitc.st, -child.pov, 
#                           -year.c)

# fc.desc<-fc.desc1%>%mutate("Black foster care caseloads per 100,000")
# Mean<-round(sapply(na.omit(fc.desc), mean),3)
# SD<-round(sapply(na.omit(fc.desc), sd),3)
# Min<-round(sapply(na.omit(fc.desc), min),3)
# Max<-round(sapply(na.omit(fc.desc), max),3)
# descriptives<-xtable(cbind(Mean, SD, Minimum, Maximum), caption="Descriptive Statistics")
# print(descriptives, file="desctable.tex")


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



b.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.blk~1+scale(b.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(blk.child),
      data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

# w.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.white~1+scale(w.incarrt)+scale(incarrt)+
#           scale(w.unemp.rt)+scale(w.singpar.rt)+scale(wht.lessHS)+
#           scale(chpov.wht.pc)+scale(pctwht)+scale(w.welf.incl)+
#           scale(inst6014_nom)+scale(v.crime.rt)+
#           year.c+
#           (1|stname) + (1|obs), family=poisson, offset=log(wht.child),
#         data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

l.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.latino~1+scale(l.incarrt)+
        scale(l.unemp.rt)+scale(l.singpar.rt)+scale(latino.lessHS)+
        scale(chpov.latino.pc)+scale(pctlat)+scale(l.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(latino.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

a.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.nat.am~1+scale(a.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(amind.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

## Disproportion models
b.disp<-lapply(fc.imp$imputations, function(d) lmer(log(bw.disp)~1+scale(b.incardisp)+
        scale(bdisp.chpov)+
        scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
        scale(I(blk.lessHS/wht.lessHS))+scale(I(b.welf.incl/w.welf.incl))+
        scale(pctblk)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

a.disp<-lapply(fc.imp$imputations, function(d) lmer(log(ami.disp)~1+scale(a.incardisp)+
        scale(adisp.chpov)+
        scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
        scale(I(amind.lessHS/wht.lessHS))+scale(I(a.welf.incl/w.welf.incl))+
        scale(pctami)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

l.disp<-lapply(fc.imp$imputations, function(d) lmer(log(lw.disp)~1+scale(l.incardisp)+
        scale(ldisp.chpov)+
        scale(I(l.unemp.rt/w.unemp.rt))+scale(I(l.singpar.rt/w.singpar.rt))+
        scale(I(latino.lessHS/wht.lessHS))+scale(I(l.welf.incl/w.welf.incl))+
        scale(pctlat)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))


b.c.tab<-makeMIRegTab(b.ineq)
row.names(b.c.tab)<- c("Intercept", "Black incarceration rate", "Black unemployment rate", "Black single parent rate",
                       "Black adults w/o HS rate", "Black child poverty rate", "Percent Black population",
                       "Black welfare enrollment per child poverty",
                       "Leg. ideology", "Violent crime rate", "Year", "Variance of gamma")

print(xtable(b.c.tab, caption="Black foster care caseloads. Poisson multilevel regression (overdispersed), 
             offset by Black child population with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units.
             Results combined across imputations (m=12).", label="b.c.tab", digits=3),  
      type="latex", file="b_c_tab.tex", caption.placement="top")

a.c.tab<-makeMIRegTab(a.ineq)
row.names(a.c.tab)<- c("Intercept", "Native Am. incarceration rate", "Native Am. unemployment rate", "Native Am. single parent rate",
                       "Native Am. adults w/o HS rate", "Native Am. child poverty rate", "Percent Native Am. population",
                       "Native Am. welfare enrollment per child poverty",
                       "Leg. ideology", "Violent crime rate", "Year", "Variance of gamma")
print(xtable(a.c.tab, caption="Native American foster care caseloads. Poisson multilevel regression, 
             offset by Native American child population with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units. 
             Results combined across imputations (m=12).", label="a.c.tab", digits=3),
      type="latex", file="a_c_tab.tex", caption.placement="top")

l.c.tab<-makeMIRegTab(l.ineq)
row.names(l.c.tab)<- c("Intercept", "Latino Incarceration rate", "Latino Unemployment rate", "Latino Single parent rate",
                       "Latino Adults w/o HS rate", "Latino Child poverty rate", "Percent Latino population",
                       "Latino welfare enrollment per child poverty",
                       "Leg. Ideology", "Violent crime rate", "Year", "Variance of gamma")
print(xtable(l.c.tab, caption="Latino foster care caseloads. Poisson multilevel regression, 
             offset by Latino child population with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units.
             Results combined across imputations (m=12).", label="l.c.tab", digits=3),
      type="latex", file="l_c_tab.tex", caption.placement="top")


b.d.tab<-makeMIRegTab(b.disp)
row.names(b.d.tab)<- c("Intercept", "Black/White Incarceration rate", "Black/White Unemployment rate", "Black/White Single parent rate",
                       "Black/White Adults w/o HS rate", "Black/White Child poverty rate", "Black/White welfare enroll. ",
                       "Percent Black population",
                       "Leg. Ideology", "Violent crime rate", "Year", "Variance of gamma")
print(xtable(b.d.tab, caption="Black/White foster care caseload disproportion. Multilevel linear regression with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units.
             Results combined across imputations (m=12).", label="b.d.tab", digits=3),
      type="latex", file="b_d_tab.tex", caption.placement="top")

a.d.tab<-makeMIRegTab(a.disp)
row.names(a.d.tab)<- c("Intercept", "Native Am./White Incarceration rate", "Native Am./White Unemployment rate", "Native Am./White Single parent rate",
                       "Native Am./White Adults w/o HS rate", "Native Am./White Child poverty rate", "Native Am./White welfare enroll.",
                       "Percent Native Am.",
                       "Leg. Ideology", "Violent crime rate", "Year", "Variance of gamma")
print(xtable(a.d.tab, caption="Native American/White foster care caseload disproportion. Multilevel linear regression with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units.
             Results combined across imputations (m=12).", label="a.d.tab", digits=3),
      type="latex", file="a_d_tab.tex", caption.placement="top")

l.d.tab<-makeMIRegTab(l.disp)
row.names(l.d.tab)<- c("Intercept", "Latino/White incarceration rate", "Latino/White unemployment rate", "Latino/White single parent rate",
                       "Latino/White adults w/o HS rate", "Latino/White child poverty rate", "Latino/White welfare enroll. ",
                       "Percent Latino population",
                       "Leg. Ideology", "Violent crime rate", "Year", "Variance of gamma")
print(xtable(l.d.tab, caption="Latino/White foster care caseload disproportion. Multilevel linear regression with state intercepts. 
             All predictors mean-centered and transformed into standard deviation units.
             Results combined across imputations (m=12).", label="l.c.tab", digits=3),
      type="latex", file="l_d_tab.tex", caption.placement="top")


###PAPER DESCRIPTIVES
fc.avg<-fc.ineq%>%group_by(stname)%>%summarise("bcl"=mean(cl.blk/blk.child), 
                                               "acl"=mean(cl.nat.am/amind.child),
                                               "lcl"=mean(cl.latino/latino.child, na.rm=TRUE),
                                               "bdisp"=mean(bw.disp),
                                               "adisp"=mean(ami.disp),
                                               "ldisp"=mean(lw.disp),
                                               "binc"=mean(b.incarrt),
                                               "ainc"=mean(a.incarrt),
                                               "linc"=mean(l.incarrt),
                                               "bwinc"=mean(b.incardisp),
                                               "awinc"=mean(a.incardisp),
                                               "lwinc"=mean(l.incardisp, na.rm=TRUE))

###WANT DESRIPTIVE TABLE FOR ALL GROUPS FOR PREDICTORS, OUTCOMES
fc.descriptives.blk<-fc.ineq%>%select(cl.blk, blk.child, b.incarrt, b.unemp.rt, 
  b.singpar.rt, blk.lessHS, chpov.blk.pc, pctblk, b.welf.incl, bw.disp, b.incardisp)%>%
  mutate("cl"=cl.blk/blk.child)%>%select(-blk.child)

fc.descriptives.a<-fc.ineq%>%select(cl.nat.am, amind.child, a.incarrt, a.unemp.rt, 
  a.singpar.rt, amind.lessHS, chpov.amind.pc, pctami, a.welf.incl, ami.disp, b.incardisp)%>%
  mutate("cl"=cl.nat.am/amind.child)%>%select(-amind.child)

fc.descriptives.l<-fc.ineq%>%select(cl.latino, latino.child, l.incarrt, l.unemp.rt, 
  l.singpar.rt, latino.lessHS, chpov.latino.pc, pctlat, l.welf.incl, lw.disp, l.incardisp)%>%
  mutate("cl"=cl.latino/latino.child)%>%select(-latino.child)

means.blk<-round(sapply(fc.descriptives.blk, function(x)mean(x, na.rm=TRUE)),3)
sds.blk<-round(sapply(fc.descriptives.blk, function(x)sd(x, na.rm=TRUE)),3)
blk.desc<-cbind(means.blk, sds.blk)
row.names(blk.desc)<-c("Caseload", "Incar. rate", 
  "Unempl. rate", "Single parent rate", "Less than HS rate",
  "Child poverty rate", "Prop. of population", "Welfare per child pov",
  "Caseload disp.", "Incar. disp.", "Caseloads per capita")

means.a<-round(sapply(fc.descriptives.a, function(x)mean(x, na.rm=TRUE)),3)
sds.a<-round(sapply(fc.descriptives.a, function(x)sd(x, na.rm=TRUE)),3)
a.desc<-cbind(means.a, sds.a)
row.names(blk.desc)<-c("Caseload", "Incar. rate", 
                       "Unempl. rate", "Single parent rate", "Less than HS rate",
                       "Child poverty rate", "Prop. of population", "Welfare per child pov",
                       "Caseload disp.", "Incar. disp.", "Caseloads per capita")

means.l<-round(sapply(fc.descriptives.l, function(x)mean(x, na.rm=TRUE)),3)
sds.l<-round(sapply(fc.descriptives.l, function(x)sd(x, na.rm=TRUE)),3)
l.desc<-cbind(means.l, sds.l)
row.names(blk.desc)<-c("Caseload", "Incar. rate", 
  "Unempl. rate", "Single parent rate", "Less than HS rate",
  "Child poverty rate", "Prop. of population", "Welfare per child pov",
  "Caseload disp.", "Incar. disp.", "Caseloads per capita")

desc<-as.data.frame(cbind(blk.desc, a.desc, l.desc))
names(desc)<-c("Mean:AA", "SD:AA", "Mean:NA", 
  "SD:NA", "Mean:Lat", "SD:Lat")

print(xtable(desc, label="descriptives",
  caption="Means and Standard Deviations for predictors by Race / Ethnicity. U.S. States 2007 - 2014."), 
  file="descriptives.tex", type="latex", caption.placement="top")

##FOR MAX MIN VALUES, BIVAR CORRELATIONS
fc.avg[which(fc.avg$ldisp==max(fc.avg$ldisp, na.rm=TRUE)),]
with(fc.imp$imputations$imp1, cor(bw.disp, b.incardisp))

