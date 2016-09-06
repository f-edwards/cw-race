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

setwd("C:/Users/kilgore/Dropbox/cw-race/data/")
source("C:/Users/kilgore/Dropbox/cw-race/cw-race-functions.r")
source("C:/Users/kilgore/Dropbox/cw-race/cw-race-read.r")
setwd("C:/Users/kilgore/Dropbox/cw-race-paper/")

# ## for laptop
# setwd("~/Dropbox/cw-race/data/")
# source("~/Dropbox/cw-race/cw-race-functions.r")
# source("~/Dropbox/cw-race/cw-race-read.r")
# ##for output
# setwd("~/Dropbox/cw-race-paper/")

# ### for libra
# setwd("~/cw-race/data/")
# source("~/cw-race/cw-race-functions.r")
# source("~/cw-race/cw-race-read.r")
# setwd("~/cw-race/")

fc<-fc%>%filter(fc$year>2006)

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
 incarrt=ifelse(TOTRACEM>0, (TOTRACEM+TOTRACEF)/adult, NA),
 b.incarrt=ifelse(BLACKM>0, (BLACKM+BLACKF)/b.adult, NA),
 b.m.incarrt=ifelse(BLACKM>0, (BLACKM)/b.adult, NA),
 b.f.incarrt=ifelse(BLACKM>0, (BLACKF)/b.adult, NA),
 a.incarrt=ifelse(AIANM>0, (AIANM+AIANF)/a.adult,NA),
 a.m.incarrt=ifelse(AIANM>0, (AIANM)/a.adult,NA),
 a.f.incarrt=ifelse(AIANM>0, (AIANF)/a.adult,NA),
 w.incarrt=ifelse(WHITEM>0, (WHITEM+WHITEF)/w.adult,NA),
 b.incardisp=ifelse(BLACKM>0, b.incarrt/w.incarrt,NA),
 a.incardisp=ifelse(AIANM>0, a.incarrt/w.incarrt,NA),
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
 year.c=year-2007
 )

fc.ineq<-fc

### SOME MISSING INCARCERATION DATA REPORTED AS 0 - AK in 2013
fc$year.p<-fc$year-2000
##TS PLOTS

# race.pc.ts<-ggplot(data=fc, aes(x=year.p, y=cl.blk.pc))+geom_line(aes(color="Black"))+
#   geom_line(aes(y=cl.amind.pc, color="Native Am"))+
#   geom_line(aes(y=cl.lat.pc, color="Latino"))+
#   xlab("Year")+ylab("Caseload per cap")+
#   ylim(0, 0.20)+
#   facet_wrap(~stname)

# welf.pc.ts<-ggplot(data=fc, aes(x=year, y=b.welf.incl))+geom_line(aes(color="Black"))+
#   geom_line(aes(y=a.welf.incl, color="Native Am"))+
#   geom_line(aes(y=l.welf.incl, color="Latino"))+
#   xlab("Year")+ylab("Cash welfare recipients per children in poverty")+
#   ylim(0, 0.5)+
#   facet_wrap(~stname)

# welf.disp.ts<-ggplot(data=fc, aes(x=year, y=b.welf.incl/w.welf.incl))+geom_line(aes(color="B/W"))+
#   geom_line(aes(y=a.welf.incl/w.welf.incl, color="Native Am/W"))+
#   geom_line(aes(y=l.welf.incl/w.welf.incl, color="Latino/W"))+
#   xlab("Year")+ylab("Welfare inequality")+
#   ylim(0, 1)+
#   facet_wrap(~stname)

# ggsave("race-pc-ts.pdf", race.pc.ts, width=11, height=8)

# disp.ts<-ggplot(data=fc, aes(x=year.p, y=bw.disp))+geom_line(aes(color="Afr.Am./W"))+
#   geom_line(aes(y=ami.disp, color="Nat.Am./W"))+
#   xlab("Year")+ylab("Caseload disproportion")+
#   coord_cartesian(ylim=c(0,25))+theme_bw()+
#   facet_wrap(~stname, ncol=10)
# ggsave("race-disp-ts.pdf", disp.ts, width=11, height=8)


# disp.incar.ts<-ggplot(data=fc, aes(x=year.p, y=b.incardisp))+geom_line(aes(color="B/W"))+
#   geom_line(aes(y=l.incardisp, color="L/W"))+
#   geom_line(aes(y=a.incardisp, color="Nat.Am./W"))+
#   xlab("Year")+ylab("Incarceration disproportion")+
#   facet_wrap(~stname)
# ggsave("race-incardisp-ts.pdf", disp.incar.ts, width=11, height=8)


# incar.ts<-ggplot(data=fc, aes(x=year.p, y=b.incarrt))+geom_line(aes(color="Black"))+
#   geom_line(aes(y=l.incarrt, color="Latino"))+
#   geom_line(aes(y=a.incarrt, color="Native Am."))+
#   xlab("Year")+ylab("Incarceration rates")+
#   facet_wrap(~stname)
#   #scale_x_continuous(breaks=c(2008,2013))
# ggsave("race-incarpc-ts.pdf", incar.ts, width=11, height=8)


### HAWAII COUNTS AMIND DIFFERENTLY IN AFCARS / CENSUS. MAYBE NATIVE HAWAIIAN ARE INCLUDED IN AFCARS, NOT CENSUS COUNTS? DROPPING FOR NOW
###MI
###select vars to include

fc.ineq<-fc.ineq%>%
  dplyr::select(cl.blk, b.incarrt, chpov.blk.pc, chpovrt, 
         incarrt, 
         child.pov, 
         inst6014_nom, year.c, pctblk, stname, obs, 
         blk.child, cl.nat.am, 
         a.incarrt, chpov.amind.pc, amind.child, pctami,
         w.incarrt, b.incardisp,  a.incardisp, 
         bdisp.chpov, adisp.chpov, bw.disp, 
         ami.disp, b.welf.incl, a.welf.incl,  
         w.welf.incl, 
         #a.snap.incl, b.snap.incl, l.snap.incl, w.snap.incl,
         w.unemp.rt, b.unemp.rt, a.unemp.rt,  w.singpar.rt, b.singpar.rt, a.singpar.rt, 
         wht.lessHS, blk.lessHS, amind.lessHS, latino.lessHS,
         v.crime.rt,
         cl.white, wht.child, chpov.wht.pc, pctwht,
         b.m.incarrt, b.f.incarrt, a.m.incarrt, a.f.incarrt,
         board, pctblk1900, pctblk1930, pctblk1960, pctami1900, pctami1930, pctami1960, 
         pctimm1900, pctimm1930, pctimm1960)

### Descriptive table
# fc.desc1<-fc.ineq%>%select(-stname, -obs, -chpovrt, -incarrt, -eitc.st, -child.pov,
#                           -year.c)
# 
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
         ts="year.c", cs="stname", polytime=1, bounds=bounds, p2s=0,
         idvars=c("b.f.incarrt", "a.f.incarrt"))

# OItest<-overimpute(fc.imp, "l.incarrt")


for(i in 1:m){
  for(j in 1:ncol(fc.ineq)){
    if(colClass[j]=="integer"){
      fc.imp$imputations[[i]][,j]<-ceiling(fc.imp$imputations[[i]][,j])
    }
  }
}



###WITHIN MODELS MAKE MOST SENSE ON ENTRIES
###COULD THINK ABOUT REUNIFICATION EXITS OR LOS TOO

b.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.blk~1+scale(b.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(blk.child),
      data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

#REWRITE AS NEGBIN OR PSEUDO POIS
b.cl.fe<-lapply(fc.imp$imputations, function(d) glm(cl.blk~1+scale(b.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname) , family=quasipoisson, offset=log(blk.child),
      data=d))

b.cl.m.fe<-lapply(fc.imp$imputations, function(d) glm(cl.blk~1+scale(b.m.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname) , family=quasipoisson, offset=log(blk.child),
      data=d))

b.cl.f.fe<-lapply(fc.imp$imputations, function(d) glm(cl.blk~1+scale(b.f.incarrt)+
        scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
        scale(chpov.blk.pc)+scale(pctblk)+scale(b.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname) , family=quasipoisson, offset=log(blk.child),
      data=d))


a.ineq<-lapply(fc.imp$imputations, function(d) glmer(cl.nat.am~1+scale(a.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname) + (1|obs), family=poisson, offset=log(amind.child),
        data=d, control=glmerControl(optCtrl=list(maxfun=2e5))))

a.cl.fe<-lapply(fc.imp$imputations, function(d) glm(cl.nat.am~1+scale(a.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname), family=quasipoisson, offset=log(amind.child),
        data=d))

a.cl.m.fe<-lapply(fc.imp$imputations, function(d) glm(cl.nat.am~1+scale(a.m.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname), family=quasipoisson, offset=log(amind.child),
        data=d))

a.cl.f.fe<-lapply(fc.imp$imputations, function(d) glm(cl.nat.am~1+scale(a.f.incarrt)+
        scale(a.unemp.rt)+scale(a.singpar.rt)+scale(amind.lessHS)+
        scale(chpov.amind.pc)+scale(pctami)+scale(a.welf.incl)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        factor(stname), family=quasipoisson, offset=log(amind.child),
        data=d))

## Disproportion models
## AS PLACE TO GO AFTER INEQ EFFECTS

b.disp<-lapply(fc.imp$imputations, function(d) lmer(log(bw.disp)~1+scale(b.incardisp)+
        scale(bdisp.chpov)+
        scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
        scale(I(blk.lessHS/wht.lessHS))+scale(I(b.welf.incl/w.welf.incl))+
        scale(pctblk)+
        scale(inst6014_nom)+scale(v.crime.rt)+
        year.c+
        (1|stname),
        data=d))

b.disp.hist<-lapply(fc.imp$imputations, function(d) lmer(log(bw.disp)~1+scale(b.incardisp)+
          scale(bdisp.chpov)+
          scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
          scale(I(blk.lessHS/wht.lessHS))+scale(I(b.welf.incl/w.welf.incl))+
          scale(pctblk)+
          scale(inst6014_nom)+scale(v.crime.rt)+
          year.c+
          scale(board)+
          scale(pop.blk.1910)+
          scale(pop.ami.1910)+
          scale(pop.imm.1910)+
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

a.disp.hist<-lapply(fc.imp$imputations, function(d) lmer(log(ami.disp)~1+scale(a.incardisp)+
          scale(adisp.chpov)+
          scale(I(a.unemp.rt/w.unemp.rt))+scale(I(a.singpar.rt/w.singpar.rt))+
          scale(I(amind.lessHS/wht.lessHS))+scale(I(a.welf.incl/w.welf.incl))+
          scale(pctami)+
          scale(inst6014_nom)+scale(v.crime.rt)+
          year.c+
            scale(board)+
            scale(pop.blk.1910)+
            scale(pop.ami.1910)+
            scale(pop.imm.1910)+
          (1|stname),
        data=d))


b.c.tab<-makeMIRegTab(b.ineq)
a.c.tab<-makeMIRegTab(a.ineq)


### For html file output of results
texreg(list(b.ineq[[1]],a.ineq[[1]]),
	override.coef=list(b.c.tab[1:nrow(b.c.tab)-1,1], a.c.tab[1:nrow(b.c.tab)-1,1]),
	override.se=list(b.c.tab[1:nrow(b.c.tab)-1,2], a.c.tab[1:nrow(b.c.tab)-1,2]),
	custom.coef.names=c("Intercept",
	                   "Afr. Am. Incarceration rate", 
	                   "Afr. Am. Unemployment rate", 
	                   "Afr. Am. Single parent rate",
	                   "Afr. Am. Adults w/o HS rate", 
	                   "Afr. Am. Child poverty rate", 
	                   "Percent Afr. Am. population",
	                   "Afr. Am. Welfare per child poverty",
	                   "Leg. ideology", 
	                   "Violent crime rate", 
	                   "Year",
	                   "Nat. Am. Incarceration rate", 
	                   "Nat. Am. Unemployment rate", 
	                   "Nat. Am. Single parent rate",
	                   "Nat. Am. Adults w/o HS rate", 
	                   "Nat. Am. Child poverty rate", 
	                   "Percent Nat. Am. population",
	                   "Nat. Am. Welfare per child poverty"),
	custom.model.names=c("Afr. Am. Caseload", "Nat. Am. Caseload"),
	caption="Foster care caseloads, Poisson multilevel regression (overdispersed),
offset by child population with state intercepts. Predictors mean-
centered and transformed into standard deviation units. Results combined across
imputations (m=3).",
	caption.above=TRUE,
	label="count-models",
	include.aic=FALSE,
	include.bic=FALSE,
	include.loglik=FALSE,
	file="count-models.tex"
	)
	

b.d.tab<-makeMIRegTab(b.disp)
a.d.tab<-makeMIRegTab(a.disp)

texreg(list(b.disp[[1]],a.disp[[1]]),
       override.coef=list(b.d.tab[1:nrow(b.c.tab)-1,1], a.d.tab[1:nrow(b.c.tab)-1,1]),
       override.se=list(b.d.tab[1:nrow(b.c.tab)-1,2], a.d.tab[1:nrow(b.c.tab)-1,2]),
       custom.coef.names=c("Intercept",
                           "Afr. Am. Incarceration disp.", 
                           "Afr. Am. Child poverty disp.",
                           "Afr. Am. Unemployment disp.", 
                           "Afr. Am. Single parent disp.",
                           "Afr. Am. Adults w/o HS disp.", 
                           "Afr. Am. Welfare disp.",
                           "Percent Afr. Am. population",
                           "Leg. ideology", 
                           "Violent crime disp.", 
                           "Year",
                           "Nat. Am. Incarceration disp.", 
                           "Nat. Am. Child poverty disp.", 
                           "Nat. Am. Unemployment disp.", 
                           "Nat. Am. Single parent disp.",
                           "Nat. Am. Adults w/o HS disp.", 
                           "Nat. Am. Welfare disp.",
                           "Percent Nat. Am. population"),
       custom.model.names=c("Afr. Am. Disproportion", "Nat. Am. Disproportion"),
       caption="Foster care caseload disproportion, multilevel linear
regression with state intercepts. All predictors mean-centered and transformed
into standard deviation units. Results combined across imputations (m=3).",
       caption.above=TRUE,
       label="disp-models",
       include.aic=FALSE,
       include.bic=FALSE,
       include.loglik=FALSE,
       file="disp-models.tex"
)

###PAPER DESCRIPTIVES
fc.avg<-fc.ineq%>%group_by(stname)%>%summarise("bcl"=mean(cl.blk/blk.child), 
                                               "acl"=mean(cl.nat.am/amind.child),
                                               "bdisp"=mean(bw.disp),
                                               "adisp"=mean(ami.disp),
                                               "binc"=mean(b.incarrt),
                                               "ainc"=mean(a.incarrt),
                                               "bwinc"=mean(b.incardisp),
                                               "awinc"=mean(a.incardisp))

###WANT DESRIPTIVE TABLE FOR ALL GROUPS FOR PREDICTORS, OUTCOMES
fc.descriptives.blk<-fc.ineq%>%select(cl.blk, blk.child, b.incarrt, b.unemp.rt, 
  b.singpar.rt, blk.lessHS, chpov.blk.pc, pctblk, b.welf.incl, bw.disp, b.incardisp)%>%
  mutate("cl"=cl.blk/blk.child)%>%select(-blk.child)

fc.descriptives.a<-fc.ineq%>%select(cl.nat.am, amind.child, a.incarrt, a.unemp.rt, 
  a.singpar.rt, amind.lessHS, chpov.amind.pc, pctami, a.welf.incl, ami.disp, b.incardisp)%>%
  mutate("cl"=cl.nat.am/amind.child)%>%select(-amind.child)

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

desc<-as.data.frame(cbind(blk.desc, a.desc))
names(desc)<-c("Mean:AfrAm", "SD:AfrAm", "Mean:NatAm", 
  "SD:NatAm")

print(xtable(desc, label="descriptives",
  caption="Means and Standard Deviations for predictors by Race / Ethnicity. U.S. States 2007 - 2014."), 
  file="descriptives.tex", type="latex", caption.placement="top")

##forest plots
source("cw-forest.r")

##FOR MAX MIN VALUES, BIVAR CORRELATIONS
fc.avg[which(fc.avg$ldisp==max(fc.avg$ldisp, na.rm=TRUE)),]
with(fc.imp$imputations$imp1, cor(bw.disp, b.incardisp))

