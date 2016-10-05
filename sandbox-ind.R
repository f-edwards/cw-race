rm(list=ls())

setwd("U:/AFCARS-new")

library(data.table)
library(dplyr)

names.funct<-function(x){
  names(x)<-tolower(names(x))
  return(x)
}


make.race<-function(x){
  x$race<-ifelse(x$blkafram==1, "blk", 
                 ifelse(x$amiakn==1, "nat.am", 
                        ifelse(x$asian==1, "asian",
                               ifelse(x$hawaiipi==1, "haw.pi", 
                                      ifelse(x$hisorgin==1, "latino",
                                             ifelse(x$white==1, "white", 
                                                    "unk"))))))
  return(x)
}

fc2014<-data.frame(make.race(names.funct(fread("fc2014.csv"))))
fc2014$year<-fc2014$datayear

library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(MASS)

setwd("U:/cw-race/")
source("U:/cw-race/cw-race-functions.r")
fc<-read.csv("fc.csv")



fc<-fc%>%mutate(obs=1:nrow(fc),
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

fc.dat<-left_join(fc2014, fc, by=c("state", "year"))
fc.dat$sex<-factor(fc.dat$sex)

instmod<-glmer((curplset==5)~-1+scale(ageatstart)+factor(sex)+
                 scale(b.incarrt)+scale(b.incarrt)+
                 scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
                 scale(chpov.blk.pc)+scale(pctblk)+
                 scale(inst6014_nom)+scale(v.crime.rt)+
                 (1|stname), data=fc.dat%>%filter(race=="blk"), family="binomial")

kinmod<-glmer((curplset==2)~-1+scale(ageatstart)+factor(sex)+
                 scale(b.incarrt)+scale(b.incarrt)+
                 scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
                 scale(chpov.blk.pc)+scale(pctblk)+
                 scale(inst6014_nom)+scale(v.crime.rt)+
                 (1|stname), data=fc.dat%>%filter(race=="blk"), family="binomial")

losmod<-lmer(log(I(lifelos+1))~-1+ageatstart+factor(sex)+
                log(b.incarrt)+log(b.incarrt)+
                log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                log(chpov.blk.pc)+log(pctblk)+
                scale(inst6014_nom)+log(v.crime.rt)+
                (1|stname), data=fc.dat%>%filter(race=="blk"))