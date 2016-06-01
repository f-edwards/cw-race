#########
# These models replicate models from "Saving Children" for racial inequalities. Expanding to Nat. Am., Latino and working through separate outcomes (i.e LOS, Nplacement, placement setting, casegoal) at the individual level is the next step

library(lme4)
library(Amelia)
library(ggplot2)
source("~/Dropbox/cw-race/cw-race-functions.r")
source("~/Dropbox/cw-race/cw-race-read.r")

hist<-read.csv("~/Dropbox/data/fc-race/hist-pop.csv")

###TS PLOTS
ggplot(data=fc, aes(x=year, y=lifelos.blk))+geom_line(aes(color="black"))+
  geom_line(aes(y=lifelos.white, color="white"))+
  geom_line(aes(y=lifelos.nat.am, color="nat.am"))+
  geom_line(aes(y=lifelos.latino, color="latino"))+
  xlab("Year")+ylab("Mean Lifetime Days in FC")+
  facet_wrap(~stname)+
  ggtitle("Lifetime mean days in FC by race")


fc$bw.disp<-fc$cl.blk.pc/fc$cl.wht.pc
fc$lw.disp<-fc$cl.lat.pc/fc$cl.wht.pc
fc$ami.disp<-fc$cl.amind.pc/fc$cl.wht.pc

ggplot(data=fc, aes(x=year, y=cl.blk.pc))+geom_line(aes(color="black"))+
  geom_line(aes(y=cl.wht.pc, color="white"))+
  geom_line(aes(y=cl.amind.pc, color="nat.am"))+
  geom_line(aes(y=cl.lat.pc, color="latino"))+
  xlab("Year")+ylab("Caseload per cap")+
  ylim(0, 0.20)+
  facet_wrap(~stname)+
  ggtitle("")

ggplot(data=fc, aes(x=year, y=bw.disp))+geom_line(aes(color="b/w"))+
  geom_line(aes(y=lw.disp, color="l/w"))+
  geom_line(aes(y=ami.disp, color="a/w"))+
  xlab("Year")+ylab("Caseload per cap")+
  ylim(0,15)+
  facet_wrap(~stname)+
  ggtitle("")

###listwise deletion for now, will do imputation later
#fc.int<-amelia(fc, m=10, ts="year", cs="state", idvars=?)

#####LISTWISE DELETION
###Its the default in lme4, leaving alone for now

### BOARDING SCHOOL DATA
### FROM http://www.archives.gov/research/native-americans/bia-guide/schools.html
bs<-read.csv("boardingschool.csv")


### SAVING CHILDREN VARS
fc<-fc%>%mutate(obs_n=1:nrow(fc), unemprt=unemp/(unemp+emp),
 childnot2par=1-(kids2par/child),
 chpovrt=childpov/child, 
 gsppercap=GSP*1000000/pop,
 crime.pc=crime/pop,
 pctblk=blkpop/pop,
 incarrt=incartot/adult,
 tanf.adeq=AFDCBen3/rpp,
 wic.incl=WIC.par/childpov,
 medicaid.incl=medicaidrec/pov,
 snap.incl=SNAPRec/pov,
 tanf.incl=AFDCRec/childpov,
 ideo=inst6010_nom,
 police.pc=police.ft.emp/pop,
 welfare.pc=welfare.ft.emp/pop,
 death.rt=death.sent/new.incar)

### INEQ VARS
fc.ineq<-fc%>%mutate(disp.chpov=(blkchildpov/blkchild)/(whtchildpov/whtchild), disp.kids2par=(bkidskincare/blkchild)/(wkidskincare/whtchild), year=year-2002)

fc.ineq<-full_join(fc.ineq, hist, by="state")
fc.ineq<-full_join(fc.ineq, bs, by="stname")
fc.ineq$bs.true<-fc.ineq$boarding.n>0

fc.ineq<-fc.ineq[fc.ineq$state!=11,]

disp.lmer<-lmer(log(cl.disp)~scale(unemprt)+scale(childnot2par)+
        scale(chpovrt)+scale(LessHS)+
        scale(gsppercap)+
        scale(ideo)+scale(crime.pc)+scale(pctblk)+
        scale(tanf.adeq)+scale(snap.incl)+
        scale(medicaid.incl)+
        scale(tanf.incl)*scale(welfare.pc)+
        scale(incarrt)+
        scale(death.rt)+
        scale(police.pc)+
        scale(disp.chpov)+scale(disp.kids2par)+year+
        scale(pop.imm.1910)+scale(pop.imm.1970)+
        scale(pop.blk.1910)+scale(pop.blk.1970)+
        bs.true+
        (1+year|stname), 
        data=fc.ineq)

blkcl.glmer<-glmer(bcl~scale(unemprt)+scale(childnot2par)+
                   scale(LessHS)+
                   scale(gsppercap)+
                   scale(ideo)+scale(crime.pc)+scale(pctblk)+
                   scale(tanf.adeq)+scale(snap.incl)+
                   scale(medicaid.incl)+
                   scale(tanf.incl)+
                   scale(welfare.pc)+
                   scale(incarrt)+
                   scale(death.rt)+
                   scale(police.pc)+
                   scale(I(blkchildpov/blkchild))+
                   scale(pop.imm.1910)+scale(pop.imm.1970)+
                   scale(pop.blk.1910)+scale(pop.blk.1970)+
                   bs.true+
                   year+(1+year|stname)+(1|obs_n), offset=log(blkchild),  
                   data=fc.ineq,
                   family=poisson)

whtcl.glmer<-glmer(bcl~scale(unemprt)+scale(childnot2par)+
                     scale(LessHS)+
                     scale(gsppercap)+
                     scale(ideo)+scale(crime.pc)+scale(pctblk)+
                     scale(tanf.adeq)+scale(snap.incl)+
                     scale(medicaid.incl)+
                     scale(tanf.incl)+
                     scale(welfare.pc)+
                     scale(incarrt)+
                     scale(death.rt)+
                     scale(police.pc)+
                     scale(I(whtchildpov/whtchild))+
                     scale(pop.imm.1910)+scale(pop.imm.1970)+
                     scale(pop.blk.1910)+scale(pop.blk.1970)+
                     bs.true+
                     year+(1+year|stname)+(1|obs_n), offset=log(blkchild),  
                   data=fc.ineq,
                   family=poisson)

### WRITE THIS UP SO FAR