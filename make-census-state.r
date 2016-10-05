rm(list=ls())
library(data.table)
library(dplyr)
library(dtplyr)

setwd("U:/")

colClasses=sapply(fread("usa_00040.csv", nrows=100), class)
dat<-fread("usa_00040.csv", sep=",",
           colClasses=colClasses)

dat2<-dat%>%group_by(STATEFIP, YEAR)%>%
  dplyr::summarise(tot=sum(PERWT),
            wht=sum((RACE==1)*(HISPAN==0)*PERWT),
            blk=sum((RACE==2)*PERWT),
            amind=sum((RACE==3)*PERWT),
            foreign=sum((BPL>120)*PERWT),
            hisp=sum((HISPAN!=0)&(HISPAN!=9)*PERWT))%>%
            filter(STATEFIP<57)%>%filter(STATEFIP!=11)


d1900<-dat2%>%group_by(STATEFIP)%>%
  filter(YEAR==1900)%>%
  summarise(pctblk1900=blk/tot, pctami1900=amind/tot, pctimm1900=foreign/tot)

state<-unique(dat2$STATEFIP)
hist.pop<-data.frame("state"=rep(NA, 50), "pctblk1900"=rep(NA, 50), "pctami1900"=rep(NA, 50), "pctimm1900"=rep(NA, 50),
                     "pctblk1930"=rep(NA, 50), "pctami1930"=rep(NA, 50), "pctimm1930"=rep(NA, 50),
                     "pctblk1960"=rep(NA, 50), "pctami1960"=rep(NA, 50), "pctimm1960"=rep(NA, 50))

for(i in 1:length(state)){
  hist.pop$state[i]<-state[i]
  hist.pop$pctblk1960[i]<-with(dat2%>%filter(YEAR==1960 & STATEFIP==state[i]), blk/tot)
  hist.pop$pctami1960[i]<-with(dat2%>%filter(YEAR==1960 & STATEFIP==state[i]), amind/tot)
  hist.pop$pctimm1960[i]<-with(dat2%>%filter(YEAR==1960 & STATEFIP==state[i]), foreign/tot)
  
  hist.pop$pctblk1930[i]<-with(dat2%>%filter(YEAR==1930 & STATEFIP==state[i]), blk/tot)
  hist.pop$pctami1930[i]<-with(dat2%>%filter(YEAR==1930 & STATEFIP==state[i]), amind/tot)
  hist.pop$pctimm1930[i]<-with(dat2%>%filter(YEAR==1930 & STATEFIP==state[i]), foreign/tot)
  
  hist.pop$pctblk1900[i]<-ifelse(is.numeric(with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot)),
                                            with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot), NA)
  hist.pop$pctami1900[i]<-ifelse(is.numeric(with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot)),
                                            with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot), NA)
  hist.pop$pctimm1900[i]<-ifelse(is.numeric(with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot)),
                                            with(dat2%>%filter(YEAR==1900 & STATEFIP==state[i]), blk/tot), NA)
  
}

setwd("C:/Users/kilgore/Dropbox/cw-race/data/")
write.csv(hist.pop, "hist-pop.csv", col.names=TRUE, row.names=FALSE)


# # 
# ggplot(dat2, aes(x=YEAR, y=blk/tot))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# # 
# ggplot(dat2, aes(x=YEAR, y=log(amind/tot)))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# 
# ggplot(dat2, aes(x=YEAR, y=log(foreign/tot)))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# 
# 
# rm(dat)


setwd("U:/")

colClasses=sapply(fread("usa_00043.csv/usa_00043.csv", nrows=100), class)
dat3<-as.data.frame(fread("usa_00043.csv/usa_00043.csv", colClasses=colClasses))
dat3[dat3$INCWELFR==99999,"INCWELFR"]<-0
 
dat4<-dat3%>%
  group_by(STATEFIP, YEAR)%>%
  dplyr::summarise(tot=sum(as.numeric(PERWT)),
            wht=sum((RACE==1)*(HISPAN==0)*PERWT), wht.child=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT),
            wht.child.pov=sum((RACE==1)*(HISPAN==0)*(AGE<18)*(POVERTY<101)*PERWT),
            wht.lessHS=sum((RACE==1)*(HISPAN==0)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==1)*(HISPAN==0)*(AGE>25)*PERWT), ###LESS THAN GED, NO GED OR DIPLOMA
            blk=sum((RACE==2)*PERWT), blk.child=sum((RACE==2)*(AGE<18)*PERWT),
            blk.child.pov=sum((RACE==2)*(AGE<18)*(POVERTY<101)*PERWT),
            blk.lessHS=sum((RACE==2)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==2)*(AGE>25)*PERWT),
            amind=sum((RACE==3)*PERWT), amind.child=sum((RACE==3)*(AGE<18)*PERWT),
            amind.child.pov=sum((RACE==3)*(AGE<18)*(POVERTY<101)*PERWT),
            amind.lessHS=sum((RACE==3)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==3)*(AGE>25)*PERWT),
            latino=sum((HISPAN %in% c(1:4))*PERWT),
            latino.child=sum((HISPAN %in% c(1:4))*(AGE<18)*PERWT),
            latino.child.pov=sum((HISPAN %in% c(1:4))*(AGE<18)*(POVERTY<101)*PERWT),
            latino.lessHS=sum((HISPAN %in% c(1:4))*(AGE>25)*(EDUCD<62)*PERWT) / sum((HISPAN %in% c(1:4))*(AGE>25)*PERWT),
            foreign=sum((BPL>120)*PERWT), foreign.child=sum((BPL>120)*(AGE<18)*PERWT),
            foreign.child.pov=sum((BPL>120)*(AGE<18)*(POVERTY<101)*PERWT),
            foreign.lessHS=sum((BPL>120)*(AGE>25)*(EDUCD<62)*PERWT),
            child=sum((AGE<18)*PERWT), child.pov=sum((AGE<18)*(POVERTY<101)*PERWT),
            adult=sum((AGE>17)*PERWT), welf=sum((INCWELFR>0)*PERWT),
            wht.welf=sum((RACE==1)*(HISPAN==0)*(INCWELFR>0)*PERWT),
            w.adult=sum((AGE>17)*(RACE==1)*(HISPAN==0)*PERWT), welf=sum((INCWELFR>0)*PERWT),
            b.adult=sum((AGE>17)*(RACE==2)*PERWT), welf=sum((INCWELFR>0)*PERWT),
            a.adult=sum((AGE>17)*(RACE==3)*PERWT), welf=sum((INCWELFR>0)*PERWT),
            l.adult=sum((AGE>17)*(HISPAN %in% c(1:4))*PERWT), welf=sum((INCWELFR>0)*PERWT),
            blk.welf=sum((RACE==2)*(INCWELFR>0)*PERWT),
            latino.welf=sum((HISPAN %in% c(1:4))*PERWT*(INCWELFR>0)),
            amind.welf=sum((RACE==3)*PERWT*(INCWELFR>0)),
            wht.unemp=sum((EMPSTAT==2)*PERWT*(RACE==1)*(HISPAN==0)),
            blk.unemp=sum((RACE==2)*(EMPSTAT==2)*PERWT),
            latino.unemp=sum((HISPAN %in% c(1:4))*PERWT*(EMPSTAT==2)),
            amind.unemp=sum((RACE==3)*PERWT*(EMPSTAT==2)),
            wht.emp=sum((EMPSTAT==1)*(RACE==1)*(HISPAN==0)*PERWT),
            blk.emp=sum((RACE==2)*(EMPSTAT==1)*PERWT),
            latino.emp=sum((HISPAN %in% c(1:4))*PERWT*(EMPSTAT==1)),
            amind.emp=sum((RACE==3)*PERWT*(EMPSTAT==1)),
            blk.singpar=sum((RACE==2)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
            latino.singpar=sum((AGE<18)*PERWT* (HISPAN %in% c(1:4))*((NMOTHERS>0)+(NFATHERS>0)==1)),
            amind.singpar=sum((RACE==3)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
            wht.singpar=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
            wht.f=sum((RACE==1)*(HISPAN==0)*(AGE>17)*PERWT*(SEX==2)),
            blk.f=sum((RACE==2)*(AGE>17)*PERWT*(SEX==2)),
            amind.f=sum((RACE==3)*(AGE>17)*PERWT*(SEX==2))
            )

write.csv(dat4, "pop-race-2000-2014-5yr.csv", row.names=FALSE)

## USING 2000, 2010 Census
colClasses=sapply(fread("usa_00043.csv/usa_00043.csv", nrows=100), class)
dat5<-as.data.frame(fread("usa_00042.csv", colClasses=colClasses))
dat5<-dat5%>%filter(((YEAR==2010)&(DATANUM==1))|(YEAR==2000)) ## ONLY 2000, 2010 census, to be used for MI prior

dat6<-dat5%>%
  group_by(STATEFIP, YEAR)%>%
  dplyr::summarise(tot=sum(as.numeric(PERWT)),
                   wht=sum((RACE==1)*(HISPAN==0)*PERWT), wht.child=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT),
                   wht.child.pov=sum((RACE==1)*(HISPAN==0)*(AGE<18)*(POVERTY<101)*PERWT),
                   wht.lessHS=sum((RACE==1)*(HISPAN==0)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==1)*(HISPAN==0)*(AGE>25)*PERWT), ###LESS THAN GED, NO GED OR DIPLOMA
                   blk=sum((RACE==2)*PERWT), blk.child=sum((RACE==2)*(AGE<18)*PERWT),
                   blk.child.pov=sum((RACE==2)*(AGE<18)*(POVERTY<101)*PERWT),
                   blk.lessHS=sum((RACE==2)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==2)*(AGE>25)*PERWT),
                   amind=sum((RACE==3)*PERWT), amind.child=sum((RACE==3)*(AGE<18)*PERWT),
                   amind.child.pov=sum((RACE==3)*(AGE<18)*(POVERTY<101)*PERWT),
                   amind.lessHS=sum((RACE==3)*(AGE>25)*(EDUCD<62)*PERWT) / sum((RACE==3)*(AGE>25)*PERWT),
                   latino=sum((HISPAN %in% c(1:4))*PERWT),
                   latino.child=sum((HISPAN %in% c(1:4))*(AGE<18)*PERWT),
                   latino.child.pov=sum((HISPAN %in% c(1:4))*(AGE<18)*(POVERTY<101)*PERWT),
                   latino.lessHS=sum((HISPAN %in% c(1:4))*(AGE>25)*(EDUCD<62)*PERWT) / sum((HISPAN %in% c(1:4))*(AGE>25)*PERWT),
                   foreign=sum((BPL>120)*PERWT), foreign.child=sum((BPL>120)*(AGE<18)*PERWT),
                   foreign.child.pov=sum((BPL>120)*(AGE<18)*(POVERTY<101)*PERWT),
                   foreign.lessHS=sum((BPL>120)*(AGE>25)*(EDUCD<62)*PERWT),
                   child=sum((AGE<18)*PERWT), child.pov=sum((AGE<18)*(POVERTY<101)*PERWT),
                   adult=sum((AGE>17)*PERWT), welf=sum((INCWELFR>0)*PERWT),
                   wht.welf=sum((RACE==1)*(HISPAN==0)*(INCWELFR>0)*PERWT),
                   w.adult=sum((AGE>17)*(RACE==1)*(HISPAN==0)*PERWT), welf=sum((INCWELFR>0)*PERWT),
                   b.adult=sum((AGE>17)*(RACE==2)*PERWT), welf=sum((INCWELFR>0)*PERWT),
                   a.adult=sum((AGE>17)*(RACE==3)*PERWT), welf=sum((INCWELFR>0)*PERWT),
                   l.adult=sum((AGE>17)*(HISPAN %in% c(1:4))*PERWT), welf=sum((INCWELFR>0)*PERWT),
                   blk.welf=sum((RACE==2)*(INCWELFR>0)*PERWT),
                   latino.welf=sum((HISPAN %in% c(1:4))*PERWT*(INCWELFR>0)),
                   amind.welf=sum((RACE==3)*PERWT*(INCWELFR>0)),
                   wht.unemp=sum((EMPSTAT==2)*PERWT*(RACE==1)*(HISPAN==0)),
                   blk.unemp=sum((RACE==2)*(EMPSTAT==2)*PERWT),
                   latino.unemp=sum((HISPAN %in% c(1:4))*PERWT*(EMPSTAT==2)),
                   amind.unemp=sum((RACE==3)*PERWT*(EMPSTAT==2)),
                   wht.emp=sum((EMPSTAT==1)*(RACE==1)*(HISPAN==0)*PERWT),
                   blk.emp=sum((RACE==2)*(EMPSTAT==1)*PERWT),
                   latino.emp=sum((HISPAN %in% c(1:4))*PERWT*(EMPSTAT==1)),
                   amind.emp=sum((RACE==3)*PERWT*(EMPSTAT==1)),
                   blk.singpar=sum((RACE==2)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
                   latino.singpar=sum((AGE<18)*PERWT* (HISPAN %in% c(1:4))*((NMOTHERS>0)+(NFATHERS>0)==1)),
                   amind.singpar=sum((RACE==3)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
                   wht.singpar=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
                   wht.f=sum((RACE==1)*(HISPAN==0)*(AGE>17)*PERWT*(SEX==2)),
                   blk.f=sum((RACE==2)*(AGE>17)*PERWT*(SEX==2)),
                   amind.f=sum((RACE==3)*(AGE>17)*PERWT*(SEX==2))
  )

write.csv(dat6, "pop-census-2000-2010.csv", row.names=FALSE)
