rm(list=ls())
library(data.table)
library(dplyr)
library(ggplot2)

setwd("H:/census")

colClasses=sapply(fread("usa_00034.csv", nrows=100), class)
dat<-fread("usa_00034.csv", sep=",", 
           drop=c(2,3,4,6, 7, 8),
           colClasses=colClasses)


dat2<-dat%>%
  #mutate(FIPS=paste(STATEFIP, COUNTYFIPS, sep=""))%>%
  group_by(STATEFIP, YEAR)%>%
  summarise(tot=sum(PERWT),
            wht=sum((RACE==1)*(HISPAN==0)*PERWT),
            blk=sum((RACE==2)*PERWT),
            amind=sum((RACE==3)*PERWT),
            foreign=sum((BPL>120)*PERWT),
            hisp=sum((HISPAN!=0)&(HISPAN!=9)*PERWT)
            )
dat2<-dat2%>%
  filter(STATEFIP<56)

ggplot(dat2, aes(x=YEAR, y=wht/tot))+
  geom_line()+
  facet_wrap(~STATEFIP)

ggplot(dat2, aes(x=YEAR, y=log(amind/tot)))+
  geom_line()+
  facet_wrap(~STATEFIP)

ggplot(dat2, aes(x=YEAR, y=log(foreign/tot)))+
  geom_line()+
  facet_wrap(~STATEFIP)

write.csv(dat2, "census-race-1880-2014.csv", col.names=TRUE, row.names=FALSE)

rm(dat)

colClasses=sapply(fread("H:/usa_00037.csv", nrows=100), class)


dat3<-as.data.frame(fread("H:/usa_00037.csv", colClasses=colClasses))


dat4<-dat3%>%
  #mutate(FIPS=paste(STATEFIP, COUNTYFIPS, sep=""))%>%
  group_by(STATEFIP, YEAR)%>%
  summarise(tot=sum(PERWT),
            wht=sum((RACE==1)*(HISPAN==0)*PERWT),
            wht.child=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT),
            wht.child.pov=sum((RACE==1)*(HISPAN==0)*(AGE<18)*(POVERTY<101)*PERWT),
            wht.lessHS=sum((RACE==1)*(HISPAN==0)*(AGE>25)*(EDUCD<62)*PERWT), ###LESS THAN GED, NO GED OR DIPLOMA
            blk=sum((RACE==2)*PERWT),
            blk.child=sum((RACE==2)*(AGE<18)*PERWT),
            blk.child.pov=sum((RACE==2)*(AGE<18)*(POVERTY<101)*PERWT),
            blk.lessHS=sum((RACE==2)*(AGE>25)*(EDUCD<62)*PERWT),
            amind=sum((RACE==3)*PERWT),
            amind.child=sum((RACE==3)*(AGE<18)*PERWT),
            amind.child.pov=sum((RACE==3)*(AGE<18)*(POVERTY<101)*PERWT),
            amind.lessHS=sum((RACE==3)*(AGE>25)*(EDUCD<62)*PERWT),
            latino=sum(HISPAN!=c(0,9)*PERWT),
            latino.child=sum(HISPAN!=c(0,9)*(AGE<18)*PERWT),
            latino.child.pov=sum(HISPAN!=c(0,9)*(AGE<18)*(POVERTY<101)*PERWT),
            latino.lessHS=sum(HISPAN!=c(0,9)*(AGE>25)*(EDUCD<62)*PERWT),
            foreign=sum((BPL>120)*PERWT),
            foreign.child=sum((BPL>120)*(AGE<18)*PERWT),
            foreign.child.pov=sum((BPL>120)*(AGE<18)*(POVERTY<101)*PERWT),
            foreign.lessHS=sum((BPL>120)*(AGE>25)*(EDUCD<62)*PERWT),
            child=sum((AGE<18)*PERWT),
            child.pov=sum((AGE<18)*(POVERTY<101)*PERWT)
  )

write.csv(dat4, "H:/pop-race-2000-2014.csv", row.names=FALSE)