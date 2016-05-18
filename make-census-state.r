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

