rm(list=ls())

setwd("U:/AFCARS-new")

library(data.table, lib.loc="C:/Users/fedwards/Documents/R/win-library/3.2")
library(dplyr)
library(tidyr, lib.loc="C:/Users/fedwards/Documents/R/win-library/3.2")


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
fc2013<-data.frame(make.race(names.funct(fread("fc2013.csv"))))
fc2012<-data.frame(make.race(names.funct(fread("fc2012.csv"))))
fc2011<-data.frame(make.race(names.funct(fread("fc2011.csv"))))
fc2010<-data.frame(make.race(names.funct(fread("fc2010.csv"))))
fc2009<-data.frame(make.race(names.funct(fread("fc2009.csv"))))
fc2008<-data.frame(make.race(names.funct(fread("fc2008.csv"))))
fc2007<-data.frame(make.race(names.funct(fread("fc2007.csv"))))
fc2006<-data.frame(make.race(names.funct(fread("fc2006.csv"))))
fc2005<-data.frame(make.race(names.funct(fread("fc2005.csv"))))
fc2004<-data.frame(make.race(names.funct(fread("fc2004.csv"))))
fc2003<-data.frame(make.race(names.funct(fread("fc2003.csv"))))
fc2002<-data.frame(make.race(names.funct(fread("fc2002.csv"))))
fc2001<-data.frame(make.race(names.funct(fread("fc2001.csv"))))
fc2000<-data.frame(make.race(names.funct(fread("fc2000.csv"))))



makeState<-function(x){
  r<-unique(x$race)
  x$disreasn<-ifelse(is.na(x$disreasn), 0, x$disreasn)
  x$lifelos<-ifelse(is.na(x$lifelos), 0, x$lifelos)
  x$curplset<-ifelse(is.na(x$curplset), 99, x$curplset)
  for(i in 1:length(r)){
    out<-x%>%filter(race==r[i])%>%group_by(st, datayear)%>%
      dplyr::summarise(cl=sum(served==1), ent=sum(entered==1),
                       reun=sum((disreasn==1)|(disreasn==2)|(disreasn==5)), 
                       lifelos=mean(lifelos, na.rm=TRUE),
                       grp.inst=sum(curplset==4|curplset==5),
                       kin.fc=sum(curplset==2),
                       inatend=sum(inatend))
    names(out)[3:ncol(out)]<-paste(names(out)[3:ncol(out)], r[i], sep=".")
    if(i==1){r.out<-out}  
    if(i>1){r.out<-full_join(r.out, out, by=c("st", "datayear"))}
  }
  tot<-x%>%group_by(st, datayear)%>%dplyr::summarise(cl=sum(served==1), ent=sum(entered==1), grp.inst=sum(curplset==4|curplset==5),
                                                     reun=sum((disreasn==1)|(disreasn==2)|(disreasn==5)),
                                                     lifelos=mean(lifelos, na.rm=TRUE),
                                                     grp.inst=sum(curplset==4|curplset==5),
                                                     kin.fc=sum(curplset==2),
                                                     inatend=sum(inatend))
  r.out<-full_join(r.out, tot, by=c("st", "datayear"))
  
  return(r.out)
}

state.out<-rbind(makeState(fc2014), makeState(fc2013), makeState(fc2012), 
                 makeState(fc2011), makeState(fc2010), makeState(fc2009),
                 makeState(fc2008), makeState(fc2007), makeState(fc2006),
                 makeState(fc2005), makeState(fc2004), makeState(fc2003),
                 makeState(fc2002), makeState(fc2001), makeState(fc2000))
names(state.out)[1:2]<-c("stname", "year")

setwd("U:/cw-race/")
source("U:/cw-race/cw-race-functions.r")
fc<-read.csv("U:/cw-race/data/fc.csv")

fcts<-left_join(fc, state.out, by=c("stname", "year"))
fcclrt<-fcts%>%group_by(year)%>%summarise(clrt=sum(inatend)/sum(child), clrt.blk=sum(inatend.blk)/sum(blk.child),
                                          clrt.nat=sum(inatend.nat.am)/sum(amind.child),
                                          incarrt=sum(incartot)/sum(adult), incarrt.blk=sum(BLACKM+BLACKF)/sum(b.adult),
                                          incarrt.nat=sum(AIANM+AIANF)/sum(a.adult))
fcclrt[,2:7]<-fcclrt[,2:7]*100000
library(ggplot2)

ggplot(fcclrt, aes(x=year, y=clrt))+geom_line()+geom_line(lty=2, aes(y=incarrt))

ggplot(fcclrt, aes(x=year))+geom_line(aes(y=clrt.blk), col="blue")+geom_line(aes(y=incarrt.blk))