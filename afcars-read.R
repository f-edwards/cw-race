rm(list=ls())

setwd("H:/AFCARS-new")

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
    for(i in 1:length(r)){
      out<-x%>%filter(race==r[i])%>%group_by(st, datayear)%>%dplyr::summarise(cl=sum(served==1), ent=sum(entered==1), 
                                                                              lifelos=mean(lifelos, na.rm=TRUE),
                                                                              grp.inst=sum(curplset==4|curplset==5))
      names(out)[3:ncol(out)]<-paste(names(out)[3:ncol(out)], r[i], sep=".")
      out[is.na(out)]<-0
      if(i==1){r.out<-out}  
      if(i>1){r.out<-full_join(r.out, out, by=c("st", "datayear"))}
    }
  return(r.out)
}

state.out<-rbind(makeState(fc2014), makeState(fc2013), makeState(fc2012), 
                 makeState(fc2011), makeState(fc2010), makeState(fc2009),
                 makeState(fc2008), makeState(fc2007), makeState(fc2006),
                 makeState(fc2005), makeState(fc2004), makeState(fc2003),
                 makeState(fc2002), makeState(fc2001), makeState(fc2000))

write.csv(state.out, "fc-race-state.csv", row.names=FALSE)

makeCounty<-function(x){
  r<-unique(x$race)
  for(i in 1:length(r)){
    out<-x%>%filter(race==r[i])%>%group_by(fipscode, datayear, st)%>%dplyr::summarise(cl=sum(served==1), ent=sum(entered==1), 
                                                                            lifelos=mean(lifelos, na.rm=TRUE),
                                                                            grp.inst=sum(curplset==4|curplset==5))
    names(out)[4:ncol(out)]<-paste(names(out)[4:ncol(out)], r[i], sep=".")
    out[is.na(out)]<-0
    if(i==1){r.out<-out}  
    if(i>1){r.out<-full_join(r.out, out, by=c("fipscode", "st", "datayear"))}
  }
  return(r.out)
}


county.out<-rbind(makeCounty(fc2014), makeCounty(fc2013), makeCounty(fc2012), 
                 makeCounty(fc2011), makeCounty(fc2010), makeCounty(fc2009),
                 makeCounty(fc2008), makeCounty(fc2007), makeCounty(fc2006),
                 makeCounty(fc2005), makeCounty(fc2004), makeCounty(fc2003),
                 makeCounty(fc2002), makeCounty(fc2001), makeCounty(fc2000))

write.csv(county.out, "fc-race-county.csv", row.names=FALSE)
