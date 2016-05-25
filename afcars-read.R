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

fc2014<-make.race(names.funct(fread("fc2014.csv")))
fc2013<-make.race(names.funct(fread("fc2013.csv")))
fc2012<-make.race(names.funct(fread("fc2012.csv")))
fc2011<-make.race(names.funct(fread("fc2011.csv")))
fc2010<-make.race(names.funct(fread("fc2010.csv")))
fc2009<-make.race(names.funct(fread("fc2009.csv")))
fc2008<-make.race(names.funct(fread("fc2008.csv")))
fc2007<-make.race(names.funct(fread("fc2007.csv")))
fc2006<-make.race(names.funct(fread("fc2006.csv")))
fc2005<-make.race(names.funct(fread("fc2005.csv")))
fc2004<-make.race(names.funct(fread("fc2004.csv")))
fc2003<-make.race(names.funct(fread("fc2003.csv")))
fc2002<-make.race(names.funct(fread("fc2002.csv")))
fc2001<-make.race(names.funct(fread("fc2001.csv")))
fc2000<-make.race(names.funct(fread("fc2000.csv")))


makeState<-function(x){
    out<-x%>%group_by(st, race)%>%summarise(cl=n(), ent=sum(entered==1))
}