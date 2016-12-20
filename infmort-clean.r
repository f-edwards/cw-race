library(dplyr)
setwd("D:/sync/cw-race/data")

cmdat<-read.csv("InfMort2000-2012.csv")

cmdat$FIPS<-cmdat$FIPS/1000

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

c.nw<-cmdat[,1:10]

FIPS<-rep(c.nw$FIPS, ncol(c.nw)-1)

c.rs<-cbind(FIPS, c(rep(2012, 51), rep(2011, 51), rep(2010, 51), rep(2009, 51), rep(2008, 51), rep(2007, 51),
            rep(2006, 51), rep(2005, 51), rep(2004, 51)))

mort<-c(c.nw[,2], c.nw[,3], c.nw[,4], c.nw[,5], c.nw[,6], c.nw[,7], c.nw[,8], c.nw[,9], c.nw[,10])

c.nonw<-as.data.frame(cbind(c.rs, mort))

c.nw<-cmdat[,c(1, 11:ncol(cmdat))]

FIPS<-rep(c.nw$FIPS, ncol(c.nw)-1)

c.rs<-cbind(FIPS, c(rep(2012, 51), rep(2011, 51), rep(2010, 51), rep(2009, 51), rep(2008, 51), rep(2007, 51),
                    rep(2006, 51), rep(2005, 51), rep(2004, 51)))

mort<-c(c.nw[,2], c.nw[,3], c.nw[,4], c.nw[,5], c.nw[,6], c.nw[,7], c.nw[,8], c.nw[,9], c.nw[,10])

c.w<-as.data.frame(cbind(c.rs, mort))
c.nonw$wht.inf.mort<-c.w$mort
names(c.nonw)[2:3]<-c("year", "nonwht.inf.mort")

c.nonw$inf.mortdisp<-c.nonw$nonwht.inf.mort/c.nonw$wht.inf.mort

write.csv(c.nonw, "infmort.csv", row.names=FALSE)