rm(list=ls())
library(data.table)
library(dplyr)

setwd("H:/census")

# colClasses=sapply(fread("usa_00034.csv", nrows=100), class)
# dat<-fread("usa_00034.csv", sep=",", 
#            drop=c(2,3,4,6, 7, 8),
#            colClasses=colClasses)
# 
# 
# dat2<-dat%>%
#   #mutate(FIPS=paste(STATEFIP, COUNTYFIPS, sep=""))%>%
#   group_by(STATEFIP, YEAR)%>%
#   summarise(tot=sum(PERWT),
#             wht=sum((RACE==1)*(HISPAN==0)*PERWT),
#             blk=sum((RACE==2)*PERWT),
#             amind=sum((RACE==3)*PERWT),
#             foreign=sum((BPL>120)*PERWT),
#             hisp=sum((HISPAN!=0)&(HISPAN!=9)*PERWT)
#             )
# dat2<-dat2%>%
#   filter(STATEFIP<56)
# 
# ggplot(dat2, aes(x=YEAR, y=wht/tot))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# 
# ggplot(dat2, aes(x=YEAR, y=log(amind/tot)))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# 
# ggplot(dat2, aes(x=YEAR, y=log(foreign/tot)))+
#   geom_line()+
#   facet_wrap(~STATEFIP)
# 
# write.csv(dat2, "census-race-1880-2014.csv", col.names=TRUE, row.names=FALSE)
# 
# rm(dat)


colClasses=sapply(fread("H:/census/usa_00038.csv", nrows=100), class)
dat3<-as.data.frame(fread("H:/census/usa_00038.csv", colClasses=colClasses))
dat3[dat3$INCWELFR==99999,"INCWELFR"]<-0

# z<-sample(1:nrow(dat3), 50000)
# test<-dat3[z,]
# test%>%summarise(latino=sum((HISPAN %in% c(1:4)))*PERWT), 
#                  latino2=sum((HISPAN %in% c(1:4))*PERWT))

dat4<-dat3%>%
  group_by(STATEFIP, YEAR)%>%
  summarise(tot=sum(as.numeric(PERWT)),
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
            blk.welf=sum((RACE==2)*(INCWELFR>0)*PERWT),
            latino.welf=sum((HISPAN %in% c(1:4))*PERWT*(INCWELFR>0)),
            amind.welf=sum((RACE==3)*PERWT*(INCWELFR>0)),
            wht.snap=sum((RACE==1)*(HISPAN==0)*(FOODSTMP==2)*PERWT),
            blk.snap=sum((RACE==2)*(FOODSTMP==2)*PERWT),
            latino.snap=sum((HISPAN %in% c(1:4))*PERWT*(FOODSTMP==2)),
            amind.snap=sum((RACE==3)*PERWT*(FOODSTMP==2)),
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
            wht.singpar=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1))
            )

# PAR<-dat3%>%group_by(STATEFIP, YEAR)%>%
#   summarise(blk.singpar=sum((RACE==2)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
#             latino.singpar=sum((AGE<18)*PERWT* (HISPAN %in% c(1:4))*((NMOTHERS>0)+(NFATHERS>0)==1)),
#             amind.singpar=sum((RACE==3)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1)),
#             wht.singpar=sum((RACE==1)*(HISPAN==0)*(AGE<18)*PERWT*((NMOTHERS>0)+(NFATHERS>0)==1))
#   )
# 
# dat5<-left_join(left_join(dat4, HS, by=c("STATEFIP", "YEAR")), PAR, by=c("STATEFIP", "YEAR"))



write.csv(dat4, "H:/cw-race/data/pop-race-2000-2014.csv", row.names=FALSE)