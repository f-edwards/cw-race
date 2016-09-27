#########
rm(list=ls())
set.seed(1)
library(merTools)
library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(MASS)

setwd("C:/Users/kilgore/Dropbox/cw-race/")
source("C:/Users/kilgore/Dropbox/cw-race/cw-race-functions.r")
fc<-read.csv("fc.csv")


## for laptop
# setwd("~/Dropbox/cw-race/")
# source("~/Dropbox/cw-race/cw-race-functions.r")
# fc<-read.csv("fc.csv")
# ##for output
# setwd("~/Dropbox/cw-race-paper/")

# ### for libra
# setwd("~/cw-race/data/")
# source("~/cw-race/cw-race-functions.r")
# source("~/cw-race/cw-race-read.r")
# setwd("~/cw-race/")

##unstable TS vars, amind.lessHS, amind.child, amind.f, blk.child (WV, HI, OR, UT, VT) 

##EVERYTHING IN ACS IS SUSPECT FOR 

no.imp.plot<-ggplot(fc)+geom_line(aes(x=year, y=log(amind.child.pov)))+facet_wrap(~stname)+ggtitle("Without transformation")

###imputations on FC raw data - drop unused vars

##DROP UNUSED VARIABLES

ratios<-which(names(fc)%in%c("blk.lessHS", "wht.lessHS", "amind.lessHS"))


bounds<-cbind(1:ncol(fc),
              rep(0.000001, ncol(fc)),
              rep(Inf, ncol(fc)))

bounds[ratios, 3]<-1

colClass<-sapply(fc, class)
colClass[2]<-"year"
colClass[66:73]<-"hist"
colClass[36:39]<-"adult"
colClass[15]<-"state"

numeric.vals<-NULL
for(i in 1:ncol(fc)){
  if(colClass[i]%in%c("integer", "numeric")){
    numeric.vals<-c(numeric.vals, i)
  }
}

m<-5

# m=ceiling(max(apply(fc.ineq, 2, function(x){sum(is.na(x))}))/nrow(fc.ineq)*100)
blk.acs<-which(colnames(fc)%in%c("blk.child", "blk.child.pov", "blk.lessHS", "blk.unemp", "blk.singpar"))
amind.acs<-which(colnames(fc)%in%c("amind.child", "amind.child.pov", "amind.lessHS", "amind.unemp", "amind.singpar"))

strong<-c(2000,2007:2014)
amind.thresh<-which((fc$amind.child<100000)&(!(fc$year%in%strong)))
blk.thresh<-which((fc$blk.child<100000)&(!(fc$year%in%strong)))

amind.oi<-matrix(nrow=length(amind.acs)*length(amind.thresh), ncol=2)
amind.oi[,2]<-rep(amind.acs, length(amind.thresh))

for(i in 1:length(amind.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(amind.acs)-1
  amind.oi[oi.min:oi.max,1]<-rep(amind.thresh[i], length(amind.acs))
  oi.min<-oi.max+1
}

blk.oi<-matrix(nrow=length(blk.acs)*length(blk.thresh), ncol=2)
blk.oi[,2]<-rep(blk.acs, length(blk.thresh))

for(i in 1:length(blk.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(blk.acs)-1
  blk.oi[oi.min:oi.max,1]<-rep(blk.thresh[i], length(blk.acs))
  oi.min<-oi.max+1
}

oi<-rbind(amind.oi, blk.oi)

oi.priors<-matrix(nrow=nrow(oi), ncol=4)
oi.priors[,1:2]<-oi[,1:2]

create.prior<-function(x){
  ##function receives row,column pair to set prior based on strong observation years, compute mean and sd based on observed strong periods
  st<-fc[oi[x,1], "stname"]
  var<-oi[x,2]
  year<-fc[oi[x,1], "year"]
  strong.2000<-fc[which((fc$year==2000)&(fc$stname==st)), var]
  strong.2009<-fc[which((fc$year==2009)&(fc$stname==st)), var]
  ###WEIGHT ON PROXIMITY TO OBS, with obs - Weight is 0.1 on obs, 0.75 on 2000 at 2001, linear to 0.15 at 2009
  pr.E<-(0.1*fc[oi[x,1], var])+((0.75-(year-2001)*0.12))*strong.2000+(.9-(0.75-(year-2001)*0.12))*strong.2009
  all.set<-fc[fc$stname==st,var]
  pr.SD<-sd(all.set)/sqrt(15)
  return(c(pr.E, pr.SD))
}

for(i in 1:nrow(oi.priors)){
  oi.priors[i,3:4]<-create.prior(i)
}


##imputation model
m<-15
fc.imp<-amelia(fc, m=m,
               ts="year", cs="stname", polytime=1, 
               bounds=bounds, overimp=oi,  
               priors=oi.priors, p2s=0,
               idvars=c("state", "statename","adult", "w.adult", "b.adult", "a.adult", 
                        "pctblk1930", "pctimm1930", "pctami1930", "pctblk1960", "pctami1960", 
                        "pctimm1960", "boarding.n", "board"))


for(i in 1:m){
fc.imp$imputations[[i]]<-
  fc.imp$imputations[[i]]%>%mutate(obs=1:nrow(fc),
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
}

post.imp.plot<-ggplot(fc.imp$imputations[[2]])+geom_line(aes(x=year, y=log(amind.child.pov)))+facet_wrap(~stname)+ggtitle("After overimputation")


#### TO SET INTEGERS IF DOING COUNT MODELS
# for(i in 1:m){
#   for(j in 1:ncol(fc)){
#     if(colClass[j]=="integer"){
#       fc.imp$imputations[[i]][,j]<-ceiling(fc.imp$imputations[[i]][,j])
#     }
#   }
# }

###lags and leads
for(i in 1:m){
  fc.imp$imputations[[i]]<-fc.imp$imputations[[i]]%>%group_by(stname)%>%arrange(year.c)%>%
  mutate(incarrt.lag=lag(incarrt), b.incarrt.lag=lag(b.incarrt), a.incarrt.lag=lag(a.incarrt),
         b.m.incarrt.lag=lag(b.m.incarrt), b.f.incarrt.lag=lag(b.f.incarrt),
         a.m.incarrt.lag=lag(a.m.incarrt), a.f.incarrt.lag=lag(a.f.incarrt),
         cl.blk.lag=lag(cl.blk), cl.nat.am.lag=lag(cl.nat.am),
         clrt.blk.lag=lag(cl.blk)/lag(blk.child), clrt.nat.am.lag=lag(cl.nat.am)/lag(amind.child),
         incarrt.lead=lead(incarrt), b.incarrt.lead=lead(b.incarrt), a.incarrt.lead=lead(a.incarrt),
         b.m.incarrt.lead=lead(b.m.incarrt), b.f.incarrt.lead=lead(b.m.incarrt),
         a.m.incarrt.lead=lead(a.m.incarrt), a.f.incarrt.lead=lead(a.f.incarrt),
         cl.blk.lead=lead(cl.blk), cl.nat.am.lead=lead(cl.nat.am),
         clrt.blk.lead=lead(cl.blk)/lead(blk.child), cl.nat.am.lead=lead(cl.nat.am)/lead(amind.child),
         clrt.blk.lag2=lag(cl.blk.pc, n=2), clrt.nat.am.lag2=lag(cl.amind.pc, n=2))
}

########## PREPARE TABLE OUTPUT, PREPARE SIMULATION / PREDICTION

################
#Overimputation
################
fe.data<-fc.imp$imputations
source("FE-models.r", print.eval=TRUE)

OI.models<-list("FE.models"=FE.models, "lag.models"=lag.models, 
                "fd.models"=fd.models, "rob.models"=rob.models, 
                "FE.ent.models"=FE.ent.models, "lag.ent.models"=lag.ent.models, 
                "fd.ent.models"=fd.ent.models, "rob.ent.models"=rob.ent.models)

################
#Drop 1 yr ACS
################
fe.data<-fc.imp$imputations
for(i in 1:m){
  fe.data[[i]]<-fe.data[[i]]%>%filter(year>2006)
}
source("FE-models.r", print.eval=TRUE)
No1yr.models<-list("FE.models"=FE.models, "lag.models"=lag.models, 
                "fd.models"=fd.models, "rob.models"=rob.models, 
                "FE.ent.models"=FE.ent.models, "lag.ent.models"=lag.ent.models, 
                "fd.ent.models"=fd.ent.models, "rob.ent.models"=rob.ent.models)

##############
#Drop small pop states
##############
fe.data<-fc.imp$imputation
for(i in 1:m){
  fe.data[[i]]<-fe.data[[i]]%>%filter(amind>10000)
  fe.data[[i]]<-fe.data[[i]]%>%filter(blk>10000)
}
source("FE-models.r", print.eval=TRUE)
drop.models<-list("FE.models"=FE.models, "lag.models"=lag.models, 
                   "fd.models"=fd.models, "rob.models"=rob.models, 
                   "FE.ent.models"=FE.ent.models, "lag.ent.models"=lag.ent.models, 
                   "fd.ent.models"=fd.ent.models, "rob.ent.models"=rob.ent.models)


###############
#MI, No MO, 3 yr avg
###############
##ts structure
m<-5

fc[fc==0]<-NA

bounds<-cbind(1:ncol(fc),
              rep(0.000001, ncol(fc)),
              rep(Inf, ncol(fc)))

bounds[ratios, 3]<-1

fc.imp.TS<-amelia(fc, m=m,
               ts="year", cs="stname", polytime=1,
               bounds=bounds, p2=0,
               #priors=oi.priors,
               idvars=c("state", "statename","adult", "w.adult", "b.adult", "a.adult",
                        "pctblk1930", "pctimm1930", "pctami1930", "pctblk1960", "pctami1960",
                        "pctimm1960", "boarding.n", "board", "grp.inst.white", "grp.inst.blk",
                        "grp.inst.nat.am"))

###transformations
for(i in 1:m){
  fc.imp.TS$imputations[[i]]<-
    fc.imp.TS$imputations[[i]]%>%mutate(obs=1:nrow(fc),
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
}

###lags and leads
for(i in 1:m){
  fc.imp.TS$imputations[[i]]<-fc.imp$imputations[[i]]%>%group_by(stname)%>%arrange(year.c)%>%
    mutate(incarrt.lag=lag(incarrt), b.incarrt.lag=lag(b.incarrt), a.incarrt.lag=lag(a.incarrt),
           b.m.incarrt.lag=lag(b.m.incarrt), b.f.incarrt.lag=lag(b.f.incarrt),
           a.m.incarrt.lag=lag(a.m.incarrt), a.f.incarrt.lag=lag(a.f.incarrt),
           cl.blk.lag=lag(cl.blk), cl.nat.am.lag=lag(cl.nat.am),
           clrt.blk.lag=lag(cl.blk)/lag(blk.child), clrt.nat.am.lag=lag(cl.nat.am)/lag(amind.child),
           incarrt.lead=lead(incarrt), b.incarrt.lead=lead(b.incarrt), a.incarrt.lead=lead(a.incarrt),
           b.m.incarrt.lead=lead(b.m.incarrt), b.f.incarrt.lead=lead(b.m.incarrt),
           a.m.incarrt.lead=lead(a.m.incarrt), a.f.incarrt.lead=lead(a.f.incarrt),
           cl.blk.lead=lead(cl.blk), cl.nat.am.lead=lead(cl.nat.am),
           clrt.blk.lead=lead(cl.blk)/lead(blk.child), cl.nat.am.lead=lead(cl.nat.am)/lead(amind.child),
           clrt.blk.lag2=lag(cl.blk.pc, n=2), clrt.nat.am.lag2=lag(cl.amind.pc, n=2))
}

fe.data<-fc.imp.TS$imputations
source("FE-models.r", print.eval=TRUE)

MI.models<-list("FE.models"=FE.models, "lag.models"=lag.models, 
                  "fd.models"=fd.models, "rob.models"=rob.models, 
                  "FE.ent.models"=FE.ent.models, "lag.ent.models"=lag.ent.models, 
                  "fd.ent.models"=fd.ent.models, "rob.ent.models"=rob.ent.models)

##########################
#### VISUAL, FOREST PLOT FOR FOCAL RESULTS
##########################


forest.data<-data.frame("beta"=NA, "upper"=NA, "lower"=NA, "model"=NA, "var"=NA, "method"=NA)

var.names<-c(row.names(OI.models$FE.models$b$merge[[1]])[1:2], row.names(OI.models$FE.models$b.m$merge[[1]])[1:2], row.names(OI.models$FE.models$b.f$merge[[1]])[1:2],
        row.names(OI.models$FE.models$a$merge[[1]])[1:2], row.names(OI.models$FE.models$a.m$merge[[1]])[1:2], row.names(OI.models$FE.models$a.f$merge[[1]])[1:2])
vars<-c("b","b.m", "b.f", "a", "a.m", "a.f")
models<-c("FE.models", "lag.models", "fd.models", "rob.models", "FE.ent.models", "lag.ent.models", "fd.ent.models", "rob.ent.models")
methods<-c("Overimp", "No1yr", "DropSmall", "MovingAve")

for(m in 1:length(methods)){
  for(n in 1:length(models)){
    for(o in 1:length(vars)){
      if(m==1){
          vals<-OI.models[[models[n]]][[vars[o]]]$merge[[1]][which(row.names(OI.models[[models[n]]][[vars[o]]]$merge[[1]])%in%var.names),1:2]
          out<-data.frame("beta"=vals[,1], "upper"=vals[,1]+1.96*vals[,2],"lower"=vals[,1]-1.96*vals[,2], "model"=rep(models[n],2), "var"=row.names(vals), "method"=rep(methods[m]))
          forest.data<-rbind(forest.data, out)
        }
      if(m==2){
        vals<-No1yr.models[[models[n]]][[vars[o]]]$merge[[1]][which(row.names(No1yr.models[[models[n]]][[vars[o]]]$merge[[1]])%in%var.names),1:2]
        out<-data.frame("beta"=vals[,1], "upper"=vals[,1]+1.96*vals[,2],"lower"=vals[,1]-1.96*vals[,2], "model"=rep(models[n],2), "var"=row.names(vals), "method"=rep(methods[m]))
        forest.data<-rbind(forest.data, out)
        }
      if(m==3){
          vals<-drop.models[[models[n]]][[vars[o]]]$merge[[1]][which(row.names(drop.models[[models[n]]][[vars[o]]]$merge[[1]])%in%var.names),1:2]
          out<-data.frame("beta"=vals[,1], "upper"=vals[,1]+1.96*vals[,2],"lower"=vals[,1]-1.96*vals[,2], "model"=rep(models[n],2), "var"=row.names(vals), "method"=rep(methods[m]))
          forest.data<-rbind(forest.data, out)
        }
      if(m==4){
          vals<-MI.models[[models[n]]][[vars[o]]]$merge[[1]][which(row.names(MI.models[[models[n]]][[vars[o]]]$merge[[1]])%in%var.names),1:2]
          out<-data.frame("beta"=vals[,1], "upper"=vals[,1]+1.96*vals[,2],"lower"=vals[,1]-1.96*vals[,2], "model"=rep(models[n],2), "var"=row.names(vals), "method"=rep(methods[m]))
          forest.data<-rbind(forest.data, out)
        }
      
    }
  }
}


forest.data<-forest.data[-1,]
forest.data$model<-factor(forest.data$model)
forest.data$var<-factor(forest.data$var)
forest.data$method<-factor(forest.data$method)
  
ForestFE<-ggplot(data=forest.data[forest.data$model=="FE.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, State FE Caseload models")

ForestFEent<-ggplot(data=forest.data[forest.data$model=="FE.ent.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, State FE Entry models")

ForestRob<-ggplot(data=forest.data[forest.data$model=="rob.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, State/Year FE Caseload models")

ForestRobEnt<-ggplot(data=forest.data[forest.data$model=="rob.ent.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, State/Year FE Entry models")

ForestLag<-ggplot(data=forest.data[forest.data$model=="lag.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, Lagged DV Caseload models")


ForestLagEnt<-ggplot(data=forest.data[forest.data$model=="lag.ent.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, Lagged DV Entry models")

ForestFD<-ggplot(data=forest.data[forest.data$model=="fd.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, First Difference Caseload models")

ForestFDEnt<-ggplot(data=forest.data[forest.data$model=="fd.ent.models",], aes(x=beta,y=var))+
  geom_point(size=2)+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Error method")+
  coord_cartesian(xlim=c(-1, 1))+
  facet_wrap(~method)+
  ggtitle("Estimates and 95% CI, First Difference Entry models")
