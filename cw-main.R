#########
rm(list=ls())
set.seed(1)
library(lme4)
library(Amelia)
library(ggplot2)
library(texreg)
library(dplyr)
library(arm)
library(MASS)
# library(rstanarm)
# 
# options(mc.cores = parallel::detectCores())
# 
# setwd("U:/cw-race/")
# source("U:/cw-race/cw-race-functions.r")
# fc<-read.csv("U:/cw-race/data/fc.csv")

# setwd("D:/sync/cw-race/")
# source("D:/sync/cw-race/cw-race-functions.r")
# fc<-read.csv("D:/sync/cw-race/data/fc.csv")


# # for laptop
setwd("~/sync/cw-race/")
source("~/sync/cw-race/cw-race-functions.r")
fc<-read.csv("fc.csv")
##for output
# setwd("~/Dropbox/cw-race-paper/")

fc$inst6014_nom<-fc$inst6014_nom/100

no.imp.plot<-ggplot(fc)+geom_line(aes(x=year, y=log(amind.child.pov/amind.child)))+facet_wrap(~stname)+ggtitle("Without transformation")

bounds<-cbind(1:ncol(fc),
              rep(0.0000001, ncol(fc)),
              rep(Inf, ncol(fc)))
ratios<-which(names(fc)%in%c("blk.lessHS", "wht.lessHS", "amind.lessHS"))
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

m<-15

###THINK ABT IMPUTATION OF CHILD POP - DENOM IS REALLY IMPORTANT, MAYBE PRESERVE?
blk.acs<-which(colnames(fc)%in%c( "blk.child.pov", "blk.lessHS", "blk.unemp", "blk.emp", "blk.singpar"))
amind.acs<-which(colnames(fc)%in%c("amind.child.pov", "amind.lessHS", "amind.unemp", "amind.emp" , "amind.singpar"))

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

###MANUALLY add HI 2007 (AMIND Child Pov observed at 0)
###MANUALLY ADD PROBLEMATIC CHILD POP AND CHILD POV OBS FOR OI
### HI 2007, NH amind child 2001


amind.oi<-rbind(amind.oi, c(361, 23))
amind.oi<-rbind(amind.oi, c(680, 22))

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


####THIS IS TO IMPLEMENT A SAMPLE SIZE WEIGHTED VARIANCE BY POP
####WOULD REQUIRE COUNTING SAMPLED INDIVIDUALS FROM EACH GROUP FROM ACS
####IF I GET COMMENTS, CAN GO BACK AND IMPLEMENT TO MAKE
####Var=pop*(p*(1-p)/samplesize)
# amind.child<-which(names(fc)%in%c("amind.child", "amind.child.pov", "amind.singpar"))
# amind.adult<-which(names(fc)%in%c("amind.lessHS", "amind.unemp", "amind.emp"))
# blk.child<-which(names(fc)%in%c("blk.child", "blk.child.pov", "blk.singpar"))
# blk.adult<-which(names(fc)%in%c("blk.lesSHS", "blk.unemp", "blk.emp"))

create.prior<-function(x){
  ##function receives row,column pair to set prior based on strong observation years, compute mean and sd based on observed strong periods
  st<-fc[oi[x,1], "stname"]
  var<-oi[x,2]
  year<-fc[oi[x,1], "year"]
  ###set pop based on underlying pop of interest
  strong.2000<-fc[which((fc$year==2000)&(fc$stname==st)), var]
  strong.2009<-fc[which((fc$year==2009)&(fc$stname==st)), var]
  strong.set<-fc[which((fc$year%in%strong)&(fc$stname==st)), var]
  # if(var%in%amind.child){
  #   pop<-fc[which((fc$year==2009)&(fc$stname==st)), "amind.child"]
  # }
  # if(var%in%amind.adult){
  #   pop<-fc[which((fc$year==2009)&(fc$stname==st)), "a.adult"]
  # }
  # if(var%in%blk.child){
  #   pop<-fc[which((fc$year==2009)&(fc$stname==st)), "blk.child"]
  # }
  # if(var%in%blk.adult){
  #   pop<-fc[which((fc$year==2009)&(fc$stname==st)), "b.adult"]
  # }
  ###WEIGHT ON PROXIMITY TO OBS, with obs - Weight is 0.1 on obs, 0.75 on 2000 at 2001, linear to 0.15 at 2009
  pr.E<-((0.9-(year-2001)*0.12))*strong.2000+(1-(0.9-(year-2001)*0.12))*strong.2009
  all.set<-fc[fc$stname==st,var]
  pr.SD<-1/2*(max(all.set)-min(all.set))##as invese pop penalized - heteroskedastic error
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
               priors=oi.priors, p2s=1,
               idvars=c("state", "statename", "St", "adult", "w.adult", "b.adult", "a.adult", "l.adult",
                        "pctblk1930", "pctimm1930", "pctami1930", "boarding.n", "board"))

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
  incarrt=incartot/adult,
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
  ami.disp=ifelse(cl.amind.pc/cl.wht.pc>0,cl.amind.pc/cl.wht.pc, 0),
  year.c=year-2000)
}

post.imp.plot<-ggplot(fc.imp$imputations[[1]])+geom_line(aes(x=year, y=log(a.unemp.rt)))+facet_wrap(~stname)+ggtitle("After overimputation")


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

##########
# RE MODELS
##########

b.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(log(bw.disp)~log(b.incardisp)+
  log(bdisp.chpov)+
  log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
  log(I(blk.lessHS/wht.lessHS))+
  log(pctblk)+
  scale(inst6014_nom)+log(v.crime.rt)+
  year.c+
  (1|stname),
  data=d))

a.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(I(ami.disp^(1/2))~I(a.incardisp^(1/2))+
  log(adisp.chpov)+
  log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
  log(amind.lessHS/wht.lessHS)+
  log(pctami)+
  scale(inst6014_nom)+log(v.crime.rt)+
  year.c+
  (1|stname),
  data=d))

b.d.tab<-makeMIRegTab(b.disp)
a.d.tab<-makeMIRegTab(a.disp)

ggplot(data=fc.imp$imputations[[1]], aes(x=year.c, y=log(bw.disp)))+geom_line()+facet_wrap(~stname)


test<-fc.imp$imputations[[1]]
fe.count<-glm.nb(as.integer(cl.blk)~ log(b.incarrt)+
                         log(w.incarrt)+
                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                         log(chpov.blk.pc)+log(pctblk)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+factor(year.c),
                 offset(log(test$blk.child)), data=test)

# 
# b.d.bayes<-lapply(fc.imp$imputations, function(d) stan_glmer(log(bw.disp)~log(b.incardisp)+
#        log(bdisp.chpov)+
#        log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
#        log(I(blk.lessHS/wht.lessHS))+
#        log(pctblk)+
#        scale(inst6014_nom)+log(v.crime.rt)+
#        year.c+
#        (1|stname),
#      data=d))
# 
# a.d.bayes<-lapply(fc.imp$imputations, function(d) 
#   stan_glmer(I(ami.disp^(1/2))~I(a.incardisp^(1/2))+
#          log(adisp.chpov)+
#          log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
#          log(amind.lessHS/wht.lessHS)+
#          log(pctami)+
#          scale(inst6014_nom)+log(v.crime.rt)+
#          year.c+
#          (1|stname),
#        data=d))

########## PREPARE TABLE OUTPUT, PREPARE SIMULATION / PREDICTION

################
#Overimputation
################
fe.data<-fc.imp$imputations
source("FE-models.r", print.eval=TRUE)
# 
# # within.models<-list("FE.models"=FE.models, 
# #                     #"lag.models"=lag.models,
# #                     #"fd.models"=fd.models, 
# #                     "rob.models"=rob.models)
# #                     
# #                 #"FE.ent.models"=FE.ent.models, 
# #                 #"lag.ent.models"=lag.ent.models,
# #                 #"fd.ent.models"=fd.ent.models, 
# #                 #"rob.ent.models"=rob.ent.models)
# 
# 
# ####################
# #Table output
# ####################
# texreg(list(b.disp[[1]],a.disp[[1]]),
#        override.coef=list(b.d.tab[1:nrow(b.d.tab)-1,1], a.d.tab[1:nrow(b.d.tab)-1,1]),
#        override.se=list(b.d.tab[1:nrow(b.d.tab)-1,2], a.d.tab[1:nrow(b.d.tab)-1,2]),
#        override.pvalues = list(b.d.tab[1:nrow(b.d.tab)-1,4], a.d.tab[1:nrow(b.d.tab)-1,4]),
#        custom.coef.names=c("Intercept",
#                            "Afr. Am. Incarceration disp.",
#                            "Afr. Am. Child poverty disp.",
#                            "Afr. Am. Unemployment disp.",
#                            "Afr. Am. Single parent disp.",
#                            "Afr. Am. Adults w/o HS disp.",
#                            "Percent Afr. Am. population",
#                            "Leg. ideology",
#                            "Violent crime rate",
#                            "Year",
#                            "Nat. Am. Incarceration disp.",
#                            "Nat. Am. Child poverty disp.",
#                            "Nat. Am. Unemployment disp.",
#                            "Nat. Am. Single parent disp.",
#                            "Nat. Am. Adults w/o HS disp.",
#                            "Percent Nat. Am. population"),
#        custom.model.names=c("Afr. Am. Disproportion", "Nat. Am. Disproportion"),
#        caption="Foster care caseload disproportion, multilevel linear
# regression with state intercepts. Results combined across imputations (m=15).",
#        caption.above=TRUE,
#        label="disp-models",
#        include.aic=FALSE,
#        include.bic=FALSE,
#        include.loglik=FALSE,
#        file="re-models.tex"
# )
# #
# texreg(list(FE.models$b$models[[1]], FE.models$a$models[[1]]),
#        override.coef=list(FE.models$b$merge[[1]][,1],
#                           FE.models$a$merge[[1]][,1]),
#        override.se=list(FE.models$b$merge[[1]][,2],
#                         FE.models$a$merge[[1]][,2]
#                        ),
#        override.pvalues = list(FE.models$b$merge[[1]][,4],
#                                FE.models$a$merge[[1]][,4]
#                                ),
#        custom.coef.names=c(
#          "Afr. Am. incarceration rate",
#          "Afr. Am. Unemployment rate",
#          "Afr. Am. Single parent rate",
#          "Afr. Am. Adults w/o HS rate",
#          "Afr. Am. child poverty rate",
#          "Percent Afr. Am. population",
#          "Leg. ideology",
#          "Violent crime rate",
#          "Nat. Am. Incarceration rate",
#          "Nat. Am. Unemployment rate",
#          "Nat. Am. Single parent rate",
#          "Nat. Am. Adults w/o HS rate",
#          "Nat. Am. child poverty rate",
#          "Percent Nat. Am. population"),
#        custom.model.names=c("Afr. Am.", "Nat. Am."),
#        caption="Foster care caseloads, OLS with state fixed effects. Results combined across imputations (m=15).",
#        caption.above=TRUE,
#        label="disp-models",
#        include.aic=FALSE,
#        include.bic=FALSE,
#        include.loglik=FALSE,
#        file="fe-models.tex")
# 
# texreg(list(FE.models$b.m$models[[1]], FE.models$a.m$models[[1]]),
#        override.coef=list(FE.models$b.m$merge[[1]][,1],
#                           FE.models$a.m$merge[[1]][,1]),
#        override.se=list(FE.models$b.m$merge[[1]][,2],
#                         FE.models$a.m$merge[[1]][,2]
#        ),
#        override.pvalues = list(FE.models$b.m$merge[[1]][,4],
#                                FE.models$a.m$merge[[1]][,4]
#        ),
#        custom.coef.names=c(
#          "Afr. Am. Male Incarceration rate",
#          "Afr. Am. Unemployment rate",
#          "Afr. Am. Single parent rate",
#          "Afr. Am. Adults w/o HS rate",
#          "Afr. Am. child poverty rate",
#          "Percent Afr. Am. population",
#          "Leg. ideology",
#          "Violent crime rate",
#          "Nat. Am. Male Incarceration rate",
#          "Nat. Am. Unemployment rate",
#          "Nat. Am. Single parent rate",
#          "Nat. Am. Adults w/o HS rate",
#          "Nat. Am. child poverty rate",
#          "Percent Nat. Am. population"),
#        custom.model.names=c("Afr. Am. State FE", "Nat. Am. State FE"),
#         caption="Foster care caseloads, OLS with state fixed effects. Results combined across imputations (m=15).",
#        caption.above=TRUE,
#        label="disp-models",
#        include.aic=FALSE,
#        include.bic=FALSE,
#        include.loglik=FALSE,
#        file="fe-models-male.tex")
# 
# texreg(list(FE.models$b.f$models[[1]], FE.models$a.f$models[[1]]),
#        override.coef=list(FE.models$b.f$merge[[1]][,1],
#                           FE.models$a.f$merge[[1]][,1]),
#        override.se=list(FE.models$b.f$merge[[1]][,2],
#                         FE.models$a.f$merge[[1]][,2]
#        ),
#        override.pvalues = list(FE.models$b.f$merge[[1]][,4],
#                                FE.models$a.f$merge[[1]][,4]
#        ),
#        custom.coef.names=c(
#          "Afr. Am. Female Incarceration rate",
#          "Afr. Am. Unemployment rate",
#          "Afr. Am. Single parent rate",
#          "Afr. Am. Adults w/o HS rate",
#          "Afr. Am. child poverty rate",
#          "Percent Afr. Am. population",
#          "Leg. ideology",
#          "Violent crime rate",
#          "Nat. Am. Female Incarceration rate",
#          "Nat. Am. Unemployment rate",
#          "Nat. Am. Single parent rate",
#          "Nat. Am. Adults w/o HS rate",
#          "Nat. Am. child poverty rate",
#          "Percent Nat. Am. population"),
#        custom.model.names=c("Afr. Am. State FE", "Nat. Am. State FE"),
#         caption="Foster care caseloads, OLS with state fixed effects. Results combined across imputations (m=15).",
#        caption.above=TRUE,
#        label="disp-models",
#        include.aic=FALSE,
#        include.bic=FALSE,
#        include.loglik=FALSE,
#        file="fe-models-male.tex")