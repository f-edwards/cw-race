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
library(rstanarm, options(mc.cores = parallel::detectCores()))
library(multiwayvcov)
library(lmtest)
library(sandwich)
library(plm)

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
fc<-read.csv("~/sync/cw-race/data/fc.csv")

source("cw-imputation.r", echo=TRUE)

### MAKE INCAR AND FC FIGURE


### FILTER OUT THOSE W/NO BLACK, NA CASELOADS
fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.blk>1)
  fcn.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.nat.am>1)
}

######## MAIN STATE FE MODEL RESULTS
#source("state-models.r")

######################################
## RE MODELS
source("disp-models.r")

#######################
## Sim visuals
setwd("D:/sync/cw-race/figures")
#source("D:/sync/cw-race/sim.R")


####################
######## EXPECTED VALUES AND CIs
####################
## Retrieve reasonable within-state changes in incar
## calculate mean within-state sd in incar for tot, B, NA, m, f

# #####################
# ## SET UP SCENARIOS
# #####################
# 
# len<-1000
# 
# all.scen.m<-all.scen.f<-all.scen<-matrix(rep(with(fc.imp$imputations[[1]], c(1, 0, median(log(unemp.rt)), median(log(singpar.rt)), median(log(lessHS)),
#                                           median(log(chpovrt)), median(inst6014_nom),
#                                           median(log(v.crime.rt)), rep(0, 45), 1, rep(0, 3), 0)), each=len), nrow=len)
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$incarrt)
# all.scen[,2]<-log(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(incarrt.m)))[,2])
# median<-median(fc.imp$imputations[[1]]$incarrt.m)
# all.scen.m[,2]<-log(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(incarrt.f)))[,2])
# median<-median(fc.imp$imputations[[1]]$incarrt.f)
# all.scen.f[,2]<-log(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# b.scen<-b.scen.m<-b.scen.f<-matrix(rep(with(fc.imp$imputations[[1]], c(1, 0, median(log(b.unemp.rt)), 
#                                                  median(log(b.singpar.rt)), median(log(blk.lessHS)),
#                                                  median(log(chpov.blk.pc)), median(log(pctblk)), median(inst6014_nom),
#                                                  median(log(v.crime.rt)), rep(0, 45), 1, rep(0, 3), 0)), each=len), nrow=len)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(b.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$b.incarrt)
# b.scen[,2]<-log(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(b.m.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$b.m.incarrt)
# b.scen.m[,2]<-log(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(b.f.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$b.f.incarrt)
# b.scen.f[,2]<-sqrt(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# na.scen<-na.scen.m<-na.scen.f<-matrix(rep(with(fc.imp$imputations[[1]], c(1, 0, median(log(a.unemp.rt)), 
#                                      median(log(a.singpar.rt)), median(log(amind.lessHS)),
#                                      median(log(chpov.amind.pc)), median(log(pctami)), median(inst6014_nom),
#                                      median(log(v.crime.rt)), rep(0, 45), 1, rep(0, 3), 0)), each=len), nrow=len)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(a.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$a.incarrt)
# na.scen[,2]<-sqrt(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(a.m.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$a.m.incarrt)
# na.scen.m[,2]<-sqrt(seq(from=-2, to=2, length.out=len) * sd + median)
# 
# sd<-median(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd=sd(a.f.incarrt)))[,2])
# median<-median(fc.imp$imputations[[1]]$a.f.incarrt)
# na.scen.f[,2]<-sqrt(seq(from=-2, to=2, length.out=len) * sd + median)


##### EXPECTED VALS
# 
# model<-cl.models$cl.all$model
# cl.all<-data.frame(yhat=(exp(all.scen%*%model[,1])), lower=exp(all.scen%*%(1.96*model[,2]+model[,1])), upper=exp(all.scen%*%(model[,1]-1.96*model[,2])),
#                     sd=seq(from=-2, to=2, length.out=len), Group="All")
# model<-cl.models$cl.blk$model
# cl.blk<-data.frame(yhat=(exp(b.scen%*%model[,1])), lower=exp(b.scen%*%(1.96*model[,2]+model[,1])), upper=exp(b.scen%*%(model[,1]-1.96*model[,2])),
#                    sd=seq(from=-2, to=2, length.out=len), Group="Afr. Am.")
# model<-cl.models$cl.na$model
# cl.na<-data.frame(yhat=(exp(na.scen%*%model[,1])), lower=exp(na.scen%*%(1.96*model[,2]+model[,1])), upper=exp(na.scen%*%(model[,1]-1.96*model[,2])),
#                    sd=seq(from=-2, to=2, length.out=len), Group="Nat. Am.")
# 
# yhat.cl<-rbind(cl.all)
# 
# ggplot(yhat.cl, aes(y=yhat, x=sd))+geom_line(aes(y=lower), lty=2)+geom_line(aes(y=upper), lty=2)+geom_line()+facet_wrap(~Group)
################
##COMPUTE EXPECTATIONS
################
###### Computes expected values for changes in outcome for variation of [-2,2] sd of within state incarceration variation
###### Supply with model object [2,] is incar var in model, and within state sd for variable
# yhat.ci<-function(model, scen){
#   beta<-model[2,1]
#   b.up<-beta+(1.96*se)
#   b.low<-beta-(1.96*se)
#   yhat.median<-exp(beta*log(median))
#   se<-model[2,2]
#   yhat<-data.frame("delta.yhat"=yhat.median-exp(beta*incar.seq), "lower"=yhat.median-exp(b.low*log(incar.seq)), 
#                    "upper" = yhat.median-exp(b.up*log(incar.seq)), "incar"=incar.seq, "sd"=seq(from=-2, to=2, length.out=len))
# }
# 
# 
# pred<-
# tdat<-fc.imp$imputations[[1]]
# tdat$obs<-1:nrow(tdat)
# test<-glmer(cl.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#   log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+year.c+offset(log(amind.child))+(1|stname)+(1|obs), data=tdat, family="poisson")
  
  
##########
# RE MODELS
##########
#source("disp-models.r")
# los.re<-lmer(log(lifelos)~log(incarrt)+
#                log(unemp.rt)+log(singpar.rt)+log(lessHS)+
#                log(chpovrt)+
#                inst6014_nom+log(v.crime.rt)+
#                (1|stname), data=fc.imp$imputations[[1]])
# 
# los.b.re<-lmer(log(lifelos.blk)~log(b.incarrt)+
#                  log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                  log(chpov.blk.pc)+
#                  log(pctblk)+
#                  inst6014_nom+log(v.crime.rt)+
#                  (1|stname), data=fc.imp$imputations[[1]])
# 
# los.n.re<-lmer(sqrt(lifelos.nat.am)~sqrt(a.incarrt)+
#                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                  sqrt(chpov.amind.pc)+log(pctami)+
#                  inst6014_nom+log(v.crime.rt)+
#                  (1|stname), data=fc.imp$imputations[[1]])

###########################ROBUSTNESS CHECKS

# # # ##### BAYESIAN ROBUSTNESS CHECKS 
# b.cl.bayes<-stan_glmer.nb(cl.blk~log(b.incarrt)+log(b.incarrt.lag)+
#        log(chpov.blk.pc)+
#        log(b.unemp.rt)+log(b.singpar.rt)+
#        log(blk.lessHS)+
#        log(pctblk)+
#        scale(inst6014_nom)+log(v.crime.rt)+
#        year.c+offset(log(blk.child))+
#        (1|stname),
#      data=fc.imp$imputations[[1]])
# 
# a.cl.bayes<-lapply(fc.imp$imputations, function(d)
#   stan_glmer(I(ami.disp^(1/2))~I(a.incardisp^(1/2))+
#          log(adisp.chpov)+
#          log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
#          log(amind.lessHS/wht.lessHS)+
#          log(pctami)+
#          scale(inst6014_nom)+log(v.crime.rt)+
#          year.c+
#          (1|stname),
#        data=d))
# 
# ########## PREPARE TABLE OUTPUT, PREPARE SIMULATION / PREDICTION

################
#Overimputation
################
# fe.data<-fc.imp$imputations
# source("FE-models.r", print.eval=TRUE)
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