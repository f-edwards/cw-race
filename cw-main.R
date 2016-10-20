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

# 
# setwd("U:/cw-race/")
# source("U:/cw-race/cw-race-functions.r")
# fc<-read.csv("U:/cw-race/data/fc.csv")

setwd("D:/sync/cw-race/")
source("D:/sync/cw-race/cw-race-functions.r")
fc<-read.csv("D:/sync/cw-race/data/fc.csv")

# # # for laptop
# setwd("~/sync/cw-race/")
# source("~/sync/cw-race/cw-race-functions.r")
# fc<-read.csv("fc.csv")

source("cw-imputation.r", echo=TRUE)

##########
# RE MODELS
##########
#source("disp-models.r")


###################
## NegBin FE Models
###################

##DEVIANCE SE CORRECTION FROM ALLISON &W 2002 - sqrt(deviance/df)
nb.dev<-function(x){
  correction<-sqrt(deviance(x)/df.residual(x))
  beta<-coef(x)
  se<-summary(x)$coefficients[,2]*correction
  z<-beta/se
  p<-round(2*pnorm(-abs(z)),3)
  out<-list()
  out[["model"]]<-as.data.frame(cbind(beta, se, z, p))
  out[["theta"]]<-data.frame("theta"=x$theta, "theta SE"=x$SE.theta)
  return(out)
}

nb.merge<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) d$model[,1]))
  se<-do.call(rbind, lapply(model.imp, function(d)  d$model[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-list()
  out$model<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$model$z<-out$model[,1]/out$model[,2]
  out$model$p<-round(2*pnorm(-abs(out$model$z)),3)
  names(out$model)[1:2]<-c("Beta", "SE")
  row.names(out$model)<-row.names(model.imp[[1]]$model)
  theta<-do.call(rbind, lapply(model.imp, function(d) d$theta[1]))
  theta.SE<-do.call(rbind, lapply(model.imp, function(d) d$theta[2]))
  out$theta<-t(mi.meld(q=theta, se=theta.SE))
  row.names(out$theta)<-"Theta"
  return(out)
}

merge.mi<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) d[,1]))
  se<-do.call(rbind, lapply(model.imp, function(d) d[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$z<-out[,1]/out[,2]
  out$p<-round(2*pnorm(-abs(out$z)),3)
  names(out)[1:2]<-c("Beta", "SE")
  out<-list(out)
  return(out)
}


nb.merge.rob<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) d$model[,1]))
  se<-do.call(rbind, lapply(model.imp, function(d)  d$model[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-list()
  out$model<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$model$z<-out$model[,1]/out$model[,2]
  out$model$p<-round(2*pnorm(-abs(out$model$z)),3)
  names(out$model)[1:2]<-c("Beta", "SE")
  row.names(out$model)<-row.names(model.imp[[1]]$model)
  theta<-do.call(rbind, lapply(model.imp, function(d) d$theta[1]))
  theta.SE<-do.call(rbind, lapply(model.imp, function(d) d$theta[2]))
  out$theta<-t(mi.meld(q=theta, se=theta.SE))
  row.names(out$theta)<-"Theta"
  return(out)
}

mi.robust.nb<-function(model, data){
  results.biased<-list()
  theta<-as.data.frame(matrix(nrow=m, ncol=2))
  for(i in 1:m){
    mod<-glm.nb(model, data=data[[i]])
    results.biased[[i]]<-coeftest(mod, cluster.vcov(mod, cbind(data[[i]]$stname, data[[i]]$year.c)))
    theta[i,1]<-mod$theta; theta[i,2]<-mod$SE.theta
  }
  beta<-do.call(rbind, lapply(results.biased, function(d) d[,1]))
  se<-do.call(rbind, lapply(results.biased, function(d)  d[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-list()
  out$model<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$model$z<-out$model[,1]/out$model[,2]
  out$model$p<-round(2*pnorm(-abs(out$model$z)),3)
  names(out$model)[1:2]<-c("Beta", "SE")
  row.names(out$model)<-row.names(results.biased[[1]])
  out$theta<-t(mi.meld(q=as.data.frame(theta[,1]), se=theta[,2]))
  row.names(out$theta)<-"Theta"
  out$raw<-results.biased[[1]]
  return(out)
}

models<-list()

models[["cl.all"]]<-mi.robust.nb(model=cl~log(incarrt)+log(incarrt.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
  log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
models[["cl.all.m"]]<-mi.robust.nb(model=cl~log(incarrt.m)+log(incarrt.m.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
  log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
models[["cl.all.f"]]<-mi.robust.nb(model=cl~log(incarrt.f)+log(incarrt.f.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                     log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)


models[["cl.blk"]]<-cl.blk ~ log(b.incarrt)+ log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
  log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child))
models[["cl.wht"]]<-cl.white ~ log(w.incarrt)+ log(w.incarrt.lag)+log(w.unemp.rt)+log(w.singpar.rt)+log(wht.lessHS)+log(chpov.wht.pc)+
  inst6014_nom+log(v.crime.rt)+log(pctwht)+factor(stname)+year.c+offset(log(wht.child))
models[["cl.na"]]<-cl.nat.am ~ sqrt(a.incarrt)+ sqrt(a.incarrt.lag)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
  log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child))




###############FOR TOTAL POP MODELS, NEED UNEMP, SINGPAR FOR ALL - rerun make-census and cw-read on TS3
cl.count<-lapply(fc.imp$imputations, function(d) glm.nb(cl.white ~
                         log(w.incarrt)+ log(w.incarrt.lag)+
                         log(w.unemp.rt)+log(w.singpar.rt)+log(wht.lessHS)+
                         log(chpov.wht.pc)+
                         inst6014_nom+log(v.crime.rt)+
                           log(pctwht)+
                   factor(stname)+year.c+
                   offset(log(wht.child)),
                  data=d))



#specify formulas
#apply for loop over imputation for results


nb.cl.w<-list("models"=cl.count, "merge"=nb.merge(cl.count))

cl.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(cl.blk ~ 
                         log(b.incarrt)+ log(b.incarrt.lag)+
                         log(w.incarrt)+
                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                         log(chpov.blk.pc)+log(pctblk)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(blk.child)),
                  data=d)))
nb.cl.b<-list("models"=cl.count, "merge"=nb.merge(cl.count))

cl.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(cl.nat.am ~ 
                   sqrt(a.incarrt)+ sqrt(a.incarrt.lag)+
                   log(w.incarrt)+
                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                  log(chpov.amind.pc)+log(pctami)+
                  inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(amind.child)),
                  data=d)))
nb.cl.n<-list("models"=cl.count, "merge"=nb.merge(cl.count))

ent.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(ent ~
                         log(incarrt)+ log(incarrt.lag)+
                         log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                         log(chpovrt)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(child)),
                  data=d)))
nb.ent<-list("models"=ent.count, "merge"=nb.merge(ent.count))

ent.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(ent.blk ~ 
                         log(b.incarrt)+ log(b.incarrt.lag)+
                         log(w.incarrt)+
                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                         log(chpov.blk.pc)+log(pctblk)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(blk.child)),
                  data=d)))

nb.ent.b<-list("models"=ent.count, "merge"=nb.merge(ent.count))

ent.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(cl.nat.am ~ 
                   sqrt(a.incarrt)+ sqrt(a.incarrt.lag)+
                   log(w.incarrt)+
                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                  log(chpov.amind.pc)+log(pctami)+
                  inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(amind.child)),
                  data=d)))

nb.ent.n<-list("models"=ent.count, "merge"=nb.merge(ent.count))

fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-as.data.frame(fc.imp$imputations[[i]]%>%filter(cl.blk>1))
  fcn.reun.imp[[i]]<-as.data.frame(fc.imp$imputations[[i]]%>%filter(cl.nat.am>1))
}

ex.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(reun ~ 
                         log(incarrt)+log(incarrt.lag)+
                         log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                         log(chpovrt)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(child)),
                  data=(d))))
nb.ex<-list("models"=ex.count, "merge"=nb.merge(ex.count))

ex.count<-lapply(fcb.reun.imp, function(d) nb.dev(glm.nb(reun.blk ~ 
                         log(b.incarrt)+log(b.incarrt.lag)+
                         log(w.incarrt)+
                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                         log(chpov.blk.pc)+log(pctblk)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(cl.blk)),
                  data=(d))))

nb.ex.b<-list("models"=ex.count, "merge"=nb.merge(ex.count))

ex.count<-lapply(fcn.reun.imp, function(d) nb.dev(glm.nb(reun.nat.am ~ 
                   sqrt(a.incarrt)+ sqrt(a.incarrt.lag)+
                   log(w.incarrt)+
                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                  log(chpov.amind.pc)+log(pctami)+
                  inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(cl.nat.am)),
                  data=d)))

nb.ex.n<-list("models"=ex.count, "merge"=nb.merge(ex.count))

inst.count<-lapply(fc.imp$imputations, function(d) nb.dev(glm.nb(grp.inst ~ 
                         log(incarrt)+log(incarrt.lag)+
                         log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                         log(chpovrt)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(child)),
                  data=(d))))
nb.inst<-list("models"=inst.count, "merge"=nb.merge(inst.count))

inst.count<-lapply(fcb.reun.imp, function(d) nb.dev(glm.nb(grp.inst.blk ~ 
                         log(b.incarrt)+log(b.incarrt.lag)+
                         log(w.incarrt)+
                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                         log(chpov.blk.pc)+log(pctblk)+
                         inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(cl.blk)),
                  data=(d))))

nb.inst.b<-list("models"=inst.count, "merge"=nb.merge(inst.count))

inst.count<-lapply(fcn.reun.imp, function(d) nb.dev(glm.nb(grp.inst.nat.am ~ 
                   sqrt(a.incarrt)+ sqrt(a.incarrt.lag)+
                   log(w.incarrt)+
                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                  log(chpov.amind.pc)+log(pctami)+
                  inst6014_nom+log(v.crime.rt)+
                   factor(stname)+year.c+
                   offset(log(cl.nat.am)),
                  data=d)))

nb.inst.n<-list("models"=inst.count, "merge"=nb.merge(inst.count))

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos.nat.am)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
                                         log(w.incarrt)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
los.n<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

los<-lapply(fc.imp$imputations, function(d) plm(log(lifelos.blk)~log(b.incarrt)+log(b.incarrt.lag)+
                                         log(w.incarrt)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+
                                         log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
los.b<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


##### BAYESIAN ROBUSTNESS CHECKS 
b.d.bayes<-stan_glmer.nb(cl.blk~log(b.incarrt)+log(b.incarrt.lag)+
       log(chpov.blk.pc)+
       log(b.unemp.rt)+log(b.singpar.rt)+
       log(blk.lessHS)+
       log(pctblk)+
       scale(inst6014_nom)+log(v.crime.rt)+
       year.c+offset(log(blk.child))+
       (1|stname),
     data=fc.imp$imputations[[1]])
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