

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
    results.biased[[i]]<-coeftest(mod, cluster.vcov(mod, cbind(data[[i]]$stname)))
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
  out$raw<-mod
  return(out)
}

mi.robust.nb.rob<-function(model, data){
  results.biased<-list()
  theta<-as.data.frame(matrix(nrow=m, ncol=2))
  for(i in 1:m){
    mod<-glm.nb(model, data=data[[i]])
    results.biased[[i]]<-coeftest(mod, cluster.vcov(mod, cbind(data[[i]]$stname, cbind(data[[i]]$year.c))))
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
  out$raw<-mod
  return(out)
}

cl.models<-list()

########################################################################################
# State Fixed Effects

########################
##### CASELOAD MODELS
########################

cl.models[["cl.all"]]<-mi.robust.nb(model=cl~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                      log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
cl.models[["cl.all.m"]]<-mi.robust.nb(model=cl~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
cl.models[["cl.all.f"]]<-mi.robust.nb(model=cl~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)

cl.models[["cl.blk"]]<-mi.robust.nb(model=cl.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                      log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["cl.blk.m"]]<-mi.robust.nb(model=cl.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["cl.blk.f"]]<-mi.robust.nb(model=cl.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)

cl.models[["cl.na"]]<-mi.robust.nb(model=cl.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                     log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["cl.na.m"]]<-mi.robust.nb(model=cl.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["cl.na.f"]]<-mi.robust.nb(model=cl.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)

########################
##### ENTRY MODELS
########################
ent.models<-list()

ent.models[["ent.all"]]<-mi.robust.nb(model=ent~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
ent.models[["ent.all.m"]]<-mi.robust.nb(model=ent~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)
ent.models[["ent.all.f"]]<-mi.robust.nb(model=ent~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+year.c+offset(log(child)), data=fc.imp$imputations)

ent.models[["ent.blk"]]<-mi.robust.nb(model=ent.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["ent.blk.m"]]<-mi.robust.nb(model=ent.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["ent.blk.f"]]<-mi.robust.nb(model=ent.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(blk.child)), data=fc.imp$imputations)

ent.models[["ent.na"]]<-mi.robust.nb(model=ent.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["ent.na.m"]]<-mi.robust.nb(model=ent.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["ent.na.f"]]<-mi.robust.nb(model=ent.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(amind.child)), data=fc.imp$imputations)


###############################
## REUNIFICATION EXITS
###############################

### FILTER OUT THOSE W/NO BLACK, NA CASELOADS
fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.blk>1)
  fcn.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.nat.am>1)
}

reun.models<-list()

reun.models[["reun.all"]]<-mi.robust.nb(model=reun~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)
reun.models[["reun.all.m"]]<-mi.robust.nb(model=reun~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                            log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)
reun.models[["reun.all.f"]]<-mi.robust.nb(model=reun~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                            log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)

reun.models[["reun.blk"]]<-mi.robust.nb(model=reun.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["reun.blk.m"]]<-mi.robust.nb(model=reun.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                            log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["reun.blk.f"]]<-mi.robust.nb(model=reun.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                            log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)

reun.models[["reun.na"]]<-mi.robust.nb(model=reun.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["reun.na.m"]]<-mi.robust.nb(model=reun.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["reun.na.f"]]<-mi.robust.nb(model=reun.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)

#############################
## Institutionalization models
#############################

grp.inst.models<-list()

grp.inst.models[["grp.inst.all"]]<-mi.robust.nb(model=grp.inst~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                  log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["grp.inst.all.m"]]<-mi.robust.nb(model=grp.inst~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                    log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["grp.inst.all.f"]]<-mi.robust.nb(model=grp.inst~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                    log(v.crime.rt)+factor(stname)+year.c+offset(log(cl)), data=fc.imp$imputations)

grp.inst.models[["grp.inst.blk"]]<-mi.robust.nb(model=grp.inst.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                  log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["grp.inst.blk.m"]]<-mi.robust.nb(model=grp.inst.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["grp.inst.blk.f"]]<-mi.robust.nb(model=grp.inst.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.blk)), data=fcb.reun.imp)

grp.inst.models[["grp.inst.na"]]<-mi.robust.nb(model=grp.inst.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                 log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["grp.inst.na.m"]]<-mi.robust.nb(model=grp.inst.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                   log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["grp.inst.na.f"]]<-mi.robust.nb(model=grp.inst.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                   log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+year.c+offset(log(cl.nat.am)), data=fcn.reun.imp)

#####################
## Length of stay
#####################
los.models<-list()

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos)~log(incarrt)+
                                                  log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                                                  log(chpovrt)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="individual",
                                                model="within",
                                                data=d))
los.models[["los.all"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname")))))

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos.nat.am)~sqrt(a.incarrt)+
                                                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                  sqrt(chpov.amind.pc)+log(pctami)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="individual",
                                                model="within",
                                                data=d))
los.models[["los.na"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname")))))

los<-lapply(fc.imp$imputations, function(d) plm(log(lifelos.blk)~log(b.incarrt)+
                                                  log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                  log(chpov.blk.pc)+
                                                  log(pctblk)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="individual",
                                                model="within",
                                                data=d))
los.models[["los.b"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname")))))

setwd("D:/sync/cw-race/tables/")
maketables.plm<-function(x){
  library(xtable)
  filename<-names(x)
  for(i in 1:length(x)){
    tab<-xtable(as.data.frame(x[[i]]$merge))
  print(tab, file=paste(filename[i], ".tex", sep=""), type="latex")
  }
}

maketables.glm<-function(x){
  library(xtable)
  filename<-names(x)
  for(i in 1:length(x)){
    tab<-xtable(as.data.frame(x[[i]]$model))
    print(tab, file=paste(filename[i], ".tex", sep=""), type="latex")
  }
}

maketables.glm(cl.models)
maketables.glm(ent.models)
maketables.glm(reun.models)
maketables.glm(grp.inst.models)
maketables.plm(los.models)


##################################################################################################
### State-year FEs
#################################################

cl.models[["SY.cl.all"]]<-mi.robust.nb.rob(model=cl~log(incarrt)+ log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                      log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
cl.models[["SY.cl.all.m"]]<-mi.robust.nb.rob(model=cl~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
cl.models[["SY.cl.all.f"]]<-mi.robust.nb.rob(model=cl~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)

cl.models[["SY.cl.blk"]]<-mi.robust.nb.rob(model=cl.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                      log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["SY.cl.blk.m"]]<-mi.robust.nb.rob(model=cl.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["SY.cl.blk.f"]]<-mi.robust.nb.rob(model=cl.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)

cl.models[["SY.cl.na"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                     log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["SY.cl.na.m"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["SY.cl.na.f"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)

########################
##### ENTRY MODELS
########################
ent.models<-list()

ent.models[["SY.ent.all"]]<-mi.robust.nb.rob(model=ent~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                        log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
ent.models[["SY.ent.all.m"]]<-mi.robust.nb.rob(model=ent~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
ent.models[["SY.ent.all.f"]]<-mi.robust.nb.rob(model=ent~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)

ent.models[["SY.ent.blk"]]<-mi.robust.nb.rob(model=ent.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                        log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["SY.ent.blk.m"]]<-mi.robust.nb.rob(model=ent.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["SY.ent.blk.f"]]<-mi.robust.nb.rob(model=ent.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)

ent.models[["SY.ent.na"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                       log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["SY.ent.na.m"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["SY.ent.na.f"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)


###############################
## REUNIFICATION EXITS
###############################

### FILTER OUT THOSE W/NO BLACK, NA CASELOADS
fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.blk>1)
  fcn.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.nat.am>1)
}

reun.models<-list()

reun.models[["SY.reun.all"]]<-mi.robust.nb.rob(model=reun~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                          log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
reun.models[["SY.reun.all.m"]]<-mi.robust.nb.rob(model=reun~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                            log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
reun.models[["SY.reun.all.f"]]<-mi.robust.nb.rob(model=reun~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                            log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)

reun.models[["SY.reun.blk"]]<-mi.robust.nb.rob(model=reun.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                          log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["SY.reun.blk.m"]]<-mi.robust.nb.rob(model=reun.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                            log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["SY.reun.blk.f"]]<-mi.robust.nb.rob(model=reun.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                            log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)

reun.models[["SY.reun.na"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["SY.reun.na.m"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["SY.reun.na.f"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)

#############################
## Institutionalization models
#############################

grp.inst.models<-list()

grp.inst.models[["SY.grp.inst.all"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                  log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["SY.grp.inst.all.m"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt.m)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                    log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["SY.grp.inst.all.f"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt.f)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                    log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)

grp.inst.models[["SY.grp.inst.blk"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ log(b.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                  log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["SY.grp.inst.blk.m"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ log(b.m.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["SY.grp.inst.blk.f"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ sqrt(b.f.incarrt)+ log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)

grp.inst.models[["SY.grp.inst.na"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                 log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["SY.grp.inst.na.m"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.m.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                   log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["SY.grp.inst.na.f"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.f.incarrt)+ +log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                   log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)

#####################
## Length of stay
#####################
los.models<-list()

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos)~log(incarrt)+
                                                  log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                                                  log(chpovrt)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["SY.los.all"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos.nat.am)~sqrt(a.incarrt)+
                                                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                  sqrt(chpov.amind.pc)+log(pctami)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["SY.los.na"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

los<-lapply(fc.imp$imputations, function(d) plm(log(lifelos.blk)~log(b.incarrt)+
                                                  log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                  log(chpov.blk.pc)+
                                                  log(pctblk)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["SY.los.b"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


maketables.glm(cl.models)
maketables.glm(ent.models)
maketables.glm(reun.models)
maketables.glm(grp.inst.models)
maketables.plm(los.models)

##################################################################################################
### State-year FEs with lagged incar, white incar
#################################################


cl.models[["ROB.cl.all"]]<-mi.robust.nb.rob(model=cl~log(incarrt)+log(incarrt.lag) + log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                             log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
cl.models[["ROB.cl.all.m"]]<-mi.robust.nb.rob(model=cl~log(incarrt.m)+log(incarrt.m.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                               log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
cl.models[["ROB.cl.all.f"]]<-mi.robust.nb.rob(model=cl~log(incarrt.f)+log(incarrt.f.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                               log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)

cl.models[["ROB.cl.blk"]]<-mi.robust.nb.rob(model=cl.blk ~ log(b.incarrt)+log(b.incarrt.lag)+log(w.incarrt)+
                                             log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                             log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["ROB.cl.blk.m"]]<-mi.robust.nb.rob(model=cl.blk ~ log(b.m.incarrt)+log(b.m.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                               log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
cl.models[["ROB.cl.blk.f"]]<-mi.robust.nb.rob(model=cl.blk ~ sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                               log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)

cl.models[["ROB.cl.na"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.incarrt)+sqrt(a.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                            log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["ROB.cl.na.m"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                              log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
cl.models[["ROB.cl.na.f"]]<-mi.robust.nb.rob(model=cl.nat.am ~ sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                              log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)

########################
##### ENTRY MODELS
########################
ent.models<-list()

ent.models[["ROB.ent.all"]]<-mi.robust.nb.rob(model=ent~log(incarrt)+log(incarrt.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                               log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
ent.models[["ROB.ent.all.m"]]<-mi.robust.nb.rob(model=ent~log(incarrt.m)+log(incarrt.m.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                 log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)
ent.models[["ROB.ent.all.f"]]<-mi.robust.nb.rob(model=ent~log(incarrt.f)+log(incarrt.f.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                 log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(child)), data=fc.imp$imputations)

ent.models[["ROB.ent.blk"]]<-mi.robust.nb.rob(model=ent.blk ~ log(b.incarrt)+log(b.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                               log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["ROB.ent.blk.m"]]<-mi.robust.nb.rob(model=ent.blk ~ log(b.m.incarrt)+log(b.m.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                 log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)
ent.models[["ROB.ent.blk.f"]]<-mi.robust.nb.rob(model=ent.blk ~ sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                 log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(blk.child)), data=fc.imp$imputations)

ent.models[["ROB.ent.na"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.incarrt)+sqrt(a.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                              log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["ROB.ent.na.m"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)
ent.models[["ROB.ent.na.f"]]<-mi.robust.nb.rob(model=ent.nat.am ~ sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(amind.child)), data=fc.imp$imputations)


###############################
## REUNIFICATION EXITS
###############################

### FILTER OUT THOSE W/NO BLACK, NA CASELOADS
fcb.reun.imp<-list()
fcn.reun.imp<-list()
for(i in (1:m)){
  fcb.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.blk>1)
  fcn.reun.imp[[i]]<-fc.imp$imputations[[i]]%>%filter(cl.nat.am>1)
}

reun.models<-list()

reun.models[["ROB.reun.all"]]<-mi.robust.nb.rob(model=reun~log(incarrt)+log(incarrt.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                 log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
reun.models[["ROB.reun.all.m"]]<-mi.robust.nb.rob(model=reun~log(incarrt.m)+log(incarrt.m.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                   log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
reun.models[["ROB.reun.all.f"]]<-mi.robust.nb.rob(model=reun~log(incarrt.f)+log(incarrt.f.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                   log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)

reun.models[["ROB.reun.blk"]]<-mi.robust.nb.rob(model=reun.blk ~ log(b.incarrt)+log(b.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                 log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["ROB.reun.blk.m"]]<-mi.robust.nb.rob(model=reun.blk ~ log(b.m.incarrt)+log(b.m.incarrt.lag)+log(w.incarrt)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                   log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
reun.models[["ROB.reun.blk.f"]]<-mi.robust.nb.rob(model=reun.blk ~ sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                   log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)

reun.models[["ROB.reun.na"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.incarrt)+sqrt(a.incarrt.lag)+log(w.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["ROB.reun.na.m"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+log(w.incarrt)+ log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                  log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
reun.models[["ROB.reun.na.f"]]<-mi.robust.nb.rob(model=reun.nat.am ~ sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                  log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)

#############################
## Institutionalization models
#############################

grp.inst.models<-list()

grp.inst.models[["ROB.grp.inst.all"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt)+log(incarrt.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                         log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["ROB.grp.inst.all.m"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt.m)+log(incarrt.m.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                           log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)
grp.inst.models[["ROB.grp.inst.all.f"]]<-mi.robust.nb.rob(model=grp.inst~log(incarrt.f)+log(incarrt.f.lag)+log(unemp.rt)+log(singpar.rt)+log(lessHS)+log(chpovrt)+inst6014_nom+
                                                           log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl)), data=fc.imp$imputations)

grp.inst.models[["ROB.grp.inst.blk"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ log(b.incarrt)+log(b.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                         log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["ROB.grp.inst.blk.m"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ log(b.m.incarrt)+log(b.m.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                           log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)
grp.inst.models[["ROB.grp.inst.blk.f"]]<-mi.robust.nb.rob(model=grp.inst.blk ~ sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+ log(w.incarrt)+log(b.unemp.rt)+ +log(b.singpar.rt)+log(blk.lessHS)+
                                                           log(chpov.blk.pc)+log(pctblk)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.blk)), data=fcb.reun.imp)

grp.inst.models[["ROB.grp.inst.na"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.incarrt)+sqrt(a.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                        log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["ROB.grp.inst.na.m"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                          log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)
grp.inst.models[["ROB.grp.inst.na.f"]]<-mi.robust.nb.rob(model=grp.inst.nat.am ~ sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+ log(w.incarrt)+log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                          log(chpov.amind.pc)+log(pctami)+inst6014_nom+log(v.crime.rt)+factor(stname)+factor(year.c)+offset(log(cl.nat.am)), data=fcn.reun.imp)

#####################
## Length of stay
#####################
los.models<-list()

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos)~log(incarrt)+log(incarrt.lag)+
                                                  log(unemp.rt)+log(singpar.rt)+log(lessHS)+
                                                  log(chpovrt)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["ROB.los.all"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

los<-lapply(fc.imp$imputations, function(d) plm(sqrt(lifelos.nat.am)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+log(w.incarrt)+
                                                  log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                  sqrt(chpov.amind.pc)+log(pctami)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["ROB.los.na"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

los<-lapply(fc.imp$imputations, function(d) plm(log(lifelos.blk)~log(b.incarrt)+log(b.incarrt.lag)+log(w.incarrt)+
                                                  log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                  log(chpov.blk.pc)+
                                                  log(pctblk)+
                                                  inst6014_nom+log(v.crime.rt),
                                                index=c("stname", "year.c"),
                                                effect="twoways",
                                                model="within",
                                                data=d))
los.models[["ROB.los.b"]]<-list("models"=los, "merge"=merge.mi(lapply(los, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


maketables.glm(cl.models)
maketables.glm(ent.models)
maketables.glm(reun.models)
maketables.glm(grp.inst.models)
maketables.plm(los.models)