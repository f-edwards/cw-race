library(plm)
library(lmtest)

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

merge.mi.fd<-function(model.imp){
  beta<-do.call(rbind, lapply(model.imp, function(d) coef(summary(d))[,1]))
  se<-do.call(rbind, lapply(model.imp, function(d) coef(summary(d))[,2]))
  meld<-mi.meld(q=beta, se=se)
  out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
  out$z<-out[,1]/out[,2]
  out$p<-round(2*pnorm(-abs(out$z)),3)
  names(out)[1:2]<-c("Beta", "SE")
  out<-list(out)
  return(out)
}

# 
# merge.mi.nb<-function(model.imp){
#   beta<-do.call(rbind, lapply(model.imp, function(d) coef(d)))
#   se<-do.call(rbind, lapply(model.imp, function(d) sqrt(diag(vcov(d)))))
#   meld<-mi.meld(q=beta, se=se)
#   out<-as.data.frame(cbind(t(meld[[1]]),t(meld[[2]])))
#   out$z<-out[,1]/out[,2]
#   out$p<-2*pnorm(-abs(out$z))
#   out<-rbind(out, NA); row.names(out)[nrow(out)]<-"theta"
# 
#   theta.out<-data.frame(matrix(nrow=m, ncol=2))
#   for(i in 1:m){
#     theta.out[i,1]<-model.imp[[i]]$theta
#     theta.out[i,2]<-model.imp[[i]]$SE.theta
#   }
#   
#   theta<-t(mi.meld(q=as.matrix(theta.out[,1]),
#                         se=as.matrix(theta.out[,2])))
# 
#   out<-list(out, theta)
#   
#   return(out)
# }
# 
# # b.cl.fe<-lapply(fe.data, function(d) glm.nb(cl.blk~-1+scale(b.incarrt)+
# #                       scale(b.unemp.rt)+scale(b.singpar.rt)+scale(blk.lessHS)+
# #                       scale(chpov.blk.pc)+scale(pctblk)+
# #                       scale(inst6014_nom)+scale(v.crime.rt)+
# #                       year.c+factor(stname)+offset(log(blk.child)),
# #                     data=d))
# # 
# # b.res<-merge.mi.nb(b.cl.fe)
# 

###PLM CL FE models, PCSE (Beck and Katz)

FE.models<-list()
model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+#log(b.incarrt.lag)+
                                         log(w.incarrt)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+
                                         log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
FE.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+#sqrt(a.incarrt.lag)+
                                         log(w.incarrt)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
FE.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+#log(b.m.incarrt.lag)+
                                         log(w.incarrt)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+#sqrt(a.m.incarrt.lag)+
                                         log(w.incarrt)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+#sqrt(b.f.incarrt.lag)+
                                         log(w.incarrt)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+#sqrt(a.f.incarrt.lag)+
                                         log(w.incarrt)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


# ####################
# ##LAGGED DV
# ####################
# lag.models<-list()
# 
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+
#                                                         log(b.incarrt.lag)+
#                                                         log(b.unemp.rt)+
#                                                         log(b.singpar.rt)+log(blk.lessHS)+
#                                                         log(chpov.blk.pc)+log(pctblk)+
#                                                         inst6014_nom+log(v.crime.rt)+
#                                                         log(clrt.blk.lag)+log(clrt.blk.lag2),
#                                                       index=c("stname", "year.c"),
#                                                       model="pooling",
#                                                       effect=NULL,
#                                                       data=d))
# lag.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+
#                                                           log(b.m.incarrt.lag)+log(b.unemp.rt)+
#                                                           log(b.singpar.rt)+log(blk.lessHS)+
#                                                           log(chpov.blk.pc)+log(pctblk)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           log(clrt.blk.lag)+log(clrt.blk.lag2),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         effect=NULL,
#                                                         data=d))
# lag.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+
#                                                           sqrt(b.f.incarrt.lag)+log(b.unemp.rt)+
#                                                           log(b.singpar.rt)+log(blk.lessHS)+
#                                                           log(chpov.blk.pc)+log(pctblk)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           log(clrt.blk.lag)+log(clrt.blk.lag2),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         effect=NULL,
#                                                         data=d))
# lag.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
#                                                         log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                         sqrt(chpov.amind.pc)+log(pctami)+
#                                                         inst6014_nom+log(v.crime.rt)+
#                                                         sqrt(clrt.nat.am.lag)+sqrt(clrt.nat.am.lag2),
#                                                       index=c("stname", "year.c"),
#                                                       model="pooling",
#                                                       data=d))
# lag.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
#                                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                           sqrt(chpov.amind.pc)+log(pctami)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           sqrt(clrt.nat.am.lag)+sqrt(clrt.nat.am.lag2),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         data=d))
# lag.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
#                                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                           sqrt(chpov.amind.pc)+log(pctami)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           sqrt(clrt.nat.am.lag)+sqrt(clrt.nat.am.lag2),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         data=d))
# lag.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# ###########################################
# ## FD MODELS
# ###########################################
# fd.models<-list()
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+log(b.incarrt.lag)+
#                                                     log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                     log(chpov.blk.pc)+log(pctblk)+
#                                                     inst6014_nom+log(v.crime.rt),
#                                                   index=c("stname", "year.c"),
#                                                   effect="individual",
#                                                   model="fd",
#                                                   data=d))
# fd.models$b<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
#                                                     log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                     sqrt(chpov.amind.pc)+log(pctami)+
#                                                     inst6014_nom+log(v.crime.rt),
#                                                   index=c("stname", "year.c"),
#                                                   effect="individual",
#                                                   model="fd",
#                                                   data=d))
# fd.models$a<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+log(b.m.incarrt.lag)+
#                                                       log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                       log(chpov.blk.pc)+log(pctblk)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.models$b.m<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
#                                                       log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                       sqrt(chpov.amind.pc)+log(pctami)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.models$a.m<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+
#                                                       log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                       log(chpov.blk.pc)+log(pctblk)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.models$b.f<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
#                                                       log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                       sqrt(chpov.amind.pc)+log(pctami)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.models$a.f<-list("models"=model, "merge"=merge.mi.fd(model))

################################
##Kitchen sink CL
################################

###PLM CL FE models, PCSE (Beck and Katz)
rob.models<-list()

model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.incarrt)+#log(b.incarrt.lag)+
                                         log(w.incarrt)+
                                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                         log(chpov.blk.pc)+log(pctblk)+
                                         inst6014_nom+log(v.crime.rt),
                                       index=c("stname", "year.c"),
                                       effect="twoways",
                                       model="within",
                                       data=d))
rob.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.incarrt)+#sqrt(a.incarrt.lag)+
                                         log(w.incarrt)+
                                         log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         sqrt(chpov.amind.pc)+log(pctami)+
                                         inst6014_nom+log(v.crime.rt),
                                       index=c("stname", "year.c"),
                                       effect="twoways",
                                       model="within",
                                       data=d))
rob.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~log(b.m.incarrt)+#log(b.m.incarrt.lag)+
                                         log(w.incarrt)+
                                           log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                           log(chpov.blk.pc)+log(pctblk)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.m.incarrt)+#sqrt(a.m.incarrt.lag)+
                                         log(w.incarrt)+
                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           sqrt(chpov.amind.pc)+log(pctami)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(log(cl.blk.pc)~sqrt(b.f.incarrt)+#sqrt(b.f.incarrt.lag)+
                                         log(w.incarrt)+
                                           log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                           log(chpov.blk.pc)+log(pctblk)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(cl.amind.pc)~sqrt(a.f.incarrt)+#sqrt(a.f.incarrt.lag)+
                                         log(w.incarrt)+
                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           sqrt(chpov.amind.pc)+log(pctami)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

# ###################################################
# ###PLM ENTRIES FE models, PCSE (Beck and Katz)
# ###################################################
FE.ent.models<-list()

model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+
                                         log(w.incarrt)+
                                                    log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                    log(chpov.blk.pc)+log(pctblk)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
FE.ent.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+
                                         log(w.incarrt)+
                                                    log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                    sqrt(chpov.amind.pc)+log(pctami)+
                                                    inst6014_nom+log(v.crime.rt),
                                                  index=c("stname", "year.c"),
                                                  effect="individual",
                                                  model="within",
                                                  data=d))
FE.ent.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+
                                         log(w.incarrt)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.ent.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+
                                         log(w.incarrt)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.ent.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+
                                         log(w.incarrt)+
                                                      log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                                      log(chpov.blk.pc)+log(pctblk)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.ent.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+
                                         log(w.incarrt)+
                                                      log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                                      sqrt(chpov.amind.pc)+log(pctami)+
                                                      inst6014_nom+log(v.crime.rt),
                                                    index=c("stname", "year.c"),
                                                    effect="individual",
                                                    model="within",
                                                    data=d))
FE.ent.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))



# ################################# 
# # ###LAGGED DV
# ################################# 
# lag.ent.models<-list()
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+
#                                                         log(b.incarrt.lag)+log(b.unemp.rt)+
#                                                         log(b.singpar.rt)+log(blk.lessHS)+
#                                                         log(chpov.blk.pc)+log(pctblk)+
#                                                         inst6014_nom+log(v.crime.rt)+
#                                                         log(clrt.blk.lag),
#                                                       index=c("stname", "year.c"),
#                                                       model="pooling",
#                                                       effect=NULL,
#                                                       data=d))
# lag.ent.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+
#                                                           log(b.m.incarrt.lag)+log(b.unemp.rt)+
#                                                           log(b.singpar.rt)+log(blk.lessHS)+
#                                                           log(chpov.blk.pc)+log(pctblk)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           log(clrt.blk.lag),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         effect=NULL,
#                                                         data=d))
# lag.ent.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+
#                                                           sqrt(b.f.incarrt.lag)+log(b.unemp.rt)+
#                                                           log(b.singpar.rt)+log(blk.lessHS)+
#                                                           log(chpov.blk.pc)+log(pctblk)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           log(clrt.blk.lag),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         effect=NULL,
#                                                         data=d))
# lag.ent.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
#                                                         log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                         sqrt(chpov.amind.pc)+log(pctami)+
#                                                         inst6014_nom+log(v.crime.rt)+
#                                                         sqrt(clrt.nat.am.lag),
#                                                       index=c("stname", "year.c"),
#                                                       model="pooling",
#                                                       data=d))
# lag.ent.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
#                                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                           sqrt(chpov.amind.pc)+log(pctami)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           sqrt(clrt.nat.am.lag),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         data=d))
# lag.ent.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
#                                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                           sqrt(chpov.amind.pc)+log(pctami)+
#                                                           inst6014_nom+log(v.crime.rt)+
#                                                           sqrt(clrt.nat.am.lag),
#                                                         index=c("stname", "year.c"),
#                                                         model="pooling",
#                                                         data=d))
# lag.ent.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))
# 
# ###########################################
# ## FD MODELS
# ###########################################
# 
# fd.ent.models<-list()
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+log(b.incarrt.lag)+
#                                                     log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                     log(chpov.blk.pc)+log(pctblk)+
#                                                     inst6014_nom+log(v.crime.rt),
#                                                   index=c("stname", "year.c"),
#                                                   effect="individual",
#                                                   model="fd",
#                                                   data=d))
# fd.ent.models$b<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+sqrt(a.incarrt.lag)+
#                                                     log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                     sqrt(chpov.amind.pc)+log(pctami)+
#                                                     inst6014_nom+log(v.crime.rt),
#                                                   index=c("stname", "year.c"),
#                                                   effect="individual",
#                                                   model="fd",
#                                                   data=d))
# fd.ent.models$a<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+log(b.m.incarrt.lag)+
#                                                       log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                       log(chpov.blk.pc)+log(pctblk)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.ent.models$b.m<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+sqrt(a.m.incarrt.lag)+
#                                                       log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                       sqrt(chpov.amind.pc)+log(pctami)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.ent.models$a.m<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
# model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+sqrt(b.f.incarrt.lag)+
#                                                       log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
#                                                       log(chpov.blk.pc)+log(pctblk)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.ent.models$b.f<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+sqrt(a.f.incarrt.lag)+
#                                                       log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
#                                                       sqrt(chpov.amind.pc)+log(pctami)+
#                                                       inst6014_nom+log(v.crime.rt),
#                                                     index=c("stname", "year.c"),
#                                                     effect="individual",
#                                                     model="fd",
#                                                     data=d))
# fd.ent.models$a.f<-list("models"=model, "merge"=merge.mi.fd(model))
# 
# 
################################
##Kitchen sink ENT
################################

###PLM CL FE models, PCSE (Beck and Katz)
rob.ent.models<-list()

model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.incarrt)+
                                         log(w.incarrt)+
                                         log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                         log(chpov.blk.pc)+log(pctblk)+
                                         inst6014_nom+log(v.crime.rt),
                                       index=c("stname", "year.c"),
                                       effect="twoways",
                                       model="within",
                                       data=d))
rob.ent.models$b<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.incarrt)+
                                         log(w.incarrt)+
                                         log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                         sqrt(chpov.amind.pc)+log(pctami)+
                                         inst6014_nom+log(v.crime.rt),
                                       index=c("stname", "year.c"),
                                       effect="twoways",
                                       model="within",
                                       data=d))
rob.ent.models$a<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~log(b.m.incarrt)+
                                           +log(w.incarrt)+
                                           log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                           log(chpov.blk.pc)+log(pctblk)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.ent.models$b.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.m.incarrt)+
                                           +log(w.incarrt)+
                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           sqrt(chpov.amind.pc)+log(pctami)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.ent.models$a.m<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))

model<-lapply(fe.data, function(d) plm(log(ent.blk.pc)~sqrt(b.f.incarrt)+
                                           +log(w.incarrt)+
                                           log(b.unemp.rt)+log(b.singpar.rt)+log(blk.lessHS)+
                                           log(chpov.blk.pc)+log(pctblk)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.ent.models$b.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


model<-lapply(fe.data, function(d) plm(sqrt(ent.amind.pc)~sqrt(a.f.incarrt)+
                                           +log(w.incarrt)+
                                           log(a.unemp.rt)+log(a.singpar.rt)+log(amind.lessHS)+
                                           sqrt(chpov.amind.pc)+log(pctami)+
                                           inst6014_nom+log(v.crime.rt),
                                         index=c("stname", "year.c"),
                                         effect="twoways",
                                         model="within",
                                         data=d))
rob.ent.models$a.f<-list("models"=model, "merge"=merge.mi(lapply(model, function(d) coeftest(d, vcov=vcovBK,cluster=c("stname", "year.c")))))


