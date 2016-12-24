setwd("U:/cw-race/tables")
b.disp.bayes<-lapply(fc.imp$imputations, function(d) 
  stan_lmer(log(bw.disp)~
              scale(b.incardisp)+scale(b.incardisp.mean)+
              scale(bdisp.chpov)+scale(bdisp.chpov.mean)+
              scale(I(b.unemp.rt/w.unemp.rt))+scale(b.unempdisp.mean)+
              scale(I(b.singpar.rt/w.singpar.rt))+scale(b.singpardisp.mean)+
              scale(I(blk.lessHS/wht.lessHS))+scale(b.lessHS.mean)+
              scale(pctblk)+scale(pctblk.mean)+
              scale(inst6014_nom)+scale(inst.mean)+
              scale(b.arrest.disp)+scale(b.arrest.mean)+
              year.c+
              scale(tanf.adeq)+scale(tanf.adeq.mean)+
              scale(tanf.incl)+scale(tanf.incl.mean)+
              scale(medicaid.incl)+scale(medicaid.incl.mean)+
              scale(snap.incl)+scale(snap.incl.mean)+
              (1|stname),
            data=d))

b.disp.bayes.stan<-sflist2stanfit(list(b.disp.bayes[[1]]$stanfit, b.disp.bayes[[2]]$stanfit,
                                       b.disp.bayes[[3]]$stanfit, b.disp.bayes[[4]]$stanfit,
                                       b.disp.bayes[[5]]$stanfit, b.disp.bayes[[6]]$stanfit,
                                       b.disp.bayes[[7]]$stanfit, b.disp.bayes[[8]]$stanfit,
                                       b.disp.bayes[[9]]$stanfit, b.disp.bayes[[10]]$stanfit,
                                       b.disp.bayes[[11]]$stanfit, b.disp.bayes[[12]]$stanfit,
                                       b.disp.bayes[[13]]$stanfit, b.disp.bayes[[14]]$stanfit,
                                       b.disp.bayes[[15]]$stanfit))

out<-as.data.frame(summary(b.disp.bayes.stan))
write.csv(out, "b-disp-bayes.csv", row.names=TRUE)
rm(b.disp.bayes)
rm(b.disp.bayes.stan)

a.disp.bayes<-lapply(fc.imp$imputations, function(d) 
  stan_lmer(sqrt(ami.disp)~
         scale(a.incardisp)+scale(a.incardisp.mean)+
         scale(adisp.chpov)+scale(adisp.chpov.mean)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.unempdisp.mean)+
         scale(a.singpar.rt/w.singpar.rt)+scale(a.singpardisp.mean)+
         scale(amind.lessHS/wht.lessHS)+scale(a.lessHS.mean)+
         scale(pctami)+scale(pctami.mean)+
         scale(inst6014_nom)+scale(inst.mean)+
         scale(ai.arrest.disp)+scale(ai.arrest.mean)+
         year.c+
         scale(tanf.adeq)+scale(tanf.adeq.mean)+
         scale(tanf.incl)+scale(tanf.incl.mean)+
         scale(medicaid.incl)+scale(medicaid.incl.mean)+
         scale(snap.incl)+scale(snap.incl.mean)+
         (1|stname),
       data=d))

a.disp.bayes.stan<-sflist2stanfit(list(a.disp.bayes[[1]]$stanfit, a.disp.bayes[[2]]$stanfit,
                                       a.disp.bayes[[3]]$stanfit, a.disp.bayes[[4]]$stanfit,
                                       a.disp.bayes[[5]]$stanfit, a.disp.bayes[[6]]$stanfit,
                                       a.disp.bayes[[7]]$stanfit, a.disp.bayes[[8]]$stanfit,
                                       a.disp.bayes[[9]]$stanfit, a.disp.bayes[[10]]$stanfit,
                                       a.disp.bayes[[11]]$stanfit, a.disp.bayes[[12]]$stanfit,
                                       a.disp.bayes[[13]]$stanfit, a.disp.bayes[[14]]$stanfit,
                                       a.disp.bayes[[15]]$stanfit))

out<-as.data.frame(summary(a.disp.bayes.stan))
write.csv(out, "a-disp-bayes.csv", row.names=TRUE)
rm(a.disp.bayes)
rm(a.disp.bayes.stan)

b.ent.bayes<-lapply(fc.imp$imputations, function(d) 
  stan_lmer(log((ent.blk/blk.child)/(ent.white/wht.child))~
         scale(b.incardisp)+scale(b.incardisp.mean)+
         scale(bdisp.chpov)+scale(bdisp.chpov.mean)+
         scale(I(b.unemp.rt/w.unemp.rt))+scale(b.unempdisp.mean)+
         scale(I(b.singpar.rt/w.singpar.rt))+scale(b.singpardisp.mean)+
         scale(I(blk.lessHS/wht.lessHS))+scale(b.lessHS.mean)+
         scale(pctblk)+scale(pctblk.mean)+
         scale(inst6014_nom)+scale(inst.mean)+
         scale(b.arrest.disp)+scale(b.arrest.mean)+
         year.c+
         scale(tanf.adeq)+scale(tanf.adeq.mean)+
         scale(tanf.incl)+scale(tanf.incl.mean)+
         scale(medicaid.incl)+scale(medicaid.incl.mean)+
         scale(snap.incl)+scale(snap.incl.mean)+
         (1|stname),
       data=d))

b.ent.bayes.stan<-sflist2stanfit(list(b.ent.bayes[[1]]$stanfit, b.ent.bayes[[2]]$stanfit,
                                       b.ent.bayes[[3]]$stanfit, b.ent.bayes[[4]]$stanfit,
                                       b.ent.bayes[[5]]$stanfit, b.ent.bayes[[6]]$stanfit,
                                       b.ent.bayes[[7]]$stanfit, b.ent.bayes[[8]]$stanfit,
                                       b.ent.bayes[[9]]$stanfit, b.ent.bayes[[10]]$stanfit,
                                       b.ent.bayes[[11]]$stanfit, b.ent.bayes[[12]]$stanfit,
                                       b.ent.bayes[[13]]$stanfit, b.ent.bayes[[14]]$stanfit,
                                       b.ent.bayes[[15]]$stanfit))

out<-as.data.frame(summary(b.ent.bayes.stan))
write.csv(out, "b-ent-bayes.csv", row.names=TRUE)
rm(b.ent.bayes)
rm(b.ent.bayes.stan)

a.ent.bayes<-lapply(fc.imp$imputations, function(d) 
  stan_lmer(sqrt((ent.nat.am/amind.child)/(ent.white/wht.child))~
         scale(a.incardisp)+scale(a.incardisp.mean)+
         scale(adisp.chpov)+scale(adisp.chpov.mean)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.unempdisp.mean)+
         scale(a.singpar.rt/w.singpar.rt)+scale(a.singpardisp.mean)+
         scale(amind.lessHS/wht.lessHS)+scale(a.lessHS.mean)+
         scale(pctami)+scale(pctami.mean)+
         scale(inst6014_nom)+scale(inst.mean)+
         scale(ai.arrest.disp)+scale(ai.arrest.mean)+
         year.c+
         scale(tanf.adeq)+scale(tanf.adeq.mean)+
         scale(tanf.incl)+scale(tanf.incl.mean)+
         scale(medicaid.incl)+scale(medicaid.incl.mean)+
         scale(snap.incl)+scale(snap.incl.mean)+
         (1|stname),
       data=d))

a.ent.bayes.stan<-sflist2stanfit(list(a.ent.bayes[[1]]$stanfit, a.ent.bayes[[2]]$stanfit,
                                      a.ent.bayes[[3]]$stanfit, a.ent.bayes[[4]]$stanfit,
                                      a.ent.bayes[[5]]$stanfit, a.ent.bayes[[6]]$stanfit,
                                      a.ent.bayes[[7]]$stanfit, a.ent.bayes[[8]]$stanfit,
                                      a.ent.bayes[[9]]$stanfit, a.ent.bayes[[10]]$stanfit,
                                      a.ent.bayes[[11]]$stanfit, a.ent.bayes[[12]]$stanfit,
                                      a.ent.bayes[[13]]$stanfit, a.ent.bayes[[14]]$stanfit,
                                      a.ent.bayes[[15]]$stanfit))

out<-as.data.frame(summary(a.ent.bayes.stan))
write.csv(out, "a-ent-bayes.csv", row.names=TRUE)
rm(a.ent.bayes)
rm(a.ent.bayes.stan)

b.reun.bayes<-lapply(fcb.reun.imp, function(d) stan_lmer(log((reun.blk/cl.blk)/(reun.white/cl.white)) ~ 
                                                     scale(b.incardisp)+scale(b.incardisp.mean)+
                                                     scale(bdisp.chpov)+scale(bdisp.chpov.mean)+
                                                     scale(I(b.unemp.rt/w.unemp.rt))+scale(b.unempdisp.mean)+
                                                     scale(I(b.singpar.rt/w.singpar.rt))+scale(b.singpardisp.mean)+
                                                     scale(I(blk.lessHS/wht.lessHS))+scale(b.lessHS.mean)+
                                                     scale(pctblk)+scale(pctblk.mean)+
                                                     scale(inst6014_nom)+scale(inst.mean)+
                                                     scale(b.arrest.disp)+scale(b.arrest.mean)+
                                                     year.c+
                                                     scale(tanf.adeq)+scale(tanf.adeq.mean)+
                                                     scale(tanf.incl)+scale(tanf.incl.mean)+
                                                     scale(medicaid.incl)+scale(medicaid.incl.mean)+
                                                     scale(snap.incl)+scale(snap.incl.mean)+
                                                     (1|stname), data=d))

b.reun.bayes.stan<-sflist2stanfit(list(b.reun.bayes[[1]]$stanfit, b.reun.bayes[[2]]$stanfit,
                                      b.reun.bayes[[3]]$stanfit, b.reun.bayes[[4]]$stanfit,
                                      b.reun.bayes[[5]]$stanfit, b.reun.bayes[[6]]$stanfit,
                                      b.reun.bayes[[7]]$stanfit, b.reun.bayes[[8]]$stanfit,
                                      b.reun.bayes[[9]]$stanfit, b.reun.bayes[[10]]$stanfit,
                                      b.reun.bayes[[11]]$stanfit, b.reun.bayes[[12]]$stanfit,
                                      b.reun.bayes[[13]]$stanfit, b.reun.bayes[[14]]$stanfit,
                                      b.reun.bayes[[15]]$stanfit))

out<-as.data.frame(summary(b.reun.bayes.stan))
write.csv(out, "b-reun-bayes.csv", row.names=TRUE)
rm(b.reun.bayes)
rm(b.reun.bayes.stan)

a.reun.bayes<-lapply(fcn.reun.imp, function(d) 
  stan_lmer((sqrt((reun.nat.am/cl.nat.am)/(reun.white/cl.white)))~
         scale(a.incardisp)+scale(a.incardisp.mean)+
         scale(adisp.chpov)+scale(adisp.chpov.mean)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.unempdisp.mean)+
         scale(a.singpar.rt/w.singpar.rt)+scale(a.singpardisp.mean)+
         scale(amind.lessHS/wht.lessHS)+scale(a.lessHS.mean)+
         scale(pctami)+scale(pctami.mean)+
         scale(inst6014_nom)+scale(inst.mean)+
         scale(ai.arrest.disp)+scale(ai.arrest.mean)+
         year.c+
         scale(tanf.adeq)+scale(tanf.adeq.mean)+
         scale(tanf.incl)+scale(tanf.incl.mean)+
         scale(medicaid.incl)+scale(medicaid.incl.mean)+
         scale(snap.incl)+scale(snap.incl.mean)+
         (1|stname),
       data=d))

a.reun.bayes.stan<-sflist2stanfit(list(a.reun.bayes[[1]]$stanfit, a.reun.bayes[[2]]$stanfit,
                                       a.reun.bayes[[3]]$stanfit, a.reun.bayes[[4]]$stanfit,
                                       a.reun.bayes[[5]]$stanfit, a.reun.bayes[[6]]$stanfit,
                                       a.reun.bayes[[7]]$stanfit, a.reun.bayes[[8]]$stanfit,
                                       a.reun.bayes[[9]]$stanfit, a.reun.bayes[[10]]$stanfit,
                                       a.reun.bayes[[11]]$stanfit, a.reun.bayes[[12]]$stanfit,
                                       a.reun.bayes[[13]]$stanfit, a.reun.bayes[[14]]$stanfit,
                                       a.reun.bayes[[15]]$stanfit))

out<-as.data.frame(summary(a.reun.bayes.stan))
write.csv(out, "a-reun-bayes.csv", row.names=TRUE)
rm(a.reun.bayes)
rm(a.reun.bayes.stan)

q("no")
