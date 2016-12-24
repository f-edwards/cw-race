

b.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(log(bw.disp)~scale(b.incardisp)+
         scale(bdisp.chpov)+
         scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
         scale(I(blk.lessHS/wht.lessHS))+
         scale(pctblk)+
         scale(inst6014_nom)+
         scale(b.arrest.disp)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

b.disp.mean<-lapply(fc.imp$imputations, function(d) 
  lmer(log(bw.disp)~
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
         (1|state),
       data=d))

b.disp.mean.tab<-makeMIRegTab(b.disp.mean)

# b.disp.fe<-lapply(fc.imp$imputations, function(d) 
#   lm(log(bw.disp)~scale(b.incardisp)+
#          scale(bdisp.chpov)+
#          scale(I(b.unemp.rt/w.unemp.rt))+
#          scale(I(b.singpar.rt/w.singpar.rt))+
#          scale(I(blk.lessHS/wht.lessHS))+
#          scale(pctblk)+
#          scale(inst6014_nom)+
#          scale(b.arrest.disp)+
#          year.c+
#          scale(tanf.adeq)+
#          scale(tanf.incl)+
#          scale(medicaid.incl)+
#          scale(snap.incl)+
#          factor(state),
#        data=d))
# NEAT! RE yields identical results

a.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt(ami.disp)~
         scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.disp)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

a.disp.mean<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt(ami.disp)~
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
         (1|state),
       data=d))

a.disp.mean.tab<-makeMIRegTab(a.disp.mean)

b.d.tab<-makeMIRegTab(b.disp)
a.d.tab<-makeMIRegTab(a.disp)


b.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(log((ent.blk/blk.child)/(ent.white/wht.child))~scale(b.incardisp)+
         scale(bdisp.chpov)+
         scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
         scale(I(blk.lessHS/wht.lessHS))+
         scale(pctblk)+
         scale(inst6014_nom)+
         scale(b.arrest.disp)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

b.ent.disp.mean<-lapply(fc.imp$imputations, function(d) 
  lmer(log((ent.blk/blk.child)/(ent.white/wht.child))~
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
         (1|state),
       data=d))

a.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt((ent.nat.am/amind.child)/(ent.white/wht.child))~scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.disp)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

a.ent.disp.mean<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt((ent.nat.am/amind.child)/(ent.white/wht.child))~
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
         (1|state),
       data=d))

b.ent.tab<-makeMIRegTab(b.ent.disp)
a.ent.tab<-makeMIRegTab(a.ent.disp)
b.ent.tab.mean<-makeMIRegTab(b.ent.disp.mean)
a.ent.tab.mean<-makeMIRegTab(a.ent.disp.mean)


b.reun<-lapply(fcb.reun.imp, function(d) lmer(log((reun.blk/cl.blk)/(reun.white/cl.white)) ~ scale(b.incardisp)+
             scale(bdisp.chpov)+
             scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
             scale(I(blk.lessHS/wht.lessHS))+
             scale(pctblk)+
             scale(inst6014_nom)+
               scale(b.arrest.disp)+
               year.c+
               scale(tanf.adeq)+
               scale(tanf.incl)+
               scale(medicaid.incl)+
               scale(snap.incl)+
             (1|state), data=d))

b.reun.mean<-lapply(fcb.reun.imp, function(d) lmer(log((reun.blk/cl.blk)/(reun.white/cl.white)) ~ 
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
                                                (1|state), data=d))

a.reun<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((reun.nat.am/cl.nat.am)/(reun.white/cl.white)))~scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.disp)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

a.reun.mean<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((reun.nat.am/cl.nat.am)/(reun.white/cl.white)))~
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
         (1|state),
       data=d))


b.reun.tab<-makeMIRegTab(b.reun)
a.reun.tab<-makeMIRegTab(a.reun)
b.reun.tab.mean<-makeMIRegTab(b.reun.mean)
a.reun.tab.mean<-makeMIRegTab(a.reun.mean)



#### INSTITUTIONALIZATION AND LOS MODELS
# 
# b.inst<-lapply(fcb.reun.imp, function(d) lmer(sqrt((grp.inst.blk/cl.blk)/(grp.inst.white/cl.white)) ~ scale(b.incardisp)+
#                                                 scale(bdisp.chpov)+
#                                                 scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
#                                                 scale(I(blk.lessHS/wht.lessHS))+
#                                                 scale(pctblk)+
#                                                 scale(inst6014_nom)+year.c+
#                                                 scale(tanf.adeq)+
#                                                 scale(tanf.incl)+
#                                                 scale(medicaid.incl)+
#                                                 scale(snap.incl)+
#                                                 (1|state), data=d))
# 
# a.inst<-lapply(fcn.reun.imp, function(d) 
#   lmer((sqrt((grp.inst.nat.am/cl.nat.am)/(grp.inst.white/cl.white)))~scale(a.incardisp)+
#          scale(adisp.chpov)+
#          scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
#          scale(amind.lessHS/wht.lessHS)+
#          scale(pctami)+
#          scale(inst6014_nom)+
#          year.c+
#          scale(tanf.adeq)+
#          scale(tanf.incl)+
#          scale(medicaid.incl)+
#          scale(snap.incl)+
#          (1|state),
#        data=d))
# 
# b.inst.tab<-makeMIRegTab(b.inst)
# n.inst.tab<-makeMIRegTab(a.inst)
# 
# b.inst<-lapply(fcb.reun.imp, function(d) lmer(sqrt((grp.inst.blk/cl.blk)/(grp.inst.white/cl.white)) ~ log(b.incardisp)+
#                                                 log(bdisp.chpov)+
#                                                 log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
#                                                 log(I(blk.lessHS/wht.lessHS))+
#                                                 log(pctblk)+
#                                                 scale(inst6014_nom)+log(v.crime.rt)+year.c+
#                                                 (1|state), data=d))
# 
# a.inst<-lapply(fcn.reun.imp, function(d) 
#   lmer((sqrt((grp.inst.nat.am/cl.nat.am)/(grp.inst.white/cl.white)))~sqrt(a.incardisp)+
#          log(adisp.chpov)+
#          log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
#          log(amind.lessHS/wht.lessHS)+
#          log(pctami)+
#          scale(inst6014_nom)+log(v.crime.rt)+
#          year.c+
#          (1|state),
#        data=d))
# 
# b.inst.tab<-makeMIRegTab(b.inst)
# n.inst.tab<-makeMIRegTab(a.inst)
# 
# b.los<-lapply(fcb.reun.imp, function(d) lmer(log(lifelos.blk/lifelos.white) ~ log(b.incardisp)+
#                                                 log(bdisp.chpov)+
#                                                 log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
#                                                 log(I(blk.lessHS/wht.lessHS))+
#                                                 log(pctblk)+
#                                                 scale(inst6014_nom)+log(v.crime.rt)+year.c+
#                                                 (1|state), data=d))
# 
# a.los<-lapply(fcn.reun.imp, function(d) 
#   lmer(log(lifelos.nat.am/lifelos.white)~sqrt(a.incardisp)+
#          log(adisp.chpov)+
#          log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
#          log(amind.lessHS/wht.lessHS)+
#          log(pctami)+
#          scale(inst6014_nom)+log(v.crime.rt)+
#          year.c+
#          (1|state),
#        data=d))
# 
# b.los.tab<-makeMIRegTab(b.los)
# n.los.tab<-makeMIRegTab(a.los)