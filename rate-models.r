

b.rate<-lapply(fc.imp$imputations, function(d) 
  lmer(log(cl.blk.pc)~scale(b.incarrt)+
         scale(chpov.blk.pc)+
         scale(I(b.unemp.rt))+scale(I(b.singpar.rt))+
         scale(I(blk.lessHS))+
         scale(pctblk)+
         scale(inst6014_nom)+
         scale(b.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

a.rate<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt(cl.amind.pc)~
         scale(a.incarrt)+
         scale(chpov.amind.pc)+
         scale(a.unemp.rt)+scale(a.singpar.rt)+
         scale(amind.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))

b.d.tab<-makeMIRegTab(b.rate)
a.d.tab<-makeMIRegTab(a.rate)


b.ent.rate<-lapply(fc.imp$imputations, function(d) 
  lmer(log((ent.blk/blk.child))~scale(b.incarrt)+
         scale(chpov.blk.pc)+
         scale(I(b.unemp.rt))+scale(I(b.singpar.rt))+
         scale(I(blk.lessHS))+
         scale(pctblk)+
         scale(inst6014_nom)+
         scale(b.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))


a.ent.rate<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt((ent.nat.am/amind.child))~
         scale(a.incarrt)+
         scale(chpov.amind.pc)+
         scale(a.unemp.rt)+scale(a.singpar.rt)+
         scale(amind.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))


b.ent.tab<-makeMIRegTab(b.ent.rate)
a.ent.tab<-makeMIRegTab(a.ent.rate)

b.reun<-lapply(fcb.reun.imp, function(d) lmer(log((reun.blk/cl.blk)) ~scale(b.incarrt)+
                                                scale(chpov.blk.pc)+
                                                scale(I(b.unemp.rt))+scale(I(b.singpar.rt))+
                                                scale(I(blk.lessHS))+
                                                scale(pctblk)+
                                                scale(inst6014_nom)+
                                                scale(b.arrest.pc)+
                                                year.c+
                                                scale(tanf.adeq)+
                                                scale(tanf.incl)+
                                                scale(medicaid.incl)+
                                                scale(snap.incl)+
             (1|state), data=d))


a.reun<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((reun.nat.am/cl.nat.am)))~
         scale(a.incarrt)+
         scale(chpov.amind.pc)+
         scale(a.unemp.rt)+scale(a.singpar.rt)+
         scale(amind.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+
         scale(ai.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|state),
       data=d))


b.reun.tab<-makeMIRegTab(b.reun)
a.reun.tab<-makeMIRegTab(a.reun)




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