
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


b.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(log((ent.blk/blk.child)/(ent.white/wht.child))~log(b.incardisp)+
         log(bdisp.chpov)+
         log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
         log(I(blk.lessHS/wht.lessHS))+
         log(pctblk)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

a.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt((ent.nat.am/amind.child)/(ent.white/wht.child))~I(a.incardisp^(1/2))+
         log(adisp.chpov)+
         log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
         log(amind.lessHS/wht.lessHS)+
         log(pctami)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

b.ent.tab<-makeMIRegTab(b.ent.disp)
a.ent.tab<-makeMIRegTab(a.ent.disp)

b.reun<-lapply(fcb.reun.imp, function(d) lmer(log((reun.blk/cl.blk)/(reun.white/cl.white)) ~ log(b.incardisp)+
             log(bdisp.chpov)+
             log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
             log(I(blk.lessHS/wht.lessHS))+
             log(pctblk)+
             scale(inst6014_nom)+log(v.crime.rt)+year.c+
             (1|stname), data=d))

a.reun<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((reun.nat.am/cl.nat.am)/(reun.white/cl.white)))~sqrt(a.incardisp)+
         log(adisp.chpov)+
         log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
         log(amind.lessHS/wht.lessHS)+
         log(pctami)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

b.reun.tab<-makeMIRegTab(b.reun)
n.reun.tab<-makeMIRegTab(a.reun)

b.inst<-lapply(fcb.reun.imp, function(d) lmer(sqrt((grp.inst.blk/cl.blk)/(grp.inst.white/cl.white)) ~ log(b.incardisp)+
                                                log(bdisp.chpov)+
                                                log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
                                                log(I(blk.lessHS/wht.lessHS))+
                                                log(pctblk)+
                                                scale(inst6014_nom)+log(v.crime.rt)+year.c+
                                                (1|stname), data=d))

a.inst<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((grp.inst.nat.am/cl.nat.am)/(grp.inst.white/cl.white)))~sqrt(a.incardisp)+
         log(adisp.chpov)+
         log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
         log(amind.lessHS/wht.lessHS)+
         log(pctami)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

b.inst.tab<-makeMIRegTab(b.inst)
n.inst.tab<-makeMIRegTab(a.inst)

b.inst<-lapply(fcb.reun.imp, function(d) lmer(sqrt((grp.inst.blk/cl.blk)/(grp.inst.white/cl.white)) ~ log(b.incardisp)+
                                                log(bdisp.chpov)+
                                                log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
                                                log(I(blk.lessHS/wht.lessHS))+
                                                log(pctblk)+
                                                scale(inst6014_nom)+log(v.crime.rt)+year.c+
                                                (1|stname), data=d))

a.inst<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((grp.inst.nat.am/cl.nat.am)/(grp.inst.white/cl.white)))~sqrt(a.incardisp)+
         log(adisp.chpov)+
         log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
         log(amind.lessHS/wht.lessHS)+
         log(pctami)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

b.inst.tab<-makeMIRegTab(b.inst)
n.inst.tab<-makeMIRegTab(a.inst)

b.los<-lapply(fcb.reun.imp, function(d) lmer(log(lifelos.blk/lifelos.white) ~ log(b.incardisp)+
                                                log(bdisp.chpov)+
                                                log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
                                                log(I(blk.lessHS/wht.lessHS))+
                                                log(pctblk)+
                                                scale(inst6014_nom)+log(v.crime.rt)+year.c+
                                                (1|stname), data=d))

a.los<-lapply(fcn.reun.imp, function(d) 
  lmer(log(lifelos.nat.am/lifelos.white)~sqrt(a.incardisp)+
         log(adisp.chpov)+
         log(a.unemp.rt/w.unemp.rt)+log(a.singpar.rt/w.singpar.rt)+
         log(amind.lessHS/wht.lessHS)+
         log(pctami)+
         scale(inst6014_nom)+log(v.crime.rt)+
         year.c+
         (1|stname),
       data=d))

b.los.tab<-makeMIRegTab(b.los)
n.los.tab<-makeMIRegTab(a.los)
