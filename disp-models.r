# mean predictor model suggested by gelman
# testdat<-fc.imp$imputations[[1]]
# testdat<-testdat%>%group_by(stname)%>%mutate(b.incardisp.mean=mean(b.incardisp))
# 
# tm<-  lmer(log(bw.disp)~log(b.incardisp)+
#          log(bdisp.chpov)+log(b.incardisp.mean)+
#          log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
#          log(I(blk.lessHS/wht.lessHS))+
#          log(pctblk)+
#          scale(inst6014_nom)+log(v.crime.rt)+
#          year.c+
#          (1|stname),
#        data=testdat))


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
         (1|stname),
       data=d))

b.disp.bayes<-stan_lmer(log(bw.disp)~scale(b.incardisp)+
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
  (1|stname),
data=fc.imp$imputations[[1]])

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
         (1|stname),
       data=d))


# to include state means as predictors, maybe better for unbiased, but interpretation is weird
# test<-fc.imp$imputations[[1]]
sdat<-fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(a.incardisp.mean=mean(a.incardisp),
                                         adisp.chpov.mean=mean(adisp.chpov),
                                         a.unempdisp.mean=mean(a.unemp.rt/w.unemp.rt),
                                         a.singpardisp.mean=mean(a.singpar.rt/w.singpar.rt),
                                         a.lessHS.mean=mean(amind.lessHS/wht.lessHS),
                                         pctami.mean=mean(pctami),
                                         b.incardisp.mean=mean(b.incardisp),
                                         bdisp.chpov.mean=mean(bdisp.chpov),
                                         b.unempdisp.mean=mean(b.unemp.rt/w.unemp.rt),
                                         b.singpardisp.mean=mean(b.singpar.rt/w.singpar.rt),
                                         b.lessHS.mean=mean(blk.lessHS/wht.lessHS),
                                         pctblk.mean=mean(pctblk),
                                         inst.mean=mean(inst6014_nom),
                                         v.crime.mean=mean(v.crime.rt))

sdat$b.ranef<-as.numeric(ranef(b.disp[[1]])$stname[[1]])
# test<-left_join(test, sdat, by="state")
# 
# a.disp.mean<- lmer(I(ami.disp^(1/2))~scale(a.incardisp)+scale(a.incardisp.mean)+
#                     scale(adisp.chpov)+scale(adisp.chpov.mean)+
#                     scale(a.unemp.rt/w.unemp.rt)+scale(a.unempdisp.mean)+
#                      scale(a.singpar.rt/w.singpar.rt)+scale(a.singpardisp.mean)+
#                     scale(amind.lessHS/wht.lessHS)+scale(a.lessHS.mean)+
#                     scale(pctami)+scale(pctami.mean)+
#                     scale(inst6014_nom)+scale(inst.mean)+
#                      scale(v.crime.rt)+scale(v.crime.mean)+
#                     year.c+
#                     (1|stname),
#                   data=test)
# 
# b.disp.mean<-lmer(log(bw.disp)~log(b.incardisp)+log(b.incardisp.mean)+
#                    log(bdisp.chpov)+log(bdisp.chpov.mean)+
#                    log(I(b.unemp.rt/w.unemp.rt))+log(b.unempdisp.mean)+
#                    log(I(b.singpar.rt/w.singpar.rt))+log(b.singpardisp.mean)+
#                    log(I(blk.lessHS/wht.lessHS))+log(b.lessHS.mean)+
#                    log(pctblk)+log(pctblk.mean)+
#                    scale(inst6014_nom)+scale(inst.mean)+
#                      log(v.crime.rt)+log(v.crime.mean)+
#                    year.c+
#                    (1|stname),
#                  data=test)


b.d.tab<-makeMIRegTab(b.disp)
a.d.tab<-makeMIRegTab(a.disp)


b.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(log((ent.blk/blk.child)/(ent.white/wht.child))~scale(b.incardisp)+
         scale(bdisp.chpov)+
         scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
         scale(I(blk.lessHS/wht.lessHS))+
         scale(pctblk)+
         scale(inst6014_nom)+scale(v.crime.rt)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|stname),
       data=d))

a.ent.disp<-lapply(fc.imp$imputations, function(d) 
  lmer(sqrt((ent.nat.am/amind.child)/(ent.white/wht.child))~scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+scale(v.crime.rt)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|stname),
       data=d))

b.ent.tab<-makeMIRegTab(b.ent.disp)
a.ent.tab<-makeMIRegTab(a.ent.disp)

b.reun<-lapply(fcb.reun.imp, function(d) lmer(log((reun.blk/cl.blk)/(reun.white/cl.white)) ~ scale(b.incardisp)+
             scale(bdisp.chpov)+
             scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
             scale(I(blk.lessHS/wht.lessHS))+
             scale(pctblk)+
             scale(inst6014_nom)+scale(v.crime.rt)+year.c+
               scale(tanf.adeq)+
               scale(tanf.incl)+
               scale(medicaid.incl)+
               scale(snap.incl)+
             (1|stname), data=d))

a.reun<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((reun.nat.am/cl.nat.am)/(reun.white/cl.white)))~scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+scale(v.crime.rt)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
         (1|stname),
       data=d))

b.reun.tab<-makeMIRegTab(b.reun)
n.reun.tab<-makeMIRegTab(a.reun)

b.inst<-lapply(fcb.reun.imp, function(d) lmer(sqrt((grp.inst.blk/cl.blk)/(grp.inst.white/cl.white)) ~ scale(b.incardisp)+
                                                scale(bdisp.chpov)+
                                                scale(I(b.unemp.rt/w.unemp.rt))+scale(I(b.singpar.rt/w.singpar.rt))+
                                                scale(I(blk.lessHS/wht.lessHS))+
                                                scale(pctblk)+
                                                scale(inst6014_nom)+scale(v.crime.rt)+year.c+
                                                scale(tanf.adeq)+
                                                scale(tanf.incl)+
                                                scale(medicaid.incl)+
                                                scale(snap.incl)+
                                                (1|stname), data=d))

a.inst<-lapply(fcn.reun.imp, function(d) 
  lmer((sqrt((grp.inst.nat.am/cl.nat.am)/(grp.inst.white/cl.white)))~scale(a.incardisp)+
         scale(adisp.chpov)+
         scale(a.unemp.rt/w.unemp.rt)+scale(a.singpar.rt/w.singpar.rt)+
         scale(amind.lessHS/wht.lessHS)+
         scale(pctami)+
         scale(inst6014_nom)+scale(v.crime.rt)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(medicaid.incl)+
         scale(snap.incl)+
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

setwd("~/sync/cw-race/tables/")
library(xtable)

row.names(b.d.tab)<-row.names(a.d.tab)<-row.names(b.ent.tab)<-row.names(a.ent.tab)<-row.names(b.reun.tab)<-row.names(n.reun.tab)<-c("Intercept", "Incarceration ineq.", "Unemployment ineq.", "Single parent ineq.",
                      "Less than HS ineq.", "Child poverty ineq.", "Percent pop.",
                      "Leg. ideology", "Violent crime rate", "Year", "RE Variance")

print(xtable(b.d.tab,
             caption="Estimates of relationships between Black / White foster care caseload inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
      file="b-cl-disp.tex", 
      caption.placement="top")
print(xtable(a.d.tab,
             caption="Estimates of relationships between Native American / White foster care caseload inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
      file="a-cl-disp.tex", caption.placement="top")
print(xtable(b.ent.tab,
             caption="Estimates of relationships between Black / White foster care entry inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
             file="b-ent-disp.tex", caption.placement="top")
print(xtable(a.ent.tab,
             caption="Estimates of relationships between Native American / White foster care entry inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
              file="a-ent-disp.tex", caption.placement="top")
print(xtable(b.reun.tab,
             caption="Estimates of relationships between Black / White foster care reunification exit inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
             file="b-reun-disp.tex", caption.placement="top")
print(xtable(n.reun.tab,
      caption="Estimates of relationships between Native American / White foster care reunification exit inequality and incarceration inequality, including controls for family and socio-economic racial inequality, crime, and political context. Multilevel models with state intercepts, results combined across imputations"), 
      file="a-reun-disp.tex", caption.placement="top")