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
  lmer(log(bw.disp)~log(b.incardisp)+
         log(bdisp.chpov)+
         log(I(b.unemp.rt/w.unemp.rt))+log(I(b.singpar.rt/w.singpar.rt))+
         log(I(blk.lessHS/wht.lessHS))+
         log(pctblk)+I(log(pctblk^2))+
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