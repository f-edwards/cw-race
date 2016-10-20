
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