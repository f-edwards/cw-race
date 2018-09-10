b.cl<-lmer(log(cl.blk/blk.child)~scale(b.incarrt)+
         scale(chpov.blk.pc)+
         scale(b.unemp.rt)+scale(b.singpar.rt)+
         scale(blk.lessHS)+
         scale(pctblk)+
         scale(pctblk1930)+
         scale(inst6014_nom)+
         scale(b.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(snap.incl)+
         scale(medicaid.incl)+
         (1|state),
       data=fc.imp$imputations[[1]])


a.cl<-lmer(sqrt(cl.amind.pc)~scale(a.incarrt)+
             scale(chpov.amind.pc)+
             scale(a.unemp.rt)+scale(a.singpar.rt)+
             scale(amind.lessHS)+
             scale(pctami)+
             scale(inst6014_nom)+
             scale(ai.arrest.pc)+
             year.c+
             scale(tanf.adeq)+
             scale(tanf.incl)+
             scale(snap.incl)+
             scale(medicaid.incl)+
             (1|state),
       data=fc.imp$imputations[[1]])

w.cl<-lmer(log(cl.wht.pc)~scale(w.incarrt)+
         scale(chpov.wht.pc)+
         scale(w.unemp.rt)+scale(w.singpar.rt)+
         scale(wht.lessHS)+
         scale(pctblk)+
         scale(inst6014_nom)+
         scale(w.arrest.pc)+
         year.c+
         scale(tanf.adeq)+
         scale(tanf.incl)+
         scale(snap.incl)+
         scale(medicaid.incl)+
         (1|state),
       data=fc.imp$imputations[[1]])

