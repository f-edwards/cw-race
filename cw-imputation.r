fc[which((fc$stname=="MI")&(fc$year%in%(2000:2001))),3:17]<-NA
# fc[which((fc$stname=="UT") & (fc$year==2001)), "blk.child"]<-NA

fc$cl.blk<-as.integer(fc$cl.blk)
fc$cl.nat.am<-as.integer(fc$cl.nat.am)
fc$cl<-as.integer(fc$cl)



bounds<-cbind(1:ncol(fc),
              rep(0.0000001, ncol(fc)),
              rep(Inf, ncol(fc)))
ratios<-which(names(fc)%in%c("blk.lessHS", "wht.lessHS", "amind.lessHS"))
bounds[ratios, 3]<-1

colClass<-sapply(fc, class)
colClass[2]<-"year"
colClass[66:73]<-"hist"
colClass[36:39]<-"adult"
colClass[15]<-"state"

numeric.vals<-NULL
for(i in 1:ncol(fc)){
  if(colClass[i]%in%c("integer", "numeric")){
    numeric.vals<-c(numeric.vals, i)
  }
}

m<-15

###THINK ABT IMPUTATION OF CHILD POP - DENOM IS REALLY IMPORTANT, MAYBE PRESERVE?
blk.acs<-which(colnames(fc)%in%c( "blk.child.pov", "blk.lessHS", "blk.unemp", "blk.emp", "blk.singpar"))
amind.acs<-which(colnames(fc)%in%c("amind.child.pov", "amind.lessHS", "amind.unemp", "amind.emp" , "amind.singpar"))

strong<-c(2000,2007:2014)
amind.thresh<-which((fc$amind.child<100000)&(!(fc$year%in%strong)))
blk.thresh<-which((fc$blk.child<100000)&(!(fc$year%in%strong)))

amind.oi<-matrix(nrow=length(amind.acs)*length(amind.thresh), ncol=2)
amind.oi[,2]<-rep(amind.acs, length(amind.thresh))

for(i in 1:length(amind.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(amind.acs)-1
  amind.oi[oi.min:oi.max,1]<-rep(amind.thresh[i], length(amind.acs))
  oi.min<-oi.max+1
}

###MANUALLY add HI 2007 (AMIND Child Pov observed at 0)
###MANUALLY ADD PROBLEMATIC CHILD POP AND CHILD POV OBS FOR OI
### HI 2007, NH amind child 2001


amind.oi<-rbind(amind.oi, c(367, 37) )
amind.oi<-rbind(amind.oi, c(693, 36))

blk.oi<-matrix(nrow=length(blk.acs)*length(blk.thresh), ncol=2)
blk.oi[,2]<-rep(blk.acs, length(blk.thresh))

for(i in 1:length(blk.thresh)){
  if(i==1){oi.min<-1}
  oi.max<-oi.min+length(blk.acs)-1
  blk.oi[oi.min:oi.max,1]<-rep(blk.thresh[i], length(blk.acs))
  oi.min<-oi.max+1
}

oi<-rbind(amind.oi, blk.oi)

oi.priors<-matrix(nrow=nrow(oi), ncol=4)
oi.priors[,1:2]<-oi[,1:2]


####THIS IS TO IMPLEMENT A SAMPLE SIZE WEIGHTED VARIANCE BY POP
####WOULD REQUIRE COUNTING SAMPLED INDIVIDUALS FROM EACH GROUP FROM ACS
####IF I GET COMMENTS, CAN GO BACK AND IMPLEMENT TO MAKE
####Var=pop*(p*(1-p)/samplesize)
# amind.child<-which(names(fc)%in%c("amind.child", "amind.child.pov", "amind.singpar"))
# amind.adult<-which(names(fc)%in%c("amind.lessHS", "amind.unemp", "amind.emp"))
# blk.child<-which(names(fc)%in%c("blk.child", "blk.child.pov", "blk.singpar"))
# blk.adult<-which(names(fc)%in%c("blk.lesSHS", "blk.unemp", "blk.emp"))

create.prior<-function(x){
  ##function receives row,column pair to set prior based on strong observation years, compute mean and sd based on observed strong periods
  st<-fc[oi[x,1], "stname"]
  var<-oi[x,2]
  year<-fc[oi[x,1], "year"]
  ###set pop based on underlying pop of interest
  strong.2000<-fc[which((fc$year==2000)&(fc$stname==st)), var]
  strong.2009<-fc[which((fc$year==2009)&(fc$stname==st)), var]
  strong.set<-fc[which((fc$year%in%strong)&(fc$stname==st)), var]
  ###WEIGHT ON PROXIMITY TO OBS, with obs - Weight is 0.1 on obs, 0.75 on 2000 at 2001, linear to 0.15 at 2009
  pr.E<-((0.9-(year-2001)*0.12))*strong.2000+(1-(0.9-(year-2001)*0.12))*strong.2009
  all.set<-fc[fc$stname==st,var]
  pr.SD<-1/2*(max(all.set)-min(all.set))##as invese pop penalized - heteroskedastic error
  return(c(pr.E, pr.SD))
}

for(i in 1:nrow(oi.priors)){
  oi.priors[i,3:4]<-create.prior(i)
}

fc<-left_join(fc, arrest, by=c("state", "year"))

##imputation model
m<-15
fc.imp<-amelia(fc, m=m,
               ts="year", cs="stname", polytime=1, 
               bounds=bounds, overimp=oi,  
               priors=oi.priors, p2s=1,
               idvars=c("state", "statename", "St", "adult", "w.adult", "b.adult", "a.adult", "l.adult",
                        "pctblk1930", "pctimm1930", "pctami1930", "boarding.n", "board", "incartot"))

for(i in 1:m){
  fc.imp$imputations[[i]]<-
    fc.imp$imputations[[i]]%>%mutate(obs=1:nrow(fc),
                                     cl.wht.pc=cl.white/wht.child, 
                                     cl.blk.pc=cl.blk/blk.child, 
                                     cl.amind.pc=cl.nat.am/amind.child, 
                                     ent.wht.pc=ent.white/wht.child, 
                                     ent.blk.pc=ent.blk/blk.child,
                                     ent.amind.pc=ent.nat.am/amind.child, 
                                     chpov.wht.pc=wht.child.pov/wht.child, 
                                     chpov.blk.pc=blk.child.pov/blk.child,
                                     chpov.amind.pc=amind.child.pov/amind.child, 
                                     chpovrt=child.pov/child, 
                                     pctblk=blk/tot,
                                     pctami=amind/tot,
                                     pctwht=wht/tot,
                                     incarrt=incartot/adult,
                                     incarrt.m=incar.m/(adult-f),
                                     incarrt.f=incar.f/f,
                                     b.incarrt=(BLACKM+BLACKF)/b.adult,
                                     b.m.incarrt=(BLACKM)/(b.adult-blk.f),
                                     b.f.incarrt=(BLACKF)/blk.f,
                                     a.incarrt=(AIANM+AIANF)/(a.adult),
                                     a.m.incarrt=(AIANM)/(a.adult-amind.f),
                                     a.f.incarrt=(AIANF)/amind.f,
                                     w.incarrt=(WHITEM+WHITEF)/w.adult,
                                     b.incardisp=b.incarrt/w.incarrt,
                                     a.incardisp=a.incarrt/w.incarrt,
                                     bdisp.chpov=(blk.child.pov/blk.child)/(wht.child.pov/wht.child),
                                     adisp.chpov=(chpov.amind.pc/chpov.wht.pc),
                                     w.unemp.rt=wht.unemp/(wht.emp+wht.unemp),
                                     b.unemp.rt=blk.unemp/(blk.emp+blk.unemp),
                                     a.unemp.rt=amind.unemp/(amind.unemp+amind.emp),
                                     unemp.rt=unemp/(unemp+emp),
                                     w.singpar.rt=wht.singpar/wht.child,
                                     b.singpar.rt=blk.singpar/blk.child,
                                     a.singpar.rt=amind.singpar/amind.child,
                                     singpar.rt=singpar/child,
                                     bw.disp=cl.blk.pc/cl.wht.pc,
                                     ami.disp=ifelse((cl.amind.pc/cl.wht.pc)>0,cl.amind.pc/cl.wht.pc, 0),
                                     year.c=year-2000,
                                     tanf.adeq=AFDC.TANF.Benefit.for.3.person.family,
                                     tanf.incl=AFDC.TANF.Recipients/(child.pov/child),
                                     snap.incl=Food.Stamp.SNAP.Recipients/(child.pov/child),
                                     medicaid.incl=Medicaid.beneficiaries/(child.pov/child),
                                     b.arrest.pc=b.arrest/b.adult,
                                     w.arrest.pc=w.arrest/w.adult,
                                     ai.arrest.pc=ai.arrest/a.adult,
                                     b.arrest.disp=b.arrest.pc/w.arrest.pc,
                                     ai.arrest.disp=ai.arrest.pc/b.arrest.pc
                                     )
  fc.imp$imputations[[i]]$cl.blk<-as.integer(fc.imp$imputations[[i]]$cl.blk)
  fc.imp$imputations[[i]]$cl.nat.am<-as.integer(fc.imp$imputations[[i]]$cl.nat.am)
  fc.imp$imputations[[i]]$ent.blk<-as.integer(fc.imp$imputations[[i]]$ent.blk)
  fc.imp$imputations[[i]]$ent.nat.am<-as.integer(fc.imp$imputations[[i]]$ent.nat.am)
  fc.imp$imputations[[i]]$reun.nat.am<-as.integer(fc.imp$imputations[[i]]$reun.nat.am)
  fc.imp$imputations[[i]]$reun.blk<-as.integer(fc.imp$imputations[[i]]$reun.blk)
  fc.imp$imputations[[i]]$grp.inst.blk<-as.integer(fc.imp$imputations[[i]]$grp.inst.nat.am)
  fc.imp$imputations[[i]]$grp.inst.nat.am<-as.integer(fc.imp$imputations[[i]]$grp.inst.blk)
  fc.imp$imputations[[i]]$grp.inst.white<-as.integer(fc.imp$imputations[[i]]$grp.inst.white)
  fc.imp$imputations[[i]]$ent.white<-as.integer(fc.imp$imputations[[i]]$ent.white)
  fc.imp$imputations[[i]]$cl.white<-as.integer(fc.imp$imputations[[i]]$cl.white)
  fc.imp$imputations[[i]]$reun.white<-as.integer(fc.imp$imputations[[i]]$reun.white)
  
}


###lags and leads
for(i in 1:m){
  fc.imp$imputations[[i]]<-fc.imp$imputations[[i]]%>%group_by(stname)%>%arrange(year.c)%>%
    mutate(incarrt.lag=lag(incarrt), incarrt.m.lag=lag(incarrt.m), incarrt.f.lag=lag(incarrt.f),  
           b.incarrt.lag=lag(b.incarrt), a.incarrt.lag=lag(a.incarrt), w.incarrt.lag=lag(w.incarrt),
           b.m.incarrt.lag=lag(b.m.incarrt), b.f.incarrt.lag=lag(b.f.incarrt),
           a.m.incarrt.lag=lag(a.m.incarrt), a.f.incarrt.lag=lag(a.f.incarrt),
           cl.blk.lag=lag(cl.blk), cl.nat.am.lag=lag(cl.nat.am),
           clrt.blk.lag=lag(cl.blk)/lag(blk.child), clrt.nat.am.lag=lag(cl.nat.am)/lag(amind.child),
           incarrt.lead=lead(incarrt), b.incarrt.lead=lead(b.incarrt), a.incarrt.lead=lead(a.incarrt),
           b.m.incarrt.lead=lead(b.m.incarrt), b.f.incarrt.lead=lead(b.m.incarrt),
           a.m.incarrt.lead=lead(a.m.incarrt), a.f.incarrt.lead=lead(a.f.incarrt),
           cl.blk.lead=lead(cl.blk), cl.nat.am.lead=lead(cl.nat.am),
           clrt.blk.lead=lead(cl.blk)/lead(blk.child), cl.nat.am.lead=lead(cl.nat.am)/lead(amind.child),
           clrt.blk.lag2=lag(cl.blk.pc, n=2), clrt.nat.am.lag2=lag(cl.amind.pc, n=2))
}