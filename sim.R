library(arm)
set.seed(1)
setwd("~/sync/cw-race/figures/")

extractSim<-function(x){
	e<-mean(x)
	upper<-quantile(x, 0.975)
	lower<-quantile(x, 0.025)
	return(c(e, upper, lower))
}

nsims<-1000

b.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(b.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(b.disp[[i]], nsims))
	b.disp.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

b.disp.welf.sim<-matrix(nrow=nsims*m, ncol=nrow(b.d.welf.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(b.disp.welf[[i]], nsims))
  b.disp.welf.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

a.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.disp[[i]], nsims))
	a.disp.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

a.disp.welf.sim<-matrix(nrow=nsims*m, ncol=nrow(a.d.welf.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(a.disp.welf[[i]], nsims))
  a.disp.welf.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

ncount<-1000
counterb<-matrix(nrow=ncount, ncol=length(fixef(b.disp[[1]])))

##from 5th-95th quantile of observed
counterb[,1]<-1
counterb[,2:length(fixef(b.disp[[1]]))]<-0

q.seq<-seq(from=0.001, to=0.99, length.out=ncount)

sim.bd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
sim.bd.new<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))

bd.scale<-scale(fc.imp$imputations[[1]]$b.incardisp)
tanf.adeq.scale<-scale(fc.imp$imputations[[1]]$tanf.adeq)
tanf.incl.scale<-scale(fc.imp$imputations[[1]]$tanf.incl)
counterb.new<-counterb
counterb[,2]<-quantile(bd.scale, q.seq)
counterb.new[,11]<-quantile(tanf.adeq.scale, q.seq)
counterb.new[,12]<-quantile(tanf.incl.scale, q.seq)
for(i in 1:ncount){
	sim.bd[i,1:3]<-extractSim(exp(b.disp.sim%*%counterb[i,]))
	sim.bd[i,4]<-mean(fc.imp$imputations[[1]]$b.incardisp)+
	                    sd(fc.imp$imputations[[1]]$b.incardisp)*
	                         (counterb[i,2])
	sim.bd[i,5]<-q.seq[i]
	sim.bd.new[i, 1:3]<-extractSim(exp(b.disp.sim%*%counterb.new[i,]))
	sim.bd.new[i, 5]<-q.seq[i]
}

#### for quadratic models
mod.names<-all.vars(summary(b.disp.welf[[1]])$call[[2]])
mod.names<-mod.names[-1]
counterb2<-as.data.frame(matrix(0, ncol=length(mod.names), nrow=ncount))
names(counterb2)<-mod.names


### set counterfact year at 2014
### set all vars at median
for(i in 1:length(mod.names)){
  var<-mod.names[i]
  set.var<-median(unlist(fc.imp$imputations[[1]][, var]))
  counterb2[,var]<-set.var
}

counterb2$year.c<-14
### set counterfact state at WA
counterb2$state<-53

tanf.adeq.scale<-scale(fc.imp$imputations[[1]]$tanf.adeq)

counterb2<-matrix(nrow=ncount, ncol=length(fixef(b.disp.welf[[1]])))

##from 5th-95th quantile of observed
counterb2[,1]<-1
counterb2[,2:length(fixef(b.disp.welf[[1]]))]<-0

index.adeq<-which(row.names(b.d.welf.tab)=="scale(tanf.adeq)")
index.adeq2<-which(row.names(b.d.welf.tab)=="scale(I(tanf.adeq)^2)")

index.incl<-which(row.names(b.d.welf.tab)=="scale(tanf.incl)")
index.incl2<-which(row.names(b.d.welf.tab)=="scale(I(tanf.incl)^2)")

counterb2[, index.adeq]<-quantile(scale(fc.imp$imputations[[1]]$tanf.adeq), q.seq)
counterb2[, index.adeq2]<-quantile(scale(fc.imp$imputations[[1]]$tanf.adeq^2), q.seq)

counterb2[, index.incl]<-quantile(scale(fc.imp$imputations[[1]]$tanf.incl), q.seq)
counterb2[, index.incl2]<-quantile(scale(fc.imp$imputations[[1]]$tanf.incl^2), q.seq)

# p.out<-data.frame(bw.disp=rep(NA, ncount), tanf.adeq=rep(NA, ncount), tanf.incl=rep(NA, ncount))
# for(i in 1:ncount){
#   p.out$bw.disp[i]<-exp(predict(b.disp.welf[[1]], newdata=counterb2[i,]))
#   p.out$tanf.adeq[i]<-counterb2[i,"tanf.adeq"]
#   p.out$tanf.incl[i]<-counterb2[i,"tanf.incl"]
# }

sim.bd.welf<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "p"=rep(NA, ncount))

for(i in 1:ncount){
  sim.bd.welf[i,1:3]<-extractSim(exp(b.disp.welf.sim%*%counterb2[i,]))
  sim.bd.welf[i,4]<-q.seq[i]
}

sim.bd.welf$tanf.adeq<-quantile(fc.imp$imputations[[1]]$tanf.adeq, q.seq)
sim.bd.welf$tanf.incl<-quantile(fc.imp$imputations[[1]]$tanf.incl, q.seq)

b.welf.plot<-ggplot(as.data.frame(sim.bd.welf), aes(x=p))+geom_line(aes(y=e))+
  geom_line(aes(y=upper), lty=2)+
  geom_line(aes(y=lower), lty=2)+
  xlab("TANF inclusion, percentiles of observed data")+
  ylab("Disparity")+
  # coord_cartesian(ylim=c(1, 8))+
  # facet_wrap(~m)+
  theme_bw()


# sim.pd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
# bd.scale<-scale(fc.imp$imputations[[1]]$bdisp.chpov)
# counterb[,2]<-q.seq
# for(i in 1:ncount){
#   sim.pd[i,1:3]<-extractSim(exp(b.disp.sim%*%counterb[i,]))
#   sim.pd[i,4]<-mean(fc.imp$imputations[[1]]$bdisp.chpov)+
#     sd(fc.imp$imputations[[1]]$bdisp.chpov)*
#     (counterb[i,2])
#   sim.pd[i,5]<-q.seq[i]
# }
# sim.pd$m<-"ChPov"

countera<-matrix(nrow=ncount, ncol=length(fixef(a.disp[[1]])))
##from 5th-95th quantile of observed
countera[,1]<-1
countera[,2:length(fixef(a.disp[[1]]))]<-0

sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ad.scale<-scale(fc.imp$imputations[[1]]$a.incardisp)
countera[,2]<-quantile(ad.scale, q.seq)


sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
	sim.ad[i,1:3]<-extractSim((a.disp.sim%*%countera[i,])^2)
	sim.ad[i,4]<-mean(fc.imp$imputations[[1]]$a.incardisp)+
	  sd(fc.imp$imputations[[1]]$a.incardisp)*countera[i,2]
	sim.ad[i,5]<-q.seq[i]
}

####################################################
#############ENTRIES

b.ent.sim<-matrix(nrow=nsims*m, ncol=length(fixef(b.ent.disp[[1]])))
for(i in 1:m){
  s.temp<-fixef(sim(b.ent.disp[[i]], nsims))
  b.ent.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

a.ent.sim<-matrix(nrow=nsims*m, ncol=nrow(a.ent.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(a.ent.disp[[i]], nsims))
  a.ent.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

sim.b.ent<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.b.ent[i,1:3]<-extractSim(exp(b.ent.sim%*%counterb[i,]))
  sim.b.ent[i,4]<-mean(fc.imp$imputations[[1]]$b.incardisp)+
    sd(fc.imp$imputations[[1]]$b.incardisp)*counterb[i,2]
  sim.b.ent[i,5]<-q.seq[i]
}


sim.a.ent<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.a.ent[i,1:3]<-extractSim((a.ent.sim%*%countera[i,])^2)
  sim.a.ent[i,4]<-mean(fc.imp$imputations[[1]]$a.incardisp)+
    sd(fc.imp$imputations[[1]]$a.incardisp)*countera[i,2]
  sim.a.ent[i,5]<-q.seq[i]
}


####################################################
#############REUNIFICATION EXITS

b.reun.sim<-matrix(nrow=nsims*m, ncol=length(fixef(b.reun[[1]])))
for(i in 1:m){
  s.temp<-fixef(sim(b.reun[[i]], nsims))
  b.reun.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

a.reun.sim<-matrix(nrow=nsims*m, ncol=length(fixef(b.reun[[1]])))
for(i in 1:m){
  s.temp<-fixef(sim(a.reun[[i]], nsims))
  a.reun.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

sim.b.reun<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.b.reun[i,1:3]<-extractSim(exp(b.reun.sim%*%counterb[i,]))
  sim.b.reun[i,4]<-mean(fc.imp$imputations[[1]]$b.incardisp)+
    sd(fc.imp$imputations[[1]]$b.incardisp)*(counterb[i,2])
  sim.b.reun[i,5]<-q.seq[i]
}


sim.a.reun<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.a.reun[i,1:3]<-extractSim((a.reun.sim%*%countera[i,])^2)
  sim.a.reun[i,4]<-mean(fc.imp$imputations[[1]]$a.incardisp)+
    sd(fc.imp$imputations[[1]]$a.incardisp)*(countera[i,2])
  sim.a.reun[i,5]<-q.seq[i]
}

sim.bd$m<-"African American"
sim.ad$m<-"Native American"

sim.bd1<-sim.bd
sim.bd1$m<-"Caseloads"
sim.ad1<-sim.ad
sim.ad1$m<-"Caseloads"

sim.b.ent$m<-"African American"
sim.a.ent$m<-"Native American"

sim.b.ent1<-sim.b.ent
sim.a.ent1<-sim.a.ent
sim.b.ent1$m<-"Entries"
sim.a.ent1$m<-"Entries"

sim.ent<-rbind(sim.b.ent, sim.a.ent)

sim.b<-rbind(sim.bd1, sim.b.ent1)
sim.a<-rbind(sim.ad1, sim.a.ent1)

sim.ent<-rbind(sim.b.ent, sim.a.ent)
sim.cl<-rbind(sim.bd, sim.ad)

b.plot<-ggplot(as.data.frame(sim.b), aes(y=e, x=x))+geom_line()+
  geom_line(aes(y=upper, x=x), lty=2)+
  geom_line(aes(y=lower, x=x), lty=2)+
  xlab("Incarceration disparity, percentiles of observed data")+
  ylab("Disparity")+
  coord_cartesian(ylim=c(1, 8))+
  facet_wrap(~m)+
  theme_bw()

a.plot<-ggplot(as.data.frame(sim.a), aes(y=e, x=x))+geom_line()+
  geom_line(aes(y=upper, x=x), lty=2)+
  geom_line(aes(y=lower, x=x), lty=2)+
  xlab("Incarceration disparity, percentiles of observed data")+
  ylab("Disparity")+
  coord_cartesian(ylim=c(1, 8))+
  facet_wrap(~m)+
  theme_bw()
# calculate sd for observed data, exclude imputations, they're extreme

ggsave("b-sim-plot.pdf", b.plot, width=6, height=3)
ggsave("a-sim-plot.pdf", a.plot, width=6, height=3)


sim.b.reun$m<-"African American"
sim.a.reun$m<-"Native American"
sim.reun<-rbind(sim.b.reun, sim.a.reun)

reun.plot<-ggplot(as.data.frame(sim.reun), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  geom_line(aes(y=1, x=p), lty=3)+
  xlab("Incarceration disparity, percentiles of observed data")+
  ylab("Reunification disparity")+
  coord_cartesian(ylim=c(0.6, 1.05))+
  facet_wrap(~m)+
  theme_bw()

ggsave("reun-sim-plot.pdf", reun.plot, width=6, height=3)

cl.plot<-ggplot(as.data.frame(sim.cl), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  xlab("Incarceration disparity, percentiles of observed data")+
  ylab("Caseload disparity")+
  coord_cartesian(ylim=c(1,8))+
  facet_wrap(~m)+
  theme_bw()

ggsave("cl-sim-plot.pdf", cl.plot, width=6, height=3)


ent.plot<-ggplot(as.data.frame(sim.ent), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  xlab("Incarceration disparity, percentiles of observed data")+
  ylab("Entry disparity")+
  coord_cartesian(ylim=c(1,8))+
  facet_wrap(~m)+
  theme_bw()

ggsave("ent-sim-plot.pdf", ent.plot, width=6, height=3)


##point estimates
sink("exp-out.txt")
print("CL Sims")
rbind(sim.bd[500,], sim.bd[1,], sim.bd[1000,])
rbind(sim.ad[500,], sim.ad[1,], sim.ad[1000,])
print("Ent sims")
rbind(sim.b.ent[500,], sim.b.ent[1,], sim.b.ent[1000,])
rbind(sim.a.ent[500,], sim.a.ent[1,], sim.a.ent[1000,])
print("Reun sims")
rbind(sim.b.reun[500,], sim.b.reun[1,], sim.b.reun[1000,])
rbind(sim.a.reun[500,], sim.a.reun[1,], sim.a.reun[1000,])
sink()




