library(arm)
set.seed(1)


extractSim<-function(x){
	e<-mean(x)
	upper<-quantile(x, 0.975)
	lower<-quantile(x, 0.025)
	return(c(e, upper, lower))
}

nsims<-100

b.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(b.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(b.disp[[i]], nsims))
	b.disp.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

a.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.disp[[i]], nsims))
	a.disp.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

ncount<-1000
counterb<-matrix(nrow=ncount, ncol=10)

##from 5th-95th quantile of observed
counterb[,1]<-1
counterb[,3]<-log(median(fc.imp$imputations[[1]]$bdisp.chpov))
counterb[,4]<-log(median(fc.imp$imputations[[1]]$b.unemp.rt/fc.imp$imputations[[1]]$w.unemp.rt))
counterb[,5]<-log(median(fc.imp$imputations[[1]]$b.singpar.rt/fc.imp$imputations[[1]]$w.singpar.rt))
counterb[,6]<-log(median(fc.imp$imputations[[1]]$blk.lessHS/fc.imp$imputations[[1]]$wht.lessHS))
counterb[,7]<-log(median(fc.imp$imputations[[1]]$pctblk))
counterb[,8]<-0
counterb[,9]<-log(median(fc$v.crime.rt))
counterb[,10]<-0


q.seq<-seq(from=0.05, to=0.95, length.out=ncount)

sim.bd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
bd.scale<-log(fc.imp$imputations[[1]]$b.incardisp)
counterb[,2]<-quantile(bd.scale, q.seq)
for(i in 1:ncount){
	sim.bd[i,1:3]<-extractSim(exp(b.disp.sim%*%counterb[i,]))
	sim.bd[i,4]<-exp(counterb[i,2])
	sim.bd[i,5]<-q.seq[i]
}

countera<-matrix(nrow=ncount, ncol=10)
##from 5th-95th quantile of observed
countera[,1]<-1
countera[,3]<-log(median(fc.imp$imputations[[1]]$adisp.chpov))
countera[,4]<-log(median(fc.imp$imputations[[1]]$a.unemp.rt/fc.imp$imputations[[1]]$w.unemp.rt))
countera[,5]<-log(median(fc.imp$imputations[[1]]$a.singpar.rt/fc.imp$imputations[[1]]$w.singpar.rt))
countera[,6]<-log(median(fc.imp$imputations[[1]]$amind.lessHS/fc.imp$imputations[[1]]$wht.lessHS))
countera[,7]<-log(median(fc.imp$imputations[[1]]$pctami))
countera[,8]<-0
countera[,9]<-log(median(fc$v.crime.rt))
countera[,10]<-0

q.seq<-seq(from=0.05, to=0.95, length.out=ncount)
sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ad.scale<-sqrt(fc.imp$imputations[[1]]$a.incardisp)
countera[,2]<-quantile(ad.scale, q.seq)


sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
	sim.ad[i,1:3]<-extractSim((a.disp.sim%*%countera[i,])^2)
	sim.ad[i,4]<-countera[i,2]^2
	sim.ad[i,5]<-q.seq[i]
}

sim.bd$m<-"African American"
sim.ad$m<-"Native American"
sim.disp<-rbind(sim.bd, sim.ad)
sim.disp[, 5]<-sim.disp[,5]*100


disp.plot<-ggplot(as.data.frame(sim.disp), aes(y=e, x=p))+geom_line()+
	geom_line(aes(y=upper, x=p), lty=2)+
	geom_line(aes(y=lower, x=p), lty=2)+
	geom_line(aes(y=1, x=p), lty=3)+
	xlab("Incarceration disproportion, percentiles of observed data")+
	ylab("Foster care caseload disproportion")+
  coord_cartesian(ylim=c(0.5, 10))+
  facet_wrap(~m)+
	theme_bw()

ggsave("cl-sim-plot.pdf", disp.plot, width=7, height=3)

###point estimates
sink("exp-out.txt")
print("RE Sims")
rbind(sim.bd[500,], sim.bd[223,], sim.bd[778,])
rbind(sim.ad[500,], sim.ad[223,], sim.ad[778,])
sink()
####################################################
#############ENTRIES

b.ent.sim<-matrix(nrow=nsims*m, ncol=nrow(b.ent.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(b.ent.disp[[i]], nsims))
  b.ent.sim[(((i-1)*100)+1):(i*100),]<-s.temp
}

a.ent.sim<-matrix(nrow=nsims*m, ncol=nrow(a.ent.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(a.ent.disp[[i]], nsims))
  a.ent.sim[(((i-1)*100)+1):(i*100),]<-s.temp
}

sim.b.ent<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.b.ent[i,1:3]<-extractSim(exp(b.ent.sim%*%counterb[i,]))
  sim.b.ent[i,4]<-exp(counterb[i,2])
  sim.b.ent[i,5]<-q.seq[i]
}


sim.a.ent<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.a.ent[i,1:3]<-extractSim((a.ent.sim%*%countera[i,])^2)
  sim.a.ent[i,4]<-countera[i,2]^2
  sim.a.ent[i,5]<-q.seq[i]
}

sim.b.ent$m<-"African American"
sim.a.ent$m<-"Native American"
sim.ent<-rbind(sim.b.ent, sim.a.ent)
sim.ent[, 5]<-sim.ent[,5]*100

ent.plot<-ggplot(as.data.frame(sim.ent), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  geom_line(aes(y=1, x=p), lty=3)+
  xlab("Incarceration disproportion, percentiles of observed data")+
  ylab("Foster care entry disproportion")+
  coord_cartesian(ylim=c(0.5, 10))+
  facet_wrap(~m)+
  theme_bw()

ggsave("entry-sim-plot.pdf", ent.plot, width=7, height=3)


####################################################
#############REUNIFICATION EXITS

b.reun.sim<-matrix(nrow=nsims*m, ncol=nrow(b.reun.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(b.reun[[i]], nsims))
  b.reun.sim[(((i-1)*100)+1):(i*100),]<-s.temp
}

a.reun.sim<-matrix(nrow=nsims*m, ncol=nrow(n.reun.tab)-1)
for(i in 1:m){
  s.temp<-fixef(sim(a.reun[[i]], nsims))
  a.reun.sim[(((i-1)*100)+1):(i*100),]<-s.temp
}

sim.b.reun<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.b.reun[i,1:3]<-extractSim(exp(b.reun.sim%*%counterb[i,]))
  sim.b.reun[i,4]<-exp(counterb[i,2])
  sim.b.reun[i,5]<-q.seq[i]
}


sim.a.reun<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
for(i in 1:ncount){
  sim.a.reun[i,1:3]<-extractSim((a.reun.sim%*%countera[i,])^2)
  sim.a.reun[i,4]<-countera[i,2]^2
  sim.a.reun[i,5]<-q.seq[i]
}

sim.b.reun$m<-"African American"
sim.a.reun$m<-"Native American"
sim.reun<-rbind(sim.b.reun, sim.a.reun)
sim.reun[, 5]<-sim.reun[,5]*100

reun.plot<-ggplot(as.data.frame(sim.reun), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  geom_line(aes(y=1, x=p), lty=3)+
  xlab("Incarceration disproportion, percentiles of observed data")+
  ylab("Reunification rate relative to White")+
  coord_cartesian(ylim=c(0.5, 1))+
  facet_wrap(~m)+
  theme_bw()

ggsave("reun-sim-plot.pdf", reun.plot, width=7, height=3)








# rbind(sim.ld[500,], sim.ld[223,], sim.ld[778,])
# quantile(fc$l.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)

# ### FE Expected values
# "FE Expected Values for 10 per 1,000 increase in incar from mean"
# 
# beta.b<-FE.models$b$merge[[1]][1,1]
# se.b<-FE.models$b$merge[[1]][1,2]
# beta.CI<-c(beta.b, beta.b+1.96*se.b, beta.b-1.96*se.b)
# 
# median.b<-median(fc.imp$imputations[[1]]$b.incarrt)
# within.bincar.sd<-mean(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd(b.incarrt)))[,2])
# delta.b<-median.b+0.005
# FC.0<-exp(beta.CI*log(median.b))
# FC.1<-exp(beta.CI*log(delta.b))
# delta.FCb<-FC.1-FC.0
# pct.FCb<-FC.1/FC.0
# print(c("Black FC Caseload pc", "For within state 0.005 increase (5 per 1,000), within state sd", within.bincar.sd, "Over median", median.b,
#         "Caseload PC increase", delta.FCb, "Caseload pct increase", pct.FCb))
# 
# beta.a<-FE.models$a$merge[[1]][1,1]
# se.a<-FE.models$a$merge[[1]][1,2]
# beta.CI<-c(beta.a, beta.a+1.96*se.a, beta.a-1.96*se.a)
# 
# median.a<-median(fc.imp$imputations[[1]]$a.incarrt)
# within.aincar.sd<-mean(as.data.frame(fc.imp$imputations[[1]]%>%group_by(stname)%>%summarise(sd(a.incarrt)))[,2])
# delta.a<-median.a+0.005
# FC.0<-exp(beta.CI*log(median.a))
# FC.1<-exp(beta.CI*log(delta.a))
# delta.FCa<-FC.1-FC.0
# pct.FCa<-FC.1/FC.0
# print(c("NatAM FC Caseload pc", "For within state 0.005 increase (5 per 1,000), within state sd", within.aincar.sd, "Over median", median.a,
#         "Caseload PC increase", delta.FCa, "Caseload pct increase", pct.FCa))
# sink()