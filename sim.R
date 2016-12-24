library(arm)
set.seed(1)
setwd("U:/cw-race/figures/")

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

a.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.disp[[i]], nsims))
	a.disp.sim[(((i-1)*nsims)+1):(i*nsims),]<-s.temp
}

ncount<-1000
counterb<-matrix(nrow=ncount, ncol=length(fixef(b.disp[[1]])))

##from 5th-95th quantile of observed
counterb[,1]<-1
counterb[,2:length(fixef(b.disp[[1]]))]<-0

q.seq<-seq(from=-1, to=2, length.out=ncount)

sim.bd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
bd.scale<-scale(fc.imp$imputations[[1]]$b.incardisp)
counterb[,2]<-q.seq
for(i in 1:ncount){
	sim.bd[i,1:3]<-extractSim(exp(b.disp.sim%*%counterb[i,]))
	sim.bd[i,4]<-mean(fc.imp$imputations[[1]]$b.incardisp)+
	                    sd(fc.imp$imputations[[1]]$b.incardisp)*
	                         (counterb[i,2])
	sim.bd[i,5]<-q.seq[i]
}

sim.pd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
bd.scale<-scale(fc.imp$imputations[[1]]$bdisp.chpov)
counterb[,2]<-q.seq
for(i in 1:ncount){
  sim.pd[i,1:3]<-extractSim(exp(b.disp.sim%*%counterb[i,]))
  sim.pd[i,4]<-mean(fc.imp$imputations[[1]]$bdisp.chpov)+
    sd(fc.imp$imputations[[1]]$bdisp.chpov)*
    (counterb[i,2])
  sim.pd[i,5]<-q.seq[i]
}
sim.pd$m<-"ChPov"

countera<-matrix(nrow=ncount, ncol=length(fixef(a.disp[[1]])))
##from 5th-95th quantile of observed
countera[,1]<-1
countera[,2:length(fixef(a.disp[[1]]))]<-0

sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ad.scale<-scale(fc.imp$imputations[[1]]$a.incardisp)
countera[,2]<-q.seq


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

sim.bd$m<-"African American caseload"
sim.ad$m<-"Native American caseload"

sim.b.ent$m<-"African American entries"
sim.a.ent$m<-"Native American entris"
sim.ent<-rbind(sim.b.ent, sim.a.ent)

sim.b<-rbind(sim.bd, sim.b.ent)
sim.a<-rbind(sim.ad, sim.a.ent)


b.plot<-ggplot(as.data.frame(sim.b), aes(y=e, x=x))+geom_line()+
  geom_line(aes(y=upper, x=x), lty=2)+
  geom_line(aes(y=lower, x=x), lty=2)+
  xlab("Incarceration disparity")+
  ylab("Disparity")+
  coord_cartesian(ylim=c(1, 8))+
  facet_wrap(~m)+
  theme_bw()

a.plot<-ggplot(as.data.frame(sim.a), aes(y=e, x=x))+geom_line()+
  geom_line(aes(y=upper, x=x), lty=2)+
  geom_line(aes(y=lower, x=x), lty=2)+
  xlab("Incarceration disparity")+
  ylab("Disparity")+
  coord_cartesian(ylim=c(1, 8))+
  facet_wrap(~m)+
  theme_bw()
# calculate sd for observed data, exclude imputations, they're extreme

ggsave("b-sim-plot.pdf", b.plot, width=7, height=3)
ggsave("a-sim-plot.pdf", b.plot, width=7, height=3)


sim.b.reun$m<-"African American"
sim.a.reun$m<-"Native American"
sim.reun<-rbind(sim.b.reun, sim.a.reun)

reun.plot<-ggplot(as.data.frame(sim.reun), aes(y=e, x=p))+geom_line()+
  geom_line(aes(y=upper, x=p), lty=2)+
  geom_line(aes(y=lower, x=p), lty=2)+
  geom_line(aes(y=1, x=p), lty=3)+
  xlab("Incarceration disparity, SD units")+
  ylab("Reunification disparity")+
  coord_cartesian(ylim=c(0.6, 1.05))+
  facet_wrap(~m)+
  theme_bw()

ggsave("reun-sim-plot.pdf", reun.plot, width=7, height=3)

# ###point estimates
# sink("exp-out.txt")
# print("CL Sims")
# rbind(sim.bd[500,], sim.bd[223,], sim.bd[778,])
# rbind(sim.ad[500,], sim.ad[223,], sim.ad[778,])
# print("Ent sims")
# rbind(sim.b.ent[500,], sim.b.ent[223,], sim.b.ent[778,])
# rbind(sim.a.ent[500,], sim.a.ent[223,], sim.a.ent[778,])
# print("Reun sims")
# rbind(sim.b.reun[500,], sim.b.reun[223,], sim.b.reun[778,])
# rbind(sim.a.reun[500,], sim.a.reun[223,], sim.a.reun[778,])
# sink()




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