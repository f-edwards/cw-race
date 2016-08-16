source("~/Dropbox/cw-race/models.R",)

library(arm)


extractSim<-function(x){
	e<-mean(x)
	upper<-quantile(x, 0.975)
	lower<-quantile(x, 0.025)
	return(c(e, upper, lower))
}

nsims<-1000

b.count.sim<-matrix(nrow=nsims*m, ncol=nrow(b.c.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(b.ineq[[i]], nsims))
	b.count.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

a.count.sim<-matrix(nrow=nsims*m, ncol=nrow(a.c.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.ineq[[i]], nsims))
	a.count.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

l.count.sim<-matrix(nrow=nsims*m, ncol=nrow(l.c.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(l.ineq[[i]], nsims))
	l.count.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

b.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(b.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(b.disp[[i]], nsims))
	b.disp.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

a.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.c.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.disp[[i]], nsims))
	a.disp.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

l.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.c.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(l.disp[[i]], nsims))
	l.disp.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}


ncount<-1000
counter<-matrix(nrow=ncount, ncol=11)

##from 5th-95th quantile of observed
counter[,1]<-1
counter[,3:11]<-0
q.seq<-seq(from=0.05, to=0.95, length.out=ncount)

sim.bc<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
bi.scale<-na.omit(scale(fc.ineq$b.incarrt))
counter[,2]<-quantile(bi.scale, q.seq)

for(i in 1:ncount){
	sim.bc[i,1:3]<-extractSim(exp(b.count.sim%*%counter[i,]))
	sim.bc[i,4]<-counter[i,2]
	sim.bc[i,5]<-q.seq[i]
}

sim.ac<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ai.scale<-na.omit(scale(fc.ineq$a.incarrt))
counter[,2]<-quantile(ai.scale, q.seq)
for(i in 1:ncount){
	sim.ac[i,1:3]<-extractSim(exp(a.count.sim%*%counter[i,]))
	sim.ac[i,4]<-counter[i,2]
	sim.ac[i,5]<-q.seq[i]
}

sim.lc<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
li.scale<-na.omit(scale(fc.ineq$l.incarrt))
counter[,2]<-quantile(li.scale, q.seq)
for(i in 1:ncount){
	sim.lc[i,1:3]<-extractSim(exp(l.count.sim%*%counter[i,]))
	sim.lc[i,4]<-counter[i,2]
	sim.lc[i,5]<-q.seq[i]
}

sim.bd<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
bd.scale<-na.omit(scale(fc.ineq$b.incardisp))
counter[,2]<-quantile(bd.scale, q.seq)
for(i in 1:ncount){
	sim.bd[i,1:3]<-extractSim(exp(b.disp.sim%*%counter[i,]))
	sim.bd[i,4]<-counter[i,2]
	sim.bd[i,5]<-q.seq[i]
}

sim.ad<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ad.scale<-na.omit(scale(fc.ineq$a.incardisp))
counter[,2]<-quantile(ad.scale, q.seq)
for(i in 1:ncount){
	sim.ad[i,1:3]<-extractSim(exp(a.disp.sim%*%counter[i,]))
	sim.ad[i,4]<-counter[i,2]
	sim.ad[i,5]<-q.seq[i]
}

sim.ld<-data.frame("e"=rep(NA, ncount), "upper"=rep(NA, ncount), "lower"=rep(NA, ncount), "x"=rep(NA, ncount), "p"=rep(NA, ncount))
ld.scale<-na.omit(scale(fc.ineq$l.incardisp))
counter[,2]<-quantile(ld.scale, q.seq)
for(i in 1:ncount){
	sim.ld[i,1:3]<-extractSim(exp(l.disp.sim%*%counter[i,]))
	sim.ld[i,4]<-counter[i,2]
	sim.ld[i,5]<-q.seq[i]
}

sim.bc$m<-"African American"
sim.ac$m<-"Native American"
sim.lc$m<-"Latino"
sim.count<-rbind(sim.bc, sim.ac, sim.lc)
sim.count[,c(1:3, 5)]<-sim.count[,c(1:3,5)]*100

count.plot<-ggplot(as.data.frame(sim.count), aes(y=e, x=p))+geom_line()+
	geom_line(aes(y=upper, x=p), lty=2)+
	geom_line(aes(y=lower, x=p), lty=2)+
  geom_line(aes(y=mean(fc.ineq$cl.white/fc.ineq$wht.child)*100, x=p), lty=3)+
	xlab("Incarceration rate, percentiles of observed data")+
	ylab("Percent of children in foster care")+
  coord_cartesian(ylim=c(0, 10))+
	facet_wrap(~m)+
	theme_bw()

ggsave("count-sim-plot.pdf", count.plot, width=9, height=3)

sim.bd$m<-"African American"
sim.ad$m<-"Native American"
sim.ld$m<-"Latino"
sim.disp<-rbind(sim.bd, sim.ad, sim.ld)
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

ggsave("disp-sim-plot.pdf", disp.plot, width=9, height=3)

###point estimates

rbind(sim.bc[500,], sim.bc[223,], sim.bc[778,])
quantile(fc.ineq$b.incarrt, c(0.25, .5, 0.75), na.rm=TRUE)
rbind(sim.ac[500,], sim.ac[223,], sim.ac[778,])
quantile(fc.ineq$a.incarrt, c(0.25, .5, 0.75), na.rm=TRUE)
rbind(sim.lc[500,], sim.lc[223,], sim.lc[778,])
quantile(fc.ineq$l.incarrt, c(0.25, .5, 0.75), na.rm=TRUE)

rbind(sim.bd[500,], sim.bd[223,], sim.bd[778,])
quantile(fc.ineq$b.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)
rbind(sim.ad[500,], sim.ad[223,], sim.ad[778,])
quantile(fc.ineq$a.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)
rbind(sim.ld[500,], sim.ld[223,], sim.ld[778,])
quantile(fc.ineq$l.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)
