library(arm)
set.seed(1)


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
	b.disp.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
}

a.disp.sim<-matrix(nrow=nsims*m, ncol=nrow(a.d.tab)-1)
for(i in 1:m){
	s.temp<-fixef(sim(a.disp[[i]], nsims))
	a.disp.sim[(((i-1)*1000)+1):(i*1000),]<-s.temp
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
	sim.bd[i,4]<-counterb[i,2]
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
ad.scale<-sqrt(fc.imp$imputations[[1]]$b.incardisp)
counter[,2]<-quantile(ad.scale, q.seq)
for(i in 1:ncount){
	sim.ad[i,1:3]<-extractSim((a.disp.sim%*%countera[i,])^2)
	sim.ad[i,4]<-countera[i,2]
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

ggsave("disp-sim-plot.pdf", disp.plot, width=9, height=3)

###point estimates
rbind(sim.bd[500,], sim.bd[223,], sim.bd[778,])
quantile(fc.imp$imputations[[1]]$b.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)
rbind(sim.ad[500,], sim.ad[223,], sim.ad[778,])
quantile(fc.imp$imputations[[1]]$a.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)
# rbind(sim.ld[500,], sim.ld[223,], sim.ld[778,])
# quantile(fc$l.incardisp, c(0.25, .5, 0.75), na.rm=TRUE)

### FE Expected values
