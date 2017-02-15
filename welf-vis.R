#### visualizations of welfare regime types
dat<-fc.imp$imputations[[1]]
dat14<-dat%>%filter(year==2014)

par(mfrow=c(2,2))

plot(log(dat14$tanf.adeq), log(dat14$tanf.incl), pch=" ", xlab="TANF benefits (adjusted)", ylab="TANF enrolled/child pov (inclusion)")
text(jitter(log(dat14$tanf.adeq)), jitter(log(dat14$tanf.incl)), dat14$stname)

plot(log(dat14$snap.incl), log(dat14$tanf.incl), pch=" ", xlab="TANF inclusion", ylab="SNAP inclusion")
text(jitter(log(dat14$snap.incl)), jitter(log(dat14$tanf.incl)), dat14$stname)

plot(log(dat14$medicaid.incl), log(dat14$tanf.incl), pch=" ", xlab="Medicaid inclusion", ylab="TANF inclusion")
text(jitter(log(dat14$medicaid.incl)), jitter(log(dat14$tanf.incl)), dat14$stname)

plot(log(dat14$medicaid.incl), log(dat14$snap.incl), pch=" ", xlab="Medicaid inclusion", ylab="TANF inclusion")
text(jitter(log(dat14$medicaid.incl)), jitter(log(dat14$snap.incl)), dat14$stname)

