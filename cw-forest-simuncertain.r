
### For forest plot of results
### ent and inst models
### Depends on model output from FCmodels.r in working environment (objects ent.results, inst.results)
library(ggplot2)
library(dplyr)
source("~/sync/cw-race/sim.R")

###sim from posterior - pull results from sim.R
### Count models

forest.est<-data.frame("var"=rep(c("Intercept", "Incarceration rate", "Unemployment rate", "Single parent rate",
                                 "Adults w/o HS rate", "Child poverty rate", "Percent population",
                                 "Welfare enrollment per child poverty",
                                 "Leg. ideology", "Violent crime rate", "Year"),2),
                       "Race"=c(rep("African American", 11), rep("Native American",11)), 
                       "beta"=c(colMeans(b.count.sim), colMeans(a.count.sim)), 
                       "upper"=c(apply(b.count.sim, 2, FUN = function(x){quantile(x, 0.975)}), 
                                 apply(a.count.sim, 2, FUN = function(x){quantile(x, 0.975)})), 
                       "lower"=c(apply(b.count.sim, 2, FUN = function(x){quantile(x, 0.025)}),
                                 apply(a.count.sim, 2, FUN = function(x){quantile(x, 0.025)})))

forest.est<-forest.est%>%filter(var!="Intercept")

# 
forest.est$var<-factor(forest.est$var)
forest.est$var<-factor(forest.est$var, levels(forest.est$var)[c(10, 8, 4,5, 9, 2, 1, 6, 7, 3)])

# forest.est$varname<-factor(forest.est$varname, levels(forest.est$varname)[c(1:9, 18,10:14, 15:17)])

ggplot(data=forest.est, aes(x=beta,y=var))+
  geom_point()+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  guides(colour=FALSE)+
  facet_wrap(~Race)+
  ggsave(file="ForestCount.pdf", w=9, h=6)

### Disp models

forest.est<-data.frame("var"=rep(c("Intercept", "Incarceration disproportion", "Child poverty disproportion",
                                   "Unemployment disproportion", "Single parent disproportion",
                                   "Adults w/o HS disproportion",  
                                   "Welfare per child poverty disproportion",
                                   "Percent population",
                                   "Leg. ideology", "Violent crime rate", "Year"),2),
                       "Race"=c(rep("African American/White", 11), rep("Native American/White",11)), 
                       "beta"=c(colMeans(b.disp.sim), colMeans(a.disp.sim)), 
                       "upper"=c(apply(b.disp.sim, 2, FUN = function(x){quantile(x, 0.975)}), 
                                 apply(a.disp.sim, 2, FUN = function(x){quantile(x, 0.975)})), 
                       "lower"=c(apply(b.disp.sim, 2, FUN = function(x){quantile(x, 0.025)}),
                                 apply(a.disp.sim, 2, FUN = function(x){quantile(x, 0.025)})))

forest.est<-forest.est%>%filter(var!="Intercept")

# 
forest.est$var<-factor(forest.est$var)
forest.est$var<-factor(forest.est$var, levels(forest.est$var)[c(10, 8, 4, 5, 9, 2, 1, 6, 7, 3)])

# forest.est$varname<-factor(forest.est$varname, levels(forest.est$varname)[c(1:9, 18,10:14, 15:17)])

ggplot(data=forest.est, aes(x=beta,y=var))+
  geom_point()+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  guides(colour=FALSE)+
  facet_wrap(~Race)+
  ggsave(file="ForestDisp.pdf", w=9, h=6)