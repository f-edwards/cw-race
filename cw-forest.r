
### For forest plot of results
### ent and inst models
### Depends on model output from FCmodels.r in working environment (objects ent.results, inst.results)
library(ggplot2)
library(dplyr)
source("~/Dropbox/cw-race/sim.R")

###merged results from Rubin combination - alt using posterior sim pooling in cw-forest-simuncertainty.r
### Count models
outcomes<-3
predictors<-nrow(b.d.tab)-1
forest.est<-data.frame("var"=rep(c("Intercept", "Incarceration ineq.", "Unemployment ineq.", "Single parent ineq.",
                                 "Less than HS ineq.", "Child poverty ineq.", "Percent pop.",
                                 "Leg. ideology", "Violent crime rate", "Year"),outcomes*2),
                       "Race"=c(rep("African American", predictors*outcomes), rep("Native American",predictors*outcomes)), 
                       "Outcome"=c(rep("Afr Am Caseload ineq", predictors), rep("Afr Am Entry ineq", predictors), rep("Afr Am Reunification ineq", predictors), 
                                   rep("Native Am Caseload ineq", predictors), rep("Native Am Entry ineq", predictors), rep("Native Am Reunification ineq", predictors)),
                       "beta"=c(b.d.tab$Beta[1:predictors], b.ent.tab$Beta[1:predictors], b.reun.tab$Beta[1:predictors], 
                       a.d.tab$Beta[1:predictors], a.ent.tab$Beta[1:predictors], n.reun.tab$Beta[1:predictors]), 
                       "upper"=c(b.d.tab$Beta[1:predictors]+1.96*b.d.tab$SE[1:predictors], b.ent.tab$Beta[1:predictors]+1.96*b.ent.tab$SE[1:predictors], b.reun.tab$Beta[1:predictors]+1.96*b.reun.tab$SE[1:predictors], 
                                 a.d.tab$Beta[1:predictors]+1.96*a.d.tab$SE[1:predictors], a.ent.tab$Beta[1:predictors]+1.96*a.ent.tab$SE[1:predictors], n.reun.tab$Beta[1:predictors]+1.96*n.reun.tab$SE[1:predictors]), 
                       "lower"=c(b.d.tab$Beta[1:predictors]-1.96*b.d.tab$SE[1:predictors], b.ent.tab$Beta[1:predictors]-1.96*b.ent.tab$SE[1:predictors], b.reun.tab$Beta[1:predictors]-1.96*b.reun.tab$SE[1:predictors], 
                                 a.d.tab$Beta[1:predictors]-1.96*a.d.tab$SE[1:predictors], a.ent.tab$Beta[1:predictors]-1.96*a.ent.tab$SE[1:predictors], n.reun.tab$Beta[1:predictors]-1.96*n.reun.tab$SE[1:predictors]))

forest.est<-forest.est%>%filter(var!="Intercept")

# 
forest.est$var<-factor(forest.est$var)
forest.est$var<-factor(forest.est$var, levels(forest.est$var)[c(9,8, 3,5,7,6,4,1,2)])

# forest.est$varname<-factor(forest.est$varname, levels(forest.est$varname)[c(1:9, 18,10:14, 15:17)])

ggplot(data=forest.est, aes(x=beta,y=var))+
  geom_point()+
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate and 95 Percent Confidence Interval")+
  ylab(" ")+
  theme_minimal()+
  guides(colour=FALSE)+
  facet_wrap(~Outcome)+
  coord_cartesian(xlim=c(-1, 0.5))+
  ggsave(file="~/sync/cw-race/figures/ForestDisp.pdf", w=9, h=7)