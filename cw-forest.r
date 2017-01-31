setwd("~/sync/cw-race/figures")
### For forest plot of results
### ent and inst models
### Depends on model output from FCmodels.r in working environment (objects ent.results, inst.results)
library(ggplot2)
library(dplyr)
#source("~/Dropbox/cw-race/sim.R")

###merged results from Rubin combination - alt using posterior sim pooling in cw-forest-simuncertainty.r
### Count models
outcomes<-3
predictors<-nrow(b.d.tab)-1
forest.est<-data.frame("var"=rep(c("Intercept", "Incarceration", "Child poverty",
                                   "Unemployment", "Single parent",
                                 "Less than HS", "Percent pop",
                                 "Leg ideology", "Arrest", "Year", 
                                 "TANF adeq", "TANF incl", 
                                 "Medicaid incl", "SNAP incl"),outcomes*2),
                       "Race"=c(rep("African American", predictors*outcomes), rep("Native American",predictors*outcomes)), 
                       "Outcome"=c(rep("Caseload", predictors), rep("Entry", predictors), rep("Reunification", predictors), 
                                   rep("Native Am Caseload", predictors), rep("Native Am Entry", predictors), rep("Native Am Reun", predictors)),
                       "beta"=c(b.d.tab$Beta[1:predictors], b.ent.tab$Beta[1:predictors], b.reun.tab$Beta[1:predictors], 
                       a.d.tab$Beta[1:predictors], a.ent.tab$Beta[1:predictors], a.reun.tab$Beta[1:predictors]), 
                       "upper"=c(b.d.tab$Beta[1:predictors]+1.96*b.d.tab$SE[1:predictors], b.ent.tab$Beta[1:predictors]+1.96*b.ent.tab$SE[1:predictors], b.reun.tab$Beta[1:predictors]+1.96*b.reun.tab$SE[1:predictors], 
                                 a.d.tab$Beta[1:predictors]+1.96*a.d.tab$SE[1:predictors], a.ent.tab$Beta[1:predictors]+1.96*a.ent.tab$SE[1:predictors], a.reun.tab$Beta[1:predictors]+1.96*a.reun.tab$SE[1:predictors]), 
                       "lower"=c(b.d.tab$Beta[1:predictors]-1.96*b.d.tab$SE[1:predictors], b.ent.tab$Beta[1:predictors]-1.96*b.ent.tab$SE[1:predictors], b.reun.tab$Beta[1:predictors]-1.96*b.reun.tab$SE[1:predictors], 
                                 a.d.tab$Beta[1:predictors]-1.96*a.d.tab$SE[1:predictors], a.ent.tab$Beta[1:predictors]-1.96*a.ent.tab$SE[1:predictors], a.reun.tab$Beta[1:predictors]-1.96*a.reun.tab$SE[1:predictors]))

forest.est<-forest.est%>%filter(var!="Intercept")%>%
  filter(var!="Percent pop")%>%filter(var!="Year")

# 
forest.est$var<-factor(forest.est$var, levels=c("Intercept", "Incarceration", "Child poverty",
                                                "Unemployment", "Single parent",
                                                "Less than HS", "Percent pop",
                                                "Arrest",  
                                                "TANF adeq", "TANF incl", 
                                                "Medicaid incl", "SNAP incl", 
                                                "Leg ideology","Year", "Percent pop"))

forest.est$var = with(forest.est, factor(var, levels = rev(levels(var))))

forest.b<-forest.est%>%filter(Race=="African American")
forest.a<-forest.est%>%filter(Race=="Native American")

# forest.est$varname<-factor(forest.est$varname, levels(forest.est$varname)[c(1:9, 18,10:14, 15:17)])
 