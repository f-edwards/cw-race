
### For forest plot of results
### ent and inst models
### Depends on model output from FCmodels.r in working environment (objects ent.results, inst.results)
require(ggplot2)
require(dplyr)
forest.est<-data.frame("var"=row.names(t(ent.results[[1]])),"model"="Entry", "beta"=t(ent.results[[1]]), 
                       "upper"=t(ent.results[[1]])+1.96*t(ent.results[[2]]), 
                       "lower"=t(ent.results[[1]])-1.96*t(ent.results[[2]]))
forest.est<-rbind(forest.est,
                  data.frame("var"=row.names(t(inst.results[[1]])),"model"="Institutionalization", "beta"=t(inst.results[[1]]), 
                             "upper"=t(inst.results[[1]])+1.96*t(inst.results[[2]]), 
                             "lower"=t(inst.results[[1]])-1.96*t(inst.results[[2]])))
forest.est<-forest.est%>%
  filter(var!="(Intercept)")%>%
  mutate(varname= ifelse(var=="scale(unemprt)", "Unemployment",
                         ifelse(var=="scale(childnot2par)", "Single Parent HH",
                                ifelse(var=="scale(food.insec)", "Food Insecure HH",
                                       ifelse(var=="scale(chpovrt)", "Child Poverty",
                                              ifelse(var=="scale(LessHS)", "Adults w/o HS equiv",
                                                     ifelse(var=="scale(gsppercap)", "GSP per capita",
                                                            ifelse(var=="scale(ideo)", "State Leg. Ideology",
                                                                   ifelse(var=="scale(crime.pc)", "Crime per capita",
                                                                          ifelse(var=="scale(pctblk)", "% Black pop",
                                                                                 ifelse(var=="scale(tanf.adeq)", "TANF Benefits",
                                                                                        ifelse(var=="scale(snap.incl)", "SNAP enrolled per poverty",
                                                                                               ifelse(var=="scale(medicaid.incl)", "Medicaid enrolled per poverty",
                                                                                                      ifelse(var=="scale(tanf.incl)", "TANF enrolled per child pov.",
                                                                                                             ifelse(var=="scale(welfare.pc)", "Welfare Staff per cap.",
                                                                                                                    ifelse(var=="scale(incarrt)", "Incarceration per cap.",
                                                                                                                           ifelse(var=="scale(death.rt)", "Death sentence rate",
                                                                                                                                  ifelse(var=="scale(police.pc)", "Police per capita",
                                                                                                                                         ifelse(var=="scale(tanf.incl):scale(welfare.pc)", "TANF enroll * Welf Staff",NA
                                                                                                                                         )))))))))))))))))))

forest.est$varname<-factor(forest.est$varname, levels=forest.est$varname[1:18])

forest.est$varname<-factor(forest.est$varname, levels(forest.est$varname)[c(1:9, 18,10:14, 15:17)])
forest.est$varcat<-factor(ifelse(as.numeric(forest.est$varname)%in%(16:18), "Criminal Justice Regime", 
                                 ifelse(as.numeric(forest.est$varname)%in%(1:9), "Control", 
                                        ifelse(as.numeric(forest.est$varname)%in%(10:15), "Welfare Regime",NA))))
forest.est$varcat<-factor(forest.est$varcat, levels=c("Criminal Justice Regime", "Welfare Regime", "Control"))

ggplot(data=forest.est, aes(x=beta,y=varname))+
  geom_point(shape=21, size=3,aes(colour=varcat, fill=varcat))+
  geom_errorbarh(aes(xmin=lower,xmax=upper, colour=varcat),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Variable Category")+
  guides(colour=FALSE)+
  facet_wrap(~model)+
  ggsave(file="ForestAll.pdf", w=9, h=6)

ggplot(data=forest.est[forest.est$varcat%in%c("Criminal Justice Regime", "Welfare Regime"),], aes(x=beta,y=varname))+
  geom_point(shape=21, size=3,aes(colour=varcat, fill=varcat))+
  geom_errorbarh(aes(xmin=lower,xmax=upper, colour=varcat),height=0.2)+
  geom_vline(xintercept=0,linetype="dashed")+
  xlab("Parameter Estimate")+
  ylab(" ")+
  theme_minimal()+
  scale_fill_discrete(name="Variable Category")+
  scale_x_continuous(limits=c(-0.3, 0.3))+
  guides(colour=FALSE)+
  facet_wrap(~model)+
  ggsave(file="ForestFocal.pdf", w=9, h=6)