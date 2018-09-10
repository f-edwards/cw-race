dat<-read.csv("./data/dat_imp.csv")

gg<-ggplot(dat%>%filter(state==31), aes(x=year, y=chpov.amind.pc, group=imp))+
  geom_line()+
  facet_wrap(~state)



#REVISION PLAN
#USE ACS RISK VARS
#USE SEER POP FOR DENOMS
#REWORK TO LONG
#THINK ABT CRIME MEASURES? MAYBE UCR FROM OTHER PAPER?