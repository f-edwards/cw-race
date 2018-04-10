#devtools::install_github("hrbrmstr/albersusa")
library(dplyr)
library(DT)
library(ggplot2)
library(data.table)
library(viridis)
library(mapproj)
library(fiftystater)

returnquant<-function(x){
  l<-5 ### number of quantiles
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
  }
  return(as.factor(quant))
}

fc14<-fc.imp$imputations[[1]]%>%filter(year==2014)
dat<-fc14%>%dplyr::select(stname, bw.disp, ami.disp, b.incardisp, a.incardisp, 
                     b.incarrt, a.incarrt, cl.blk, cl.nat.am, blk.child, amind.child,
                     stname, cl, child, child.pov)%>%
                     mutate("bcl.rt"=cl.blk/blk.child, "acl.rt"=cl.nat.am/amind.child,
                            "cl.rt"=cl/child, "pov.rt"=child.pov/child)
  

data(state)
st<-data.frame(stname=state.abb, 
               state=state.name)
st$stname<-as.character(st$stname); st$state<-as.character(st$state)

dat<-dat%>%
  left_join(st)%>%
  mutate(state=tolower(state))


make.quant<-function(rate){  
  rate.quant<-round(quantile(rate, probs=c(0.125, 0.25, 0.375,0.5, 0.625,0.75, 0.875), na.rm=TRUE),1)
  quant.out<-ifelse(rate<=rate.quant[1], 1, 
                          ifelse(rate<=rate.quant[2], 2,
                                 ifelse(rate<=rate.quant[3], 3,
                                        ifelse(rate<=rate.quant[4], 4,
                                               ifelse(rate<=rate.quant[5],5,
                                                      ifelse(rate<=rate.quant[6], 6,
                                                             ifelse(rate<=rate.quant[7],7,8)))))))
  
  
  quant.out<-ordered(quant.out, levels=unique(quant.out)[order(unique(quant.out))],
                           labels=c(paste("0-",rate.quant[1], sep=""),
                                    paste(rate.quant[1],"-",rate.quant[2],sep=""),
                                    paste(rate.quant[2],"-",rate.quant[3],sep=""),
                                    paste(rate.quant[3],"-",rate.quant[4],sep=""),
                                    paste(rate.quant[4],"-",rate.quant[5], sep=""),
                                    paste(rate.quant[5],"-",rate.quant[6],sep=""),
                                    paste(rate.quant[6],"-",rate.quant[7],sep=""),
                                    paste(rate.quant[7],"-",sep="")))
} 

temp<-list()

temp[[1]]<-dat%>%
  select(state, bw.disp)%>%
  ungroup
temp[[1]]$rate.quant<-make.quant(temp[[1]]$bw.disp)

temp[[2]]<-dat%>%
  select(state, ami.disp)%>%
  ungroup
temp[[2]]$rate.quant<-make.quant(temp[[2]]$ami.disp)

temp[[3]]<-dat%>%
  select(state, b.incardisp)%>%
  ungroup
temp[[3]]$rate.quant<-make.quant(temp[[3]]$b.incardisp)

temp[[4]]<-dat%>%
  select(state, a.incardisp)%>%
  ungroup
temp[[4]]$rate.quant<-make.quant(temp[[4]]$a.incardisp)

RACE.list<-c("Black/White foster care", "American Indian/White foster care",
             "Black/White incarceration", "American Indian/White incarceration")

p<-list()
for(i in 1:4){
  # map_id creates the aesthetic mapping to the state name column in your dat
  p[[i]] <- ggplot(temp[[i]], aes(map_id = state)) +
    # map points to the fifty_states shape dat
    geom_map(aes(fill = rate.quant), map = fifty_states,
             color = "black") +
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    labs(x = "", y = "") +
    theme(legend.position = "right",
          panel.background = element_blank()) +
    scale_fill_viridis(name = " ", 
                       discrete = TRUE,
                       option="magma",
                       begin=0.15,
                       end=0.85
    ) + 
    coord_map("albers", lat0 = 39, lat1 = 45)+ 
    theme(text=element_text(size=13))+
    ggtitle(paste(RACE.list[i]))
  
}

library(gridExtra)

ggsave(file="map-four-disp.pdf",
       grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]]),
       width=10.7, height=8)

ggsave(file="map-two-disp.pdf",
       grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]]),
       width=10.7, height=8)
