library(RColorBrewer)
library(ggplot2)
library(maps)

### Depends on transformed data from FCmodels.r
#source("FCmodels.r")

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

fc14<-fc.ineq%>%filter(year.c==7)
fcmap<-fc14%>%select(stname, bw.disp, lw.disp, ami.disp, b.incardisp, a.incardisp, 
                     l.incardisp, l.incarrt, b.incarrt, a.incarrt, cl.blk, cl.nat.am, cl.latino, blk.child, latino.child, amind.child)%>%
                     mutate("bcl.rt"=cl.blk/blk.child, "acl.rt"=cl.nat.am/amind.child, "lcl.rt"=cl.latino/latino.child)

fcmap$state<-fcmap$St<-fcmap$stname
fcmap<-StateNames(fcmap)
fcmap$region<-tolower(fcmap$state)

states<-map_data("state")
n<-nrow(fcmap)

fclong<-with(fcmap, 
             data.frame(region=rep(region, 6),
              q=c(returnquant(bcl.rt), returnquant(acl.rt), returnquant(lcl.rt), returnquant(b.incarrt), returnquant(a.incarrt), returnquant(l.incarrt))))
              
fclong$c<-c(rep("Black children in foster care per capita", n), 
            rep("Native American children in foster care per capita", n), 
            rep("Latino children in foster care per capita", n), 
            rep("Black Incarceration per capita", n),
            rep("Native American incarceration per capita", n),
            rep("Latino incarceration per capita", n))

fclong$q<-as.factor(fclong$q)

fclong$c<-factor(fclong$c, levels=c("Black children in foster care per capita", 
                                    "Native American children in foster care per capita", "Latino children in foster care per capita",
                                    "Black Incarceration per capita", "Native American incarceration per capita", "Latino incarceration per capita"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

gray.pal<-brewer.pal(5, "Greys")

MapPlot <- ggplot(choro,
                  aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "black", size = 0.2) +
  scale_fill_manual(values = gray.pal,
                    name="State Value 2014\nMissing in color", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"),
                    na.value="darkgoldenrod4")

MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.

MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL)+
  scale_x_continuous(name="", breaks=NULL)+
  theme(legend.title=element_text(size=10))+
  theme(legend.text=element_text(size=10))+
  theme(legend.position="bottom")+
  theme(legend.key.size= unit(0.3, "cm"))

MapPlot<- MapPlot + theme(strip.background=element_blank(), 
                          strip.text.x=element_text(size=10),
                          strip.text.y=element_blank())

MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)

MapPlot <- MapPlot + facet_wrap(~c, ncol=3)

print(MapPlot)
ggsave(plot = MapPlot, "CLMap.pdf", h = 8, w = 16)

fclong<-with(fcmap, 
             data.frame(region=rep(region, 6),
              q=c(returnquant(bw.disp), returnquant(aw.disp), returnquant(lw.disp), returnquant(b.incardisp), returnquant(a.incardisp), returnquant(l.incardisp))))
              
fclong$c<-c(rep("Black/White foster care caseload disproportion", n), 
            rep("Native American/White foster care caseload disproportion", n), 
            rep("Latino/White foster care caseload disproportion", n), 
            rep("Black/White incarceration disproportion", n),
            rep("Native American/White incarceration disproportion", n),
            rep("Latino/White incarceration disproportion", n))

fclong$q<-as.factor(fclong$q)
###To preserve plotting order
fclong$c<-factor(fclong$c, levels=c("Black/White foster care caseload disproportion", 
                                    "Native American/White foster care caseload disproportion", 
                                    "Latino/White foster care caseload disproportion",
                                    "Black/White incarceration disproportion", 
                                    "Native American/White incarceration disproportion", 
                                    "Latino/White incarceration disproportion"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

gray.pal<-brewer.pal(5, "Greys")

MapPlot <- ggplot(choro,
                  aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "black", size = 0.2) +
  scale_fill_manual(values = gray.pal,
                    name="State Value 2014\nMissing in color", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"),
                    na.value="darkgoldenrod4")

MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.

MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
                            panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL)+
  scale_x_continuous(name="", breaks=NULL)+
  theme(legend.title=element_text(size=10))+
  theme(legend.text=element_text(size=10))+
  theme(legend.position="bottom")+
  theme(legend.key.size= unit(0.3, "cm"))

MapPlot<- MapPlot + theme(strip.background=element_blank(), 
                          strip.text.x=element_text(size=10),
                          strip.text.y=element_blank())

MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)

MapPlot <- MapPlot + facet_wrap(~c, ncol=3)

print(MapPlot)
ggsave(plot = MapPlot, "DispMap.pdf", h = 8, w = 16)