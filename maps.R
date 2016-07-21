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

fclong$c<-factor(fclong$c, levels=c("Black children in foster care per capita", 
                                    "Native American children in foster care per capita", "Latino children in foster care per capita",
                                    "Black Incarceration per capita", "Native American incarceration per capita", "Latino incarceration per capita"))
choro<-merge(states, fclong, by="region")
choro <- choro[order(choro$order), ]

gray.pal<-c("gray99", "gray65", "gray40", "gray15", "gray1")

MapPlot <- ggplot(choro,
                  aes(x = long, y = lat, group = group, fill = q))
MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.2) +
  scale_fill_manual(values = gray.pal,
                    name="Average\nState Value\n2002-2011", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))

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

MapPlot <- MapPlot + facet_wrap(~c, ncol=2)

print(MapPlot)

ggsave(plot = MapPlot, "FacetMapGray.png", h = 8, w = 8)

# ### FOR 1X2 ENTRY/POV MAPS FOR TALK
# 
# n<-nrow(fc10)
# fclong<-with(fc10, data.frame(region=rep(tolower(fc10$statename), 2),
#                               q=as.factor(c(returnquant(entrt), returnquant(chpov))),
#                               c=c(rep("Foster Care Entries per Child Population", n), rep("Child Poverty Rate", n))
# ))
# fclong$c<-factor(fclong$c, levels=c("Foster Care Entries per Child Population", 
#                                     "Child Poverty Rate"))
# choro<-merge(states, fclong, by="region")
# choro <- choro[order(choro$order), ]
# 
# MapPlot <- ggplot(choro,
#                   aes(x = long, y = lat, group = group, fill = q))
# MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.1) +
#   scale_fill_brewer(palette = "Blues",
#                     name="Average\nState Value\n2002-2011", labels=c("Lowest 20%", " ", " ", " ", "Highest 20%"))
# 
# MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.
# 
# MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#                             panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
#   theme(legend.title=element_text(size=10))+
#   theme(legend.text=element_text(size=10))+
#   theme(legend.position="bottom")+
#   theme(legend.key.size= unit(0.3, "cm"))
# 
# MapPlot<- MapPlot + theme(strip.background=element_blank(), 
#                           strip.text.x=element_text(size=10),
#                           strip.text.y=element_blank())
# 
# MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)
# 
# MapPlot <- MapPlot + facet_wrap(~c, ncol=2)
# 
# print(MapPlot)
# 
# ggsave(plot = MapPlot, "PovEntMap.pdf", h = 6, w = 8)
# 
# ### For solo map of entry rates
# 
# n<-nrow(fc10)
# fclong<-with(fc10, data.frame(region=rep(tolower(fc10$statename), 1),
#                               q=as.factor(c(returnquant(entrt))),
#                               c=c(rep("Foster Care Entries per Child Population", n))
# ))
# fclong$c<-factor(fclong$c, levels=c("Foster Care Entries per Child Population"))
# choro<-merge(states, fclong, by="region")
# choro <- choro[order(choro$order), ]
# 
# MapPlot <- ggplot(choro,
#                   aes(x = long, y = lat, group = group, fill = q))
# MapPlot <- MapPlot + geom_polygon(aes(fill = q), colour = "gray20", size = 0.1) +
#   scale_fill_brewer(palette = "Blues",
#                     name="Average Entries\nper 1,000 Children\n2002-2011", labels=c("1.7-2.9", "2.9-4.0 ", "4.0-4.8 ", "4.8-5.8 ", "5.8-8.7"))
# 
# MapPlot <- MapPlot + coord_map(project="albers", at0 = 45.5, lat1 = 29.5)  # Changes the projection to something other than Mercator.
# 
# MapPlot <- MapPlot +  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
#                             panel.border = element_blank(), panel.background=element_blank())+
#   scale_y_continuous(name="", breaks=NULL)+
#   scale_x_continuous(name="", breaks=NULL)+
# #   theme(legend.title=element_text(size=10))+
# #   theme(legend.text=element_text(size=10))+
#   theme(legend.position="right")
#   # theme(legend.key.size= unit(0.3, "cm"))
# 
# MapPlot<- MapPlot + theme(strip.background=element_blank(), 
#                           strip.text.x=element_text(size=10),
#                           strip.text.y=element_blank())
# 
# MapPlot <- MapPlot + xlab(NULL) + ylab(NULL)
# 
# print(MapPlot)
# 
# ggsave(plot = MapPlot, "EntMap.pdf", h = 6, w = 8)


