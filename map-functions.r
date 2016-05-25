##### requires var named "state" as two letter state code, cname as county name
##### data frame cnty.dat, focal variable (or list of variables to facet), 
##### variable labels (descriptive names for caption), 
##### and number of quantiles to return
countymap<-function(cnty.dat, map.var, nquant, labels){
require(ggplot2)
require(RColorBrewer)
require(maps)
require(rgdal)
require(dplyr)
require(mapproj)
require(grid)

n.maps<-length(labels)

rq<-function(x,l){
  temp<-cut_number(x,l)
  quant<-rep(NA, length(x))
  for(i in (1:l)){
    z<-which(levels(temp)[i]==temp)
    quant[z]<-i
    }
    return(as.factor(quant))
}

cleanstate<-function(x){
  x$region<-ifelse((x$state=="AL"),"Alabama", x$region)
  x$region<-ifelse((x$state=="AK"),"Alaska", x$region)
  x$region<-ifelse((x$state=="AZ"),"Arizona", x$region)
  x$region<-ifelse((x$state=="AR"),"Arkansas", x$region)
  x$region<-ifelse((x$state=="CA"),"California", x$region)
  x$region<-ifelse((x$state=="CO"),"Colorado", x$region)
  x$region<-ifelse((x$state=="CT"),"Connecticut", x$region)
  x$region<-ifelse((x$state=="DE"),"Delaware", x$region)
  x$region<-ifelse((x$state=="DC"),"District of Columbia", x$region)
  x$region<-ifelse((x$state=="FL"),"Florida", x$region)
  x$region<-ifelse((x$state=="GA"),"Georgia", x$region)
  x$region<-ifelse((x$state=="HI"),"Hawaii", x$region)
  x$region<-ifelse((x$state=="ID"),"Idaho", x$region)
  x$region<-ifelse((x$state=="IL"),"Illinois", x$region)
  x$region<-ifelse((x$state=="IN"),"Indiana", x$region)
  x$region<-ifelse((x$state=="IA"),"Iowa", x$region)
  x$region<-ifelse((x$state=="KS"),"Kansas", x$region)
  x$region<-ifelse((x$state=="KY"),"Kentucky", x$region)
  x$region<-ifelse((x$state=="LA"),"Louisiana", x$region)
  x$region<-ifelse((x$state=="ME"),"Maine", x$region)
  x$region<-ifelse((x$state=="MD"),"Maryland", x$region)
  x$region<-ifelse((x$state=="MA"),"Massachusetts", x$region)
  x$region<-ifelse((x$state=="MI"),"Michigan", x$region)
  x$region<-ifelse((x$state=="MN"),"Minnesota", x$region)
  x$region<-ifelse((x$state=="MS"),"Mississippi", x$region)
  x$region<-ifelse((x$state=="MO"),"Missouri", x$region)
  x$region<-ifelse((x$state=="MT"),"Montana", x$region)
  x$region<-ifelse((x$state=="NE"),"Nebraska", x$region)
  x$region<-ifelse((x$state=="NV"),"Nevada", x$region)
  x$region<-ifelse((x$state=="NH"),"New Hampshire", x$region)
  x$region<-ifelse((x$state=="NJ"),"New Jersey", x$region)
  x$region<-ifelse((x$state=="NM"),"New Mexico", x$region)
  x$region<-ifelse((x$state=="NY"),"New York", x$region)
  x$region<-ifelse((x$state=="NC"),"North Carolina", x$region)
  x$region<-ifelse((x$state=="ND"),"North Dakota", x$region)
  x$region<-ifelse((x$state=="OH"),"Ohio", x$region)
  x$region<-ifelse((x$state=="OK"),"Oklahoma", x$region)
  x$region<-ifelse((x$state=="OR"),"Oregon", x$region)
  x$region<-ifelse((x$state=="PA"),"Pennsylvania", x$region)
  x$region<-ifelse((x$state=="RI"),"Rhode Island", x$region)
  x$region<-ifelse((x$state=="SC"),"South Carolina", x$region)
  x$region<-ifelse((x$state=="SD"),"South Dakota", x$region)
  x$region<-ifelse((x$state=="TN"),"Tennessee", x$region)
  x$region<-ifelse((x$state=="TX"),"Texas", x$region)
  x$region<-ifelse((x$state=="UT"),"Utah", x$region)
  x$region<-ifelse((x$state=="VT"),"Vermont", x$region)
  x$region<-ifelse((x$state=="VA"),"Virginia", x$region)
  x$region<-ifelse((x$state=="WA"),"Washington", x$region)
  x$region<-ifelse((x$state=="WV"),"West Virginia", x$region)
  x$region<-ifelse((x$state=="WI"),"Wisconsin", x$region)
  x$region<-ifelse((x$state=="WY"),"Wyoming", x$region)
  x$region<-ifelse((x$state=="PR"), "Puerto Rico", x$region)
  x$region<-tolower(x$region)
  return(x)
}

county_map <- map_data("county")
county_map$subregion<-ifelse(county_map$subregion=="de kalb", "dekalb", county_map$subregion)
county_map$subregion<-ifelse(county_map$subregion=="du page", "dupage", county_map$subregion)
county_map$subregion<-ifelse(county_map$subregion=="la salle", "lasalle", county_map$subregion)

if("TX"%in%cnty.dat$state){cnty.dat<-cleanstate(cnty.dat$state)}
cnty.dat$region<-tolower(cnty.dat$state)

cnty.dat<-cleanstate(cnty.dat)
cnty.dat$subregion<-tolower(cnty.dat$cname)
cnty.dat$subregion<-as.character(strsplit(cnty.dat$subregion, " county"))
cnty.dat$subregion<-as.character(strsplit(cnty.dat$subregion, " parish"))
cnty.dat$subregion<-gsub("[.]", "", cnty.dat$subregion)
### make long df for faceting
n<-nrow(cnty.dat)

if(n.maps==1){
  cnty.dat$q<-as.factor(rq(map.var, nquant))
  cnty.dat$c<-as.factor(labels)
}

if(n.maps>1){
  for(i in 1:n.maps){
    cnty.dat$q<-as.factor(rq(map.var[[i]],nquant))
    cnty.dat$c<-as.factor(labels[i])
    ifelse((i==1),cnty.long<-cnty.dat, cnty.long<-rbind(cnty.long, cnty.dat))
   }
}

choro<-left_join(county_map, cnty.long, by=c("region","subregion"))
choro <- choro[order(choro$order), ]

### IF FOR LIST LENGTH

legend.lab<-c("Lowest quantile", rep("", nquant-2), "Highest quantile")

CntyPlot <- ggplot(choro,
                       aes(x = long, y = lat, group = group, fill = q)) +
  geom_polygon(aes(fill = q), colour = "black", size=0.1) +
  scale_fill_brewer(palette = "Blues", labels=legend.lab,
                    name=" ", na.value="grey50") +
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        panel.border = element_blank(), panel.background=element_blank())+
  scale_y_continuous(name="", breaks=NULL) +
  scale_x_continuous(name="", breaks=NULL) +
  theme(strip.background=element_blank(), 
        strip.text.x=element_text(size=10),
        strip.text.y=element_blank())+
  theme(legend.position="bottom")+
  theme(legend.key.size= unit(0.3, "cm"))+
  coord_map(project="albers", at0 = 45.5, lat1 = 29.5)+
  xlab(NULL) + ylab(NULL)+
  facet_wrap(~c)

return(CntyPlot)
}

statemap<-function(state.dat, map.var, nquant, labels){
  require(ggplot2)
  require(RColorBrewer)
  require(maps)
  require(rgdal)
  require(dplyr)
  require(mapproj)
  require(grid)
  
  n.maps<-length(labels)
  
  rq<-function(x,nquant){
    l<-nquant
    temp<-cut_number(x,l)
    quant<-rep(NA, length(x))
    for(i in (1:l)){
      z<-which(levels(temp)[i]==temp)
      quant[z]<-i
    }
    return(as.factor(quant))
  }
  
  cleanstate<-function(x){
    x$region<-ifelse((x$state=="AL"),"Alabama", x$region)
    x$region<-ifelse((x$state=="AK"),"Alaska", x$region)
    x$region<-ifelse((x$state=="AZ"),"Arizona", x$region)
    x$region<-ifelse((x$state=="AR"),"Arkansas", x$region)
    x$region<-ifelse((x$state=="CA"),"California", x$region)
    x$region<-ifelse((x$state=="CO"),"Colorado", x$region)
    x$region<-ifelse((x$state=="CT"),"Connecticut", x$region)
    x$region<-ifelse((x$state=="DE"),"Delaware", x$region)
    x$region<-ifelse((x$state=="DC"),"District of Columbia", x$region)
    x$region<-ifelse((x$state=="FL"),"Florida", x$region)
    x$region<-ifelse((x$state=="GA"),"Georgia", x$region)
    x$region<-ifelse((x$state=="HI"),"Hawaii", x$region)
    x$region<-ifelse((x$state=="ID"),"Idaho", x$region)
    x$region<-ifelse((x$state=="IL"),"Illinois", x$region)
    x$region<-ifelse((x$state=="IN"),"Indiana", x$region)
    x$region<-ifelse((x$state=="IA"),"Iowa", x$region)
    x$region<-ifelse((x$state=="KS"),"Kansas", x$region)
    x$region<-ifelse((x$state=="KY"),"Kentucky", x$region)
    x$region<-ifelse((x$state=="LA"),"Louisiana", x$region)
    x$region<-ifelse((x$state=="ME"),"Maine", x$region)
    x$region<-ifelse((x$state=="MD"),"Maryland", x$region)
    x$region<-ifelse((x$state=="MA"),"Massachusetts", x$region)
    x$region<-ifelse((x$state=="MI"),"Michigan", x$region)
    x$region<-ifelse((x$state=="MN"),"Minnesota", x$region)
    x$region<-ifelse((x$state=="MS"),"Mississippi", x$region)
    x$region<-ifelse((x$state=="MO"),"Missouri", x$region)
    x$region<-ifelse((x$state=="MT"),"Montana", x$region)
    x$region<-ifelse((x$state=="NE"),"Nebraska", x$region)
    x$region<-ifelse((x$state=="NV"),"Nevada", x$region)
    x$region<-ifelse((x$state=="NH"),"New Hampshire", x$region)
    x$region<-ifelse((x$state=="NJ"),"New Jersey", x$region)
    x$region<-ifelse((x$state=="NM"),"New Mexico", x$region)
    x$region<-ifelse((x$state=="NY"),"New York", x$region)
    x$region<-ifelse((x$state=="NC"),"North Carolina", x$region)
    x$region<-ifelse((x$state=="ND"),"North Dakota", x$region)
    x$region<-ifelse((x$state=="OH"),"Ohio", x$region)
    x$region<-ifelse((x$state=="OK"),"Oklahoma", x$region)
    x$region<-ifelse((x$state=="OR"),"Oregon", x$region)
    x$region<-ifelse((x$state=="PA"),"Pennsylvania", x$region)
    x$region<-ifelse((x$state=="RI"),"Rhode Island", x$region)
    x$region<-ifelse((x$state=="SC"),"South Carolina", x$region)
    x$region<-ifelse((x$state=="SD"),"South Dakota", x$region)
    x$region<-ifelse((x$state=="TN"),"Tennessee", x$region)
    x$region<-ifelse((x$state=="TX"),"Texas", x$region)
    x$region<-ifelse((x$state=="UT"),"Utah", x$region)
    x$region<-ifelse((x$state=="VT"),"Vermont", x$region)
    x$region<-ifelse((x$state=="VA"),"Virginia", x$region)
    x$region<-ifelse((x$state=="WA"),"Washington", x$region)
    x$region<-ifelse((x$state=="WV"),"West Virginia", x$region)
    x$region<-ifelse((x$state=="WI"),"Wisconsin", x$region)
    x$region<-ifelse((x$state=="WY"),"Wyoming", x$region)
    x$region<-ifelse((x$state=="PR"), "Puerto Rico", x$region)
    x$region<-tolower(x$region)
    return(x)
  }
  
  state_map <- map_data("state")
  
  state.dat$region<-state.dat$state
  if("TX"%in%state.dat$state){cnty.dat<-cleanstate(state.dat$state)}
  state.dat<-cleanstate(state.dat)
 
  ### make long df for faceting
  n<-nrow(state.dat)

  if(n.maps==1){
    state.dat$q<-as.factor(rq(map.var, nquant))
    state.dat$c<-as.factor(labels)
    state.long<-state.dat
  }
  
  if(n.maps>1){
    for(i in 1:n.maps){
      state.dat$q<-as.factor(rq(map.var[[i]], nquant))
      state.dat$c<-as.factor(labels[i])
      ifelse((i==1),state.long<-state.dat, state.long<-rbind(state.long, state.dat))
      }
  }
  
  choro<-left_join(state_map, state.long, by=c("region"))
  choro <- choro[order(choro$order), ]
  choro <- choro[-which(choro$region=="district of columbia"),]
  
  ### IF FOR LIST LENGTH
  
  legend.lab<-c("Lowest quantile", rep("", nquant-2), "Highest quantile")
  
  StatePlot <- ggplot(choro,
                     aes(x = long, y = lat, group = group, fill = q)) +
    geom_polygon(aes(fill = q), colour = "black", size=0.1) +
    scale_fill_brewer(palette = "Blues", labels=legend.lab,
                      name=" ", na.value="grey50") +
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          panel.border = element_blank(), panel.background=element_blank())+
    scale_y_continuous(name="", breaks=NULL) +
    scale_x_continuous(name="", breaks=NULL) +
    theme(strip.background=element_blank(), 
          strip.text.x=element_text(size=10),
          strip.text.y=element_blank())+
    theme(legend.position="bottom")+
    theme(legend.key.size= unit(0.3, "cm"))+
    coord_map(project="albers", at0 = 45.5, lat1 = 29.5)+
    xlab(NULL) + ylab(NULL)+
    facet_wrap(~c)
  
  return(StatePlot)
}