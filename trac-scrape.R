library(rvest)
library(dplyr)

setwd("H:/cw-race/data/")
years<-2007:2014

url<-"http://trac.syr.edu/phptools/immigration/charges/state.php?project=1.75&stat=total&fy="
url.list<-list()
dat<-list()
data<-data.frame("state"=NA, "year"=NA, "cases"=NA)

for(y in 1:length(years)){
  url.list[[y]]<-paste(url, years[y], sep="")
  dat[[y]]<-read_html(url.list[[y]])%>%
    html_nodes(".Data")%>%html_text
  for(i in (1:(length(dat[[y]])/2))){
    data<-rbind(data, c(dat[[y]][i*2-1], years[y], dat[[y]][i*2]))
  }
}
data<-data[-1,]
data[,3]<-gsub(",", "", data[,3])
data[which(data[,3]=="*"),3]<-NA
data[,3]<-as.numeric(data[,3])
data[,2]<-as.numeric(data[,2])
data<-data[-which(data$state=="Entire US"),]

write.csv(data, "deport-proceedings.csv", row.names=FALSE)