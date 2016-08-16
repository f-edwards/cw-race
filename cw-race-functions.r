### Assorted functions for data cleaning and analysis
### Some unused in the current repository


### Merges imputation results according to Rubin for beta, se

makeMIRegTab<-function(x){
  r<-mi.meld(ldply(x, fixef)[,-1], ldply(x, se.fixef)[,-1])
  beta<-t(r[[1]])
  se<-t(r[[2]])
  z<-beta/se
  ### TO SUMMARIZE VARIANCE OF REs ACROSS IMPUTATIONS
  ranefs.disp<-matrix(ncol=nrow(fc.ineq), nrow=m)
  ranefs.disp.se<-matrix(ncol=nrow(fc.ineq), nrow=m)
  ranefs.st<-ranefs.st.se<-matrix(ncol=length(unique(fc.ineq$stname)), nrow=m)
  for(i in 1:m){
  	if(length(ranef(x[[i]]))>1){ 	
    ranefs.disp[i,]<-ranef(x[[i]])[[1]][,1]
    ranefs.disp.se[i,]<-se.ranef(x[[i]])[[1]][,1]
    ranefs.st[i,]<-ranef(x[[i]])[[2]][,1]
    ranefs.st.se[i,]<-as.numeric(se.ranef(x[[i]])[[2]][,1])
	}
	if(length(ranef(x[[i]]))==1) {
		ranefs.st[i,]<-ranef(x[[i]])[[1]][,1]
    	ranefs.st.se[i,]<-as.numeric(se.ranef(x[[i]])[[1]][,1])
	}

  }
  # re.d<-mi.meld(ranefs.disp, ranefs.disp.se)
  re.s<-mi.meld(ranefs.st, ranefs.st.se)
  # Sig2.ep<-var(t(re.d[[1]]))
  # names(Sig2.ep)<-"Sig2.ep"
  Sig2.gam<-var(t(re.s[[1]]))
  names(Sig2.gam)<-"Sig2.gam"
  results<-as.data.frame(cbind(beta, se, z))
  names(results)<-c("Beta", "SE", "z")
  # results[11,1]<-Sig2.ep
  results[12,1]<-Sig2.gam
  return(results)
}


##Recode state abbreviations to fips
cleanpol<-function(x){
	x$state<-ifelse(x$stname=="AL",1, x$state)
	x$state<-ifelse(x$stname=="AK",2, x$state)
	x$state<-ifelse(x$stname=="AZ",4, x$state)
	x$state<-ifelse(x$stname=="AR",5, x$state)
	x$state<-ifelse(x$stname=="CA",6, x$state)
	x$state<-ifelse(x$stname=="CO",8, x$state)
	x$state<-ifelse(x$stname=="CT",9, x$state)
	x$state<-ifelse(x$stname=="DE",10, x$state)
	x$state<-ifelse(x$stname=="DC",11, x$state)
	x$state<-ifelse(x$stname=="FL",12, x$state)
	x$state<-ifelse(x$stname=="GA",13, x$state)
	x$state<-ifelse(x$stname=="HI",15, x$state)
	x$state<-ifelse(x$stname=="ID",16, x$state)
	x$state<-ifelse(x$stname=="IL",17, x$state)
	x$state<-ifelse(x$stname=="IN",18, x$state)
	x$state<-ifelse(x$stname=="IA",19, x$state)
	x$state<-ifelse(x$stname=="KS",20, x$state)
	x$state<-ifelse(x$stname=="KY",21, x$state)
	x$state<-ifelse(x$stname=="LA",22, x$state)
	x$state<-ifelse(x$stname=="ME",23, x$state)
	x$state<-ifelse(x$stname=="MD",24, x$state)
	x$state<-ifelse(x$stname=="MA",25, x$state)
	x$state<-ifelse(x$stname=="MI",26, x$state)
	x$state<-ifelse(x$stname=="MN",27, x$state)
	x$state<-ifelse(x$stname=="MS",28, x$state)
	x$state<-ifelse(x$stname=="MO",29, x$state)
	x$state<-ifelse(x$stname=="MT",30, x$state)
	x$state<-ifelse(x$stname=="NE",31, x$state)
	x$state<-ifelse(x$stname=="NV",32, x$state)
	x$state<-ifelse(x$stname=="NH",33, x$state)
	x$state<-ifelse(x$stname=="NJ",34, x$state)
	x$state<-ifelse(x$stname=="NM",35, x$state)
	x$state<-ifelse(x$stname=="NY",36, x$state)
	x$state<-ifelse(x$stname=="NC",37, x$state)
	x$state<-ifelse(x$stname=="ND",38, x$state)
	x$state<-ifelse(x$stname=="OH",39, x$state)
	x$state<-ifelse(x$stname=="OK",40, x$state)
	x$state<-ifelse(x$stname=="OR",41, x$state)
	x$state<-ifelse(x$stname=="PA",42, x$state)
	x$state<-ifelse(x$stname=="RI",44, x$state)
	x$state<-ifelse(x$stname=="SC",45, x$state)
	x$state<-ifelse(x$stname=="SD",46, x$state)
	x$state<-ifelse(x$stname=="TN",47, x$state)
	x$state<-ifelse(x$stname=="TX",48, x$state)
	x$state<-ifelse(x$stname=="UT",49, x$state)
	x$state<-ifelse(x$stname=="VT",50, x$state)
	x$state<-ifelse(x$stname=="VA",51, x$state)
	x$state<-ifelse(x$stname=="WA",53, x$state)
	x$state<-ifelse(x$stname=="WV",54, x$state)
	x$state<-ifelse(x$stname=="WI",55, x$state)
	x$state<-ifelse(x$stname=="WY",56, x$state)
return(x)
}

### Drop PR, DC
cleanfc<-function(y){
	y<-y[y$state!=72,]
	y<-y[y$state!=11,]
	return(y)
	}

cleanpol2<-function(x){
	x$St<-x$state
	x$state<-ifelse((x$St=="AL"|x$St=="Alabama"),1, x$state)
	x$state<-ifelse((x$St=="AK"|x$St=="Alaska"),2, x$state)
	x$state<-ifelse((x$St=="AZ"|x$St=="Arizona"),4, x$state)
	x$state<-ifelse((x$St=="AR"|x$St=="Arkansas"),5, x$state)
	x$state<-ifelse((x$St=="CA"|x$St=="California"),6, x$state)
	x$state<-ifelse((x$St=="CO"|x$St=="Colorado"),8, x$state)
	x$state<-ifelse((x$St=="CT"|x$St=="Connecticut"),9, x$state)
	x$state<-ifelse((x$St=="DE"|x$St=="Delaware"),10, x$state)
	x$state<-ifelse((x$St=="DC"|x$St=="District of Columbia"),11, x$state)
	x$state<-ifelse((x$St=="FL"|x$St=="Florida"),12, x$state)
	x$state<-ifelse((x$St=="GA"|x$St=="Georgia"),13, x$state)
	x$state<-ifelse((x$St=="HI"|x$St=="Hawaii"),15, x$state)
	x$state<-ifelse((x$St=="ID"|x$St=="Idaho"),16, x$state)
	x$state<-ifelse((x$St=="IL"|x$St=="Illinois"),17, x$state)
	x$state<-ifelse((x$St=="IN"|x$St=="Indiana"),18, x$state)
	x$state<-ifelse((x$St=="IA"|x$St=="Iowa"),19, x$state)
	x$state<-ifelse((x$St=="KS"|x$St=="Kansas"),20, x$state)
	x$state<-ifelse((x$St=="KY"|x$St=="Kentucky"),21, x$state)
	x$state<-ifelse((x$St=="LA"|x$St=="Louisiana"),22, x$state)
	x$state<-ifelse((x$St=="ME"|x$St=="Maine"),23, x$state)
	x$state<-ifelse((x$St=="MD"|x$St=="Maryland"),24, x$state)
	x$state<-ifelse((x$St=="MA"|x$St=="Massachusetts"),25, x$state)
	x$state<-ifelse((x$St=="MI"|x$St=="Michigan"),26, x$state)
	x$state<-ifelse((x$St=="MN"|x$St=="Minnesota"),27, x$state)
	x$state<-ifelse((x$St=="MS"|x$St=="Mississippi"),28, x$state)
	x$state<-ifelse((x$St=="MO"|x$St=="Missouri"),29, x$state)
	x$state<-ifelse((x$St=="MT"|x$St=="Montana"),30, x$state)
	x$state<-ifelse((x$St=="NE"|x$St=="Nebraska"),31, x$state)
	x$state<-ifelse((x$St=="NV"|x$St=="Nevada"),32, x$state)
	x$state<-ifelse((x$St=="NH"|x$St=="New Hampshire"),33, x$state)
	x$state<-ifelse((x$St=="NJ"|x$St=="New Jersey"),34, x$state)
	x$state<-ifelse((x$St=="NM"|x$St=="New Mexico"),35, x$state)
	x$state<-ifelse((x$St=="NY"|x$St=="New York"),36, x$state)
	x$state<-ifelse((x$St=="NC"|x$St=="North Carolina"),37, x$state)
	x$state<-ifelse((x$St=="ND"|x$St=="North Dakota"),38, x$state)
	x$state<-ifelse((x$St=="OH"|x$St=="Ohio"),39, x$state)
	x$state<-ifelse((x$St=="OK"|x$St=="Oklahoma"),40, x$state)
	x$state<-ifelse((x$St=="OR"|x$St=="Oregon"),41, x$state)
	x$state<-ifelse((x$St=="PA"|x$St=="Pennsylvania"),42, x$state)
	x$state<-ifelse((x$St=="RI"|x$St=="Rhode Island"),44, x$state)
	x$state<-ifelse((x$St=="SC"|x$St=="South Carolina"),45, x$state)
	x$state<-ifelse((x$St=="SD"|x$St=="South Dakota"),46, x$state)
	x$state<-ifelse((x$St=="TN"|x$St=="Tennessee"),47, x$state)
	x$state<-ifelse((x$St=="TX"|x$St=="Texas"),48, x$state)
	x$state<-ifelse((x$St=="UT"|x$St=="Utah"),49, x$state)
	x$state<-ifelse((x$St=="VT"|x$St=="Vermont"),50, x$state)
	x$state<-ifelse((x$St=="VA"|x$St=="Virginia"),51, x$state)
	x$state<-ifelse((x$St=="WA"|x$St=="Washington"),53, x$state)
	x$state<-ifelse((x$St=="WV"|x$St=="West Virginia"),54, x$state)
	x$state<-ifelse((x$St=="WI"|x$St=="Wisconsin"),55, x$state)
	x$state<-ifelse((x$St=="WY"|x$St=="Wyoming"),56, x$state)
	x$state<-ifelse((x$St=="PR"|x$St=="Puerto Rico"), 72, x$state)
return(x)
}


StateNames<-function(x){
  x$St<-x$state
  x$state<-ifelse(x$St=="AL", "Alabama", x$state)
  x$state<-ifelse(x$St=="AK", "Alaska", x$state)
  x$state<-ifelse(x$St=="AZ", "Arizona", x$state)
  x$state<-ifelse(x$St=="AR", "Arkansas", x$state)
  x$state<-ifelse(x$St=="CA", "California", x$state)
  x$state<-ifelse(x$St=="CO", "Colorado", x$state)
  x$state<-ifelse(x$St=="CT", "Connecticut", x$state)
  x$state<-ifelse(x$St=="DE", "Delaware", x$state)
  x$state<-ifelse(x$St=="DC", "District of Columbia", x$state)
  x$state<-ifelse(x$St=="FL", "Florida", x$state)
  x$state<-ifelse(x$St=="GA", "Georgia", x$state)
  x$state<-ifelse(x$St=="HI", "Hawaii", x$state)
  x$state<-ifelse(x$St=="ID", "Idaho", x$state)
  x$state<-ifelse(x$St=="IL", "Illinois", x$state)
  x$state<-ifelse(x$St=="IN", "Indiana", x$state)
  x$state<-ifelse(x$St=="IA", "Iowa", x$state)
  x$state<-ifelse(x$St=="KS", "Kansas", x$state)
  x$state<-ifelse(x$St=="KY", "Kentucky", x$state)
  x$state<-ifelse(x$St=="LA", "Louisiana", x$state)
  x$state<-ifelse(x$St=="ME", "Maine", x$state)
  x$state<-ifelse(x$St=="MD", "Maryland", x$state)
  x$state<-ifelse(x$St=="MA", "Massachusetts", x$state)
  x$state<-ifelse(x$St=="MI", "Michigan", x$state)
  x$state<-ifelse(x$St=="MN", "Minnesota", x$state)
  x$state<-ifelse(x$St=="MS", "Mississippi", x$state)
  x$state<-ifelse(x$St=="MO", "Missouri", x$state)
  x$state<-ifelse(x$St=="MT", "Montana", x$state)
  x$state<-ifelse(x$St=="NE", "Nebraska", x$state)
  x$state<-ifelse(x$St=="NV", "Nevada", x$state)
  x$state<-ifelse(x$St=="NH", "New Hampshire", x$state)
  x$state<-ifelse(x$St=="NJ", "New Jersey", x$state)
  x$state<-ifelse(x$St=="NM", "New Mexico", x$state)
  x$state<-ifelse(x$St=="NY", "New York", x$state)
  x$state<-ifelse(x$St=="NC", "North Carolina", x$state)
  x$state<-ifelse(x$St=="ND", "North Dakota", x$state)
  x$state<-ifelse(x$St=="OH", "Ohio", x$state)
  x$state<-ifelse(x$St=="OK", "Oklahoma", x$state)
  x$state<-ifelse(x$St=="OR", "Oregon", x$state)
  x$state<-ifelse(x$St=="PA", "Pennsylvania", x$state)
  x$state<-ifelse(x$St=="RI", "Rhode Island", x$state)
  x$state<-ifelse(x$St=="SC", "South Carolina", x$state)
  x$state<-ifelse(x$St=="SD", "South Dakota", x$state)
  x$state<-ifelse(x$St=="TN", "Tennessee", x$state)
  x$state<-ifelse(x$St=="TX", "Texas", x$state)
  x$state<-ifelse(x$St=="UT", "Utah", x$state)
  x$state<-ifelse(x$St=="VT", "Vermont", x$state)
  x$state<-ifelse(x$St=="VA", "Virginia", x$state)
  x$state<-ifelse(x$St=="WA", "Washington", x$state)
  x$state<-ifelse(x$St=="WV", "West Virginia", x$state)
  x$state<-ifelse(x$St=="WI", "Wisconsin", x$state)
  x$state<-ifelse(x$St=="WY", "Wyoming", x$state)
  x$state<-ifelse(x$St=="PR", "Puerto Rico", x$state)
  return(x)
}

stnames<-function(x){
	x$stname<-ifelse(x$state==1,"AL", x$stname)
	x$stname<-ifelse(x$state==2,"AK", x$stname)
	x$stname<-ifelse(x$state==4,"AZ", x$stname)
	x$stname<-ifelse(x$state==5,"AR", x$stname)
	x$stname<-ifelse(x$state==6,"CA", x$stname)
	x$stname<-ifelse(x$state==8,"CO", x$stname)
	x$stname<-ifelse(x$state==9,"CT", x$stname)
	x$stname<-ifelse(x$state==10,"DE", x$stname)
	x$stname<-ifelse(x$state==11,"DC", x$stname)
	x$stname<-ifelse(x$state==12,"FL", x$stname)
	x$stname<-ifelse(x$state==13,"GA", x$stname)
	x$stname<-ifelse(x$state==15,"HI", x$stname)
	x$stname<-ifelse(x$state==16, "ID",x$stname)
	x$stname<-ifelse(x$state==17,"IL", x$stname)
	x$stname<-ifelse(x$state==18,"IN", x$stname)
	x$stname<-ifelse(x$state==19,"IA", x$stname)
	x$stname<-ifelse(x$state==20,"KS", x$stname)
	x$stname<-ifelse(x$state==21,"KY", x$stname)
	x$stname<-ifelse(x$state==22,"LA", x$stname)
	x$stname<-ifelse(x$state==23,"ME", x$stname)
	x$stname<-ifelse(x$state==24,"MD", x$stname)
	x$stname<-ifelse(x$state==25,"MA", x$stname)
	x$stname<-ifelse(x$state==26,"MI", x$stname)
	x$stname<-ifelse(x$state==27,"MN", x$stname)
	x$stname<-ifelse(x$state==28,"MS", x$stname)
	x$stname<-ifelse(x$state==29,"MO", x$stname)
	x$stname<-ifelse(x$state==30,"MT", x$stname)
	x$stname<-ifelse(x$state==31,"NE", x$stname)
	x$stname<-ifelse(x$state==32,"NV", x$stname)
	x$stname<-ifelse(x$state==33,"NH", x$stname)
	x$stname<-ifelse(x$state==34,"NJ", x$stname)
	x$stname<-ifelse(x$state==35,"NM", x$stname)
	x$stname<-ifelse(x$state==36,"NY", x$stname)
	x$stname<-ifelse(x$state==37,"NC", x$stname)
	x$stname<-ifelse(x$state==38,"ND", x$stname)
	x$stname<-ifelse(x$state==39,"OH", x$stname)
	x$stname<-ifelse(x$state==40,"OK", x$stname)
	x$stname<-ifelse(x$state==41,"OR", x$stname)
	x$stname<-ifelse(x$state==42,"PA", x$stname)
	x$stname<-ifelse(x$state==44,"RI", x$stname)
	x$stname<-ifelse(x$state==45,"SC", x$stname)
	x$stname<-ifelse(x$state==46,"SD", x$stname)
	x$stname<-ifelse(x$state==47,"TN", x$stname)
	x$stname<-ifelse(x$state==48,"TX", x$stname)
	x$stname<-ifelse(x$state==49,"UT", x$stname)
	x$stname<-ifelse(x$state==50,"VT", x$stname)
	x$stname<-ifelse(x$state==51,"VA", x$stname)
	x$stname<-ifelse(x$state==53,"WA", x$stname)
	x$stname<-ifelse(x$state==54,"WV", x$stname)
	x$stname<-ifelse(x$state==55,"WI", x$stname)
	x$stname<-ifelse(x$state==56,"WY", x$stname)
return(x)
}

regname<-function(x){
	x$regname<-ifelse(x$reg==1, "New England", x$regname)
	x$regname<-ifelse(x$reg==2, "Mid Atlantic", x$regname)
	x$regname<-ifelse(x$reg==3, "E North Central", x$regname)
	x$regname<-ifelse(x$reg==4, "W North Central", x$regname)
	x$regname<-ifelse(x$reg==5, "S Atlantic", x$regname)
	x$regname<-ifelse(x$reg==6, "E South Central", x$regname)
	x$regname<-ifelse(x$reg==7, "W South Central", x$regname)
	x$regname<-ifelse(x$reg==8, "Mountain", x$regname)
	x$regname<-ifelse(x$reg==9, "Pacific", x$regname)
	x$region<-ifelse(x$reg<3, "Northeast", x$region)
	x$region<-ifelse(x$reg==3|x$reg==4, "Midwest", x$region)
	x$region<-ifelse(x$reg==5|x$reg==6|x$reg==7, "South", x$region)
	x$region<-ifelse(x$reg>7, "West", x$region)
	return(x)
}

cleanwel<-function(x){
	x$state<-ifelse(x$stname=="01",1, x$state)
	x$state<-ifelse(x$stname=="02",2, x$state)
	x$state<-ifelse(x$stname=="03",4, x$state)
	x$state<-ifelse(x$stname=="04",5, x$state)
	x$state<-ifelse(x$stname=="05",6, x$state)
	x$state<-ifelse(x$stname=="06",8, x$state)
	x$state<-ifelse(x$stname=="07",9, x$state)
	x$state<-ifelse(x$stname=="08",10, x$state)
	x$state<-ifelse(x$stname=="09",11, x$state)
	x$state<-ifelse(x$stname=="10",12, x$state)
	x$state<-ifelse(x$stname=="11",13, x$state)
	x$state<-ifelse(x$stname=="12",15, x$state)
	x$state<-ifelse(x$stname=="13",16, x$state)
	x$state<-ifelse(x$stname=="14",17, x$state)
	x$state<-ifelse(x$stname=="15",18, x$state)
	x$state<-ifelse(x$stname=="16",19, x$state)
	x$state<-ifelse(x$stname=="17",20, x$state)
	x$state<-ifelse(x$stname=="18",21, x$state)
	x$state<-ifelse(x$stname=="19",22, x$state)
	x$state<-ifelse(x$stname=="20",23, x$state)
	x$state<-ifelse(x$stname=="21",24, x$state)
	x$state<-ifelse(x$stname=="22",25, x$state)
	x$state<-ifelse(x$stname=="23",26, x$state)
	x$state<-ifelse(x$stname=="24",27, x$state)
	x$state<-ifelse(x$stname=="25",28, x$state)
	x$state<-ifelse(x$stname=="26",29, x$state)
	x$state<-ifelse(x$stname=="27",30, x$state)
	x$state<-ifelse(x$stname=="28",31, x$state)
	x$state<-ifelse(x$stname=="29",32, x$state)
	x$state<-ifelse(x$stname=="30",33, x$state)
	x$state<-ifelse(x$stname=="31",34, x$state)
	x$state<-ifelse(x$stname=="32",35, x$state)
	x$state<-ifelse(x$stname=="33",36, x$state)
	x$state<-ifelse(x$stname=="34",37, x$state)
	x$state<-ifelse(x$stname=="35",38, x$state)
	x$state<-ifelse(x$stname=="36",39, x$state)
	x$state<-ifelse(x$stname=="37",40, x$state)
	x$state<-ifelse(x$stname=="38",41, x$state)
	x$state<-ifelse(x$stname=="39",42, x$state)
	x$state<-ifelse(x$stname=="40",44, x$state)
	x$state<-ifelse(x$stname=="41",45, x$state)
	x$state<-ifelse(x$stname=="42",46, x$state)
	x$state<-ifelse(x$stname=="43",47, x$state)
	x$state<-ifelse(x$stname=="44",48, x$state)
	x$state<-ifelse(x$stname=="45",49, x$state)
	x$state<-ifelse(x$stname=="46",50, x$state)
	x$state<-ifelse(x$stname=="47",51, x$state)
	x$state<-ifelse(x$stname=="48",53, x$state)
	x$state<-ifelse(x$stname=="49",54, x$state)
	x$state<-ifelse(x$stname=="50",55, x$state)
	x$state<-ifelse(x$stname=="51",56, x$state)
return(x)
}

conwel<-function(a){
	b<-a[substr(a[,1], 3, 3)==1,]
	b<-b[substr(b[,1], 1, 2)!="00",]
	b[,1]<-substr(b[,1], 1, 2)
	colnames(b)<-c("stname", "func", "ftemp", "ftpay", "ptemp", "ptpay", "pthrs", "fteq", "totemp", "totpay")
	b$state<-NA
	b<-b[b$func=="079",]
	return(b)
}

conwel2<-function(a){
	b<-a[substr(a[,1], 3, 3)==1,]
	b<-b[substr(b[,1], 1, 2)!="00",]
	b[,1]<-substr(b[,1], 1, 2)
	colnames(b)<-c("stname", "func", "ftemp", "ftpay", "ptemp", "ptpay", "pthrs", "fteq")
	b$totemp<-NA
	b$totpay<-NA
	b$state<-NA
	b<-b[b$func=="079",]
	return(b)
}

descriptives<-function(x){
    desc<-data.frame(Variable=rep(NA, ncol(x)),Mean=rep(NA,ncol(x)), 
      StdDev=rep(NA, ncol(x)), Minimum=rep(NA, ncol(x)), Maximum=rep(NA, ncol(x)))
    for(i in 1:ncol(x)){
      desc$Variable[i]=names(x[i])
      desc$Mean[i]=mean(x[,i], na.rm=TRUE)
      desc$StdDev[i]=sd(x[,i], na.rm=TRUE)
      desc$Minimum[i]=min(x[,i], na.rm=TRUE)
      desc$Maximum[i]=max(x[,i], na.rm=TRUE)
    }
    return(desc)
}

welspend<-function(q){
q$amount<-as.numeric(as.character(q$amount))
states<-unique(q$state)
staten<-ifelse(states<10,
		paste("0",states, sep=""), 
		as.character(states))

wels<-data.frame("stname"=rep(NA, length(states)), 
	"spend"=rep(NA, length(states)), 
	"year"=rep(NA, length(states)), "state"=NA)

yr<-unique(q$year)

for(s in (1:length(unique(states)))){
	wels$stname[s]<-staten[s]

	z<-q[q$state==states[s],]

	E67<-ifelse(length(z[z$item=="E67", "amount"])>0, z[z$item=="E67", "amount"], 0)
	E68<-ifelse(length(z[z$item=="E68", "amount"])>0, z[z$item=="E68", "amount"], 0)
	J67<-ifelse(length(z[z$item=="J67", "amount"])>0, z[z$item=="J67", "amount"], 0)
	J68<-ifelse(length(z[z$item=="J68", "amount"])>0, z[z$item=="J68", "amount"], 0)
	E74<-ifelse(length(z[z$item=="E74", "amount"])>0, z[z$item=="E74", "amount"], 0)
	E75<-ifelse(length(z[z$item=="E75", "amount"])>0, z[z$item=="E75", "amount"], 0)
	E77<-ifelse(length(z[z$item=="E77", "amount"])>0, z[z$item=="E77", "amount"], 0)
	F77<-ifelse(length(z[z$item=="F77", "amount"])>0, z[z$item=="F77", "amount"], 0)
	G77<-ifelse(length(z[z$item=="G77", "amount"])>0, z[z$item=="G77", "amount"], 0)
	E79<-ifelse(length(z[z$item=="E79", "amount"])>0, z[z$item=="E79", "amount"], 0)
	F79<-ifelse(length(z[z$item=="F79", "amount"])>0, z[z$item=="F79", "amount"], 0)
	G79<-ifelse(length(z[z$item=="G79", "amount"])>0, z[z$item=="G79", "amount"], 0)

	I67<-ifelse(length(z[z$item=="I67", "amount"])>0, z[z$item=="I67", "amount"], 0)
	L67<-ifelse(length(z[z$item=="L67", "amount"])>0, z[z$item=="L67", "amount"], 0)
	M67<-ifelse(length(z[z$item=="M67", "amount"])>0, z[z$item=="M67", "amount"], 0)
	N67<-ifelse(length(z[z$item=="N67", "amount"])>0, z[z$item=="N67", "amount"], 0)
	O67<-ifelse(length(z[z$item=="O67", "amount"])>0, z[z$item=="O67", "amount"], 0)
	P67<-ifelse(length(z[z$item=="P67", "amount"])>0, z[z$item=="P67", "amount"], 0)
	S67<-ifelse(length(z[z$item=="S67", "amount"])>0, z[z$item=="S67", "amount"], 0)
	I68<-ifelse(length(z[z$item=="I68", "amount"])>0, z[z$item=="I68", "amount"], 0)
	M68<-ifelse(length(z[z$item=="M68", "amount"])>0, z[z$item=="M68", "amount"], 0)
	N68<-ifelse(length(z[z$item=="N68", "amount"])>0, z[z$item=="N68", "amount"], 0)
	O68<-ifelse(length(z[z$item=="O68", "amount"])>0, z[z$item=="O68", "amount"], 0)
	P68<-ifelse(length(z[z$item=="P68", "amount"])>0, z[z$item=="P68", "amount"], 0)
	I79<-ifelse(length(z[z$item=="I79", "amount"])>0, z[z$item=="I79", "amount"], 0)
	L79<-ifelse(length(z[z$item=="L79", "amount"])>0, z[z$item=="L79", "amount"], 0)
	M79<-ifelse(length(z[z$item=="M79", "amount"])>0, z[z$item=="M79", "amount"], 0)
	N79<-ifelse(length(z[z$item=="N79", "amount"])>0, z[z$item=="N79", "amount"], 0)
	O79<-ifelse(length(z[z$item=="O79", "amount"])>0, z[z$item=="O79", "amount"], 0)
	P79<-ifelse(length(z[z$item=="P79", "amount"])>0, z[z$item=="P79", "amount"], 0)
	R79<-ifelse(length(z[z$item=="R79", "amount"])>0, z[z$item=="R79", "amount"], 0)



	wels$spend[s]<-E67+E68+J67+J68+E74+E75+E77+F77+G77+E79+F79+G79+
	I67+L67+M67+N67+O67+P67+S67+I68+M68+N68+O68+P68+I79+L79+M79+N79+O79+P79+R79

	wels$year<-ifelse(yr>=10, as.numeric(paste("20",yr , sep="")), 
		as.numeric(paste("200",yr , sep="")))

}
	wels<-wels[wels$stname!="00",]
	return(wels)
}





welspend2<-function(q){
	q$amount<-as.numeric(as.character(q$amount))
	states<-unique(q$state)
	staten<-ifelse(states<10,
			paste("0",states, sep=""), 
			as.character(states))
	
	yr<-unique(q$year)

	i<-unique(as.character(q$item))
	q<-q[q$level==1,]
	
	wels<-data.frame(matrix(0, ncol=3+length(i), nrow=length(states)))
	names(wels)<-c("stname", "year","state", i)

	for(s in (1:length(unique(states)))){
		wels$stname[s]<-staten[s]

		z<-q[q$state==states[s],]

		for(k in (1:length(i))){
			wels[s,k+3]<-
				as.numeric(ifelse(length(z[z$item==i[k], "amount"])>0, z[z$item==i[k], "amount"], 0))
		}	

	}

		wels$year<-ifelse(yr>=10, as.numeric(paste("20",yr , sep="")), 
			as.numeric(paste("200",yr , sep="")))

		wels<-wels[wels$stname!="00",]
		return(wels)
}




spendvars<-function(x){
	### Create series of aggregate spending measures from Census State/local finance data
	### Codes from 2006 Classification manual
	
	totrev.d<-cbind(x$B01,x$B21,x$B22,x$B30,x$B42,x$B43,x$B46,x$B50,x$B54,x$B79,x$B80,x$B89,x$B91
		,x$B92,x$B93,x$B94,x$D21,x$D30,x$D42,x$D46,x$D50,x$D79,x$D80,x$D89,x$D91,x$D92,x$D93
		,x$D94,x$T01,x$T09,x$T10,x$T11,x$T12,x$T13,x$T14,x$T15,x$T16,x$T19,x$T20,x$T21,x$T22,x$T23
		,x$T24,x$T25,x$T27,x$T28,x$T29,x$T40,x$T41,x$T50,x$T51,x$T53,x$T99,x$A01,x$A03,x$A09
		,x$A10,x$A12,x$A16,x$A18,x$A21,x$A36,x$A44,x$A45,x$A50,x$A54,x$A56,x$A59,x$A60,x$A16
		,x$A80,x$A81,x$A87,x$A89,x$U01,x$U11,x$U20,x$U21,x$U30,x$U40,x$U41,x$U50,x$U95,x$U99
		,x$A90,x$A91,x$A93,x$A94,x$X01,x$X02,x$X05,x$X08,x$Y01,x$Y02,x$Y04,x$Y10,x$Y11,x$Y12
		,x$Y50,x$Y51,x$Y52)

	totrev<-rowSums(totrev.d)

	totspend.d<-cbind(x$E01,x$E03,x$E04,x$E05,x$E12,x$E16,x$E18,x$E21,x$E22,x$E23,x$E25,x$E26,x$E27,x$E29,x$E31,
	x$E32,x$E36,x$E44,x$E45,x$E50,x$E52,x$E54,x$E55,x$E56,x$E59,x$E60,x$E61,x$E62,x$E66,x$E73,x$E74,
	x$E75,x$E77,x$E79,x$E80,x$E81,x$E85,x$E87,x$E89,x$E90,x$E91,x$E92,x$E93,x$E94,
	x$I89,x$I91,x$I92,x$I93,x$I94x$J19,x$J67,x$J68,x$J85x$X11,x$X12,x$Y05,x$Y06,x$Y15,x$Y14,x$Y53,x$Y54,
	x$F01,x$F03,x$F04,x$F05,x$F12,x$F16,x$F18,x$F21,x$F22,x$F23,x$F25,x$F26,x$F27,x$F29,x$F31,
	x$F32,x$F36,x$F44,x$F45,x$F50,x$F52,x$F54,x$F55,x$F56,x$F59,x$F60,x$F61,x$F62,x$F66,x$F77,
	x$F79,x$F80,x$F81,x$F85,x$F87,x$F89,x$F90,x$F91,x$F92,x$F93,x$F94,x$G01,x$G03,x$G04,
	x$G05,x$G12,x$G16,x$G18,x$G21,x$G22,x$G23,x$G24,x$G25,x$G26,x$G27,x$G29,x$G31,x$G32,x$G36,
	x$G44,x$G45,x$G50,x$G52,x$G54,x$G55,x$G56,x$G59,x$G60,x$G61,x$G62,x$G66,x$G77,x$G79,x$G80,
	x$G81,x$G85,x$G87,x$G89,x$G90,x$G91,x$G92,x$G93,x$G94,
	x$M01,x$M04,x$M05,x$M12,x$M18,x$M21,x$M23,x$M25,x$M27,x$M29,x$M30,x$M32,x$M36,x$M44,
	x$M50,x$M52,x$M54,x$M55,x$M56,x$M59,x$M60,x$M61,x$M62,x$M66,x$M67,x$M68,x$M79,x$M80,
	x$M81,x$M87,x$M89,x$M91,x$M92,x$M93,x$M94,x$Q12,x$Q18,x$S67,x$S74,x$S89)

	totspend<-(rowSums(totspend.d))

	eduspend.d<-cbind(x$E12,x$F12,x$G12,x$M12,x$Q12,x$E16,x$F16,x$G16,x$E18,x$F18,x$G18,x$M18,x$Q18,
		x$J19,x$E21,x$F21,x$G21,x$M21)

	eduspend<-rowSums(eduspend.d)

	welspend.d<-cbind(x$J67,x$M67,x$S67,x$S74,x$J68,x$M68,x$E73,x$E74,x$E75,x$E77,x$F77,x$G77,
		x$E79,x$M79,x$F79,x$G79)

	welspend<-rowSums(welspend.d)

	hospitalspend.d<-cbind(x$E36,x$F36,x$G36,x$M36)

	hospitalspend<-rowSums(hospitalspend.d)

	healthspend.d<-cbind(x$E27,x$E32,x$F27,x$F32,x$G27,x$G32,x$M27,x$M32)
	
	healthspend<-rowSums(healthspend.d)

	policespend.d<-cbind(x$E62,x$F62,x$G62,x$M62)

	policespend<-rowSums(policespend.d)

	correctionspend.d<-cbind(x$E04,x$M04,x$F04,x$G04,x$E05,x$F05,x$G05,x$M05)

	correctionspend<-rowSums(correctionspend.d)
	wel.transfer<-rowSums(cbind(x$J67, x$M67, x$S67, x$J68, x$M68, x$E74, x$E75))
	wel.institution<-rowSums(cbind(x$E77, x$F77, x$G77, x$K77))
	wel.socserv<-rowSums(cbind(x$E79, x$F79, x$G79, x$K79, x$J79, x$L79, x$M79))

	expend<-cbind(x$state, x$year, totrev, totspend, eduspend, welspend, hospitalspend, healthspend, policespend, correctionspend,
		wel.transfer, wel.institution, wel.socserv)

	expend<-data.frame(expend)

	names(expend)[1:2]<-c("state", "year")
	return(expend)
}


crimestate<-function(x){
	states<-unique(x$FIPS_ST)
	cout<-data.frame("state"=states, "year"=unique(x$year), "crime"=NA)
	for (s in 1:length(states)){
		cout[s, "crime"]<-sum(x[x$FIPS_ST==states[s],"INDEX"])
	}
	return(cout)
}

read.ucr<-function(x, year){
	if(year<2009){
	out<-read.fwf(x, widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4),
	col.names=c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST", "FIPS_CTY", "CPOPARST", 
		"CPOCRIM","AG_ARRST", "AG_OFF", "COVIND", "INDEX",
		 "MODINDX", "MURDER", "RAPE", "ROBBERY",
		"AGASLT","BURGLRY", "LARCENY","MVTHEFT", "ARSON"))
	}
	if(year>2008){
	out<-read.fwf(x, widths=c(4,1,1,4,2,3,8,8,3,3,8,6,6,4,4,5,5,6,6,6,4),
	col.names=c("STUDYNO", "EDITION", "PART", "IDNO", "FIPS_ST", "FIPS_CTY", "CPOPARST", 
		"CPOCRIM","AG_ARRST", "AG_OFF", "COVIND", "VIOL","PROP",
		  "MURDER", "RAPE", "ROBBERY",
		"AGASLT","BURGLRY", "LARCENY","MVTHEFT", "ARSON"))
	out$INDEX<-out$VIOL+out$PROP
	}

	out$year<-year
	crime<-crimestate(out)
	return(crime)
}

inflation<-function(x,money){
	inflate<-cbind(c(2000:2011), c(1.31,1.27, 1.25, 1.22, 1.19, 1.15, 1.12, 1.08,1.04, 1.05, 1.03,1))
	for(i in 1:nrow(x)){
		for(s in 1:length(money)){
		z<-which(x[i, "year"]==inflate[,1])	
		x[i,money[s]]<-x[i,money[s]]*inflate[z,2]

		}
	}
	return(x)
}
