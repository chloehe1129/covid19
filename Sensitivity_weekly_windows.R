## County Level Movement data
movement = read.csv("C:/Users/sn233/Dropbox (Personal)/COVID/covid19/Data/Global_Mobility_Report.csv")
movement = movement[which(movement$country_region=="United States"),] # only US data
movement = movement[which(movement$sub_region_2!=""),]                # only state data
movement_county = movement[,-c(1,2,4,5)]

names(movement_county) <- c("state","FIPS","date", "retail_rec", "grocery_pharmacy", "parks", "transit", "work", "residential")
movement_county$date <- as.Date(movement_county$date)
write.csv(movement_county, "C:/Users/sn233/Dropbox (Personal)/COVID/covid19/Data/us_county_daily_movement.csv")

library(dplyr)
library(tidyr)
library(lubridate)
library(zoo)


##################### SA : RAW MAP ########################################################################################################################################################################
library(dplyr)
movement_county = movement_county %>% filter(rowSums(is.na(movement_county[,c(4:9)])) != ncol(movement_county[,c(4:9)]))
mydata3 = merge(mydata,movement_county,by=c("date","FIPS"))
mydata3$weeks_since_first = 0
mydata3$date = as.Date(mydata3$date)

for (i in 1:length(unique(mydata3$FIPS))){
  if(nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])<2){mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"]=0}else{
    for (k in 2:nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])){
      mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"][k]=
        as.numeric((mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][k]-mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][1])/7)
    }}
}
#library(rgdal)
#devtools::install_version("sp",version="1.4-1")
#library(devtools)
library(tigris); library(reshape); library(dplyr); library(INLA); library(spdep); library(splines)
us_co <- counties(cb=TRUE, class="sp") # Download a US Counties shape-file into R
us_co$struct<-1:dim(us_co@data)[1]

us_co1<-st_as_sf(us_co) # Convert foreign object to an sf object
us_co1$FIPS <- as.numeric(paste(us_co1$STATEFP, us_co1$COUNTYFP, sep=""))
final.dat<-merge(us_co1,mydata3, by="FIPS")
final.dat <- final.dat[order(final.dat$struct),]

tmp <- data.frame(FIPS=unique(final.dat$FIPS), IDcounty1=1:length(unique(final.dat$struct)))
final.dat <- left_join(final.dat, tmp, by="FIPS")
final.dat$IDcounty2 <- final.dat$IDcounty1
final.dat$IDcounty3 <- final.dat$IDcounty1

data = final.dat
data$case_pop = data$cum_case/data$population*100000

tcum_cut <- quantile(mydat[mydat$week==28,]$true_cum_pop, p=seq(0,1,length.out = 6))
tcum_cut[1] <- 0

pdf("~/Dropbox (Personal)/covid19_paper/tables_figures/SA_raw_map.pdf",width=20,height=30)
data%>%
  filter(week%in%c(28))%>%
  mutate(qrr=cut(case_pop, breaks = tcum_cut, include.lowest = T))%>%
  ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdBu", na.value="grey")+guides(fill=guide_legend(title="COVID-19 cases per 100K Quartile"))+
  ggtitle(label="County-level Cumulative COVID-19 cases per 100K")
dev.off()
##################################################################################################################################################################################################################

movement_component = c("residential","work","retail_rec","grocery_pharmacy","transit","parks")

################################################# Creating Weekly dataset #################################################################################################################################################################
for (m in (1:length(movement_component))){
  ## Weekly movement: start from Wednesday
  movement_county$FIPS = as.factor(movement_county$FIPS)
  tmp <- movement_county %>% group_by(week = week(date), FIPS)
  tmp$date = as.Date(tmp$date)
  movement_week <- aggregate(.~week+ state+FIPS , data=tmp[,c("state","FIPS","week",movement_component[m])], mean, na.rm=TRUE) ####### Only average one component at a time
  movement_week$date <- ymd("2020-01-01" ) + weeks(movement_week$week - 1 )
  movement_week <- movement_week[, c("week", "state", "FIPS","date",movement_component[m])]
  movement_week$date <- as.Date(movement_week$date)

  movement = movement_week
  mydata <-  read.csv("/Dropbox (Personal)/COVID/covid19/Data/us_county_weekly_case_JHU.csv")[,-1]
  mydata$date <- as.Date(mydata$date)
  county_predic <-  read.csv("C:/Users/sn233/Dropbox (Personal)/COVID/covid19/Data/us_county_predictors.csv")[,-1]

  mydata <- merge(mydata, county_predic, by=c("FIPS"))
  mydata$date <- as.Date(mydata$date)
  mydata <- mydata[order(mydata$FIPS, as.Date(mydata$date)),]

  movement$date = as.Date(movement$date)

  lagk = 8
  for (kk in (1:lagk)){
    create = paste0(movement_component[m],"_weeks_ago",kk)
    for(i in (1:length(unique(movement$FIPS)))){
      dates_before = movement[which(movement$FIPS == unique(movement$FIPS)[i]),]$date-7*kk
      ind = match(dates_before,movement[which(movement$FIPS == unique(movement$FIPS)[i]),]$date)
      movement[which(movement$FIPS==unique(movement$FIPS)[i]),create]=movement[which(movement$FIPS==unique(movement$FIPS)[i]),][ind,movement_component[m]]
    }
  }


  mydata2 = merge(mydata,movement[,-c(2,4)],by=c("FIPS","week"))

  mydata2 <- mydata2[mydata2$cum_case>=50, ]
  mydata2 <- mydata2[mydata2$state.x!="Maine",]

  mydata3 <- na.omit(mydata2)
  mydata3$weeks_since_first = 0
  mydata3$date = as.Date(mydata3$date)

  for (i in 1:length(unique(mydata3$FIPS))){
    if(nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])<2){mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"]=0}else{
      for (k in 2:nrow(mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),])){
        mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"weeks_since_first"][k]=
          as.numeric((mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][k]-mydata3[which(mydata3$FIPS==unique(mydata3$FIPS)[i]),"date"][1])/7)
      }}
  }

  # if want to get rid of 0-1 weeks
  #write.csv(mydata3,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/wo01_weeklydata_",movement_component[m],".csv"))
  write.csv(mydata3,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/weeklydata_",movement_component[m],".csv"))
}


##################################################################################################################################################################################################################

movement_component = c("residential"," work","retail_rec","grocery_pharmacy","transit","parks")

for (m in (1:length(movement_component))){
  # INLA Set up
  mydata3 = read.csv(paste0("~/Dropbox/covid19_paper/weeklydata_",movement_component[m],".csv"))

  mydata3[grepl("residential", names(mydata3))] <- as.matrix(lapply(mydata3[grepl("residential", names(mydata3))], `/`, 10))#??

  #install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
  library(tigris); library(reshape); library(dplyr); library(INLA); library(spdep); library(splines)
  us_co <- counties(cb=TRUE, class="sp") # Download a US Counties shape-file into R
  us_co$struct<-1:dim(us_co@data)[1]

  us_co1<-st_as_sf(us_co) # Convert foreign object to an sf object
  us_co1$FIPS <- as.numeric(paste(us_co1$STATEFP, us_co1$COUNTYFP, sep=""))
  final.dat<-merge(us_co1,mydata3, by="FIPS")
  final.dat <- final.dat[order(final.dat$struct),]

  tmp <- data.frame(FIPS=unique(final.dat$FIPS), IDcounty1=1:length(unique(final.dat$struct)))
  final.dat <- left_join(final.dat, tmp, by="FIPS")
  tmp <- data.frame(STATEFP=unique(final.dat$STATEFP), IDSTATE1=1:length(unique(final.dat$STATEFP)))
  final.dat <- left_join(final.dat, tmp, by="STATEFP")

  final.dat$IDcounty2 <- final.dat$IDcounty1
  final.dat$IDcounty3 <- final.dat$IDcounty1

  final.dat$IDSTATE0 <- final.dat$IDSTATE1
  final.dat$IDSTATE2 <- final.dat$IDSTATE1
  final.dat$IDSTATE3 <- final.dat$IDSTATE1


  # different adjacency matrix for each movement component!
  H<-inla.read.graph(filename=paste0("~/Dropbox/covid19_paper/Code/cl_graph_",movement_component[m]))
  prec.prior <- list(theta = list(prior="loggamma", param = c(1, 0.001)))

  # Lag-specific weekly model
  nk=3
  library(Hmisc)
  time_RCS = scale(rcspline.eval(final.dat$weeks_since_first, nk=nk, inclx=TRUE, knots.only=FALSE, type="ordinary", norm=2, rpm=NULL))
  colnames(time_RCS) <- paste("time_RCS", seq(1:(nk-1)), sep="")
  final.dat = cbind(final.dat, time_RCS)

  ######### Sinlge Lag Models #########

  ns <- length(unique(final.dat$IDSTATE1))
  newIDSTATE1 <-  matrix(0, nrow=ns, ncol=ns)
  diag(newIDSTATE1) <- rep(1, ns)
  lc <- inla.make.lincombs(parks=rep(1, ns), IDSTATE1=newIDSTATE1)
  names(lc) <- paste("IDSTATE", 1:ns, sep="")

  model1 <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
                 + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior) + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
                 + time_RCS1 + time_RCS2+ factor(q_popdensity)
                 + scale(young_perc) + scale(median_perc) + scale(old_perc)
                 + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
                 + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc)
                 + parks
                 + f(IDSTATE1, parks,model="iid",hyper=prec.prior),
                 data = final.dat, lincomb=lc,
                 offset=log(population),
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1))
  assign(paste0("LS_",movement_component[m],"_0_lc"),model1)
  l=0
  saveRDS(model1,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/Model_result/staterandomeffect_Model_result/Lag-specificlc/LS_weekly_lag_",l, movement_component[m], "lc", ".rds"))

  lag = c(1,2,3,4,5,6,7,8)
  for (l in (1:length(lag))){
    l=8
    newIDSTATE1 <-  matrix(0, nrow=ns, ncol=ns)
    diag(newIDSTATE1) <- rep(1, ns)
    lc <- inla.make.lincombs(parks_weeks_ago8=rep(1, ns), IDSTATE1=newIDSTATE1)
    names(lc) <- paste("IDSTATE", 1:ns, sep="")
    model <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
                + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior) + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
                + time_RCS1 + time_RCS2 + factor(q_popdensity)
                + scale(young_perc) + scale(median_perc) + scale(old_perc)
                + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
                + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc)
                + parks_weeks_ago8
                + f(IDSTATE1, parks_weeks_ago8,model="iid",hyper=prec.prior),
                data = final.dat,lincomb=lc,
                offset=log(population),
                family = "nbinomial",
                control.predictor = list(compute = TRUE, link = 1))
    assign(paste0("LS_",movement_component[m],"_",l,"_lc"),model)
    saveRDS(model,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/Model_result/staterandomeffect_Model_result/Lag-specificlc/LS_weekly_lag_",l, movement_component[m], "lc", ".rds"))
  }


  ######### DLM weekly #########
  library(Hmisc)
  library(sf)

  # SPLINE SET UP
  lagd <- 8
  lages <- c(paste(movement_component[m],"_weeks_ago", seq(1:lagd), sep=""))
  #lages <- c(paste("grocery","_weeks_ago", seq(1:lagd), sep=""))
  Movement <- final.dat[, c(lages)]
  Movement <- `st_geometry<-`(Movement, NULL)
  lagpoint <- matrix(rep(seq(0, lagd), nrow(final.dat)), nrow=nrow(final.dat), byrow=T)

  #####  B-spline  ####
  kk=3
  RCS = as.matrix(Movement)%*%as.matrix(bs(seq(1, lagd), degree=2, intercept=TRUE)) # change knots #
  colnames(RCS) <- paste("RCS", seq(1:ncol(RCS)), sep="")
  mydat = cbind(final.dat,RCS)

  x <- seq(1, lagd, length=100)
  newB1 <- bs(x, degree=2, intercept=TRUE) # change knots #
  # FIXED EFFECT LC
  #lc <- inla.make.lincombs(RCS1 = newB1[,1] , RCS2 = newB1[,2], RCS3 = newB1[,3]) # change knots #

  # RANDOM SLOPE LC
  ns <- length(unique(final.dat$IDSTATE1))
  newIDSTATE1 = newIDSTATE2 = newIDSTATE3 = matrix(0, nrow=ns, ncol=ns)
  diag(newIDSTATE1) <- rep(as.numeric(newB1[1,1]), ns)
  lc <- inla.make.lincombs(RCS1 = rep(as.numeric(newB1[1,1]), ns),
                           RCS2 = rep(as.numeric(newB1[1,2]), ns),
                           RCS3 = rep(as.numeric(newB1[1,3]), ns),
                           IDSTATE1=newIDSTATE1)
  names(lc) <- paste("IDSTATE", 1:ns, "_", 1, sep="")
  for(j in 2:99){
    newIDSTATE1 <- newIDSTATE2 <- newIDSTATE3 <- matrix(0, nrow=ns, ncol=ns)
    diag(newIDSTATE1) <- rep(as.numeric(newB1[j,1]), ns)
    diag(newIDSTATE2) <- rep(as.numeric(newB1[j,2]), ns)
    diag(newIDSTATE3) <- rep(as.numeric(newB1[j,3]), ns)
    templc <- inla.make.lincombs(RCS1 = rep(as.numeric(newB1[j,1]), ns),
                                 RCS2 = rep(as.numeric(newB1[j,2]), ns),
                                 RCS3 = rep(as.numeric(newB1[j,3]), ns),
                                 IDSTATE1 = newIDSTATE1,
                                 IDSTATE2 = newIDSTATE2,
                                 IDSTATE3 = newIDSTATE3)
    names(templc) <- paste("IDSTATE", 1:ns, "_", j, sep="")
    lc <- c(lc, templc)
  }
  newIDSTATE1 <- newIDSTATE2 <- newIDSTATE3 <- matrix(0, nrow=ns, ncol=ns)
  diag(newIDSTATE3) <- rep(as.numeric(newB1[100,3]), ns)
  templc <- inla.make.lincombs(RCS1 = rep(as.numeric(newB1[100,1]), ns),
                               RCS2 = rep(as.numeric(newB1[100,2]), ns),
                               RCS3 = rep(as.numeric(newB1[100,3]), ns),
                               IDSTATE3=newIDSTATE3)
  names(templc) <- paste("IDSTATE", 1:ns,"_", 100, sep="")
  lc <- c(lc, templc)

  # RANDOM INTERCEPT LC
  ns <- length(unique(final.dat$IDSTATE1))
  newIDSTATE0 = matrix(0, nrow=ns, ncol=ns)
  diag(newIDSTATE0) <- rep(1, ns)
  lc <- inla.make.lincombs(RCS1 = rep(newB1[1,1], ns) , RCS2 = rep(newB1[1,2], ns), RCS3 = rep(newB1[1,3], ns), IDSTATE0=newIDSTATE0) # change knots #
  names(lc) <- paste("IDSTATE", 1:ns, "_", 1, sep="")

  for(j in 2:100){
    newIDSTATE0 <- matrix(0, nrow=ns, ncol=ns)
    diag(newIDSTATE0) <- rep(1, ns)
    templc <- inla.make.lincombs(RCS1 = rep(as.numeric(newB1[j,1]), ns),
                                 RCS2 = rep(as.numeric(newB1[j,2]), ns),
                                 RCS3 = rep(as.numeric(newB1[j,3]), ns),
                                 IDSTATE0 = newIDSTATE0)
    names(templc) <- paste("IDSTATE", 1:ns, "_", j, sep="")
    lc <- c(lc, templc)
  }


  #####  B-spline  ####
  model1 <- inla(weekly_case ~ f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
                 + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior)
                 + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
                 + time_RCS1 + time_RCS2
                 + factor(q_popdensity)
                 + scale(young_perc) + scale(median_perc) + scale(old_perc)
                 + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
                 + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc)
                 + RCS1 + RCS2 + RCS3
                 + f(IDSTATE1,RCS1, model="iid", hyper = prec.prior)
                 + f(IDSTATE2,RCS2, model="iid", hyper = prec.prior)
                 + f(IDSTATE3,RCS3, model="iid", hyper = prec.prior)
                 ,data = mydat, lincomb=lc,
                 offset=log(population),
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1), control.compute = list(dic=T))
  saveRDS(model1,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/Model_result/staterandomeffect_Model_result/DLM_lc/BS_",kk,"_weekly_",movement_component[m],"lc",".rds"))

  # random intercept only
  model1 <- inla(weekly_case ~ f(IDSTATE0,model="iid")
                 + f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
                 + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior)
                 + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
                 + time_RCS1 + time_RCS2
                 + factor(q_popdensity)
                 + scale(young_perc) + scale(median_perc) + scale(old_perc)
                 + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
                 + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc)
                 + RCS1 + RCS2 + RCS3
                 #+ f(IDSTATE1,RCS1, model="iid", hyper = prec.prior)
                 #+ f(IDSTATE2,RCS2, model="iid", hyper = prec.prior)
                 #+ f(IDSTATE3,RCS3, model="iid", hyper = prec.prior)
                 ,data = mydat, lincomb=lc,
                 offset=log(population),
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1), control.compute = list(dic=T))
  saveRDS(model1,paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/Model_result/staterandomeffect_Model_result/DLM_lc/BS_",kk,"_weekly_",movement_component[m],"lc_randomintercept",".rds"))

  # correlation between RCS coefficients
  lc <- inla.make.lincombs(RCS1 = c(1, 0, 0), RCS2 = c(0, 1, 0), RCS3 = c(0, 0, 1))
  model1 <- inla(weekly_case ~ f(IDSTATE0,model="iid")
                 + f(IDcounty1, model = "bym", graph = H, hyper = prec.prior)
                 + f(IDcounty2, time_RCS1, model="iid", hyper = prec.prior)
                 + f(IDcounty3, time_RCS2, model="iid", hyper = prec.prior)
                 + time_RCS1 + time_RCS2
                 + factor(q_popdensity)
                 + scale(young_perc) + scale(median_perc) + scale(old_perc)
                 + scale(poverty_perc) + scale(Black.perc) + scale(Hispanic.or.Latino.perc)
                 + scale(temp)+ scale(Bachelor_orhigher_perc) + scale(obese_perc)
                 + RCS1 + RCS2 + RCS3
                 ,data = mydat, lincomb=lc,
                 offset=log(population),
                 family = "nbinomial",
                 control.predictor = list(compute = TRUE, link = 1), control.compute = list(dic=T),
                 control.inla = list(lincomb.derived.correlation.matrix = TRUE))

  ## covariance matrix
  model1$misc$lincomb.derived.covariance.matrix
  ## correlation matrix
  model1$misc$lincomb.derived.correlation.matrix

  ############## Plot the raw map, predicted map and predicted line ###############

  mydat$pred = model1$summary.fitted.values$mean
  mydat = mydat[order(mydat$FIPS,mydat$week),]

  tmp <- data.frame(FIPS=mydat$FIPS, pred=mydat$pred)
  tmp <- tmp %>% group_by(FIPS) %>% mutate(cum_pred = cumsum(pred))
  mydat$cum_pred <- tmp$cum_pred
  mydat$pred_pop = mydat$cum_pred/mydat$population*100000
  mydat$true_cum_pop = mydat$cum_case/mydat$population*100000

  tcum_cut <- quantile(mydat[mydat$week==28,]$true_cum_pop, p=seq(0,1,length.out = 6))
  tcum_cut[1] <- 0
  pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/SA_raw_map_",movement_component[m],".pdf"),width=10,height=15)
  mydat%>%
    filter(week%in%c(28))%>%
    mutate(qrr=cut(true_cum_pop, breaks = tcum_cut, include.lowest = T))%>%
    ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdBu", na.value="grey")+guides(fill=guide_legend(title="COVID-19 cases per 100K Quartile"))+
    ggtitle(label="County-level Cumulative COVID-19 cases per 100K")+ theme(plot.title = element_text(size=20))
  dev.off()

  pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/SA_pred_map_",movement_component[m],".pdf"),width=10,height=15)
  mydat%>%
    filter(week%in%c(28))%>%
    mutate(qrr=cut(pred_pop, breaks = tcum_cut, include.lowest = T))%>%
    ggplot()+geom_sf(aes(fill=qrr))+scale_colour_brewer(palette = "RdBu" )+scale_fill_brewer(palette = "RdBu", na.value="grey")+guides(fill=guide_legend(title="COVID-19 cases per 100K Quartile"))+
    ggtitle(label="Predicted County-level Cumulative COVID-19 cases per 100K")+ theme(plot.title = element_text(size=20))
  dev.off()


  ## U.S predicted line
  ggdata = mydat # if want certain state, then subset here

  Nypop <- sum(ggdata[!duplicated(ggdata$FIPS),"population"]$population)
  Nypred <- aggregate(ggdata$pred, by=list(ggdata$week), FUN=sum)
  Nycase <- aggregate(ggdata$weekly_case, by=list(ggdata$week), FUN=sum)

  Nydata <- data.frame(date=sort(unique(ggdata$date)),week=Nypred[,1], pred_cum=cumsum(Nypred[,2])/Nypop*100000, true_cum=cumsum(Nycase[,2])/Nypop*100000)
  Nydata$date = as.Date(Nydata$date)

  pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/SA_pred_line_",movement_component[m],".pdf"),width=8,height=5)
  ggplot(Nydata, aes(x=date))+
    geom_line(aes(y=pred_cum,colour="Predicted Incidence"),size = 1.2)+
    geom_line(aes(y=true_cum,colour="Observed Incidence"),size = 1.2)+
    xlab("Months into the pandemic") +
    ylab("COVID-19 cases per 100K population")+
    labs(title = "Predicted v.s Observed COVID-19 Incidence in the U.S",color = "Predicted v.s Observed")
  dev.off()
  #######

  ef <- function(x) exp(10*x)

  pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/BS_DLM_",kk,"_",Sys.Date(),"_",movement_component[m],".pdf"),width=5,height=4)
  est <- sapply(1:100, function(i) inla.emarginal(ef, model1$marginals.lincomb.derived[[i]]))
  CI <- sapply(1:100, function(i) inla.qmarginal(c(0.025,0.975), inla.tmarginal(ef, model1$marginals.lincomb.derived[[i]]))) #CI
  uCI <- CI[2,]  # the upper credible interval
  lCI <- CI[1,]  # the lower credible interval,
  pltdata = as.data.frame(cbind(seq(from=1,to=8,length=100),est,uCI,lCI))
  library(ggplot2)
  g= ggplot(data = pltdata,aes(x=V1,y=est))+geom_line()+
    ylab("Incidence Rate Ratio") + xlab("Weeks in the past")+
    theme(legend.title = element_text(color = "black", size = 12),legend.position = "none",plot.title=element_text(size=18,face="bold"))
  g+geom_ribbon(data=pltdata,aes(ymin=uCI,ymax=lCI),alpha=0.3,colour=NA)+ggtitle(movement_component[m])+geom_hline(yintercept = 1,lty="dashed")
  dev.off()

}


#### FOREST PLOT
ef <- function(x) exp(10*x)
movement = c("residential","work","retail_rec","grocery_pharmacy","transit","parks")
result = NULL
result_for_one_movement = NULL
for(k in 1:6){
  result_for_onelag = NULL
  for(l in c(0, 1, 2, 3, 4, 5, 6, 7, 8)){
    fname <- paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/Model_result/staterandomeffect_Model_result/Lag-specificlc/LS_weekly_lag_",l, movement[k], "lc", ".rds")
    model1 <- readRDS(fname)

    est <- lapply(1:46, function(j) inla.emarginal(ef, model1$marginals.lincomb.derived[[j]]))
    CI <- lapply(1:46, function(j)  inla.qmarginal(c(0.025,0.5, 0.975), inla.tmarginal(ef, model1$marginals.lincomb.derive[[j]])) )
    res <- lapply(1:46, function(i) cbind(mean=est[[i]], median=CI[[i]][2], lci=CI[[i]][1], uci=CI[[i]][3], STATE=i))
    res <- do.call(rbind, res)
    a <- read.table("C:/Users/sn233/Dropbox (Personal)/covid19_paper/stateid_name.txt")
    #res <- merge(res, a, by="STATE")
    result_for_onelag[[l+1]] <- cbind(merge(res, a, by="STATE"),rep(l,46),rep(movement[k],46))
  }
  result_for_one_movement[[k]] = do.call(rbind,result_for_onelag)
}
result = do.call(rbind,result_for_one_movement)

names(result) = c("stateid","mean","median","lower","upper","state","weeklag","movement")
result = as.data.frame(result)
result = result[,c("mean","lower","upper","state","weeklag","movement")]
names(result) = c("effect","lower","upper","state","weeklag","movement")

result$effect = as.numeric(as.character(result$effect))
result$lower = as.numeric(as.character(result$lower))
result$upper = as.numeric(as.character(result$upper))
result$movement = ifelse(result$movement=="retail_rec","retail and recreation",result$movement)
result$movement = ifelse(result$movement=="grocery_pharmacy","grocery and pharmacy",result$movement)

###
us_co <- states(cb=TRUE, class="sp") # Download a US Counties shape-file into R
us_co$struct<-1:dim(us_co@data)[1]
us_co1<-st_as_sf(us_co) # Convert foreign object to an sf object
names(us_co1)[6] = "state"
#us_co1$FIPS <- as.numeric(paste(us_co1$STATEFP, us_co1$COUNTYFP, sep=""))
final.dat<-merge(us_co1,result, by="state")
final.dat <- final.dat[order(final.dat$struct),]
result = final.dat

#result$effect = ifelse(result$lower<1 & 1<result$upper,"NA",result$effect)
#result$effect = as.numeric(result$effect)

pdf("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/LS_statemap_residential.pdf",width=25,height=20)

#remotes::install_github("coolbutuseless/ggpattern")
#library(ggpattern)
signif.floor <- function(x, n){
  pow <- floor( log10( abs(x) ) ) + 1 - n
  y <- floor(x / 10 ^ pow) * 10^pow
  # handle the x = 0 case
  y[x==0] <- 0
  y
}
signif.ceiling <- function(x, n){
  pow <- floor( log10( abs(x) ) ) + 1 - n
  y <- ceiling(x / 10 ^ pow) * 10^pow
  # handle the x = 0 case
  y[x==0] <- 0
  y
}

brk = sort(c(1,
             seq(signif.floor(range(subset(result,movement=="residential")$effect,na.rm = T)[1],2),
                 signif.ceiling(range(subset(result,movement=="residential")$effect,na.rm = T)[2],2),length=6)))

result$Significance = ifelse(result$lower<1 & 1<result$upper,"Not Significant","Significant")
result%>%
  filter(movement=="residential")%>%
  mutate(qrr=cut(effect, breaks = brk, include.lowest = T))%>%
  ggplot()+geom_sf_pattern(aes(fill=qrr,pattern=Significance))+
  #scale_fill_manual(name="qrr",values=c("red","darksalmon","mistyrose","aliceblue","cadetblue2","deepskyblue1","dodgerblue4"),na.translate=F)+
  scale_fill_manual(name="qrr",values=c("darksalmon","aliceblue","cadetblue2","deepskyblue1","cyan4","dodgerblue4"),na.translate=F)+
  scale_pattern_discrete(choices = c("stripe","none"))+
  guides(fill=guide_legend(title="Incidence Rate Ratio"))+
  ggtitle(label=paste("State-level Incidence Rate Ratio for 10% increase in","residential movement"))+
  facet_wrap(~weeklag)+theme(text=element_text(size=20))
dev.off()
###

######### Without Residential
x <- c("work", "grocery and pharmacy", "retail and recreation", "transit", "parks")
result = result[order(match(result$movement,x)),]
result$weeklaag = as.numeric(as.character(result$weeklag))
# result$week = ifelse(result$weeklag<4,0,1)
# result$week = as.factor(result$week)
result = result[-which(result$movement=="residential"),]
result$color = ifelse(result$lower>1,1,0)
result$color = as.factor(result$color)

pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/SA_no_residential_",Sys.Date(),"_staterandom.pdf"),width=23,height=30)
par(mfrow=c(6, 8))
for (i in 1:length(unique(result$state))){

  result_state = subset(result,result$state==unique(result$state)[i])

  tabletext=cbind(c(paste("Movement at",unique(result$state)[i]),"Work"," "," "," "," "," "," "," "," ",
                    "Grocery and Pharmacy"," "," "," "," "," "," "," "," ",
                    "Retail and recreation"," "," "," "," "," "," "," "," ",
                    "Transit"," "," "," "," "," "," "," "," ",
                    "Parks"," "," "," "," "," "," "," "," "),
                  c("Lag Week",
                    "0","1","2","3","4","5","6","7","8"
                    ,"0","1","2","3","4","5","6","7","8"
                    ,"0","1","2","3","4","5","6","7","8"
                    ,"0","1","2","3","4","5","6","7","8"
                    ,"0","1","2","3","4","5","6","7","8"),
                  c("IRR [95% CI]",paste0(format(round(result_state$effect,2),nsmall=2)," ","[",format(round(result_state$lower,2),nsmall=2),",",format(round(result_state$upper,2),nsmall=2),"]")))

  data = structure(list(
    mean=c(NA,result_state$effect),
    lower=c(NA,result_state$lower),
    upper=c(NA,result_state$upper),
    week= c(NA,result_state$color),
    .Names=c("mean","lower","upper","week"),row.names=c(NA,-11L),class="data.frame"))

  fn = local({
    i=0
    no_lines=sum(!is.na(data$color))
    b_clrs=c("black","black")[match(result_state$color,c("0","1"))]
    l_clrs=c("black","black")[match(result_state$color,c("0","1"))]

    function(...,clr.line,clr.marker){
      i<<-i+1
      fpDrawNormalCI(...,clr.line=l_clrs[i],clr.marker=b_clrs[i])
    }
  })
  library(forestplot)
  forestplot::forestplot(tabletext,fn.ci_norm=fn,xlim=c(0.5,1.3),
                         mean = data$mean,lower=data$lower,upper=data$upper,
                         hrzl_lines=list("2"=gpar(lwd=3,lty="solid",lineend="butt",col="black"),
                                         "11"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                         "20"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                         "29"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                         "38"=gpar(lwd=10,lineend="butt",col="#99999922"),
                                         "47"=gpar(lwd=10,lineend="butt",col="#99999922")),
                         zero = 1,cex = 2,lineheight = "auto",
                         colour = data$week,
                         boxsize=0.1,
                         graphwidth = unit(230, "mm"),
                         txt_gp = fpTxtGp(ticks=gpar(fontfamily="",cex=2),xlab=gpar(fontfamily="",cex=3),
                                          label=gpar(fontfamily="",cex=2)),
                         lwd.ci=2,
                         grid=T,
                         is.summary=c(TRUE,rep(FALSE,45),TRUE),
                         xlab = "Incidence Rate Ratio",
                         col=fpColors(box="royalblue",line=c("red","blue")[match(result_state$week,c("0","1"))],summary="lightblue"))
}
dev.off()


######### Residential Only
x <- c("residential")
result = result[which(result$movement=="residential"),]
result$weeklag = as.numeric(as.character(result$weeklag))
# result$week = ifelse(result$weeklag<4,0,1)
# result$week = as.factor(result$week)
result = result[which(result$movement=="residential"),]
result$color = ifelse(result$lower>1,1,0)
result$color = as.factor(result$color)

pdf(paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/SA_residential_",Sys.Date(),"_staterandom.pdf"),width=18,height=5)
par(mfrow=c(6, 8))
for (i in 1:length(unique(result$state))){

  result_state = subset(result,result$state==unique(result$state)[i])

  tabletext=cbind(c(paste("Movement at",unique(result$state)[i]),"Residential"," "," "," "," "," "," "," "," "),
                c("Lag Week",
                  "0","1","2","3","4","5","6","7","8"),
                c("IRR [95% CI]",paste0(format(round(result_state$effect,2),nsmall=2)," ","[",format(round(result_state$lower,2),nsmall=2),",",format(round(result_state$upper,2),nsmall=2),"]")))

  data = structure(list(
    mean=c(NA,result_state$effect),
    lower=c(NA,result_state$lower),
    upper=c(NA,result_state$upper),
    week= c(NA,result_state$color),
    .Names=c("mean","lower","upper","week"),row.names=c(NA,-11L),class="data.frame"))

  fn = local({
    i=0
    no_lines=sum(!is.na(data$color))
    b_clrs=c("black","black")[match(result_state$color,c("0","1"))]
    l_clrs=c("black","black")[match(result_state$color,c("0","1"))]

    function(...,clr.line,clr.marker){
     i<<-i+1
      fpDrawNormalCI(...,clr.line=l_clrs[i],clr.marker=b_clrs[i])
    }
  })

  forestplot::forestplot(tabletext,fn.ci_norm=fn,xlim=c(0,8),
                       mean = data$mean,lower=data$lower,upper=data$upper,
                       zero = 1,cex = 2,lineheight = "auto",
                       colour = data$week,
                       boxsize=0.2,
                       graphwidth = unit(230, "mm"),
                       txt_gp = fpTxtGp(ticks=gpar(fontfamily="",cex=1),xlab=gpar(fontfamily="",cex=1),
                                        label=gpar(fontfamily="",cex=1)),
                       lwd.ci=2,
                       grid=T,
                       is.summary=c(TRUE,rep(FALSE,42),TRUE),
                       col=fpColors(box="royalblue",line=c("red","blue")[match(result_state$week,c("0","1"))],summary="lightblue"))
}
dev.off()


#### Independent variable coefficients for weekly DLM models
ef <- function(x) exp(x)

syoung_perc <- scale(mydat$young_perc)
smedian_perc <- scale(mydat$median_perc)
sold_perc <- scale(mydat$old_perc)
spoverty_perc <- scale(mydat$poverty_perc)
sBlack_perc <- scale(mydat$Black.perc)
sHispanic <- scale(mydat$Hispanic.or.Latino.perc)
stemp <- scale(mydat$temp)
sBachelor <- scale(mydat$Bachelor_orhigher_perc)
sobese <-  scale(mydat$obese_perc)

write.csv(as.data.frame(cbind(
  rbind(inla.emarginal(exp, model1$marginals.fixed[[4]]),
        inla.emarginal(exp, model1$marginals.fixed[[5]]),
        inla.emarginal(exp, model1$marginals.fixed[[6]]),
        inla.emarginal(exp, model1$marginals.fixed[[7]]),
        inla.emarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*model1$marginals.fixed[[8]]),
        inla.emarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*model1$marginals.fixed[[9]]),
        inla.emarginal(function(x) exp(x/attributes(sold_perc)[[3]]), 0.01*model1$marginals.fixed[[10]]),
        inla.emarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]), 0.01*model1$marginals.fixed[[11]]),
        inla.emarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]),0.01*model1$marginals.fixed[[12]]),
        inla.emarginal(function(x) exp(x/attributes(sHispanic)[[3]]),0.01*model1$marginals.fixed[[13]]),
        inla.emarginal(function(x) exp(x/attributes(stemp)[[3]]), model1$marginals.fixed[[14]]),
        inla.emarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*model1$marginals.fixed[[15]]),
        inla.emarginal(function(x) exp(x/attributes(sobese)[[3]]), 0.01*model1$marginals.fixed[[16]])),

  rbind(paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[4]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[4]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[5]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[1]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.575), inla.tmarginal(exp, model1$marginals.fixed[[6]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[6]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[7]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(exp, model1$marginals.fixed[[7]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*model1$marginals.fixed[[8]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(syoung_perc)[[3]]), 0.01*model1$marginals.fixed[[8]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*model1$marginals.fixed[[9]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(smedian_perc)[[3]]), 0.01*model1$marginals.fixed[[9]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sold_perc)[[3]]), 0.01*model1$marginals.fixed[[10]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sold_perc)[[3]]),0.01*model1$marginals.fixed[[10]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]), 0.01*model1$marginals.fixed[[11]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(spoverty_perc)[[3]]),0.01*model1$marginals.fixed[[11]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]), 0.01*model1$marginals.fixed[[12]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBlack_perc)[[3]]),0.01*model1$marginals.fixed[[12]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sHispanic)[[3]]), 0.01*model1$marginals.fixed[[13]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sHispanic)[[3]]), 0.01*model1$marginals.fixed[[13]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(stemp)[[3]]), model1$marginals.fixed[[14]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(stemp)[[3]]), model1$marginals.fixed[[14]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*model1$marginals.fixed[[15]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sBachelor)[[3]]), 0.01*model1$marginals.fixed[[15]]))[2],2),"]"),
        paste0("[",round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sobese)[[3]]), 0.01*model1$marginals.fixed[[16]]))[1],2),",",
               round(inla.qmarginal(c(0.025,0.975), inla.tmarginal(function(x) exp(x/attributes(sobese)[[3]]), 0.01*model1$marginals.fixed[[16]]))[2],2),"]")))),
  paste0("C:/Users/sn233/Dropbox (Personal)/covid19_paper/tables_figures/INLA_weekly/staterandomeffect_DLM/supplement1_weeklyDLM_BS_",movement_component[m],".csv"))


