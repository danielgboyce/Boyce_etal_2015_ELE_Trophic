library(ggplot2)
library(gplots)
library(plotrix)
library(car)
library(MuMIn)
library(reshape2)
library(IDPmisc)
library(gplots)
library(ggplot2)
library(Hmisc)
library(matlab)
library(spatial)
library(geoR)
library(rpart)
library(RColorBrewer)
library(psych)
library(vegan)
library(perturb)
library(fields)
library(nlme)
library(lmmfit)
library(lme4)
library(car)
library(mgcv)
require(reshape)
library(MASS)
library(beanplot)
library(corrplot)
library(fossil)
library(plotrix)
library(gmt)
library(plyr)
library(akima)


#################################################################

#################################################################
currdir<-(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:/Users/copepod/Documents/aalldocuments/literature/postdoc_2013/global_trophic','C:/Users/sailfish/Documents/aalldocuments/literature/postdoc_2013/global_trophic'))
setwd(currdir)
data<-read.csv('mapping_database_june2015.csv',header=TRUE)
#data<-read.csv('mapping_database.csv')
data$yspan<-data$end.year-data$start.year
data<-subset(data,TL.diff2<2 & yspan>=5)
data$TLmax3<-round(data$TL.max2,digits=0)


dat<-subset(data,select=c("Location2",
  "Longitude",
  "Latitude",
  "references",
  "correlation",
  "n",
  "start.year",
  "end.year",
  "TL.min2",
  "TL.max2",
  "TL.diff2",
  "ID.TL.min2",
  "ID.TL.max2",
  "Spatial",
  "Trophic.aggregation",
  "Omnivory",
  "Bathymetry",
  "Dist",
  "newarea10",
  "newarea5",
  "newarea1",
  "oc",
  "marea",
  "SSTSLOPE",
  "dist.sstslope",
  "OXY.S",
  "dist.oxy",
  "chlswf",
  "dist.chlswf",
  "diam",
  "AllTaxa",
  "AllNorm",
  "CoastNorm",
  "OceanNorm",
  "Coral",
  "Cetacean",
  "Pinniped",
  "Mangrove",
  "Seagrass",
  "Squid",
  "CoasFishCK",
  "NonOcShark",
  "NonSqCeph",
  "TunaBllfsh",
  "OceanShark",
  "Euphausiid",
  "dist.div",
  "hlth",
  "dist.hlth",
  "poll",
  "dist.poll",
  "uv",
  "dist.uv",
  "npp",
  "dist.npp",
  "pp.chl",
  "hsst",
  "hsstsd",
  "dist.hsst",
  "sst85",
  "dist.sstv",
  "OI",
  "hr.ag",
  "dist.hr.ag",
  "zoop",
  "dist.zoop"))

names(dat)<-c("Location",
  "Longitude",
  "Latitude",
  "Reference",
  "Correlation",
  "N",
  "Baseline_Year",
  "Terminal_Year",
  "TL_Prey",
  "TL_Predator",
  "TL_Separation",
  "Prey_ID",
  "Predator_ID",
  "Spatial_Extent",
  "Trophic_Aggregation",
  "Predator_Omnivory",
  "Bathymetry",
  "Coast_Distance",
  "Cell10",
  "Cell5",
  "Cell1",
  "Ocean",
  "Ocean_Basin",
  "Temperature_Rate",
  "Dist_Temperature_Rate",
  "Oxygen_Stress",
  "Dist_Oxygen_Stress",
  "Chlorophyll",
  "Dist_Chlorophyll",
  "Phyto_Cell_Diameter",
  "Diversity_All_Taxa",
  "Diversity_All_Normalized",
  "Diversity_Coast_Normalized",
  "Diversity_Ocean_Normalized",
  "Diversity_Coral",
  "Diversity_Cetacean",
  "Diversity_Pinniped",
  "Diversity_Mangrove",
  "Diversity_Seagrass",
  "Diversity_Squid",
  "Diversity_Coastal_Fish",
  "Diversity_NonOceanic_Shark",
  "Diversity_NonSquid_Cephalopod",
  "Diversity_Tuna_Billfish",
  "Diversity_Oceanic_Shark",
  "Diversity_Euphausiid",
  "Dist_Diversity",
  "HII",
  "Dist_HII",
  "Pollution",
  "Dist_Pollution",
  "UV",
  "Dist_UV",
  "NPP",
  "Dist_NPP",
  "PP_Turnover",
  "Hadley_Temperature_200m",
  "Hadley_Temperature_200m_SD",
  "Dist_Hadley",
  "SST_Velocity85",
  "Dist_SST_Velocity85",
  "Ecosystem_Omnivory",
  "Exploitation_Aggregate",
  "Dist_Exploitation_Aggregate",
  "Zooplankton",
  "Dist_Zooplankton")


dat$Exploitation_Aggregate<-ifelse(dat$Dist_Exploitation_Aggregate>1000,NA,dat$Exploitation_Aggregate)


dat<-subset(dat,select=c("Location",
  "Longitude",
  "Latitude",
  "Reference",
  "Correlation",
  "N",
  "Baseline_Year",
  "Terminal_Year",
  "TL_Prey",
  "TL_Predator",
  "TL_Separation",
  "Prey_ID",
  "Predator_ID",
  "Spatial_Extent",
  "Trophic_Aggregation",
  "Predator_Omnivory",
  "Bathymetry",
  "Coast_Distance",
  "Cell10",
  "Cell5",
  "Cell1",
  "Ocean",
  "Temperature_Rate",
  "Oxygen_Stress",
  "Chlorophyll",
  "Phyto_Cell_Diameter",
  "Diversity_All_Taxa",
  "Diversity_All_Normalized",
  "Diversity_Coast_Normalized",
  "Diversity_Ocean_Normalized",
  "Diversity_Coral",
  "Diversity_Cetacean",
  "Diversity_Pinniped",
  "Diversity_Mangrove",
  "Diversity_Seagrass",
  "Diversity_Squid",
  "Diversity_Coastal_Fish",
  "Diversity_NonOceanic_Shark",
  "Diversity_NonSquid_Cephalopod",
  "Diversity_Tuna_Billfish",
  "Diversity_Oceanic_Shark",
  "Diversity_Euphausiid",
  "HII",
  "Pollution",
  "UV",
  "NPP",
  "PP_Turnover",
  "Hadley_Temperature_200m",
  "Hadley_Temperature_200m_SD",
  "SST_Velocity85",
  "Ecosystem_Omnivory",
  "Exploitation_Aggregate",
  "Zooplankton"))

dat$Reference<-tolower(dat$Reference)
dat$Location<-tolower(dat$Location)



metad<-data.frame(
  NOTES=c('Description of variables in datafile for "Boyce, D.G., Frank, K.T., Worm, B., Leggett, W.C. (2015) Spatial patterns and predictors of trophic control in marine ecosystems. Ecology Letters". Detailed description of the source of the data and how they were processed can be found in the paper and Supporting Information. Missing values (NAs) denote where biophysical measurements were unavailable or where they were located >1000 m from the associated correlation.',NA),
  Location=c('Geographic location where the trophic interaction took place',NA),
  Longitude=c(NA,NA),
  Latitude=c(NA,NA),
  Reference=c('Citation for study',NA),
  Correlation=c('Pearson correlation between timeseries of predator and prey',NA),
  N=c('Number of measurements (years) used to calculate correlation',NA),
  Baseline_Year=c('First year of the time-series',NA),
  Terminal_Year=c('Last year of the time-series',NA),
  TL_Prey=c('Estimated fractional trophic level of the prey',NA),
  TL_Predator=c('Estimated fractional trophic level of predator',NA),
  TL_Separation=c('Difference between predator and prey fractional trophic levels',NA),
  Prey_ID=c('Taxonomic identity of the prey',NA),
  Predator_ID=c('Taxonomic Identity of the predator',NA),
  Spatial_Extent=c('Geographic extent over which the study was conducted','Categorical; 1-5: 1 is station, 5 is ocean'),
  Trophic_Aggregation=c('Denotes if predator and/or prey time-series were aggregated by species, or functional groups','1=species; 2=mixed; 3=functional groups'),
  Predator_Omnivory=c('Estimated omnivory index for the predator',NA),
  Bathymetry=c('Bathymetry where the trophic interaction took place','Meters'),
  Coast_Distance=c('Distance of the trophic interaction from the nearest coastline','Kms'),
  Cell10=c('Identifier for 10 degree cell',NA),
  Cell5=c('Identifier for 5 degree cell',NA),
  Cell1=c('Identifier for 1 degree cell',NA),
  Ocean=c('Identifier for ocean where trophic interaction took place',NA),
  Temperature_Rate=c('Estimated rate of sea surface temperature change over time (1950-2010)','Degrees per year'),
  Oxygen_Stress=c('Estimated oxygen stress',NA),
  Chlorophyll=c('Chlorophyll concentration','mg m3'),
  Phyto_Cell_Diameter=c('Average phytoplankton cell diameter','um'),
  Diversity_All_Taxa=c('Species diversity for all available taxa','Number'),
  Diversity_All_Normalized=c('Normalized species diversity for all available taxa',NA),
  Diversity_Coast_Normalized=c('Normalized species diversity for all available coastal taxa',NA),
  Diversity_Ocean_Normalized=c('Normalized species diversity for all available oceanic taxa',NA),
  Diversity_Coral=c('Coral species diversity','Number'),
  Diversity_Cetacean=c('Cetacean species diversity','Number'),
  Diversity_Pinniped=c('Pinniped species diversity','Number'),
  Diversity_Mangrove=c('Mangrove species diversity','Number'),
  Diversity_Seagrass=c('Seagrass species diversity','Number'),
  Diversity_Squid=c('Squid species diversity','Number'),
  Diversity_Coastal_Fish=c('Species diversity for all coastal fish','Number'),
  Diversity_NonOceanic_Shark=c('Non-oceanic shark species diversity','Number'),
  Diversity_NonSquid_Cephalopod=c('Non-squid cephalopod species diversity','Number'),
  Diversity_Tuna_Billfish=c('Tuna and billfish species diversity','Number'),
  Diversity_Oceanic_Shark=c('Oceanic shark species diversity','Number'),
  Diversity_Euphausiid=c('Euphausiid species diversity','Number'),
  HII=c('Human impact index',NA),
  Pollution=c('Pollution index',NA),
  UV=c('UV index','J m2'),
  NPP=c('Net primary production','mg C m2 D'),
  PP_Turnover=c('Turnover rate of primary production','mg Chl m3/ mg C m2 d'),
  Hadley_Temperature_200m=c('Average temperature in the upper 200 meters','Degrees C'),
  Hadley_Temperature_200m_SD=c('Average standard deviation of temperature in the upper 200 meters','Degrees C'),
  SST_Velocity85=c('Velocity of sea surface temperature change','yrs'),
  Ecosystem_Omnivory=c('Average ecosystem omnivory',NA),
  Exploitation_Aggregate=c('Fishery exploitation rate','Proportion'),
  Zooplankton=c('Zooplankton concentration','mg C m3'))

metad<-data.frame(t(metad))
metad$Variable<-rownames(metad)
names(metad)<-c('Description','Units','Variable')
metad<-subset(metad,select=c('Variable','Description','Units'))
rownames(metad)<-NULL


setwd('C:/Users/sailfish/Documents/aalldocuments/literature/postdoc_2013/global_trophic/code/code_for_SI')
write.csv(dat,'Boyce_etal_2015_ELE_database.csv',row.names=FALSE)
write.csv(metad,'Boyce_etal_2015_ELE_database_README.csv',row.names=FALSE)





##FIGURE 2: PLOTS OUT STRONGEST UNIVARIATE PREDICTORS USING BLACK/WHITE POINTS
#####################################
#PLOTS OUT STRONGEST UNIVARIATE PREDICTORS AGAINST CORRELATIONS
d<-subset(data,select=c('hsst','correlation','n','TLmax3'))
dm<-2
lbl<-c('Temperature','Correlation')

n<-modf2(subset(data,select=c('NonOcShark','correlation','n','TLmax3')),2,c('Shark diversity','Correlation','Partial correlation'))
d<-subset(data,select=c('NonOcShark','correlation','n','TLmax3'))
dm<-2
lbl<-c('Shark diversity','Correlation','Partial correlation')

#PLOTS OUT UNIVARIATE EFFECTS AND MIMINAL ADEQUATE MODEL PARTIAL RESIDUALS
modf2<-function(d,dm,lbl){
kk<-5
nm<-names(d)[1]
names(d)<-c('x','y','n','tl')
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se)
pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se)
pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.3),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8,lwd=.1)
abline(h=0,lty=2,col='black',lwd=.2)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2,col='dodgerblue4')
#palette(rich.colors(32))
cx<-.6
palette(gray(seq(0,.95,length.out=10)))
points(d$x,d$y,ylim=c(-1,1),pch=16,col=rescale(d$n,newrange=c(1,32)),cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
pv<-round(s$coef[1,4],digits=4)
pv<-ifelse(pv<.0001,'<0.0001',paste('= ',pv))
#legend('topleft',legend=c(paste('r2 = ',r2),paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)
cl<-palette(gray(seq(0,.95,length.out=10)))
m<-gam(y~s(x,k=kk),data=d,weights=d$n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lty=2,lwd=.2)

out<-data.frame(x=pdat$x,p1=pdat$p)

legend('topleft',legend=c(paste('P = ',pv),paste('n = ',dim(d)[1]),paste('r2 = ',r2)),bty='n',cex=.5,text.col='dodgerblue4',title='Univariate',title.col='dodgerblue4')
s<-summary(mamod)
r2m<-round(s$r.squared,digits=2)
pm<-round(s$coef[1,4],digits=4)
pm<-ifelse(pm<.0001,'<0.0001',paste('= ',pm))
legend('top',legend=c(paste('P = ',pm),paste('n = ',dim(d)[1])),bty='n',cex=.5,title='Multivariate',text.col='firebrick3')
#legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)

if(nm=='hsst'){md<-update(mamod,~. -I(hsst^2))
p<-predict(mamod,newdata=data.frame(hsst=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),NonOcShark=median(data$NonOcShark),TL.max2=3),se.fit=TRUE)}
if(nm=='NonOcShark'){md<-update(mamod,~. -I(NonOcShark^2))
p<-predict(mamod,newdata=data.frame(NonOcShark=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),hsst=median(data$hsst),TL.max2=3),se.fit=TRUE)}
if(nm=='TL.max2'){md<-mamod
p<-predict(mamod,newdata=data.frame(TL.max2=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),NonOcShark=median(data$NonOcShark),TL.max2=3,hsst=median(data$hsst)),se.fit=TRUE)}
r<-data.frame(residuals(md,type='partial'))
#r<-data.frame(residuals(mamod,type='partial'))
pr<-subset(r,select=c(nm))
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
lines(pdat$x,pdat$p,lwd=.2,col='firebrick3')
names(pr)<-c('pr')

pr<-data.frame(x=d$x,pr=pr$pr,n=d$n)
plot(d$x,pr$pr,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[3],cex.axis=.8)
abline(h=0,lty=2,col='black',lwd=.2)
palette(gray(seq(0,.95,length.out=10)))
cx<-.6
m<-gam(pr~s(x,k=kk),data=pr,weights=n)
pdat$upr<-pdat$p+(1.96*pdat$se)
pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se)
pdat$lwr99<-pdat$p-(2.58*pdat$se)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
points(d$x,pr$pr,ylim=c(-1,1),pch=16,col=rescale(d$n,newrange=c(1,32)),cex=cx)
points(d$x,pr$pr,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
image.plot(legend.only=TRUE,zlim=c(5,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.19,.955))
s<-summary(md)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)
out$p2<-pdat$p
return(out)
}

d<-subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag'))
dm<-1
lbl<-c('Median exploitation rate','Correlation','Partial correlation')
l<-2
ll<-0

modf3<-function(d,dm,lbl,l,ll){
kk<-4
nm<-names(d)[1]
names(d)<-c('x','y','n','tl','oi','dist')
if(nm=='hr.ag'){d<-subset(d,is.na(x)==FALSE & dist<1000)}
if(nm!='hr.ag'){d<-subset(d,is.na(x)==FALSE)}
d2<-subset(d,is.na(x)==FALSE & dist<1000 & is.na(oi)==FALSE)
if(l==1){d$x<-log10(d$x); d2$x<-log10(d2$x)}
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
if(nm=='hr.ag'){xlm<-c(0,.9)}
if(nm!='hr.ag'){xlm<-c(min(d$x,na.rm=TRUE),max(d$x,na.rm=TRUE))}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se);pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se);pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.3),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8,xlim=xlm,lwd=.5)
abline(h=0,lty=2,col='black',lwd=.2)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2,col='dodgerblue4')
palette(gray(seq(0,.95,length.out=10)))
cx<-.6
points(d$x,d$y,ylim=c(-1,1),pch=16,col=d$n,cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
pv<-round(s$coef[1,4],digits=4)
pv<-ifelse(pv<=.0001,'<0.0001',paste('= ',pv))
n<-dim(d)[1]
cl<-palette(gray(seq(0,.95,length.out=10)))
if(ll!=0){legend('topright',legend=ll,bty='n',cex=.8)}
m<-gam(y~s(x,k=kk),data=d,weights=d$n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lty=3)
out<-data.frame(x=pdat$x,p=pdat$p)

if(nm!='hr.ag'){
x2<-seq(min(d2$x),max(d2$x),length.out=100)
md<-mamodred
r<-data.frame(residuals(md,type='partial'))
pr<-subset(r,select=c(nm))
names(pr)<-c('pr')
pr<-data.frame(x=d2$x,pr=pr$pr,n=d2$n)
p<-predict(md,newdata=data.frame(OI=x2,NonOcShark=median(data$NonOcShark),TL.max2=3),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
lines(pdat$x,pdat$p,lwd=.2,col='firebrick3')

legend('topleft',legend=c(paste('P = ',pv),paste('n = ',dim(d)[1]),paste('r2 = ',r2)),bty='n',cex=.5,text.col='dodgerblue4',title='Univariate',title.col='dodgerblue4')
s<-summary(mamodred)
r2m<-round(s$r.squared,digits=2)
pm<-round(s$coef[5,4],digits=4)
pm<-ifelse(pm<.0001,'<0.0001',paste('= ',pm))
legend('top',legend=c(paste('P = ',pm),paste('n = ',dim(d2)[1])),bty='n',cex=.5,title='Multivariate',text.col='firebrick3')

plot(d2$x,pr$pr,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[3],cex.axis=.8,lwd=.1)
abline(h=0,lty=2,col='black',lwd=.2)
palette(gray(seq(0,.95,length.out=10)))
cx<-.6
#m<-gam(pr~s(x,k=kk),data=pr,weights=n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lwd=.2)
pdat$upr<-pdat$p+(1.96*pdat$se);pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se);pdat$lwr99<-pdat$p-(2.58*pdat$se)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
points(d2$x,pr$pr,ylim=c(-1,1),pch=16,col=rescale(d2$n,newrange=c(1,32)),cex=cx)
points(d2$x,pr$pr,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
image.plot(legend.only=TRUE,zlim=c(0,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.19,.955))
s<-summary(mamodred)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d2)[1])),bty='n',cex=.5)
out$p2<-pdat$p
}else NULL
#return(out)
}

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('scatterplots3.ps',width=3.5,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.4,.6,0))
psst<-modf2(subset(data,select=c('hsst','correlation','n','TLmax3')),2,c('Temperature','Correlation','Partial correlation'))
n<-modf2(subset(data,select=c('NonOcShark','correlation','n','TLmax3')),2,c('Shark diversity','Correlation','Partial correlation'))
#modf2(subset(data,select=c('AllNorm','correlation','n','TLmax3')),2,c('Species diversity','Correlation','Partial correlation'))
n<-modf2(subset(data,select=c('TL.max2','correlation','n','TLmax3')),1,c('Consumer trophic level','Correlation','Partial correlation'))
n<-modf3(subset(data,select=c('OI','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Ecosystem omnivory index','Correlation','Partial correlation'),0,0)
#modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Log[Median exploitation rate]','Correlation','Partial correlation'),1,0)
dev.off()

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('scatterplots_fish_tl2.ps',width=3.5,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.4,.6,0))
modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Median exploitation rate','Correlation','Partial correlation'),2,0)
dev.off()




#######################
#FIGURE 1 (BARPLOT); MAKES BARPLOT SUMMARIZING THE CORRELATIONS DATABASE
dat<-data
dat$bin<-round(floor(dat$TL.max2),digits=0)
dat$bin<-ifelse(dat$TL.max2<=2.5,2,NA)
dat$bin<-ifelse(dat$TL.max2<=3 & dat$TL.max2>2.5,3,dat$bin)
dat$bin<-ifelse(dat$TL.max2<=3.5 & dat$TL.max2>3,4,dat$bin)
dat$bin<-ifelse(dat$TL.max2<=4 & dat$TL.max2>3.5,5,dat$bin)
dat$bin<-ifelse(dat$TL.max2<=4.5 & dat$TL.max2>4,6,dat$bin)
dat$bin<-ifelse(dat$TL.max2>4.5,7,dat$bin)
tl<-data.frame(nm=sort(unique(dat$bin)),n=tapply(dat$TL.max2,dat$bin,length),lbl='Predator trophic level',cl=colors()[555],lbl2=c('2 - 2.5','2.5 - 3','3 - 3.5','3.5 - 4','4 - 4.5','4.5 - 5'))
tl$p<-tl$n/dim(dat)[1]

dat$bin<-round(ceiling(dat$Latitude/10),digits=0)
lt<-data.frame(nm=sort(unique(dat$bin)),n=tapply(dat$TL.max2,dat$bin,length),lbl='Latitude',cl=colors()[91],lbl2=c('20 - 30','30 - 40','40 - 50','50 - 60','60 - 70','70 - 80'))
lt$p<-lt$n/dim(dat)[1]

dat$bin<-round(ceiling(dat$Dist/200),digits=0)
dst<-data.frame(nm=sort(unique(dat$bin)),n=tapply(dat$TL.max2,dat$bin,length),lbl='Distance from coastline',cl=colors()[142],lbl2=c('0 - 200','200 - 400','400 - 600','600 - 800','800 - 1000','1000 - 1200'))
dst$p<-dst$n/dim(dat)[1]


spt<-data.frame(nm=sort(unique(dat$Spatial)),n=tapply(dat$TL.max2,dat$Spatial,length),lbl='Spatial extent',cl=colors()[258],lbl2=c('Fixed station','<5 degree','<10 degree','Sea','> Sea'))
spt$p<-spt$n/dim(dat)[1]

dat$bin<-round(ceiling(dat$yspan/10),digits=0)
yrs<-data.frame(nm=sort(unique(dat$bin)),n=unlist(tapply(dat$TL.max2,dat$bin,length)),lbl='Time-series length',cl=colors()[132],lbl2=c('0 - 10','10 - 20','20 - 30','30 - 40','40 - 50'))
yrs$p<-yrs$n/dim(dat)[1]


dat$bin<-round(ceiling((dat$start.year-1900)/10),digits=0)
syr<-data.frame(nm=sort(unique(dat$bin)),n=unlist(tapply(dat$TL.max2,dat$bin,length)),lbl='Initial year',cl=colors()[458],lbl2=c('1950 - 1959','1960 - 1969','1970 - 1979','1980 - 1989','1990 - 1999','2000 - 2009'))
syr$p<-syr$n/dim(dat)[1]

dum<-rbind(tl,lt,spt,dst,syr,yrs)
dum$mvar<-seq(1,dim(dum)[1],1)
dum<-dum[order(dum$mvar,decreasing=TRUE),]

setwd(figsdir)
postscript('barplot_summary.ps',width=5,height=4,family=fnt,pointsize=ptsz)
par(mar=c(4,8,.5,.5))
barplot2(dum$p,names.arg=dum$lbl2,horiz=TRUE,las=1,col=as.character(dum$cl),border='lightgray',xlim=c(0,.8),space=.3,cex.names=.65,xaxt='n',xlab='Proportion',plot.grid=TRUE,grid.lty='solid',grid.col='lightgray',grid.lwd=.5)
axis(side=1,at=seq(0,.8,.2),labels=TRUE,lwd=.1,cex=.75)
l1<--.15
segments(x0=l1,x1=l1,y0=0.5,y1=7-.2,col=as.character(colors()[132]),lwd=2.5,xpd=TRUE)#blue
segments(x0=l1,x1=l1,y0=7+.2,y1=15-.2,col=as.character(colors()[458]),lwd=2.5,xpd=TRUE)#pink
segments(x0=l1,x1=l1,y0=15+.2,y1=22-.2,col=as.character(colors()[142]),lwd=2.5,xpd=TRUE)#yellow
segments(x0=l1,x1=l1,y0=22+.2,y1=29-.2,col=as.character(colors()[258]),lwd=2.5,xpd=TRUE)#green
segments(x0=l1,x1=l1,y0=29+.2,y1=37-.2,col=as.character(colors()[91]),lwd=2.5,xpd=TRUE)#orange
segments(x0=l1,x1=l1,y0=37+.2,y1=44-.2,col=as.character(colors()[555]),lwd=2.5,xpd=TRUE)#red
dev.off()




######################################
###FIGURE 3 (SCATTERPLOT); SHOWS WITHIN AND ACROSS STUDY DIFFERENCES IN TEMPERATURE EFFECT
dum<-data.frame(ref=sort(unique(data$references)),n=summary(factor(data$references)))
dum2<-subset(dum,n>=5 & !(ref %in% c('Johannesen et al. 2012','Miller et al. 2011')))#TAKES OUT NON SPATIAL

setwd(figsdir)
ptsz<-9;fnt<-'Times'
postscript('correlations_by_study.ps',height=6,width=7,font=fnt,pointsize=ptsz)
par(mfrow=c(3,3),mar=c(3,3,1,0),oma=c(1,1,1,1))

nm<-dum2$ref
for(i in 1:length(nm)){
d<-subset(data,references==nm[i])
plot(d$hsst,d$correlation,xlim=c(0,14),las=1,pch=16,main=nm[i],lwd=.1)
#mod<-gam(correlation~hsst,data=d,gamma=1.4,weights=d$n)
mod<-lm(correlation~hsst,data=d,weights=d$n)
x<-seq(min(d$hsst),max(d$hsst),length.out=100)
p<-predict(mod,newdata=data.frame(hsst=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(d)[1]),bty='n')
}

dat2<-subset(data,(references %in% dum2$ref))#TAKES OUT NON SPATIAL
plot(dat2$hsst,dat2$correlation,xlim=c(0,22),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='Above studies combined')
mod<-lm(correlation~hsst + I(hsst^2),data=dat2,weights=dat2$n)
x<-seq(min(dat2$hsst),max(dat2$hsst),length.out=100)
p<-predict(mod,newdata=data.frame(hsst=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(dat2)[1]),bty='n')

dat<-subset(data,!(references %in% dum2$ref))#TAKES OUT NON SPATIAL
plot(dat$hsst,dat$correlation,xlim=c(0,22),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='Excluding above studies')
mod<-lm(correlation~hsst + I(hsst^2),data=dat,weights=dat$n)
x<-seq(min(dat$hsst),max(dat$hsst),length.out=100)
p<-predict(mod,newdata=data.frame(hsst=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(dat)[1]),bty='n')

plot(data$hsst,data$correlation,xlim=c(0,22),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='All available studies combined')
mod<-lm(correlation~hsst + I(hsst^2),data=data,weights=data$n)
x<-seq(min(data$hsst),max(data$hsst),length.out=100)
p<-predict(mod,newdata=data.frame(hsst=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(data)[1]),bty='n')
dev.off()



#FIGURE FOR SI
setwd(figsdir)
ptsz<-9;fnt<-'Times'
postscript('correlations_by_study_diversity.ps',height=6,width=7,font=fnt,pointsize=ptsz)
par(mfrow=c(3,3),mar=c(3,3,1,0),oma=c(1,1,1,1))

par(mfrow=c(3,3),mar=c(3,3,1,0),oma=c(1,1,1,1))
nm<-dum2$ref
for(i in 1:length(nm)){
d<-subset(data,references==nm[i])
plot(d$NonOcShark,d$correlation,las=1,pch=16,main=nm[i],lwd=.1,xlim=c(0,55))
mod<-lm(correlation~NonOcShark,data=d,weights=d$n)
x<-seq(min(d$NonOcShark),max(d$NonOcShark),length.out=100)
p<-predict(mod,newdata=data.frame(NonOcShark=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(d)[1]),bty='n')
}

dat2<-subset(data,(references %in% dum2$ref))#TAKES OUT NON SPATIAL
plot(dat2$NonOcShark,dat2$correlation,xlim=c(0,60),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='Above studies combined')
mod<-lm(correlation~NonOcShark + I(NonOcShark^2),data=dat2,weights=dat2$n)
x<-seq(min(dat2$NonOcShark),max(dat2$NonOcShark),length.out=100)
p<-predict(mod,newdata=data.frame(NonOcShark=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(dat2)[1]),bty='n')

dat<-subset(data,!(references %in% dum2$ref))#TAKES OUT NON SPATIAL
plot(dat$NonOcShark,dat$correlation,xlim=c(0,60),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='Excluding above studies')
mod<-lm(correlation~NonOcShark + I(NonOcShark^2),data=dat,weights=dat$n)
x<-seq(min(dat$NonOcShark),max(dat$NonOcShark),length.out=100)
p<-predict(mod,newdata=data.frame(NonOcShark=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(dat)[1]),bty='n')

plot(data$NonOcShark,data$correlation,xlim=c(0,60),las=1,ylim=c(-1,1),pch=16,lwd=.1,main='All available studies combined')
mod<-lm(correlation~NonOcShark + I(NonOcShark^2),data=data,weights=data$n)
x<-seq(min(data$NonOcShark),max(data$NonOcShark),length.out=100)
p<-predict(mod,newdata=data.frame(NonOcShark=x),se.fit=TRUE)
lines(x,p$fit,lwd=.1)
s<-summary(mod)
pv<-ifelse(s$coef[2,4]<.0001,'<0.0001',round(s$coef[2,4],digits=4))
legend('topleft',legend=paste('n = ',dim(data)[1]),bty='n')
dev.off()




###################################################
#FIGURE 4 (IMAGE SCATTERPLOTS); TAKES MINIMAL ADEQUATE MODEL AND EXTRAPOLATES TO UNSAMPLED AREAS; NEEDS THE MODELS TO BE IN WD
setwd(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:/Users/copepod/Documents/aalldocuments/literature/postdoc_2013/global_trophic','C:/Users/sailfish/Documents/aalldocuments/literature/postdoc_2013/global_trophic'))
pred<-read.csv('global_covariates.csv',header=TRUE)
crds<-read.table('1deg.bathy.txt',sep='\t',header=FALSE,col.names=c('Longitude','Latitude','Bathymetry'))
crds2<-read.table('1deg.dist.global.txt',sep='\t',header=FALSE,col.names=c('Longitude','Latitude','Distance','lon','lat'))
crds2<-subset(crds2,select=c('Longitude','Latitude','Distance'))
pred<-merge(pred,crds,by=c('Longitude','Latitude'),all=FALSE)
pred<-merge(pred,crds2,by=c('Longitude','Latitude'),all=FALSE)
pred$mvar<-seq(1,dim(pred)[1],1)

f<-function(dat){
print(unique(dat$mvar))
d<-subset(dat,npp>0 & diam>0 & pp.chl>0 & chl<50 & diam<500)
d<-subset(d,select=c('Longitude','Latitude','npp','diam','Shark','sst','lnpp','ldiam','Shark','sst','acid','pp.chl','lpp.chl'))
d<-na.omit(d)
if(dim(d)[1]>0){p<-predict(model2,newdata=data.frame(hsst=d$sst,pp.chl=d$lpp.chl,diam=d$ldiam,NonOcShark=d$Shark),se.fit=TRUE)
dat$p<-p$fit
dat$se<-p$se.fit}
if(dim(d)[1]==0){dat$p<-NA; dat$se<-NA}
return(dat)
}
out<-ddply(pred,.(mvar),.fun=f)
write.csv(out,'out.csv',row.names=FALSE)


f<-function(dat){
print(unique(dat$mvar))
d<-subset(dat,npp>0 & diam>0 & pp.chl>0 & chl<50 & diam<500 & npp>min(data$npp) & npp<max(data$npp) & sst>min(data$hsst) & sst<max(data$hsst) & Shark>min(data$NonOcShark) & Shark<max(data$NonOcShark) & pp.chl>min(data$pp.chl) & pp.chl<max(data$pp.chl) & diam>min(data$diam) & diam<max(data$diam))
d<-subset(d,select=c('Longitude','Latitude','npp','diam','Shark','sst','lnpp','ldiam','Shark','sst','acid','pp.chl','lpp.chl'))
d<-na.omit(d)
if(dim(d)[1]>0){p<-predict(model2,newdata=data.frame(hsst=d$sst,pp.chl=d$lpp.chl,diam=d$ldiam,NonOcShark=d$Shark,TL.max2=3),se.fit=TRUE,type='response')
dat$p<-p$fit
dat$se<-p$se.fit}
if(dim(d)[1]==0){dat$p<-NA; dat$se<-NA}
return(dat)
}
outt<-ddply(pred,.(mvar),.fun=f)
write.csv(outt,'outt.csv',row.names=FALSE)


f<-function(dat){
print(unique(dat$mvar))
d<-subset(dat,npp>0 & diam>0 & pp.chl>0 & chl<50 & diam<500 & npp>min(data$npp) & npp<max(data$npp) & sst>min(data$hsst) & sst<max(data$hsst) & Shark>min(data$NonOcShark) & Shark<max(data$NonOcShark) & pp.chl>min(data$pp.chl) & pp.chl<max(data$pp.chl) & diam>min(data$diam) & diam<max(data$diam))
d<-subset(d,select=c('Longitude','Latitude','npp','diam','Shark','sst','lnpp','ldiam','Shark','sst','acid','pp.chl','lpp.chl'))
d<-na.omit(d)
if(dim(d)[1]>0){p<-predict(model2,newdata=data.frame(hsst=d$sst,pp.chl=d$lpp.chl,diam=d$ldiam,NonOcShark=d$Shark,TL.max2=4),se.fit=TRUE,type='response')
dat$p<-p$fit
dat$se<-p$se.fit}
if(dim(d)[1]==0){dat$p<-NA; dat$se<-NA}
return(dat)
}
outt4<-ddply(pred,.(mvar),.fun=f)
write.csv(outt4,'outt4.csv',row.names=FALSE)

out<-read.csv('out.csv',header=TRUE)
outt<-read.csv('outt.csv',header=TRUE)
outt4<-read.csv('outt4.csv',header=TRUE)

setwd(figsdir)
ptsz<-9;fnt<-'Times'
postscript('extrapolated.scatterplots.ps',height=6,width=5,font=fnt,pointsize=ptsz)
par(mfrow=c(3,2),mar=c(2.5,2.5,.5,0),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(out,select=c('Latitude','p')),c(-2,1),'Latitude')
f1(subset(out,select=c('sst','p')),c(-2,1),'SST')
f1(subset(out,select=c('Shark','p')),c(-2,1),'Shark diversity')
f1(subset(out,select=c('ldiam','p')),c(-2,1),'Log[phytoplankton cell diameter]')
f1(subset(out,select=c('lpp.chl','p')),c(-2,1),'Log[phytoplankton turnover]')
f1(subset(out,select=c('Bathymetry','p')),c(-2,1),'Bathymetry')
dev.off()

setwd(currdir)
outt<-read.csv('outt.csv',header=TRUE)
outa<-read.csv('out.csv',header=TRUE)

outt<-subset(outt,is.na(p)==FALSE)
outt$Bathymetry<-ifelse(outt$Bathymetry>0,0,outt$Bathymetry)
outt$Bathymetry<-abs(outt$Bathymetry)
source('Deriv.r')


out<-outt
outt<-subset(outt,is.na(p)==FALSE)
plot(outt$sst,rescale(outt$p,newrange=c(-1,1)),pch=16,col='gray50',las=1,xlab='Temperature',ylab='Relative magnitude')
abline(h=0,lty=2,lwd=.1)
points(outt$sst,rescale(outt$p,newrange=c(-1,1)),pch=1,lwd=.001)

f<-function(d){
names(d)<-c('x','y')
d$y<-rescale(d$y,newrange=c(-1,1))
mod<-gam(y~s(x,k=10),data=d)
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
out<-data.frame(sst=x2,p=p$fit,up=p$fit+(1.96*p$se.fit),dn=p$fit-(1.96*p$se.fit))
return(out)}
pshark<-f(subset(outt,select=c('sst','Shark')))
ppp.chl<-f(subset(outt,select=c('sst','lpp.chl')))
pdiam<-f(subset(outt,select=c('sst','ldiam')))

lines(ppp.chl$sst,ppp.chl$p,lwd=2,col='green3')
lines(pshark$sst,pshark$p,lwd=2,col='red3')
lines(pdiam$sst,pdiam$p,lwd=2,col='orange')
legend('topleft',legend=c('Trophic control','Turnover','Non-oceanic shark diversity','Cell diameter'),col=c('gray50','green3','red3','orange'),lwd=c(NA,2,2,2),pch=c(16,NA,NA,NA),bty='n')

f<-function(d){
names(d)<-c('x','y')
#d$y<-rescale(d$y,newrange=c(-1,1))
mod<-gam(y~s(x,k=4),data=d)
x2<-seq(min(d$x,na.rm=TRUE),max(d$x,na.rm=TRUE),length.out=500)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
out<-data.frame(sst=x2,p=p$fit,up=p$fit+(1.96*p$se.fit),dn=p$fit-(1.96*p$se.fit))
return(out)}

pshark2<-f(subset(outa,select=c('sst','Shark')))
ppp.chl2<-f(subset(outa,select=c('sst','lpp.chl')))
pdiam2<-f(subset(outa,select=c('sst','ldiam')))
psst2<-f(subset(outa,is.na(p)==FALSE,select=c('sst','p')))
psst2<-f(subset(outa,select=c('sst','p2')))
dum2<-data.frame(sst=pshark2$sst,sst2=psst2$sst,pshark=pshark2$p,ppp.chl=ppp.chl2$p,pdiam=pdiam2$p,psst2=psst2$p)

dum2$mvar<-seq(1,dim(dum2)[1],1)
d<-subset(dum2,sst>min(data$hsst) & sst<max(data$hsst) & pshark>min(data$NonOcShark) & pshark<max(data$NonOcShark) & ppp.chl>min(log10(data$pp.chl)) & ppp.chl<max(log10(data$pp.chl)) & pdiam>min(log10(data$diam)) & pdiam<max(log10(data$diam)))
dum2$p2<-ifelse(dum2$mvar %in% d$mvar,dum2$psst2,NA)
dum<-dum2

ptsz<-7;fnt<-'Times'
postscript('prac.ps',height=4,width=7,font=fnt,pointsize=ptsz)
par(mar=c(4,14,1,1))
lw=.5
plot(dum$sst,dum$pshark,pch=16,cex=1,ylim=c(0,30),axes=FALSE,xlim=c(-5,30),ylab='',xlab='Temperature',type='l',lwd=.1)
points(dum$sst,dum$pshark,pch=16,cex=rescale(abs(dum$p2),newrange=c(.4,2)),col=ifelse(dum$p2>0,'chartreuse2','darkgreen'),xlim=c(0,20))
axis(side=1,at=seq(-5,30,5),lwd=lw)
axis(side=2,at=seq(0,30,5),col='darkgreen',las=1,line=0,lwd=lw)
a<-subset(dum,is.na(p2)==FALSE)
abline(v=min(a$sst),lty=2,lwd=.4)
abline(v=max(a$sst),lty=2,lwd=.4)
legend(8,4,legend=c('','',''),col=c('lightblue','darksalmon','chartreuse2'),pch=16,bty='n',pt.cex=1.5,cex=.75)
legend(11,4,legend=c('   Phytoplankton cell diameter','   Turnover','   Non-oceanic shark diversity'),col=c('darkblue','darkred','darkgreen'),pch=16,bty='n',pt.cex=1.5,cex=.75)
text(8,5,'Resource\n control',cex=.8)
text(11,5,'Consumer\n control',cex=.8)
text(10,30,'Domain of trophic database')

par(new=TRUE)
plot(dum$sst,dum$ppp.chl,pch=16,ylim=c(2.5,3.5),axes=FALSE,xlim=c(-5,30),ylab='',xlab='',type='l',lwd=.1)
points(dum$sst,dum$ppp.chl,pch=16,cex=rescale(abs(dum$p2),newrange=c(.4,2)),col=ifelse(dum$psst>0,'darksalmon','darkred'),ylim=c(2.75,3.5),xlim=c(0,20))
axis(side=2,at=seq(2.5,3.5,.25),col='darkred',las=1,line=5,lwd=lw)
par(new=TRUE)
plot(dum$sst,dum$pdiam,pch=16,xlim=c(-5,30),ylim=c(-.5,1.5),ylab='',xlab='',type='l',axes=FALSE,lwd=.1)
points(dum$sst,dum$pdiam,pch=16,cex=rescale(abs(dum$p2),newrange=c(.4,2)),col=ifelse(dum$psst>0,'lightblue','darkblue'))
axis(side=2,at=seq(-.5,1.5,.25),col='darkblue',las=1,line=10,lwd=lw)
mtext('Non-oceanic shark diversity',side=2,line=2.5,col='black',cex=1)
mtext('Log[primary producer turnover]',side=2,line=8,col='black',cex=1)
mtext('Log[phytoplankton cell diameter]',side=2,line=13,col='black',cex=1)
dev.off()



f<-function(d){
names(d)<-c('x','y')
out<-data.frame(sst=x2,p=p$fit,up=p$fit+(1.96*p$se.fit),dn=p$fit-(1.96*p$se.fit))
plot(outt$p,outt$Shark)
points(outt$p,outt$ldiam,col='red')

f1<-function(d,ylm,xlb){
nm<-names(d)[1]
cl1<-'lightblue';cl2<-'royalblue';lw<-.25
pxs<-1;cx<-.35;kn<-4
d<-na.omit(d)
names(d)<-c('x','y','se','lon','lat')
if(nm=='dDistance'){d$x<-sqrt(d$x)}
xlm<-c(min(d$x,na.rm=T),max(d$x,na.rm=T))
ylm<-ylm
plot(0,0,xlim=xlm,ylim=ylm,las=1,cex=cx,ylab='Correlation',xlab=xlb,type='n',lwd=.01)
#cl<-colorRampPalette(c('dodgerblue4','yellow2','orange','orangered','darkred'))
cl<-colorRampPalette(c('gray80','gray70','gray60','gray50','gray40','gray30','gray20','gray10','black'))
Image(d$x,d$y,pixs=pxs,colramp=cl)
abline(h=0,lty=3,lwd=.75)
#m2<-gamm(y~s(x,k=7),data=d,correlation=corGaus(form = ~ lon+lat))
#m2<-gam(y~s(x,k=7)+s(lon,lat),data=d,weights=1/d$se)
m2<-gam(y~s(x,k=7),data=d,weights=1/d$se)
xx<-seq(min(d$x),max(d$x),length.out=500)
p2<-predict(m2,newdata=data.frame(x=xx))
lines(xx,p2,col='black',lwd=2)
m2.d <- Deriv(m2, n = 500)
CI <- confint(m2.d, alpha = 0.00001)
S <- signifD(p2, m2.d$x$deriv, CI$x$upper, CI$x$lower, eval = 0)
lines(S$decr ~ xx, col = "red3",lwd=2)
lines(S$incr ~ xx, col = "red3",lwd=2)
#cl<-colorRampPalette(c('dodgerblue4','yellow2','orange','orangered','darkred'))
#cl<-designer.colors(20,c('dodgerblue4','yellow2','orange','orangered','darkred'))
cl<-designer.colors(20,c('gray80','gray70','gray60','gray50','gray40','gray30','gray20','gray10','black'))
image.plot(legend.only=TRUE,zlim=c(1,10),col=cl,legend.lab="Number of measurements",horizontal=FALSE,smallplot=c(.97,.99,.19,.96))
}

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('extrapolated.scatterplots.truncated3.ps',height=4.5,width=3.25,family=fnt,pointsize=ptsz)
par(mfrow=c(2,1),mar=c(2.5,2.5,.5,1),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
f1(subset(outt,select=c('Distance','p','se','Distance','Latitude')),c(-1.1,1),'Distance')
dev.off()

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('extrapolated.scatterplots.truncated2.ps',height=4.5,width=3.25,family=fnt,pointsize=ptsz)
par(mfrow=c(2,1),mar=c(2.5,2.5,.5,1),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
#f1(subset(outt,select=c('Distance','p','se','Distance','Latitude')),c(-1.1,1),'Distance')
f1(subset(outt,select=c('Bathymetry','p','se','Longitude','Latitude')),c(-1.1,1),'Bathymetry')
dev.off()

setwd(figsdir)
postscript('extrapolated.scatterplots.truncated.all.ps',height=6,width=5,family=fnt,pointsize=ptsz)
par(mfrow=c(3,2),mar=c(2.5,2.5,.5,0),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
f1(subset(outt,select=c('sst','p','se','Longitude','Latitude')),c(-1.1,1),'SST')
f1(subset(outt,select=c('Shark','p','se','Longitude','Latitude')),c(-1.1,1),'Shark diversity')
f1(subset(outt,select=c('ldiam','p','se','Longitude','Latitude')),c(-1.1,1),'Log[phytoplankton cell diameter]')
f1(subset(outt,select=c('lpp.chl','p','se','Longitude','Latitude')),c(-1.1,1),'Log[phytoplankton turnover]')
f1(subset(outt,select=c('Bathymetry','p','se','Longitude','Latitude')),c(-1.1,1),'Bathymetry')
dev.off()


#####################################################
##SAME AS ABOVE BUT REPLACED COLOURS WITH BLACK AND WHITE
setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('extrapolated.scatterplots.truncated.new.ps',height=4.5,width=3.25,family=fnt,pointsize=ptsz)
par(mfrow=c(2,1),mar=c(2.5,2.5,.5,1),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
f1(subset(outt,select=c('Distance','p','se','Distance','Latitude')),c(-1.1,1),'Distance')
dev.off()

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('extrapolated.scatterplots.truncated.new2.ps',height=4.5,width=3.25,family=fnt,pointsize=ptsz)
par(mfrow=c(2,1),mar=c(2.5,2.5,.5,1),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
#f1(subset(outt,select=c('Distance','p','se','Distance','Latitude')),c(-1.1,1),'Distance')
f1(subset(outt,select=c('Bathymetry','p','se','Longitude','Latitude')),c(-1.1,1),'Bathymetry')
dev.off()

setwd(figsdir)
postscript('extrapolated.scatterplots.truncated.all.new.ps',height=6,width=5,family=fnt,pointsize=ptsz)
par(mfrow=c(3,2),mar=c(2.5,2.5,.5,0),oma=c(1,1,1,1),mgp=c(1.25,.35,0),tcl=-.25,cex.axis=.8)
f1(subset(outt,select=c('Latitude','p','se','Longitude','Latitude')),c(-1.1,1),'Latitude')
f1(subset(outt,select=c('sst','p','se','Longitude','Latitude')),c(-1.1,1),'SST')
f1(subset(outt,select=c('Shark','p','se','Longitude','Latitude')),c(-1.1,1),'Shark diversity')
f1(subset(outt,select=c('ldiam','p','se','Longitude','Latitude')),c(-1.1,1),'Log[phytoplankton cell diameter]')
f1(subset(outt,select=c('lpp.chl','p','se','Longitude','Latitude')),c(-1.1,1),'Log[phytoplankton turnover]')
f1(subset(outt,select=c('Bathymetry','p','se','Longitude','Latitude')),c(-1.1,1),'Bathymetry')
dev.off()




############################
#SI FIG (OUTPUTS FOR GMT)
f<-function(dat){
print(unique(dat$mvar))
d<-subset(dat,npp>0 & diam>0 & pp.chl>0 & chl<50 & diam<500 & npp>min(data$npp) & npp<max(data$npp) & sst>min(data$hsst) & sst<max(data$hsst) & Shark>min(data$NonOcShark) & Shark<max(data$NonOcShark) & pp.chl>min(data$pp.chl) & pp.chl<max(data$pp.chl) & diam>min(data$diam) & diam<max(data$diam))
d<-subset(d,select=c('Longitude','Latitude','npp','diam','Shark','sst','lnpp','ldiam','Shark','sst','acid','pp.chl','lpp.chl'))
lpp.chl<-log10(median(data$pp.chl))
ldiam<-log10(median(data$diam))
Shark<-median(data$NonOcShark)

d<-na.omit(d)
if(dim(d)[1]>0){p<-predict(mamod,newdata=data.frame(hsst=d$sst,pp.chl=lpp.chl,diam=ldiam,NonOcShark=Shark,TL.max2=3),se.fit=TRUE,type='response')
dat$p<-p$fit
dat$se<-p$se.fit}
if(dim(d)[1]==0){dat$p<-NA; dat$se<-NA}
return(dat)
}
outt.sst<-ddply(pred,.(mvar),.fun=f)
write.csv(outt.sst,'outt.sst.csv',row.names=FALSE)




#####################################
#FIGURE 2 (SCATTERPLOTS) BUT WITH COLOURS INSTEAD OF BW; PLOTS OUT STRONGEST UNIVARIATE PREDICTORS AGAINST CORRELATIONS
modf2<-function(d,dm,lbl){
names(d)<-c('x','y','n','tl')
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se)
pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se)
pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8)
abline(h=0,lty=3,col='gray40')
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
palette(rich.colors(32))
cx<-.6
points(d$x,d$y,ylim=c(-1,1),pch=16,col=rescale(d$n,newrange=c(1,32)),cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topleft',legend=c(paste('r2 = ',r2),paste('P value',p)),bty='n',cex=.5)
cl<-palette(rich.colors(32))
image.plot(legend.only=TRUE,zlim=c(5,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.25,.955))
box()
}


modf3<-function(d,dm,lbl,l,ll){
nm<-names(d)[1]
names(d)<-c('x','y','n','tl','dist')
d<-subset(d,is.na(x)==FALSE & dist<1000)
if(l==1){d$x<-log10(d$x)}
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
if(nm=='hr.ag'){xlm<-c(0,1)}
if(nm!='hr.ag'){xlm<-c(min(d$x,na.rm=TRUE),max(d$x,na.rm=TRUE))}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se);pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se);pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8,xlim=xlm)
abline(h=0,lty=3,col='gray40')
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
palette(rich.colors(max(d$n)))
cx<-.6
points(d$x,d$y,ylim=c(-1,1),pch=16,col=d$n,cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<=.0001,'<0.0001',paste('= ',p))
n<-dim(d)[1]
legend('topleft',legend=c(paste('r2 = ',r2),paste('P value',p)),bty='n',cex=.5)
cl<-palette(rich.colors(32))
image.plot(legend.only=TRUE,zlim=c(5,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.2,.955))
if(ll!=0){legend('topright',legend=ll,bty='n',cex=.8)}
box()
}

setwd(figsdir)
postscript('scatterplots_fish_tl.ps',width=2.75,height=7)
par(mfrow=c(4,1),mar=c(2.75,4,.5,1),mgp=c(1.5,.6,0))
modf3(subset(data,select=c('hr.ag','correlation','yspan','TL.max2','dist.hr.ag')),1,c('Aggregate exploitation rate','Correlation'),2,c('All trophic levels'))
modf3(subset(data,TL.max2>4,select=c('hr.ag','correlation','yspan','TL.max2','dist.hr.ag')),2,c('Log[Aggregate exploitation rate]','Correlation'),1,c('Consumer TL above 4'))#quad
modf3(subset(data,TL.max2>3 & TL.max2<=4,select=c('hr.ag','correlation','yspan','TL.max2','dist.hr.ag')),1,c('Log[Aggregate exploitation rate]','Correlation'),1,c('Consumer TL 3 to 4'))
modf3(subset(data,TL.max2>2 & TL.max2<=3,select=c('hr.ag','correlation','yspan','TL.max2','dist.hr.ag')),1,c('Log[Aggregate exploitation rate]','Correlation'),1,c('Consumer TL less than 3'))
dev.off()

setwd(figsdir)
postscript('scatterplots.ps',width=2.5,height=14)
par(mfrow=c(6,1),mar=c(2.5,6,.5,1),mgp=c(1.4,.6,0))
modf2(subset(data,select=c('hsst','correlation','n','TLmax3')),2,c('Temperature','Correlation'))
modf2(subset(data,select=c('NonOcShark','correlation','n','TLmax3')),2,c('Shark diversity','Correlation'))
modf2(subset(data,select=c('AllNorm','correlation','n','TLmax3')),2,c('Species diversity','Correlation'))
#modf2(subset(data,select=c('acid','correlation','n','TLmax3')),1,c('Acidity index','Correlation'))
modf2(subset(data,select=c('TL.max2','correlation','n','TLmax3')),1,c('Consumer trophic level','Correlation'))
modf3(subset(data,select=c('OI','correlation','yspan','TLmax3','dist.npp')),1,c('Ecosystem omnivory index','Correlation'),0,0)
modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','dist.hr.ag')),1,c('Log[Median exploitation rate]','Correlation'),1,0)
dev.off()



#####################################
#PLOTS OUT STRONGEST UNIVARIATE PREDICTORS AGAINST CORRELATIONS
d<-subset(data,select=c('hsst','correlation','n','TLmax3'))
dm<-2
lbl<-c('Temperature','Correlation')

#PLOTS OUT UNIVARIATE EFFECTS AND MIMINAL ADEQUATE MODEL PARTIAL RESIDUALS
modf2<-function(d,dm,lbl){
kk<-5
nm<-names(d)[1]
names(d)<-c('x','y','n','tl')
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se)
pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se)
pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.3),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8,lwd=.1)
abline(h=0,lty=2,col='black',lwd=.2)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2,col='dodgerblue4')
palette(rich.colors(32))
cx<-.6
points(d$x,d$y,ylim=c(-1,1),pch=16,col=rescale(d$n,newrange=c(1,32)),cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
pv<-round(s$coef[1,4],digits=4)
pv<-ifelse(pv<.0001,'<0.0001',paste('= ',pv))
#legend('topleft',legend=c(paste('r2 = ',r2),paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)
cl<-palette(rich.colors(32))
m<-gam(y~s(x,k=kk),data=d,weights=d$n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lty=2,lwd=.2)

out<-data.frame(x=pdat$x,p1=pdat$p)

legend('topleft',legend=c(paste('P = ',pv),paste('n = ',dim(d)[1]),paste('r2 = ',r2)),bty='n',cex=.5,text.col='dodgerblue4',title='Univariate',title.col='dodgerblue4')
s<-summary(mamod)
r2m<-round(s$r.squared,digits=2)
pm<-round(s$coef[1,4],digits=4)
pm<-ifelse(pm<.0001,'<0.0001',paste('= ',pm))
legend('top',legend=c(paste('P = ',pm),paste('n = ',dim(d)[1])),bty='n',cex=.5,title='Multivariate',text.col='firebrick3')
#legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)

if(nm=='hsst'){md<-update(mamod,~. -I(hsst^2))
p<-predict(mamod,newdata=data.frame(hsst=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),NonOcShark=median(data$NonOcShark),TL.max2=3),se.fit=TRUE)}
if(nm=='NonOcShark'){md<-update(mamod,~. -I(NonOcShark^2))
p<-predict(mamod,newdata=data.frame(NonOcShark=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),hsst=median(data$hsst),TL.max2=3),se.fit=TRUE)}
if(nm=='TL.max2'){md<-mamod
p<-predict(mamod,newdata=data.frame(TL.max2=x2,pp.chl=median(log10(data$pp.chl)),diam=median(log10(data$diam)),NonOcShark=median(data$NonOcShark),TL.max2=3,hsst=median(data$hsst)),se.fit=TRUE)}
r<-data.frame(residuals(md,type='partial'))
#r<-data.frame(residuals(mamod,type='partial'))
pr<-subset(r,select=c(nm))
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
lines(pdat$x,pdat$p,lwd=.2,col='firebrick3')
names(pr)<-c('pr')
pr<-data.frame(x=d$x,pr=pr$pr,n=d$n)
plot(d$x,pr$pr,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[3],cex.axis=.8)
abline(h=0,lty=2,col='black',lwd=.2)
palette(rich.colors(32))
cx<-.6
m<-gam(pr~s(x,k=kk),data=pr,weights=n)
pdat$upr<-pdat$p+(1.96*pdat$se)
pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se)
pdat$lwr99<-pdat$p-(2.58*pdat$se)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
points(d$x,pr$pr,ylim=c(-1,1),pch=16,col=rescale(d$n,newrange=c(1,32)),cex=cx)
points(d$x,pr$pr,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
image.plot(legend.only=TRUE,zlim=c(5,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.19,.955))
s<-summary(md)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d)[1])),bty='n',cex=.5)
out$p2<-pdat$p
return(out)
}

d<-subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag'))
dm<-1
lbl<-c('Median exploitation rate','Correlation','Partial correlation')
l<-2
ll<-0

modf3<-function(d,dm,lbl,l,ll){
kk<-4
nm<-names(d)[1]
names(d)<-c('x','y','n','tl','oi','dist')
if(nm=='hr.ag'){d<-subset(d,is.na(x)==FALSE & dist<1000)}
if(nm!='hr.ag'){d<-subset(d,is.na(x)==FALSE)}
d2<-subset(d,is.na(x)==FALSE & dist<1000 & is.na(oi)==FALSE)
if(l==1){d$x<-log10(d$x); d2$x<-log10(d2$x)}
if(dm==1){mod<-lm(y~x,weights=n,data=d)}
if(dm==2){mod<-lm(y~x+I(x^2),weights=n,data=d)}
if(dm==3){mod<-lm(y~x+I(x^2)+I(x^3),weights=n,data=d)}
if(nm=='hr.ag'){xlm<-c(0,.9)}
if(nm!='hr.ag'){xlm<-c(min(d$x,na.rm=TRUE),max(d$x,na.rm=TRUE))}
x2<-seq(min(d$x),max(d$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=x2),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
pdat$upr<-pdat$p+(1.96*pdat$se);pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se);pdat$lwr99<-pdat$p-(2.58*pdat$se)
plot(d$x,d$y,ylim=c(-1,1.3),cex=.1,las=1,xlab=lbl[1],ylab=lbl[2],cex.axis=.8,xlim=xlm,lwd=.5)
abline(h=0,lty=2,col='black',lwd=.2)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2,col='dodgerblue4')
palette(rich.colors(max(d$n)))
cx<-.6
points(d$x,d$y,ylim=c(-1,1),pch=16,col=d$n,cex=cx)
points(d$x,d$y,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
s<-summary(mod)
r2<-round(s$r.squared,digits=2)
pv<-round(s$coef[1,4],digits=4)
pv<-ifelse(pv<=.0001,'<0.0001',paste('= ',pv))
n<-dim(d)[1]
cl<-palette(rich.colors(32))
if(ll!=0){legend('topright',legend=ll,bty='n',cex=.8)}
m<-gam(y~s(x,k=kk),data=d,weights=d$n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lty=3)
out<-data.frame(x=pdat$x,p=pdat$p)

if(nm!='hr.ag'){
x2<-seq(min(d2$x),max(d2$x),length.out=100)
md<-mamodred
r<-data.frame(residuals(md,type='partial'))
pr<-subset(r,select=c(nm))
names(pr)<-c('pr')
pr<-data.frame(x=d2$x,pr=pr$pr,n=d2$n)
p<-predict(md,newdata=data.frame(OI=x2,NonOcShark=median(data$NonOcShark),TL.max2=3),se.fit=TRUE)
pdat<-data.frame(x=x2,p=p$fit,se=p$se.fit)
lines(pdat$x,pdat$p,lwd=.2,col='firebrick3')

legend('topleft',legend=c(paste('P = ',pv),paste('n = ',dim(d)[1]),paste('r2 = ',r2)),bty='n',cex=.5,text.col='dodgerblue4',title='Univariate',title.col='dodgerblue4')
s<-summary(mamodred)
r2m<-round(s$r.squared,digits=2)
pm<-round(s$coef[5,4],digits=4)
pm<-ifelse(pm<.0001,'<0.0001',paste('= ',pm))
legend('top',legend=c(paste('P = ',pm),paste('n = ',dim(d2)[1])),bty='n',cex=.5,title='Multivariate',text.col='firebrick3')

plot(d2$x,pr$pr,ylim=c(-1,1.2),cex=.1,las=1,xlab=lbl[1],ylab=lbl[3],cex.axis=.8,lwd=.1)
abline(h=0,lty=2,col='black',lwd=.2)
palette(rich.colors(32))
cx<-.6
#m<-gam(pr~s(x,k=kk),data=pr,weights=n)
#lines(x2,predict(m,newdata=data.frame(x=x2)),lwd=.2)
pdat$upr<-pdat$p+(1.96*pdat$se);pdat$lwr<-pdat$p-(1.96*pdat$se)
pdat$upr99<-pdat$p+(2.58*pdat$se);pdat$lwr99<-pdat$p-(2.58*pdat$se)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$x,pdat$x[length(pdat$x):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
lines(pdat$x,pdat$p,lwd=.2)
points(d2$x,pr$pr,ylim=c(-1,1),pch=16,col=rescale(d2$n,newrange=c(1,32)),cex=cx)
points(d2$x,pr$pr,ylim=c(-1,1),col='gray40',cex=cx,lwd=.05)
image.plot(legend.only=TRUE,zlim=c(0,54),col=cl,legend.lab="Length of time-series",horizontal=FALSE,smallplot=c(.97,.99,.19,.955))
s<-summary(mamodred)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topleft',legend=c(paste('P = ',p),paste('n = ',dim(d2)[1])),bty='n',cex=.5)
out$p2<-pdat$p
}else NULL
#return(out)
}

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('scatterplots2.ps',width=3.5,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.4,.6,0))
psst<-modf2(subset(data,select=c('hsst','correlation','n','TLmax3')),2,c('Temperature','Correlation','Partial correlation'))
n<-modf2(subset(data,select=c('NonOcShark','correlation','n','TLmax3')),2,c('Shark diversity','Correlation','Partial correlation'))
#modf2(subset(data,select=c('AllNorm','correlation','n','TLmax3')),2,c('Species diversity','Correlation','Partial correlation'))
n<-modf2(subset(data,select=c('TL.max2','correlation','n','TLmax3')),1,c('Consumer trophic level','Correlation','Partial correlation'))
n<-modf3(subset(data,select=c('OI','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Ecosystem omnivory index','Correlation','Partial correlation'),0,0)
#modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Log[Median exploitation rate]','Correlation','Partial correlation'),1,0)
dev.off()

setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('scatterplots_fish_tl.ps',width=3.5,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.4,.6,0))
modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Median exploitation rate','Correlation','Partial correlation'),2,0)
dev.off()



#############################################
#SI FIGURE (2D MODEL INTERRACTIONS AS IMAGE); 2 DIMENSIONAL INTERRACTION PLOTS
#INTERRACTION BETWEEN EXPLOITATION AND TROPHIC LEVEL
setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('2dplots_exploitation1.ps',width=4,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.6,.6,0))
plot(0,0,axes=FALSE,pch='.',xlab='',ylab='')

dat<-subset(data,dist.hr.ag<=1000)
dat$lhr.ag<-dat$hr.ag
mod2<-gam(correlation~s(lhr.ag,TL.max2,k=80,bs='ts'),dat=dat)
x1<-seq(min(dat$lhr.ag),max(dat$lhr.ag),length.out=150)
x2<-seq(min(dat$TL.max2),max(dat$TL.max2),length.out=150)
prc2<-expand.grid(lhr.ag=x1,TL.max2=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~TL.max2,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Exploitation rate',ylab='Consumer trophic level',las=1,cex.axis=.8)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4,nlevels=8)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.95),cex=.25)
box(lwd=.1)

plot(0,0,axes=FALSE,pch='.',xlab='',ylab='')

dat<-subset(data,dist.hr.ag<=1000)
dat$lhr.ag<-dat$hr.ag
mod2<-gam(correlation~s(lhr.ag,AllNorm,k=8,bs='ts'),dat=dat)
x1<-seq(min(dat$lhr.ag),max(dat$lhr.ag),length.out=150)
x2<-seq(min(dat$AllNorm),max(dat$AllNorm),length.out=150)
prc2<-expand.grid(lhr.ag=x1,AllNorm=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~AllNorm,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Exploitation rate',ylab='Species diversity',las=1,cex.axis=.8)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.95),cex=.25)
box(lwd=.1)

plot(0,0,axes=FALSE,pch='.',xlab='',ylab='')
dat<-subset(data,dist.hr.ag<=1000)
dat$lhr.ag<-dat$hr.ag
mod2<-gam(correlation~s(lhr.ag,NonOcShark,k=4,bs='ts'),dat=dat)
x1<-seq(min(dat$lhr.ag),max(dat$lhr.ag),length.out=150)
x2<-seq(min(dat$NonOcShark),max(dat$NonOcShark),length.out=150)
prc2<-expand.grid(lhr.ag=x1,NonOcShark=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~NonOcShark,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Exploitation rate',ylab='Coastal shark diversity',las=1,cex.axis=.8)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.95),cex=.25)
box(lwd=.1)
dev.off()




#INTERRACTION BETWEEN EXPLOITATION AND TROPHIC LEVEL
setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('2dplots_omnivory.ps',width=4,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.6,.6,0))
#modf3(subset(data,select=c('hr.ag','correlation','yspan','TLmax3','OI','dist.hr.ag')),1,c('Log[Median exploitation rate]','Correlation','Partial correlation'),1,0)

plot(0,0,axes=FALSE,pch='.',xlab='',ylab='')

dat<-subset(data,is.na(OI)==FALSE & is.na(hr.ag)==FALSE & dist.hr.ag<1000)
dat$hr.ag<-(dat$hr.ag)
mod2<-gam(correlation~s(OI,hr.ag,k=20),data=dat,gamma=1.4)
x1<-seq(min(dat$OI),max(dat$OI),length.out=150)
x2<-seq(min(dat$hr.ag),max(dat$hr.ag),length.out=150)
prc2<-expand.grid(OI=x1,hr.ag=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,OI~hr.ag,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),ylab='Exploitation rate',xlab='Omnivory index',las=1,cex.axis=.8)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.95),cex=.25)
box(lwd=.1)

plot(0,0,axes=FALSE,pch='.',xlab='',ylab='')

dat<-subset(data,is.na(OI)==FALSE & is.na(NonOcShark)==FALSE)
mod2<-gam(correlation~s(OI,NonOcShark,k=7),data=dat,gamma=1.4)
x1<-seq(min(dat$OI),max(dat$OI),length.out=150)
x2<-seq(min(dat$NonOcShark),max(dat$NonOcShark),length.out=150)
prc2<-expand.grid(OI=x1,NonOcShark=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,OI~NonOcShark,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),ylab='Non-oceanic shark diversity',xlab='Omnivory index',las=1,cex.axis=.8)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.95),cex=.25)
box(lwd=.1)
dev.off()




setwd(figsdir)
ptsz<-9;fnt<-'Helvetica'
postscript('2dplots_exploitation.ps',width=4,height=5,family=fnt,pointsize=ptsz)
par(mfrow=c(4,2),mar=c(2.75,2.5,.5,1),mgp=c(1.4,.6,0))
data$lhr.ag<-(data$hr.ag)
mod2<-gam(correlation~s(lhr.ag,TL.max2,k=85,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=150)
x2<-seq(min(data$TL.max2),max(data$TL.max2),length.out=150)
prc2<-expand.grid(lhr.ag=x1,TL.max2=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~TL.max2,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate]',ylab='Consumer trophic level',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
#image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()

data$lhr.ag4<-(data$hr.ag4)
mod2<-gam(correlation~s(lhr.ag4,TL.max2,k=25,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag4),max(data$lhr.ag4),length.out=150)
x2<-seq(min(data$TL.max2),max(data$TL.max2),length.out=150)
prc2<-expand.grid(lhr.ag4=x1,TL.max2=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag4~TL.max2,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate (TL>4)]',ylab='Consumer trophic level',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()

data$lhr.ag<-(data$hr.ag)
#mod2<-gam(correlation~s(lhr.ag,AllNorm,k=20,bs='ts'),data=data)
mod2<-gam(correlation~s(lhr.ag,AllNorm,k=10,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=150)
x2<-seq(min(data$AllNorm),max(data$AllNorm),length.out=150)
prc2<-expand.grid(lhr.ag=x1,AllNorm=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~AllNorm,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate]',ylab='Species diversity',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
#image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()

data$lhr.ag<-(data$hr.ag)
mod2<-gam(correlation~s(lhr.ag,NonOcShark,k=4,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=150)
x2<-seq(min(data$NonOcShark),max(data$NonOcShark),length.out=150)
prc2<-expand.grid(lhr.ag=x1,NonOcShark=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~NonOcShark,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate]',ylab='Species diversity',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()
dev.off()




setwd(figsdir)
postscript('diameter_turnover_TL.ps',width=4,height=5)
par(mfrow=c(2,1),mar=c(2.75,4,.5,1),mgp=c(1.5,.6,0))
data$ldiam<-log10(data$diam)
dat<-subset(data,correlation<0)
mod2<-gam(correlation~s(ldiam,TL.min2,k=60,bs='ts'),data=dat,weights=dat$n)
x1<-seq(min(dat$ldiam),max(dat$ldiam),length.out=150)
x2<-seq(min(dat$TL.min2),max(dat$TL.min2),length.out=150)
prc2<-expand.grid(ldiam=x1,TL.min2=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,ldiam~TL.min2,value.var="p")
cl<-palette(rich.colors(500));cl<-cl[1:250]
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=cl,xlab='Log[Phytoplankton cell diameter]',ylab='Consumer trophic level',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='firebrick3',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=cl,legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()

data$lpp.chl<-log10(data$pp.chl)
dat<-subset(data,correlation> -10)
mod2<-gam(correlation~s(lpp.chl,TL.min2,k=4,bs='ts'),data=dat,weights=dat$n)
x1<-seq(min(dat$lpp.chl),max(dat$lpp.chl),length.out=150)
x2<-seq(min(dat$TL.min2),max(dat$TL.min2),length.out=150)
prc2<-expand.grid(lpp.chl=x1,TL.min2=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lpp.chl~TL.min2,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Phytoplankton turnover]',ylab='Consumer trophic level',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='firebrick3',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()
dev.off()



setwd(figsdir)
postscript('temp_diversity_exploitation.ps',width=4,height=5)
par(mfrow=c(2,1),mar=c(2.75,4,.5,1),mgp=c(1.5,.6,0))
data$lhr.ag<-(data$hr.ag)
mod2<-gam(correlation~s(lhr.ag,hsst,k=50,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=150)
x2<-seq(min(data$hsst),max(data$hsst),length.out=150)
prc2<-expand.grid(lhr.ag=x1,hsst=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~hsst,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate]',ylab='Temperature',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='firebrick3',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()
#points(data$lhr.ag,data$hsst,pch=16)

data$lhr.ag<-(data$hr.ag)
#mod2<-gam(correlation~s(lhr.ag,AllNorm,k=20,bs='ts'),data=data)
mod2<-gam(correlation~s(lhr.ag,AllNorm,k=10,bs='ts'),data=data)
x1<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=150)
x2<-seq(min(data$AllNorm),max(data$AllNorm),length.out=150)
prc2<-expand.grid(lhr.ag=x1,AllNorm=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,lhr.ag~AllNorm,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='Log[Exploitation rate]',ylab='Species diversity',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='gray40',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()
#points(data$lhr.ag,data$AllNorm,pch=16)
dev.off()



###########################################
#SI FIGS (2D MODEL INTERRACTIONS); PLOTS INTERRACTION BETWEEN TEMP AND ALL SPECIES DIVERSITY
setwd(figsdir)
postscript('temperature_alldiversity.ps',width=4.5,height=6)
#pdf('temperature_diversity.pdf',width=4.5,height=6)
par(mfrow=c(3,1),mar=c(3,4,1,1),oma=c(1,6,1,6),mgp=c(2,.6,0))
mod2<-gam(correlation~s(hsst,AllNorm,k=80),data=data)
x1<-seq(min(data$hsst),max(data$hsst),length.out=150)
x2<-seq(min(data$AllNorm),max(data$AllNorm),length.out=150)
prc2<-expand.grid(hsst=x1,AllNorm=x2)
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,hsst~AllNorm,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='SST',ylab='Normalized diversity',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='firebrick3',labcex=.4)
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
box()

fn<-function(d,lbl){
n<-names(d)[2]
names(d)<-c('x','x2','y','n')
lw<-.1;cx<-.75
mod<-lm(y~x+I(x^2),d=d,weights=d$n)
dum<-data.frame(y=residuals(mod,type='response'),x2=d$x2,n=d$n)
if(n=='AllNorm'){dum$x3<-floor(dum$x2*10);dum$x3<-ifelse(dum$x3==0,.05,dum$x3)}
if(n=='hsst'){dum$x3<-floor(dum$x2/5)}
dm<-data.frame(X=sort(unique(dum$x3)),t=tapply(dum$y,dum$x3,mean),tsd=tapply(dum$y,dum$x3,sd),xx2=tapply(dum$x2,dum$x3,mean))
print(summary(dum))

mod2<-lm(y~x2+I(x2^2),d=dum,weights=dum$n)
plot(dum$x2,dum$y,pch=16,las=1,xlab=lbl[1],ylab=lbl[2],cex=cx,ylim=c(-1,1))
xx<-seq(min(dum$x2),max(dum$x2),length.out=100)
p<-predict(mod2,newd=data.frame(x2=xx),se.fit=TRUE)
pdat<-data.frame(xx=xx,p=p$fit,upr=p$fit+(1.96*p$se.fit),lwr=p$fit-(1.96*p$se.fit),upr99=p$fit+(2.58*p$se.fit),lwr99=p$fit-(2.58*p$se.fit))
abline(h=0,lty=3)
polygon(c(pdat$xx,pdat$xx[length(pdat$xx):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$xx,pdat$xx[length(pdat$xx):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
points(dum$x2,dum$y,pch=16,col='gray50',cex=cx)
points(dum$x2,dum$y,pch=1,lwd=lw,cex=cx)
f<-function(dd){
cl<-'gold3'
lines(c(dd$xx2,dd$xx2),c(dd$t+(dd$tsd),dd$t-(dd$tsd)),col=cl,lwd=1.5)
points(dd$xx2,dd$t,pch=16,col=cl,cex=1.25)
points(dd$xx2,dd$t,pch=1,cex=1.25,lwd=.1)}
z<-dlply(dm,.(xx2),.fun=f)
lines(xx,p$fit,lwd=lw)
s<-summary(mod2)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topright',legend=c(paste('r2 = ',r2),paste('P value',p)),bty='n',cex=.75)
}
fn(subset(data,select=c('AllNorm','hsst','correlation','yspan')),c('Temperature','Correlation \n (diversity effects removed)'))
fn(subset(data,select=c('hsst','AllNorm','correlation','yspan')),c('Normalized diversity','Temperature \n (temperature effects removed)'))
dev.off()



###########################################
#PLOTS INTERRACTION BETWEEN TEMP AND SHARK DIVERSITY
setwd(figsdir)
postscript('temperature_diversity.ps',width=4.5,height=6)
#pdf('temperature_diversity.pdf',width=4.5,height=6)
par(mfrow=c(3,1),mar=c(3,4,1,1),oma=c(1,6,1,6),mgp=c(2,.6,0))
#m<-gam(correlation~s(hsst,NonOcShark,k=50),data=data)
#vis.gam(m,view=c('hsst','NonOcShark'),plot.type='contour',too.far=0.1,color=rev('heat'),n.grid=100,contour.col='gray40',nCol=40,main='',xlab='Temperature',ylab='Non-oceanic shark diversity',lwd=.01,cex=.5,las=1)
#image.plot(legend.only=TRUE,zlim=c(-1,1),col=heat.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)
#box()

mod2<-gam(correlation~s(hsst,NonOcShark,k=80),data=data)
x1<-seq(min(data$hsst),max(data$hsst),length.out=100)
x2<-seq(min(data$NonOcShark),max(data$NonOcShark),length.out=100)
x3<-seq(min(data$lhr.ag),max(data$lhr.ag),length.out=100)
prc<-expand.grid(hsst=x1,NonOcShark=x2,lhr.ag=x3)
prc2<-subset(prc,select=c('hsst','NonOcShark'));prc2<-unique(prc2);prc2$lhr.ag<-rep(x3[1],dim(prc2)[1])#low exploitation
prc2$p<-predict(mod2,newdata=prc2)
dat<-acast(prc2,hsst~NonOcShark,value.var="p")
image(x=sort(unique(x1)),y=sort(unique(x2)),dat,col=palette(rich.colors(500)),xlab='SST',ylab='Shark diversity',las=1)
contour(x=sort(unique(x1)),y=sort(unique(x2)),dat,add=TRUE,lwd=.1,col='firebrick3',labcex=.4)
box()
image.plot(legend.only=TRUE,zlim=c(-1,1),col=rich.colors(100),legend.lab="",horizontal=FALSE,smallplot=c(.97,.99,.215,.935),cex=.25)

fn<-function(d,lbl){
n<-names(d)[2]
names(d)<-c('x','x2','y','n')
lw<-.1;cx<-.75
mod<-lm(y~x+I(x^2),d=d,weights=d$n)
dum<-data.frame(y=residuals(mod,type='response'),x2=d$x2,n=d$n)
if(n=='NonOcShark'){dum$x3<-floor(dum$x2/10)}
if(n=='hsst'){dum$x3<-floor(dum$x2/5)}
dm<-data.frame(X=sort(unique(dum$x3)),t=tapply(dum$y,dum$x3,mean),tsd=tapply(dum$y,dum$x3,sd),xx2=tapply(dum$x2,dum$x3,mean))

mod2<-lm(y~x2+I(x2^2),d=dum,weights=dum$n)
plot(dum$x2,dum$y,pch=16,las=1,xlab=lbl[1],ylab=lbl[2],cex=cx,ylim=c(-1,1))
xx<-seq(min(dum$x2),max(dum$x2),length.out=100)
p<-predict(mod2,newd=data.frame(x2=xx),se.fit=TRUE)
pdat<-data.frame(xx=xx,p=p$fit,upr=p$fit+(1.96*p$se.fit),lwr=p$fit-(1.96*p$se.fit),upr99=p$fit+(2.58*p$se.fit),lwr99=p$fit-(2.58*p$se.fit))
abline(h=0,lty=3)
polygon(c(pdat$xx,pdat$xx[length(pdat$xx):1]),c(pdat$upr99,pdat$lwr99[length(pdat$lwr99):1]),col='lightskyblue1',border=NA)
polygon(c(pdat$xx,pdat$xx[length(pdat$xx):1]),c(pdat$upr,pdat$lwr[length(pdat$lwr):1]),col='lightskyblue3',border=NA)
points(dum$x2,dum$y,pch=16,col='gray50',cex=cx)
points(dum$x2,dum$y,pch=1,lwd=lw,cex=cx)
f<-function(dd){
cl<-'gold3'
lines(c(dd$xx2,dd$xx2),c(dd$t+(dd$tsd),dd$t-(dd$tsd)),col=cl,lwd=1.5)
points(dd$xx2,dd$t,pch=16,col=cl,cex=1.25)
points(dd$xx2,dd$t,pch=1,cex=1.25,lwd=.1)}
z<-dlply(dm,.(xx2),.fun=f)
lines(xx,p$fit,lwd=lw)
s<-summary(mod2)
r2<-round(s$r.squared,digits=2)
p<-round(s$coef[1,4],digits=4)
p<-ifelse(p<.0001,'<0.0001',paste('= ',p))
legend('topright',legend=c(paste('r2 = ',r2),paste('P value',p)),bty='n',cex=.75)
}
fn(subset(data,select=c('NonOcShark','hsst','correlation','yspan')),c('Temperature','Correlation \n (diversity effects removed)'))
fn(subset(data,select=c('hsst','NonOcShark','correlation','yspan')),c('Non-oceanic shark diversity','Temperature \n (temperature effects removed)'))
dev.off()


#######################################
#BUNCH OF SUMMARY PLOTS IN SINGLE FILE
#FITS UNIVARIATE MODELS OF CORRELATION AS A FUNCTION OF TEMPERATURE TO 1) ALL DATA AND 2) DATA EXCLUDING OUTLIERS
setwd(figsdir)
postscript('summary_plots_all.ps')
par(mfrow=c(3,1),mar=c(4,20,1,20))

colid <- function( x, range=NULL, depth=255 ){
    if ( is.null( range ) )
        y <- as.integer(x-min(x))/(max(x)-min(x))*depth+1
    else    {
        y <- as.integer(x-range[1])/(range[2]-range[1])*depth+1
        y[ which( y < range[1] ) ] <- 1
        y[ which( y > range[2] ) ] <- depth    }
    y  }

range<-c(0,1)
x<-jet.colors(255)


##MAKES SUMMARY PLOTS; HISTOGRAMS OF DISTANCE, LATITUDE, TREND LENGTH BY TL; PIE PLOT WITH PROPORTION BY TROPHIC LEVEL
a<-subset(data,TL.min==1);b<-subset(data,TL.min==2);c<-subset(data,TL.min==3)
#vioplot(c$Dist,b$Dist,a$Dist)
bww<-.75
cl<-list('firebrick3','royalblue','forestgreen')
cll<-c('firebrick3','royalblue','forestgreen')
beanplot(c$Dist,b$Dist,a$Dist,log='',bw=50,horizontal=T,yaxt='n',maxwidth=.6,maxstripline=.1,beanlinewd=.01,col=cl,xaxt='n',ylim=c(0,1400),cutmin=0,lwd=.1,xlab='Distance from coastline')
lbl<-c('Zooplanktivores','Grazers','Primary producers')
axis(side=2,at=seq(1,3,1),labels=lbl,las=1)
axis(side=1,at=seq(0,1400,200),labels=T,las=1)
box()
legend(900,2,legend=c('Primary producers','Grazers','Zooplanktivores'),col=cll,pch=rep(15,15,15),pt.cex=c(2,2,2),bty='n',cex=1.5)

bww<-1
beanplot(c$n,b$n,a$n,log='',bw=bww,horizontal=T,yaxt='n',maxwidth=.6,maxstripline=.1,beanlinewd=.01,col=cl,xaxt='n',cutmin=0,xlab='Length of time-series')
axis(side=2,at=seq(1,3,1),labels=lbl,las=1)
axis(side=1,at=seq(5,55,10),labels=T,las=1)
box()

beanplot(c$Latitude,b$Latitude,a$Latitude,log='',bw=bww,horizontal=T,yaxt='n',maxwidth=.6,maxstripline=.1,beanlinewd=.01,col=cl,xaxt='n',overallline='median',xlab='Latitude')
axis(side=2,at=seq(1,3,1),labels=lbl,las=1)
axis(side=1,at=seq(25,75,10),labels=T,las=1)
box()

##MAKES PIE PLOT WITH PROPORTION BY TROPHIC LEVEL
f<-function(dat){
return(data.frame(pct=dim(dat)[1]/dim(data)[1]))}
dd<-ddply(data,.(TL.min),.fun=f)
dd$pct<-dd$pct*100
dd$TL.min<-c('1 - 2','2 - 3','3 - 4','4 - 5')
lb<-paste(dd$TL.min,' (',round(dd$pct,digits=1),'%)')
lb<-gsub(' +','',lb)
#lb<-gsub('-',' ',lb)
cll<-c('forestgreen','royalblue','firebrick3','orange')
par(mfrow=c(1,1),mar=c(1,1,1,1))
pie(dd$pct,labels=lb,lwd=.1,col=cll)

#dat<-subset(data,select=c('correlation','TL.min','SST.200','npp','chlswf','AllTaxa','CoastNorm','OceanNorm','diam','Bathymetry','OXY.S','SSTSLOPE','pp.chl','n','end.year','hlth','acid','uv','poll','apico','adia','psst','rsst','rfront','hsst','hfront','sst45','sst85'))#shipping and pollution are correlated
dat<-subset(data,select=c('correlation','TL.min','npp','chlswf','AllTaxa','CoastNorm','OceanNorm','diam','Bathymetry.factor','OXY.S','SSTSLOPE','pp.chl','n','end.year','hlth','acid','uv','poll','apico','adia','hsst','hfront','sst85','hsstsd'))#shipping and pollution are correlated
dat$hlth<-log10(dat$hlth)
dat$npp<-log10(dat$npp)
dat$hfront<-log10(dat$hfront)
#dat$rfront<-log10(dat$rfront)
dat$pp.chl<-log10(dat$pp.chl)
dat$diam<-log10(dat$diam)
dat$chlswf<-log10(dat$chlswf)
dat$apico<-log10(dat$apico+.01)
dat$adia<-log10(dat$adia+.01)
names(dat)<-c('Correlation','Prey trophic level','NPP','Chlorophyll','Total diversity','Coastal diversity','Oceanic diversity','Phytoplankton cell size','Bathymetry','Oxygen stress','SST change','Turnover','Sample size','End year','Impact','Acidification','UV','Pollution','Picophytoplankton','Diatoms','H Temperature','H Front','SST 85','SST SD')
cr<-cor(dat,use='pairwise.complete.obs')
par(mar=c(10,10,10,10))
corrplot(cr,method='color',diag=T,type='full',order='FPC',tl.col='black',addCoef.col="gray40",addCoefasPercent=T)
#,cl.cex=.5

mod<-gam(correlation~s(Longitude,Latitude,k=85),data=data)
vis.gam(mod,view=c('Longitude','Latitude'),plot.type='contour',n.grid=300,too.far=.025,color='heat',contour.col='black',main='',lwd=.1)
map('world',fill=T,col='gray50',add=T,lwd=.1)
points(data$Longitude,data$Latitude,pch=16,col=ifelse(data$correlation<0,'darkred','orange'),cex=1)
points(data$Longitude,data$Latitude,pch=1,cex=1,lwd=.1)
box()



###PLOTS OF TEMPERATURE
par(mfrow=c(2,1),mar=c(4,4,1,1),oma=c(0,9,0,9))
col<-jet.colors(255)
clrs<-(data$AllNorm)*1000
plot(data$hsst,data$correlation,pch=16,col=col[colid(clrs)],xlab='Sea surface temperature',ylab='Predator-prey correlation',las=1,ylim=c(-1,1),cex=rescale(data$n,newrange=c(.75,2.5)))
x1<-0;x2<-max(data$hsst)
y1<-min(data$correlation);y2<-max(data$correlation)
#gradient.rect(x2+2,y2+.4,x2+.5,y1-.4,col=rev(jet.colors(100)),gradient='y')
box()
mod<-lm(correlation~hsst+I(hsst^2),data=data,weights=data$n)
x<-seq(min(data$hsst),max(data$hsst),length.out=200)
p<-predict(mod,newdata=data.frame(hsst=x),se.fit=T)
upr<-p$fit+(1.96*p$se.fit)
lwr<-p$fit-(1.96*p$se.fit)
polygon(c(x,x[length(x):1]),c(upr,lwr[length(lwr):1]),col='lightgray',border=NA)
points(data$hsst,data$correlation,pch=16,col=col[colid(clrs)],cex=rescale(data$n,newrange=c(.75,2.5)))
points(data$hsst,data$correlation,pch=1,lwd=.1,cex=rescale(data$n,newrange=c(.75,2.5)))
lines(x,p$fit,lwd=.1)


dat<-subset(data,hsst<20)
col<-jet.colors(255)
clrs<-(dat$AllNorm)*1000
plot(data$hsst,data$correlation,pch=16,col=col[colid(clrs)],xlab='Sea surface temperature',ylab='Predator-prey correlation',las=1,ylim=c(-1,1),cex=rescale(dat$n,newrange=c(.75,2.5)))
x1<-0;x2<-max(data$hsst)
y1<-min(data$correlation);y2<-max(data$correlation)
#gradient.rect(x2+2,y2+.4,x2+.5,y1-.4,col=rev(jet.colors(100)),gradient='y')
box()
#mod<-lm(correlation~hsst,dat=dat,weights=dat$n)
mod<-lm(correlation~hsst+I(hsst^2),dat=dat,weights=dat$n)
x<-seq(min(dat$hsst),max(dat$hsst),length.out=200)
p<-predict(mod,newdat=data.frame(hsst=x),se.fit=T)
upr<-p$fit+(1.96*p$se.fit)
lwr<-p$fit-(1.96*p$se.fit)
polygon(c(x,x[length(x):1]),c(upr,lwr[length(lwr):1]),col='lightgray',border=NA)
points(dat$hsst,dat$correlation,pch=16,col=col[colid(clrs)],cex=rescale(dat$n,newrange=c(.75,2.5)))
points(dat$hsst,dat$correlation,pch=1,lwd=.1,cex=rescale(dat$n,newrange=c(.75,2.5)))
lines(x,p$fit,lwd=.1)
abline(v=max(dat$hsst),lty=3)

cl<-jet.colors(255)
plot(0,0)
par(mfrow=c(1,1))
image.plot(legend.only=TRUE,zlim=c(0,1),col=cl,legend.lab="Normalized species richness (number of species)",horizontal=FALSE,smallplot=c(.96,.99,0,1))


#TEMPERATURE EFFECTS SEPARATED BY OCEAN OR TL
par(mfrow=c(3,1),mar=c(4,4,1,5),oma=c(2,20,5,20))
plot(-10,1-10,pch=16,ylim=c(-1,1),xlim=c(0,25),xlab='Temperature',ylab='Predator - prey correlation',las=1)
f<-function(dat,cl){
points(dat$hsst,dat$correlation,pch=16,col=cl)
points(dat$hsst,dat$correlation,pch=1,lwd=.1)
mod1<-lm(correlation~hsst,data=dat)
mod2<-lm(correlation~hsst+I(hsst^2),data=dat)
x<-seq(min(dat$hsst),max(dat$hsst),length.out=200)
if((AIC(mod1)-1)< AIC(mod2)){
p<-predict(mod1,newdata=data.frame(hsst=x),se.fit=T)
}else {p<-predict(mod2,newdata=data.frame(hsst=x),se.fit=T)}
lines(x,p$fit,col=cl,lwd=.1)
}
f(subset(data,TL.min==1),'forestgreen')
f(subset(data,TL.min==2),'cornflowerblue')
f(subset(data,TL.min==3 |TL.min==4),'firebrick')
legend(15,-.5,legend=c('Trophic levels 1 - 2','Trophic levels 2 - 3', 'Trophic levels 3 - 4'),pch=16,col=c('forestgreen','cornflowerblue','firebrick'),bty='n',pt.cex=2)


plot(-10,1-10,pch=16,ylim=c(-1,1),xlim=c(0,25),xlab='Temperature',ylab='Predator - prey correlation',las=1)
f<-function(dat,cl){
points(dat$hsst,dat$correlation,pch=16,col=cl)
points(dat$hsst,dat$correlation,pch=1,lwd=.1)
mod1<-lm(correlation~hsst,data=dat)
mod2<-lm(correlation~hsst+I(hsst^2),data=dat)
x<-seq(min(dat$hsst),max(dat$hsst),length.out=200)
if((AIC(mod1)-1)< AIC(mod2)){
p<-predict(mod1,newdata=data.frame(hsst=x),se.fit=T)
}else {p<-predict(mod2,newdata=data.frame(hsst=x),se.fit=T)}
lines(x,p$fit,col=cl,lwd=.1)
}
f(subset(data,marea=='11a'),'forestgreen')
f(subset(data,marea=='11b'),'cornflowerblue')
f(subset(data,marea=='31'),'firebrick')
f(subset(data,marea=='10'),'orange')
legend(15,-.25,legend=c('Northwest Atlantic','Northeast Atlantic','North Pacific','Arctic'),pch=16,col=c('forestgreen','cornflowerblue','firebrick','orange'),bty='n',pt.cex=2)


#TROPHIC LEVEL EFFECTS
model2<-lm(correlation~as.factor(TL.min),data=data,weights=data$n)
x<-seq(1,4,1)
p<-predict(model2,newdata=data.frame(hsst=median(data$hsst),pp.chl=median(data$pp.chl),diam=median(data$diam),end.year=median(data$end.year),NonOcShark=median(data$NonOcShark),TL.min=x),se.fit=T)
dum<-data.frame(TL=seq(1,4,1),p=p$fit,se=p$se.fit,n=summary(as.factor(data$TL.min)))

plot(dum$TL,dum$p,ylim=c(-.5,.5),las=1,xlab='Prey trophic level',ylab='Predator - prey correlation')
f<-function(dat){lines(c(unique(dat$TL),unique(dat$TL)),c(dat$p+(dat$se*1.96),dat$p-(dat$se*1.96)))}
l<-dlply(dum,.(TL),.fun=f)
points(dum$TL,dum$p,pch=16)
abline(h=0,lty=3)

col<-rev(heat.colors(5));col<-col[-1]
dum<-dum[order(dum$n),]
dum$cl<-col
points(dum$TL,dum$p,pch=16,col=dum$cl,cex=2)
points(dum$TL,dum$p,pch=1,cex=2,lwd=.1)



#ASSESS SPATIAL DEPENDENCE TO DETERMINE IF SPATIAL MODELS ARE REQUIRED
#check residuals and raw data for spatial dependence;fit correlogram to residuals
par(mfrow=c(2,2),mar=c(4,4,1,1),oma=c(1,1,1,1))
dat<-data
a <- data.frame(x=dat$Longitude,y=dat$Latitude,z=dat$correlation)
sl<-surf.ls(2,a)
cr<-correlogram(sl,nint=50,plotit=T,ylab='Correlation',pch=16)
abline(v=0,lty=3)
lines(smooth.spline(cr$x,cr$y,nknots=4))

coords<-matrix(0,dim(data)[1],2);coords[,1]<-data$Longitude;coords[,2]<-data$Latitude
gb<-list(data=data$correlation,coords=coords)
vg<-variog(gb,max.dist=1e5)
plot(vg,ylim=c(0,.35),pch=16)
lines(smooth.spline(vg$u,vg$v,nknots=6))

mod<-lm(correlation~SST.200+I(SST.200^2),data=data)
a <- data.frame(x=dat$Longitude,y=dat$Latitude,z=residuals(mod))
sl<-surf.ls(2,a)
cr<-correlogram(sl,nint=50,plotit=T,ylab='Correlation',pch=16)
abline(v=0,lty=3)
lines(smooth.spline(cr$x,cr$y,nknots=4))

coords<-matrix(0,dim(data)[1],2);coords[,1]<-data$Longitude;coords[,2]<-data$Latitude
gb<-list(data=residuals(mod),coords=coords)
vg<-variog(gb,max.dist=1e5)
plot(vg,ylim=c(0,.35),pch=16)
lines(smooth.spline(vg$u,vg$v,nknots=6))




################################################
#SCATTERPLOTS OF RELATIONSHIPS
#plot(dat,pch=16,cex=.5)

par(mfrow=c(3,2),mar=c(4,10,2,10))
f<-function(dat){
x<-names(dat)[1]
names(dat)<-c('x','y')
plot(dat$x,dat$y,pch=16,cex=.75,xlab=x,ylab='correlation')
mod<-gam(y~s(x,k=4),data=dat)
xx<-seq(min(dat$x),max(dat$x),length.out=100)
p<-predict(mod,newdata=data.frame(x=xx))
lines(xx,p)
}
#f(subset(dat,select=c('SST.200','correlation')))
#f(subset(dat,select=c('AllTaxa','correlation')))
#f(subset(dat,select=c('chlswf','correlation')))
#f(subset(dat,select=c('pctcya','correlation')))
#f(subset(dat,select=c('pp','correlation')))
#f(subset(dat,select=c('OceanNorm','correlation')))




#################################################
#MORE SUMMARY PLOTS
data$n.scl<-rescale(data$n,newrange=c(.75,2))
f<-function(dat,nm){
#if(unique(dat$TL.min==1) {mod<-lm(correlation~SST.200,data=dat)
#}else {}
mod<-lm(correlation~SST.200 + I(SST.200^2),data=dat,weights=dat$n)
print(summary(mod))
s<-summary(mod)
plot(dat$SST.200,dat$correlation,pch=16,main=nm,ylim=c(-1,1),xlim=c(-1,25),xlab='Temperature (0-200m)',ylab='Correlation',las=1,cex=dat$n.scl,col='gray20',lwd=.1)
points(dat$SST.200,dat$correlation,pch=1,main=nm,ylim=c(-1,1),xlim=c(-1,25),xlab='Temperature (0-200m)',ylab='Correlation',las=1,cex=dat$n.scl,lwd=.1)
x<-seq(min(dat$SST.200),max(dat$SST.200),length.out=100)
p<-predict(mod,newdata=data.frame(SST.200=x),se.fit=T)
lines(x,p$fit)
lines(x,p$fit+(1.96*p$se.fit),lty=3)
lines(x,p$fit-(1.96*p$se.fit),lty=3)
abline(h=0,lty=1)
text(0,1,paste('r2 = ',round(s$r.squared,digits=2)))
text(0,.9,paste('P = ',round(s$coef[2,4],digits=4)))
}
par(mfrow=c(3,1),mar=c(4,25,1,25))
f(subset(data,TL.min==1),'Phytoplankton vs. Zooplankton')
f(subset(data,TL.min==2),'Zooplankton vs. Zooplanktivores')
f(subset(data,TL.min==3),'Zooplanktivores vs. Piscivores')

par(mfrow=c(1,1),mar=c(2,5,0,5))
plot(0,0,xlim=c(-175,-30),ylim=c(25,70),plot=F,axes=T)
map('world',xlim=c(-175,-30),ylim=c(25,70),fill=T,col='gray',lwd=.1,add=T);map.axes()
datt<-subset(data,select=c('Longitude','Latitude','Location2'));datt<-unique(datt)
points(datt$Longitude,datt$Latitude,pch=16,col='red')
f<-function(dat){return(data.frame(lon=median(dat$Longitude),lat=median(dat$Latitude)))}
crd<-ddply(datt,.(Location2),.fun=f)
text(jitter(crd$lon),jitter(crd$lat,amount=.5),labels=crd$Location2,cex=.7,adj=0)


par(mfrow=c(1,1),mar=c(2,5,0,5))
plot(0,0,xlim=c(-30,35),ylim=c(40,75),plot=F,axes=T)
map('world',xlim=c(-30,35),ylim=c(40,75),fill=T,col='gray',lwd=.1,add=T);map.axes()
datt<-subset(data,select=c('Longitude','Latitude','Location2'));datt<-unique(datt)
points(datt$Longitude,datt$Latitude,pch=16,col='red')
f<-function(dat){return(data.frame(lon=median(dat$Longitude),lat=median(dat$Latitude)))}
crd<-ddply(datt,.(Location2),.fun=f)
text(jitter(crd$lon),jitter(crd$lat,amount=.5),labels=crd$Location2,cex=.7,adj=0)


f<-function(dat){return(data.frame(Location2=unique(dat$Location2),n=length(dat$correlation)))}
ot<-ddply(data,.(Location2),.fun=f)
ot<-ot[(order(ot$n)),]
par(mar=c(4,12,1,1))
barplot(ot$n,names.arg=ot$Location2,horiz=T,las=1)
box()




##PLOTS CHANGES IN CORRELATIOIN OVER TIME WITHIN LOCATIONS
f<-function(dat){return(data.frame(Location2=unique(dat$Location2),n=length(dat$correlation),nyr=length(unique(dat$end.year)),ntl=length(unique(dat$TL.min))))}
ot<-ddply(data,.(Location2),.fun=f)
ot2<-subset(ot,n>=3 & nyr>2)
data2<-subset(data,Location2 %in% ot2$Location2)

#merges colour data with correlations
#display.brewer.all()
rd<-brewer.pal(9,'Reds');rd<-c('darkred',rd)
gn<-rev(brewer.pal(9,'Greens'));gn<-c(gn,'darkgreen')
cl<-c(rd,gn)
dum<-data.frame(rr=seq(-9.5,9.5,1),cl=cl)
data2$rr<-ifelse(data2$correlation<0,ceiling(data2$correlation*10)+.5,floor(data2$correlation*10)-.5)
data2$rr<-round(data2$rr,digits=1)
data2<-merge(data2,dum,by=c('rr'),all=F)
data2<-subset(data2,Location2!='NE Atlantic Ocean')


fn<-function(dat){
ln<-dim(dat)[1]
plot(0,0,xlim=c(1950,2010),ylim=c(.5,ln+.5),main=unique(dat$Location2),las=1,yaxt='n',xlab='Year',ylab='')
dat<-dat[rev(order(dat$end.year)),]
for(i in 1:ln){
d<-dat[i,]
print(i)
x<-seq(d$start.year,d$end.year,length=200)
y<-rep(i-.3,200);y2<-rep(i+.3,200)
polygon(c(x,x[length(x):1]),c(y,y2[length(y2):1]),col=as.character(d$cl),border='gray')#media
}
#points(rescale(dat$end.year,newrange=c(1950,2010)),rescale(dat$correlation,newrange=c(0.5,ln)),pch=16)
}
#setwd(figsdir)
#postscript('correlations.overtime.ps')
par(mfrow=c(3,4),mar=c(5,4,1,1))
l<-dlply(data2,.(Location2),.fun=fn)

f<-function(dat){
dat$cl<-ifelse(dat$TL.min==1,'forestgreen','black')
dat$cl<-ifelse(dat$TL.min==2,'royalblue',dat$cl)
dat$cl<-ifelse(dat$TL.min==3,'firebrick',dat$cl)
plot(dat$end.year,dat$correlation,col=dat$cl,pch=15,ylim=c(-1,1),las=1,xlab='End year',xlim=c(1975,2010),ylab='Correlation',main=unique(dat$Location2))
abline(h=0,lty=3)}
par(mfrow=c(3,4),mar=c(5,4,1,1))
l<-dlply(data2,.(Location2),.fun=f)


dev.off()




#####################################
#SI FIG (CHANGES IN TC OVER TIME
##PLOTS CHANGES IN CORRELATIOIN OVER TIME WITHIN LOCATIONS
f<-function(dat){return(data.frame(Location2=unique(dat$Location2),n=length(dat$correlation),nyr=length(unique(dat$end.year)),ntl=length(unique(dat$TL.min))))}
ot<-ddply(data,.(Location2),.fun=f)
ot2<-subset(ot,n>=3 & nyr>2)
data2<-subset(data,Location2 %in% ot2$Location2)

#merges colour data with correlations
#display.brewer.all()
rd<-brewer.pal(9,'Reds');rd<-c('darkred',rd)
gn<-rev(brewer.pal(9,'Greens'));gn<-c(gn,'darkgreen')
cl<-c(rd,gn)
dum<-data.frame(rr=seq(-9.5,9.5,1),cl=cl)
data2$rr<-ifelse(data2$correlation<0,ceiling(data2$correlation*10)+.5,floor(data2$correlation*10)-.5)
data2$rr<-round(data2$rr,digits=1)
data2<-merge(data2,dum,by=c('rr'),all=F)
data2<-subset(data2,Location2!='NE Atlantic Ocean')

f<-function(dat){
dat$cl<-ifelse(dat$TL.max2<=2.5,'green3','black')
dat$cl<-ifelse(dat$TL.max2>2.5 & dat$TL.max2<=3.5,'blue3',dat$cl)
dat$cl<-ifelse(dat$TL.max2>3.5 & dat$TL.max2<=4.5,'red3',dat$cl)
dat$cl<-ifelse(dat$TL.max2>4.5,'gold',dat$cl)
plot(dat$end.year,dat$correlation,col=dat$cl,pch=16,ylim=c(-1,1),las=1,xlab='End year',xlim=c(1975,2010),ylab='Correlation',main=unique(dat$Location2),lwd=.01,cex.axes=.75)
points(dat$end.year,dat$correlation,lwd=.001,pch=1,ylim=c(-1,1),las=1,xlab='End year',xlim=c(1975,2010),ylab='Correlation',main=unique(dat$Location2))
abline(h=0,lty=3)}

setwd(figsdir)
ptsz<-9;fnt<-'Times'
postscript('correlations_overtime.ps',height=6,width=5,font=fnt,pointsize=ptsz)
par(mfrow=c(4,3),mar=c(3,3,1,1),mgp=c(1.25,.5,0))
l<-dlply(data2,.(Location2),.fun=f)
plot(-1,1,xlim=c(1950,2000),ylim=c(0,1),axes=FALSE,xlab='',ylab='')
legend('center',legend=c('Less than 2.5','2.5 to 3.5','3.5 to 4.5','Greater than 4.5'),col=c('green3','blue3','red3','gold'),pch=15,bty='n',cex=1.1,title='Consumer trophic level',pt.cex=1.5)
dev.off()




##################################
#SI FIGURE (NOT USED)
##examines effect of phytoplankton cell size on zooplankton/phytoplankton relationships
setwd(figsdir)
postscript('phyto.zoop.bysize.ps')
par(mfrow=c(2,1),mar=c(4,15,1,15))
a<-subset(data,correlation<0 & TL.min==1)
a$ldiam<-log10(a$diam)
mod<-lm(correlation~ldiam,data=a,weights=a$n)
#mod2<-lmer(correlation~ldiam + (1|references),data=a)
x<-seq(min(a$ldiam),max(a$ldiam),length=200)
p<-predict(mod,newdata=data.frame(ldiam=x),se.fit=T)
#plot(a$ldiam,a$correlation,pch=16,ylim=c(-1,0),cex=rescale(a$n,newrange=c(1,2)),col=ifelse(a$references=='Kobari et al. 2003','red','black'),las=1,lwd=.1,xlab='log10[cell diameter]',ylab='Phyto/zoop correlation')
plot(a$ldiam,a$correlation,pch=16,ylim=c(-1,0),cex=rescale(a$n,newrange=c(1,2)),col='gray',las=1,lwd=.1,xlab='log10[cell diameter]',ylab='Phyto/zoop correlation',main='Negative relationships')
points(a$ldiam,a$correlation,pch=1,ylim=c(-1,0),cex=rescale(a$n,newrange=c(1,2)),col='black',lwd=.1)
lines(x,p$fit)
lines(x,p$fit+(1.95*p$se.fit),lty=3)
lines(x,p$fit-(1.95*p$se.fit),lty=3)

a<-subset(data,correlation>0 & TL.min==1)
a$ldiam<-log10(a$diam)
plot(a$ldiam,a$correlation,pch=16,ylim=c(0,1),cex=rescale(a$n,newrange=c(1,2)),col='gray',las=1,lwd=.1,xlab='log10[cell diameter]',ylab='Phyto/zoop correlation',main='Positive relationships')
points(a$ldiam,a$correlation,pch=1,ylim=c(0,1),cex=rescale(a$n,newrange=c(1,2)),col='black',lwd=.1)
mod<-lm(correlation~ldiam,data=a,weights=a$n)
x<-seq(min(a$ldiam),max(a$ldiam),length=100)
p<-predict(mod,newdata=data.frame(ldiam=x),se.fit=T)
lines(x,p$fit)
lines(x,p$fit+(1.95*p$se.fit),lty=3)
lines(x,p$fit-(1.95*p$se.fit),lty=3)
dev.off()



setwd(figsdir)
postscript('phyto.zoop.bysize2.ps')
par(mar=c(6,10,6,10))
aa<-subset(data, TL.min==1)
aa$ldiam<-log10(aa$diam)
a<-subset(aa,correlation<0 & TL.min==1)
mod<-lm(correlation~ldiam,data=a,weights=a$n)
x<-seq(min(a$ldiam),max(a$ldiam),length=200)
p<-predict(mod,newdata=data.frame(ldiam=x),se.fit=T)
cl<-'firebrick'
cl2<-'royalblue'
plot(aa$ldiam,aa$correlation,pch=16,ylim=c(-1,1),cex=rescale(aa$n,newrange=c(1,2)),col=ifelse(aa$correlation<0,cl,cl2),las=1,lwd=.1,xlab='log10[cell diameter]',ylab='Phyto/zoop correlation')
points(aa$ldiam,aa$correlation,pch=1,ylim=c(-1,0),cex=rescale(aa$n,newrange=c(1,2)),col='black',lwd=.1)
abline(h=0)
lines(x,p$fit,col=cl)
lines(x,p$fit+(1.95*p$se.fit),lty=3,col=cl)
lines(x,p$fit-(1.95*p$se.fit),lty=3,col=cl)
s<-summary(mod)
text(.3,-1,paste('r2 = ',round(s$r.squared,digits=2)),adj=0,col=cl)
text(.3,-.9,paste('P = ',round(s$coef[2,4],digits=3)),adj=0,col=cl)

a<-subset(aa,correlation>0 & TL.min==1)
mod<-lm(correlation~ldiam,data=a,weights=a$n)
x<-seq(min(a$ldiam),max(a$ldiam),length=100)
p<-predict(mod,newdata=data.frame(ldiam=x),se.fit=T)
lines(x,p$fit,col=cl2)
lines(x,p$fit+(1.95*p$se.fit),lty=3,col=cl2)
lines(x,p$fit-(1.95*p$se.fit),lty=3,col=cl2)
s<-summary(mod)
text(.3,1,paste('r2 = ',round(s$r.squared,digits=2)),adj=0,col=cl2)
text(.3,.9,paste('P = ',round(s$coef[2,4],digits=3)),adj=0,col=cl2)
dev.off()




##################################
#SI FIG
######IMPORTS HALPERN ECOSYSTEM INDEX FOR HISTOGRAM PLOTS
setwd(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:\\Users\\copepod\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index','C:\\Users\\sailfish\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index'))
dat<-read.table('model_1deg.txt',header=F,skip=6,sep=' ')
acid<-read.table('acid_1deg.txt',header=F,skip=6,sep=' ')
poll<-read.table('pollution_1deg.txt',header=F,skip=6,sep=' ')
ship<-read.table('shipping_1deg.txt',header=F,skip=6,sep=' ')
uv<-read.table('uv_1deg.txt',header=F,skip=6,sep=' ')

rep.row<-function(x,n){matrix(rep(x,each=n),nrow=n)}
rep.col<-function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
lon<-rep.row(seq(-179.5,179.5,1),180)
lat<-rep.col(seq(89.5,-89.5,-1),360)
dat<-dat[,1:360];acid<-acid[,1:360];inv<-inv[,1:360];poll<-poll[,1:360];pop<-pop[,1:360];ship<-ship[,1:360];uv<-uv[,1:360]
ot<-seq(1,360,1)
out<-list()
for(i in 1:length(ot)){out[[i]]<-data.frame(lon=lon[,i],lat=lat[,i],ind=dat[,i],acid=acid[,i],poll=poll[,i],ship=ship[,i],uv=uv[,i])}
hlth<-data.frame(do.call('rbind',out))
names(hlth)[1:2]<-c('Longitude','Latitude')
#hlth<-subset(hlth,ind>-99)


#back to back density plot
dnfn<-function(dat1,dat2,ylm,ylm2,nm,bww){
x<-dat1[,1]
y<-dat2
d1<-density(x,bw=bww,from=0)
d2<-density(y,bw=bww,from=0);d2$y<-d2$y*-1
#d2$y<-d2$y*-1
ylm<-ylm
cl1<-'firebrick3';cl2<-'royalblue'
plot(d1$x,d1$y,type='l',las=1,yaxt='n',xlab=nm,ylab='Proportion',col=cl1,ylim=ylm,xlim=c(min(dat1),max(dat1)))
upr<-d1$y;lwr<-rep(0,length(upr))
polygon(c(d1$x,d1$x[length(d1$x):1]),c(upr,lwr[length(lwr):1]),col=cl1,border=NULL,density=NULL)
upr<-d2$y;lwr<-rep(0,length(upr))
polygon(c(d2$x,d2$x[length(d2$x):1]),c(upr,lwr[length(lwr):1]),col=cl2,border=NULL,density=NULL)
axis(side=2,at=seq(ylm[1],ylm[2],ylm2),labels=T,las=1)
rug(unique(x),side=3,ticksize=.02,lwd=.1,col=cl1)
rug(unique(y),side=1,ticksize=.02,lwd=.1,col=cl2)
legend('bottomright',legend=c('Global extent','Correlations database'),col=c(cl1,cl2),pch=15,pt.cex=1.5,bty='n',cex=.8)
}
dnfn(subset(hlth,ind>=-1,select=c('ind')),data$hlth,c(-.25,.15),.25,'Human impact index',.44)
dnfn(subset(hlth,acid>=-1,select=c('acid')),data$acid,c(-4,4),1,'Acidity',.04)
dnfn(subset(hlth,poll>=-1,select=c('poll')),data$poll,c(-5,15),1,'Pollution',.02)

#multihistogram
par(mfrow=c(2,1))
dnfn(subset(hlth,ind>=-1,select=c('ind')),data$hlth,c(-.25,.15),.25,'Human impact index',.44)
multhist(list(a$ind,data$hlth),breaks=40,probability=TRUE,col=c('firebrick','royalblue'),las=2,beside=TRUE)
box()


#back to back histogram
a<-data.frame(nm=c('Global'),ind=hlth$ind);b<-data.frame(nm=c('Mapping'),ind=data$hlth)
dum<-rbind(a,b)
a<-data.frame(nm=c('Global'),poll=hlth$poll);b<-data.frame(nm=c('Mapping'),poll=data$poll)
dum2<-rbind(a,b);dum2<-subset(dum2,poll>-1)
a<-data.frame(nm=c('Global'),acid=hlth$acid);b<-data.frame(nm=c('Mapping'),acid=data$acid)
dum3<-rbind(a,b);dum3<-subset(dum3,acid>-1)

xlm<-c(-.2,.4)
ylm<-c(min(a$ind),max(a$ind))
out <- histbackback(split(dum$ind,dum$nm), xlim=xlm,brks=50, probability=TRUE,ylim=ylm,xaxt='n',yaxt='n')
barplot(-out$left, col="firebrick" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="lightblue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
rug(side=2,a$ind,col='firebrick')
rug(side=4,data$hlth,col='lightblue')


#OVERLAID HISTOGRAMS
setwd(figsdir)
pdf('histogram_global_v_mapping.pdf')
par(mfrow=c(3,1),mar=c(3,3,1,1))
ggplot(dum, aes(x=ind, fill=nm)) + geom_histogram(position="identity", binwidth=0.5, alpha=0.5,aes(y=..density..)) + xlim(0,35) + theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + xlab('Health index') + ylab('Probability density') + scale_fill_manual('Extent',values=c("cornflowerblue","firebrick"))

ggplot(dum2, aes(x=poll, fill=nm)) + geom_histogram(position="identity", binwidth=0.01, alpha=0.5,aes(y=..density..)) + xlim(0,.5) + xlab('Pollution index') + ylab('Probability density') + scale_fill_manual('Extent',values=c("cornflowerblue","firebrick"))+ opts(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggplot(dum3, aes(x=acid, fill=nm)) + geom_histogram(position="identity", binwidth=0.015, alpha=0.5,aes(y=..density..)) + xlim(0,1) + xlab('Acidification index') + ylab('Probability density') + scale_fill_manual('Extent',values=c("cornflowerblue","firebrick")) + opts(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
dev.off()

#r=.45; r2=.18












setwd(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:\\Users\\copepod\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index','C:\\Users\\sailfish\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index'))
dat<-read.table('model_1deg.txt',header=F,skip=6,sep=' ')
acid<-read.table('acid_1deg.txt',header=F,skip=6,sep=' ')
poll<-read.table('pollution_1deg.txt',header=F,skip=6,sep=' ')
ship<-read.table('shipping_1deg.txt',header=F,skip=6,sep=' ')
uv<-read.table('uv_1deg.txt',header=F,skip=6,sep=' ')

rep.row<-function(x,n){matrix(rep(x,each=n),nrow=n)}
rep.col<-function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
lon<-rep.row(seq(-179.5,179.5,1),180)
lat<-rep.col(seq(89.5,-89.5,-1),360)
dat<-dat[,1:360];acid<-acid[,1:360];inv<-inv[,1:360];poll<-poll[,1:360];pop<-pop[,1:360];ship<-ship[,1:360];uv<-uv[,1:360]
ot<-seq(1,360,1)
out<-list()
for(i in 1:length(ot)){out[[i]]<-data.frame(lon=lon[,i],lat=lat[,i],ind=dat[,i],acid=acid[,i],poll=poll[,i],ship=ship[,i],uv=uv[,i])}
hlth<-data.frame(do.call('rbind',out))
names(hlth)[1:2]<-c('Longitude','Latitude')
#hlth<-subset(hlth,ind>-99)



#back to back density plot
dnfn<-function(dat1,dat2,ylm,ylm2,nm,bww){
x<-dat1[,1]
y<-dat2
d1<-density(x,bw=bww,from=0)
d2<-density(y,bw=bww,from=0);d2$y<-d2$y*-1
#d2$y<-d2$y*-1
ylm<-ylm
cl1<-'firebrick3';cl2<-'royalblue'
plot(d1$x,d1$y,type='l',las=1,yaxt='n',xlab=nm,ylab='Proportion',col=cl1,ylim=ylm,xlim=c(min(dat1),max(dat1)))
upr<-d1$y;lwr<-rep(0,length(upr))
polygon(c(d1$x,d1$x[length(d1$x):1]),c(upr,lwr[length(lwr):1]),col=cl1,border=NULL,density=NULL)
upr<-d2$y;lwr<-rep(0,length(upr))
polygon(c(d2$x,d2$x[length(d2$x):1]),c(upr,lwr[length(lwr):1]),col=cl2,border=NULL,density=NULL)
axis(side=2,at=seq(ylm[1],ylm[2],ylm2),labels=T,las=1)
rug(unique(x),side=3,ticksize=.02,lwd=.1,col=cl1)
rug(unique(y),side=1,ticksize=.02,lwd=.1,col=cl2)
legend('bottomright',legend=c('Global extent','Correlations database'),col=c(cl1,cl2),pch=15,pt.cex=1.5,bty='n',cex=.8)
}
dnfn(subset(hlth,ind>=-1,select=c('ind')),data$hlth,c(-.25,.15),.25,'Human impact index',.44)
dnfn(subset(hlth,acid>=-1,select=c('acid')),data$acid,c(-4,4),1,'Acidity',.04)
dnfn(subset(hlth,poll>=-1,select=c('poll')),data$poll,c(-5,15),1,'Pollution',.02)

#multihistogram
par(mfrow=c(2,1))
dnfn(subset(hlth,ind>=-1,select=c('ind')),data$hlth,c(-.25,.15),.25,'Human impact index',.44)
multhist(list(a$ind,data$hlth),breaks=40,probability=TRUE,col=c('firebrick','royalblue'),las=2,beside=TRUE)
box()


#back to back histogram
a<-data.frame(nm=c('Global'),ind=hlth$ind);b<-data.frame(nm=c('Mapping'),ind=data$hlth)
dum<-rbind(a,b)
a<-data.frame(nm=c('Global'),poll=hlth$poll);b<-data.frame(nm=c('Mapping'),poll=data$poll)
dum2<-rbind(a,b);dum2<-subset(dum2,poll>-1)
a<-data.frame(nm=c('Global'),acid=hlth$acid);b<-data.frame(nm=c('Mapping'),acid=data$acid)
dum3<-rbind(a,b);dum3<-subset(dum3,acid>-1)

xlm<-c(-.2,.4)
ylm<-c(min(a$ind),max(a$ind))
out <- histbackback(split(dum$ind,dum$nm), xlim=xlm,brks=50, probability=TRUE,ylim=ylm,xaxt='n',yaxt='n')
barplot(-out$left, col="firebrick" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="lightblue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
rug(side=2,a$ind,col='firebrick')
rug(side=4,data$hlth,col='lightblue')


################################################

################################################
#MAKES OVERLAID HISTOGRAMS

setwd(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:\\Users\\copepod\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index','C:\\Users\\sailfish\\Documents\\aalldocuments\\literature\\postdoc_2013\\global_trophic\\data\\impact_index'))
dat<-read.table('model_1deg.txt',header=F,skip=6,sep=' ')
acid<-read.table('acid_1deg.txt',header=F,skip=6,sep=' ')
poll<-read.table('pollution_1deg.txt',header=F,skip=6,sep=' ')
ship<-read.table('shipping_1deg.txt',header=F,skip=6,sep=' ')
uv<-read.table('uv_1deg.txt',header=F,skip=6,sep=' ')

rep.row<-function(x,n){matrix(rep(x,each=n),nrow=n)}
rep.col<-function(x,n){matrix(rep(x,each=n), ncol=n, byrow=TRUE)}
lon<-rep.row(seq(-179.5,179.5,1),180)
lat<-rep.col(seq(89.5,-89.5,-1),360)
dat<-dat[,1:360];acid<-acid[,1:360];inv<-inv[,1:360];poll<-poll[,1:360];pop<-pop[,1:360];ship<-ship[,1:360];uv<-uv[,1:360]
ot<-seq(1,360,1)
out<-list()
for(i in 1:length(ot)){out[[i]]<-data.frame(lon=lon[,i],lat=lat[,i],ind=dat[,i],acid=acid[,i],poll=poll[,i],ship=ship[,i],uv=uv[,i])}
hlth<-data.frame(do.call('rbind',out))
names(hlth)[1:2]<-c('Longitude','Latitude')
#hlth<-subset(hlth,ind>-99)

#back to back histogram
a<-data.frame(nm=c('Global'),ind=hlth$ind);b<-data.frame(nm=c('Mapping'),ind=data$hlth)
dum<-rbind(a,b)
dum$nm<-as.character(dum$nm)
dum$nm<-ifelse(dum$nm=='Global','Global','Trophic database')

a<-data.frame(nm=c('Global'),poll=hlth$poll);b<-data.frame(nm=c('Mapping'),poll=data$poll)
dum2<-rbind(a,b);dum2<-subset(dum2,poll>-1)
dum2$nm<-as.character(dum2$nm)
dum2$nm<-ifelse(dum2$nm=='Global','Global','Trophic database')

setwd(figsdir)
pdf('histogram_global_v_mapping_humanimpact.pdf',width=5,height=3.5,family=fnt,pointsize=ptsz)
par(mar=c(4,4,1,1))
sz<-2
cl<-'gray40'
ggplot(dum, aes(x=ind, fill=nm)) + geom_histogram(position="identity", binwidth=0.5, alpha=0.5,aes(y=..density..)) + xlim(0,35) + theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + xlab('Human impact index') + ylab('Probability density') + scale_fill_manual('Spatial extent',values=c("cornflowerblue","firebrick")) + geom_vline(xintercept=c(5,12),linetype='dashed',color=cl,lwd=.75)
#+ geom_text(aes(2.5,.31,label='Low Impact'),size=sz,color=cl)
#+ geom_text(aes(8.5,.31,label='Medium Impact'),size=sz,color=cl)+ geom_text(aes(15,.31,label='High Impact'),size=sz,color=cl)
dev.off()



setwd(figsdir)
col1<-3.5;col2<-7.25;ptsz<-9;fnt<-'Times'
pdf('histogram_global_v_mapping_pollution.pdf',width=col2,height=5,family=fnt,pointsize=ptsz)
par(mar=c(4,4,1,1))
sz<-2
cl<-'gray40'
ggplot(dum2, aes(x=poll, fill=nm)) + geom_histogram(position="identity", binwidth=0.008, alpha=0.5,aes(y=..density..)) + xlim(0,.5) + theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank()) + xlab('Pollution index') + ylab('Probability density') + scale_fill_manual('Spatial extent',values=c("cornflowerblue","firebrick"))
dev.off()


ggplot(dum3, aes(x=acid, fill=nm)) + geom_histogram(position="identity", binwidth=0.015, alpha=0.5,aes(y=..density..)) + xlim(0,1) + xlab('Acidification index') + ylab('Probability density') + scale_fill_manual('Extent',values=c("cornflowerblue","firebrick")) + opts(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+ theme(legend.position=c(.8,0.8),plot.margin = unit(c(6,.5,.5,.5), "lines"),panel.background = element_rect(fill='white', colour='black'),panel.grid.major = element_blank(),panel.grid.minor = element_blank())
dev.off()

#r=.45; r2=.18



