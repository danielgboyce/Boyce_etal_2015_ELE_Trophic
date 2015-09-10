#THIS CODE RUNS SOME OF THE BASE MODELS USED IN BOYCE, ET AL. 2015. SPATIAL PATTERNS AND PREDICTORS OF TROPHIC CONTROL IN MARINE ECOSYSTEM. ECOLOGY LETTERS.#DUE THEIR ITERATIVE NATURE, THE CODE FOR SOME OF THE ANALYSES (IE SEQUENTIAL REGRESSION) WERE NOT INCLUDED HERE. SIMILARLY, MANY OF THE ROBUSTNESS AND SENSITIVITY ANALYSES WE UNDERTOOK WERE ALSO NOT INCLUDED.

#PACKAGES REQUIRED
library(parallel)
library(MuMIn)
library(nlme)
library(lmmfit)
library(lme4)
library(car)
library(mgcv)
library(MASS)
library(corrplot)
library(plyr)

######################################################################
#IMPORT DATA
######################################################################
currdir<-(ifelse((regexpr('copepod',getwd())>0)==TRUE,'C:/Users/copepod/Documents/aalldocuments/literature/postdoc_2013/global_trophic','C:/Users/sailfish/Documents/aalldocuments/literature/postdoc_2013/global_trophic'))
setwd(currdir)
data<-read.csv('mapping_database_april2015.csv')
data$yspan<-data$end.year-data$start.year
data<-subset(data,TL.diff2<2 & yspan>=5)
data$TLmax3<-round(data$TL_Predator,digits=0)

setwd('C:/Users/sailfish/Documents/aalldocuments/literature/postdoc_2013/global_trophic/code/code_for_SI')
data<-read.csv('Boyce_etal_2015_ELE_database.csv',header=TRUE)

######################################################################
#                     UNIVARIATE MODELS
######################################################################
#FITS UNIVARIATE MIXED EFFECTS MODELS WITH THE VARIABLE AS BOTH FIXED AND RANDOM EFFECTS; OUTPUTS TABLE
#NOTE: VIF AND SEQUENTIAL REGRESSION ANALYSES CONDUCTED A PRIORI TO THE BELOW ANALSYES SUGGESTED THAT SOME VARIABLES COULD (AND SHOULD) BE REMOVED FROM THE ANALYSES DUE TO THE HIGH COLLINEARITY AND LOW AMOUNT OF UNIQUE INFORMATION CONTENT. FOR INSTANCE, SEVERAL DIVERSITY INDICES WERE REMOVED DUE TO HIGH COLLINEARITY.
f<-function(d){
n<-names(d)[1]
if(n %in% c('Phyto_Cell_Diameter','PP_Turnover')){d[,1]<-log10(d[,1])} else NULL

names(d)[1:2]<-c('x','y')
if(n %in% c('Hadley_Temperature_200m','Diversity_All_Normalized','Diversity_NonOceanic_Shark','PP_Turnover')){#QUADRATIC FORM
    mod<-lme(y~x + I(x^2),random=list(Reference=~1 + x),data=d,control=lmeControl(opt='optim',maxIter=500))
#    mod<-lme(y~x + I(x^2),random=list(Reference=~1 + x),data=d,control=lmeControl(opt='nlminb',maxIter=500))
    s<-summary(mod)$tTable
    dat<-data.frame(var=rep(n,2))
    dat$beta=s[2:3,1]
    dat$se=s[2:3,2]
    dat$pv=s[2:3,5]
    dat$r2=r.squaredGLMM(mod)[1]
} else {#LINEAR FORM
    mod<-lme(y~x,random=list(Reference=~1 + x),data=d,control=lmeControl(opt='optim'))
    s<-summary(mod)$tTable
    dat<-data.frame(var=n,beta=s[2,1],se=s[2,2],pv=s[2,5],r2=r.squaredGLMM(mod)[1])
}
return(dat)
}
sst<-f(subset(data,select=c('Hadley_Temperature_200m','Correlation','Reference')))
Diversity_NonOceanic_Shark<-f(subset(data,select=c('Diversity_NonOceanic_Shark','Correlation','Reference')))
Phyto_Cell_Diameter<-f(subset(data,select=c('Phyto_Cell_Diameter','Correlation','Reference')))
Phyto_Cell_Diameter2<-f(subset(data,select=c('Phyto_Cell_Diameter','Correlation','Reference')))
PP_Turnover<-f(subset(data,select=c('PP_Turnover','Correlation','Reference')))
tl<-f(subset(data,select=c('TL_Predator','Correlation','Reference')))
Exploitation_Aggregate<-f(subset(data,Dist_Exploitation_Aggregate<=1000,select=c('Exploitation_Aggregate','Correlation','Reference')))
oi<-f(subset(data,is.na(Ecosystem_Omnivory)==FALSE,select=c('Ecosystem_Omnivory','Correlation','Reference')))
div<-f(subset(data,select=c('Diversity_All_Normalized','Correlation','Reference')))

univ.mm<-data.frame(rbind(sst, Diversity_NonOceanic_Shark, oi, div,tl,PP_Turnover, Exploitation_Aggregate, Phyto_Cell_Diameter))



######################################################################
#UNIVARIATE FIXED EFFECTS MODELS RUN ON SUBSET OF VARIABLES
######################################################################
modfn<-function(dat){
#TRANSFORM VARIABLES TO NORMALITY
dat$Bathymetry<-ifelse(dat$Bathymetry<0,1,dat$Bathymetry)
dat$HII<-log10(dat$HII)
dat$NPP<-log10(dat$NPP)
dat$PP_Turnover<-log10(dat$PP_Turnover)
dat$Phyto_Cell_Diameter<-log10(dat$Phyto_Cell_Diameter)
dat$Chlorophyll<-log10(dat$Chlorophyll)
dat$Bathymetry<-log10(dat$Bathymetry+abs(min(dat$Bathymetry)))

nm<-names(dat)
nm<-nm[6:length(names(dat))]
out1<-list()
for(i in 1:length(nm)){
dat1<-na.omit(subset(dat,select=c(as.character(nm[i]),'Correlation','n','Longitude','Latitude','Dist_Exploitation_Aggregate','Reference')))
n<-names(dat1)[1]
print(n)
if(n=='Exploitation_Aggregate'){dat1<-subset(dat1,Dist_Exploitation_Aggregate<1000)}
names(dat1)[1]<-c('x')
#WEIGHTED LINEAR MODELS
mod<-lm(Correlation~x,data=dat1,weights=dat1$N)
mod2<-lm(Correlation~x+I(x^2),data=dat1,weights=dat1$N)
s<-summary(mod);s2<-summary(mod2)

#WEIGHTED MIXED MODELS
modm<-lme(Correlation~x,random=list(Reference=~1),data=dat1,control=lmeControl(opt='optim',maxIter=500))
modm2<-lme(Correlation~x+I(x^2),random=list(Reference=~1),data=dat1,control=lmeControl(opt='optim',maxIter=500))
sm<-summary(modm);sm2<-summary(modm2)

#OUTPUT SELECTED VARIABLES
out<-data.frame(variable=n,coef_quad=s2$coef[2:3,1],se_quad<-s2$coef[2:3,2],pv_quad<-s2$coef[2:3,4])
out$r_sq_quad=round(s2$adj.r.squared*100,digits=2)
out$aic_quad<-AIC(mod2)
out$coef_lin=s$coef[2,1]
out$se_lin<-s$coef[2,2]
out$pv_lin<-s$coef[2,4]
out$r_sq_lin=round(s$adj.r.squared*100,digits=2)
out$coef_me_linear=round(sm$tTable[2,1],digits=3)
out$se_me_linear=round(sm$tTable[2,2],digits=3)
out$pval_me_linear=round(sm$tTable[2,5],digits=3)
out$r_sq_me_linear=round(lmmR2(modm)*100,digits=2)
out$coef_me_quad=round(sm2$tTable[2:3,1],digits=3)
out$se_me_quad=round(sm2$tTable[2:3,2],digits=3)
out$pval_me_quad=round(sm2$tTable[2:3,5],digits=3)
out$r_sq_me_quad=round(lmmR2(modm2)*100,digits=2)
out$aic_lin<-AIC(mod)
out$n<-length(dat1$x)
out1[[i]]<-out
}
outt<-data.frame(do.call('rbind',out1))
outt<-outt[rev(order(outt$r_sq_quad)),]
return(outt)
}

out<-modfn(subset(data,select=c('Correlation','Reference','Longitude','Latitude','Dist_Exploitation_Aggregate','TL.min2','TL_Predator','NPP','Chlorophyll','AllTaxa','CoastNorm','OceanNorm','Phyto_Cell_Diameter','Bathymetry','Oxygen_Stress','Temperature_Rate','PP_Turnover','n','end.year','HII','UV','Pollution','Hadley_Temperature_200m','hfront','SST_Velocity85','Diversity_All_Normalized','Bathymetry','Cetacean','Euphausiid','Seagrass','Squid','Diversity_NonOceanic_Shark','Pinniped','TunaBllfsh','OceanShark','Exploitation_Aggregate','Ecosystem_Omnivory','Latitude','TL.diff2')))




######################################################################
#                 MODEL SELECTION
######################################################################
#INFORMATION-THEORETIC MULTIMODEL ANALYSIS; CAN COMPARE RESULTS WITH BACKWARD STEPWISE ESTIMATION, BUT THEY PRODUCE EQUIVALENT RESULTS
#NOTES: ANALYSIS ORIGINALLY CONDUCTED ON EXPANDED SUITE OF COVARIATES, BUT THE 95% CONFIDENCE SET ENDED UP CONTAINING ONLY TWO MODELS. SINCE ADDING MORE VARIABLES RESULTED IN NO GAIN AND DID NOT ALTER THE MULTIMODEL AVERAGED PARAMETER ESTIMATES, BUT REQUIRED INCREASED COMPUTING POWER, ANALYSIS WAS ULTIMATELY CONDUCTED ON REDUCED SET OF VARIABLES THAT WERE CONTAINED IN THE 95% CONFIDENCE SET
dat<-subset(data,select=c('Hadley_Temperature_200m','PP_Turnover','Phyto_Cell_Diameter','Diversity_NonOceanic_Shark','TL_Predator','Correlation','n'))
dat$PP_Turnover<-log10(dat$PP_Turnover)
dat$Phyto_Cell_Diameter<-log10(dat$Phyto_Cell_Diameter)
dat$Diversity_NonOceanic_Shark2<-dat$Diversity_NonOceanic_Shark^2
dat$Hadley_Temperature_200m2<-dat$Hadley_Temperature_200m^2

#SET UP CLUSTER; NOT NEEDED ON THE ABOVE DATA, BUT WITH INCREASING NUMBER OF VARIABLES, MAY INCREASE COMPUTING SPEED
clusterType<-if(length(find.package('snow',quiet=TRUE))) 'SOCK' else 'PSOCK'
clust<-try(makeCluster(getOption('cl.cores',8),type=clusterType))
clusterExport(clust,'dat')

options(na.action='na.fail')
md<-lm(Correlation~.,data=dat,weights=dat$N)
system.time(dd2<-pdredge(md,cluster=clust,extra=c('R^2','adjR^2'),subset =!(n)))

ma2<-model.avg(dd2,subset=delta<4)#MODEL AVERAGED RESULTS WITH DELTA AICC<4; 95% CONFIDENCE SET
sm.ma2<-summary(ma2)
ma952<-model.avg(dd2,subset=cumsum(weight)<=.95)#95% CONFIDENCE SET; WILL GENERATE ERROR IF ONLY TWO MODELS IN SET
sm.ma952<-summary(ma952)

sm.ma952$avg.model#TTABLE
sm.ma952$coefmat.full#COEFFICIENTS
sm.ma2$coefmat.full#PVALS


######################################################################
##BACKWARD STEPWISE MODEL SELECTION; NPP, SEVERAL DIVERSITY, CHL REMOVED DUE TO COLLINEARITY WITH LIKE VARIABLES BASED ON SEQUENTIAL REGRESSION
######################################################################
dat<-data
dat$HII<-log10(dat$HII)
dat$NPP<-log10(dat$NPP)
dat$hfront<-log10(dat$hfront)
dat$PP_Turnover<-log10(dat$PP_Turnover)
dat$Phyto_Cell_Diameter<-log10(dat$Phyto_Cell_Diameter)
dat$Oxygen_Stress<-log10(dat$Oxygen_Stress+.001)

model<-lm(Correlation~Hadley_Temperature_200m+I(Hadley_Temperature_200m^2)+PP_Turnover+Phyto_Cell_Diameter+Diversity_All_Normalized+I(Diversity_All_Normalized^2)+Diversity_NonOceanic_Shark+I(Diversity_NonOceanic_Shark^2)+n+TL_Predator+Bathymetry+Oxygen_Stress+Temperature_Rate +HII+Pollution+UV+SST_Velocity85+Hadley_Temperature_200msd+ Latitude,data=dat,weights=dat$N)
model2 <- update(model,~. - acid)
model2 <- update(model2,~. - I(Diversity_All_Normalized^2))
model2 <- update(model2,~. - Diversity_All_Normalized)
model2 <- update(model2,~. - end.year)
model2 <- update(model2,~. - Oxygen_Stress)
model2 <- update(model2,~. - Latitude)
model2 <- update(model2,~. - UV)
model2 <- update(model2,~. - Hadley_Temperature_200msd)
model2 <- update(model2,~. - (HII))
model2 <- update(model2,~. - Pollution)
model2 <- update(model2,~. - Bathymetry)
model2 <- update(model2,~. - (hfront))
model2 <- update(model2,~. - n)
model2 <- update(model2,~. - SST_Velocity85)
model2 <- update(model2,~. - Temperature_Rate)
mamod <- model2



######################################################################
##STEPWISE MODEL SELECTION ON REDUCED DATASET WITH Ecosystem_Omnivory AND EXPLEcosystem_OmnivoryTATION
######################################################################
dat<-subset(data,Dist_Exploitation_Aggregate<1000 & is.na(Ecosystem_Omnivory)==FALSE)
dat$HII<-log10(dat$HII)
dat$NPP<-log10(dat$NPP)
dat$hfront<-log10(dat$hfront)
dat$PP_Turnover<-log10(dat$PP_Turnover)
dat$Phyto_Cell_Diameter<-log10(dat$Phyto_Cell_Diameter)
dat$Oxygen_Stress<-log10(dat$Oxygen_Stress+.001)

#FIT FULL MODEL, EXAMINE RESULTS, REMOVE VARIABLE, EXAMINE RESULTS, CONTINUE....
model<-lm(Correlation~Hadley_Temperature_200m+I(Hadley_Temperature_200m^2)+PP_Turnover+Phyto_Cell_Diameter+Diversity_All_Normalized+I(Diversity_All_Normalized^2)+Diversity_NonOceanic_Shark+I(Diversity_NonOceanic_Shark^2)+n+TL_Predator+Bathymetry+Oxygen_Stress+Temperature_Rate +HII+Pollution+UV+SST_Velocity85+Hadley_Temperature_200msd+ Latitude+Ecosystem_Omnivory+Exploitation_Aggregate,data=dat,weights=dat$N)
model2 <- update(model,~. - Diversity_All_Normalized)
model2 <- update(model2,~. - I(Diversity_All_Normalized^2))
model2 <- update(model2,~. - Latitude)
model2 <- update(model2,~. - (HII))
model2 <- update(model2,~. - Exploitation_Aggregate)
model2 <- update(model2,~. - SST_Velocity85)
model2 <- update(model2,~. - Temperature_Rate)
model2 <- update(model2,~. - PP_Turnover)
model2 <- update(model2,~. - UV)
model2 <- update(model2,~. - n)
model2 <- update(model2,~. - I(Hadley_Temperature_200m^2))
model2 <- update(model2,~. - Hadley_Temperature_200m)
model2 <- update(model2,~. - Hadley_Temperature_200msd)
model2 <- update(model2,~. - Oxygen_Stress)
model2 <- update(model2,~. - Pollution)
model2 <- update(model2,~. - Phyto_Cell_Diameter)
model2 <- update(model2,~. - Bathymetry)
mamodred<-model2




######################################################################
#              EXTRAPOLATION TO UNSAMPLED LOCATIONS
######################################################################
#TAKES MINIMAL ADEQUATE MODEL AND EXTRAPOLATES TO UNSAMPLED AREAS
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
d<-subset(dat,NPP>0 & Phyto_Cell_Diameter>0 & PP_Turnover>0 & chl<50 & Phyto_Cell_Diameter<500 & NPP>min(data$NPP) & NPP<max(data$NPP) & sst>min(data$Hadley_Temperature_200m) & sst<max(data$Hadley_Temperature_200m) & Shark>min(data$Diversity_NonOceanic_Shark) & Shark<max(data$Diversity_NonOceanic_Shark) & PP_Turnover>min(data$PP_Turnover) & PP_Turnover<max(data$PP_Turnover) & Phyto_Cell_Diameter>min(data$Phyto_Cell_Diameter) & Phyto_Cell_Diameter<max(data$Phyto_Cell_Diameter))
d<-na.omit(subset(d,select=c('Longitude','Latitude','NPP','Phyto_Cell_Diameter','Shark','sst','lNPP','lPhyto_Cell_Diameter','Shark','sst','acid','PP_Turnover','lPP_Turnover')))
if(dim(d)[1]>0){p<-predict(model2,newdata=data.frame(Hadley_Temperature_200m=d$sst,PP_Turnover=d$lPP_Turnover,Phyto_Cell_Diameter=d$lPhyto_Cell_Diameter,Diversity_NonOceanic_Shark=d$Shark,TL_Predator=3),se.fit=TRUE,type='response')
dat$p<-p$fit
dat$se<-p$se.fit}
if(dim(d)[1]==0){dat$p<-NA; dat$se<-NA}
return(dat)
}
outt<-ddply(pred,.(mvar),.fun=f)



######################################################################
#                STRUCTURAL EQUATION MODEL
######################################################################
sdat<-subset(data,select=c('Hadley_Temperature_200m','TL_Predator','Diversity_NonOceanic_Shark','PP_Turnover','Phyto_Cell_Diameter','Correlation'))
sdat$PP_Turnover<-log10(sdat$PP_Turnover)
sdat$Phyto_Cell_Diameter<-log10(sdat$Phyto_Cell_Diameter)

#SETS UP CORRELATION MATRIX
p<-as.matrix(cor(sdat))
p[1,2:6]<-0
p[2,3:6]<-0
p[3,4:6]<-0
p[4,5:6]<-0
p[5,6]<-0
rownames(p)<-colnames(p)<-c('Hadley_Temperature_200m','TL_Predator','Diversity_NonOceanic_Shark','PP_Turnover','Phyto_Cell_Diameter','Correlation')

#SET UP MATRIX OF MODELS
sem.ram<-matrix(c(
'PP_Turnover -> Correlation',     'sem1', NA,
'TL_Predator -> Correlation',    'sem2', NA,
'Diversity_NonOceanic_Shark -> Correlation', 'sem3', NA,
'Phyto_Cell_Diameter -> Correlation',       'sem4', NA,
'Hadley_Temperature_200m -> Correlation',       'sem5', NA,
'Hadley_Temperature_200m -> PP_Turnover',            'sem6', NA,
'Hadley_Temperature_200m -> Phyto_Cell_Diameter',              'sem7', NA,
'Hadley_Temperature_200m -> Diversity_NonOceanic_Shark',        'sem8', NA,
'Phyto_Cell_Diameter -> PP_Turnover',            'sem9', NA,
'PP_Turnover <-> PP_Turnover',         'sig2', NA,
'Phyto_Cell_Diameter <-> Phyto_Cell_Diameter',             'sig3', NA,
'Diversity_NonOceanic_Shark <-> Diversity_NonOceanic_Shark', 'sig7', NA,
'Correlation <->Correlation','sig8', NA), Ncol=3,byrow=T)
sem.mod<-sem(sem.ram,p,N=155,fixed.x=c('Hadley_Temperature_200m','TL_Predator'))





