library(ggplot2)
library(reshape2)
library(stringr)
library(xlsx)
library(Hmisc)

setwd("~/Documents/R/InSalt")

year_start=2013
year_end=2038

age_groups=c("40-49","50-59","60-69","70-79","80+")
age_groups_means<-c(45,55,65,75,83)
age_groups_2=c("35-44","45-54","55-64","65-74")
disease_names=c("ihd_icd","stroke_icd","hypertension_icd")

## Import population data

popxls=read.xlsx(file="Data/Populacja/Prognoza_2014-50.xls",sheetIndex=1)
pop_all<-popxls[4:nrow(popxls),3]
pop_num<-as.numeric(levels(pop_all))[pop_all]
pop_matr<-matrix(pop_num,nrow=22)
colnames(pop_matr)<-2013:2050
pop_matr_agegroup<-apply(pop_matr,2,function(x) {
  out=c( 
  sum(x[c(10,11)]),
  sum(x[c(12,13)]),
  sum(x[c(14,15)]),
  sum(x[c(16,17)]),
  sum(x[c(18:22)])
  )
  out
})
rownames(pop_matr_agegroup)<-age_groups

pop2013<-pop_matr_agegroup[,1]

popxls2=read.xlsx(file="Data/Populacja/Prognoza_2014-50.xls",sheetIndex=1,colClasses=c("character","character",rep("numeric",9)))
pop_all_mk<-popxls2[4:nrow(popxls2),4:5]
colnames(pop_all_mk)<-c("M","F")
pop_matr_mk<-lapply(pop_all_mk,function(x) {
  out<-matrix(x,nrow=22,ncol=38)
  colnames(out)<-2013:2050
    out
  })

pop_matr_mk_agegroup<-lapply(pop_matr_mk,function(y){
  out2<-apply(y,2,function(x) {
  out=c( 
    sum(x[c(10,11)]),
    sum(x[c(12,13)]),
    sum(x[c(14,15)]),
    sum(x[c(16,17)]),
    sum(x[c(18:22)])
  )
  out
})
rownames(out2)<-age_groups
out2
})

pop_mk_2013<-cbind(pop_matr_mk_agegroup$M[,1],pop_matr_mk_agegroup$F[,1])

##
icd_codes=list()
icd_codes[["ihd_icd"]]=c(paste("I",20:25,sep="") )
icd_codes[["stroke_icd"]]=c(paste("I",60:69,sep="") )
icd_codes[["hypertension_icd"]]=c(paste("I",10:15,sep="") )

icd_data=list()

## Import JGP Data
jgp_dir=dir("Data/JGP")
jgp_data_list<-list()

pop2013_jgp=c(sum(pop2013[1:2]),sum(pop2013[3:4]),pop2013[5])

for (i in 1:length(jgp_dir)){
sheetxls1=read.xlsx(file=paste("Data/JGP/",jgp_dir[i],sep=""),sheetIndex=1,colClasses=c("character","character","numeric"))
sheetxls4=read.xlsx(file=paste("Data/JGP/",jgp_dir[i],sep=""),sheetIndex=4,colClasses=c("character","character","numeric","numeric","numeric"))
sheetxls5=read.xlsx(file=paste("Data/JGP/",jgp_dir[i],sep=""),sheetIndex=5,colClasses=c("character","character","numeric","numeric","numeric"))
sheetxls9=read.xlsx(file=paste("Data/JGP/",jgp_dir[i],sep=""),sheetIndex=9,colClasses=c("character","character","numeric","numeric","numeric"))

mean_price<-sheetxls1[str_detect(sheetxls1[,1],"(zł)"),3][2]
num_instances<-sheetxls1[str_detect(sheetxls1[,1],"Liczba wystąpień"),3][1]
death_rate<-sheetxls4[str_detect(sheetxls4[,1],"zgon pacjenta"),4][1]/100

jgp_data_list[[i]]<-list()
jgp_data_list[[i]]$mean_price<-mean_price
jgp_data_list[[i]]$death_rate<-death_rate
  
jgp_data_list[[i]]$details=lapply(disease_names,function(x){
  pop_icd_i=c()
  
  prc_icd_i<-sum( sheetxls9[str_detect(sheetxls9[,1], paste(icd_codes[[x]],collapse="|") ),4] )/100
  x_reg<-c(50,70,83)
  y_reg<-prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1:3]/pop2013_jgp
  y_extr=approxExtrap(x=x_reg,y=y_reg,xout=c(45,55,65,75,83))$y
  y_extr=y_extr*(1*(y_extr>0))
  
  if (sum(y_extr[1:2])>0) {pop_icd_i[1]=y_extr[1]/sum(y_extr[1:2])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1]} else {pop_icd_i[1]=0}
  if (sum(y_extr[1:2])>0) {pop_icd_i[2]=y_extr[2]/sum(y_extr[1:2])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1]} else {pop_icd_i[2]=0}
  if (sum(y_extr[1:2])>0) {pop_icd_i[3]=y_extr[3]/sum(y_extr[3:4])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][2]} else {pop_icd_i[3]=0}
  if (sum(y_extr[1:2])>0) {pop_icd_i[4]=y_extr[4]/sum(y_extr[3:4])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][2]} else {pop_icd_i[4]=0}
  pop_icd_i[5]=prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][3]
  
  out=data.frame(age=age_groups,
  instances=pop_icd_i,
  cost=pop_icd_i*mean_price,
  deaths=pop_icd_i*death_rate)

  out
})
names(jgp_data_list[[i]]$details)<-disease_names
}

jgp_data<-lapply(disease_names,function(y){
  Reduce("+",lapply(jgp_data_list,function(x){
                            x$details[[y]] }))
         })

jgp_data<-lapply(jgp_data,function(x){
  x$age<-age_groups
  x$mean_cost<-x$cost/x$instances
  x$death_rate<-x$deaths/x$instances
  x
})

names(jgp_data)<-disease_names

## Import hospital DATA - ICD


## Import mortality

# hypertension estimation
popxls2=read.xlsx(file="Data/Populacja/Prognoza_2014-50.xls",sheetIndex=1,colClasses=c("character","character",rep("numeric",9)))
pop_all_mk_2<-popxls2[4:nrow(popxls2),4:5]
colnames(pop_all_mk_2)<-c("M","F")
pop_matr_mk_2<-lapply(pop_all_mk_2,function(x) {
  out<-matrix(x,nrow=22,ncol=38)
  colnames(out)<-2013:2050
  out
})

pop_matr_mk_agegroup_2<-lapply(pop_matr_mk_2,function(y){
  out2<-apply(y,2,function(x) {
    out=c( 
      sum(x[c(9,10)]),
      sum(x[c(11,12)]),
      sum(x[c(13,14)]),
      sum(x[c(15,16)])
    )
    out
  })
  rownames(out2)<-age_groups_2
  out2
})

pop_mk_2013_2<-cbind(pop_matr_mk_agegroup_2$M[,1],pop_matr_mk_agegroup_2$F[,1])


x_reg<-c(40,50,60,70)
x_out<-c(45,55,65,75,83)
hypertension_prevalence_mk<-cbind(M=c(0.25,0.4,0.5,0.56),F=c(0.09,0.27,0.50,0.58))
hypertension_prevalence_agegroups2<-apply(hypertension_prevalence_mk*pop_mk_2013_2,1,sum)/apply(pop_mk_2013_2,1,sum)
hypertension_prevalence=approxExtrap(x=x_reg,y=hypertension_prevalence_agegroups2,xout=x_out)$y
hypertension_total_mort<-5000


sheetmort=read.xlsx(file=paste("Data/Mortality/deaths.xls",sep=""),sheetIndex=1,colClasses=c("character",rep("numeric",24)))
mortality<-list()
mortality[["all"]]<-c(sum(sheetmort[str_detect(sheetmort[,1],"40 - 44|45 - 49"),2],na.rm=TRUE),
                      sum(sheetmort[str_detect(sheetmort[,1],"50 - 54|55 - 59"),2],na.rm=TRUE),
                      sum(sheetmort[str_detect(sheetmort[,1],"60 - 64|65 - 69"),2],na.rm=TRUE),
                      sum(sheetmort[str_detect(sheetmort[,1],"70 - 74|75 - 79"),2],na.rm=TRUE),
                      sum(sheetmort[str_detect(sheetmort[,1],"80 - 84|85 - 89|90 - 94|>95"),2],na.rm=TRUE))
mortality[[disease_names[1]]]<-c(sum(sheetmort[str_detect(sheetmort[,1],"40 - 44|45 - 49"),8],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"50 - 54|55 - 59"),8],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"60 - 64|65 - 69"),8],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"70 - 74|75 - 79"),8],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"80 - 84|85 - 89|90 - 94|>95"),8],na.rm=TRUE))
mortality[[disease_names[2]]]<-c(sum(sheetmort[str_detect(sheetmort[,1],"40 - 44|45 - 49"),20],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"50 - 54|55 - 59"),20],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"60 - 64|65 - 69"),20],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"70 - 74|75 - 79"),20],na.rm=TRUE),
                                 sum(sheetmort[str_detect(sheetmort[,1],"80 - 84|85 - 89|90 - 94|>95"),20],na.rm=TRUE))
mortality[[disease_names[3]]]<-hypertension_prevalence*hypertension_total_mort

## Prevalence
instances=mapply(function(x,y){
                  x$instances+(y-x$deaths)
                  },
                 jgp_data,mortality[2:4],SIMPLIFY=FALSE)

instances_deaths_outside_rate=mapply(function(x,y){(y-x$deaths)/(x$instances+(y-x$deaths))},
                                     jgp_data,mortality[2:4])
instances_hosp_rate=mapply(function(x,y){(x$instances)/(x$instances+(y-x$deaths))},
                           jgp_data,mortality[2:4])
instances_deaths_inside_rate=lapply(jgp_data, function(x) x$death_rate)

instances_rate=lapply(instances,function(x){x/pop2013})


## Deaths non-cvd
mortality_noncvd<-mortality[["all"]]-apply(do.call(rbind,mortality[2:4]),2,sum)
mortality_rate_noncvd<-mortality_noncvd/mortality[["all"]]
mortality_rate_cvd=lapply(mortality[2:4],function(x){x/mortality[["all"]]})
  
## MortalityPrognosis
popyearxls=read.xlsx(file="Data/Populacja/Prognoza_2014-50.xls",sheetIndex=4,colClasses=c("character","character",rep("numeric",9)))
popyear_total=matrix(popyearxls[4:nrow(popyearxls),3],nrow=102)[-c(1,102),]
mortalityprog<-popyear_total[1:(nrow(popyear_total)-1),1:(ncol(popyear_total)-1)]-popyear_total[2:(nrow(popyear_total)),2:(ncol(popyear_total))]
mortalityprog_agegroups<-apply(mortalityprog,2,function(x){
  out=c()
  out[1]<-sum(x[41:50])
  out[2]<-sum(x[51:60])
  out[3]<-sum(x[61:70])
  out[4]<-sum(x[71:80])
  out[5]<-sum(x[81:99])
  out
})
colnames(mortalityprog_agegroups)<-2013:2049

mortalityprog_noncvd_agegroups<-apply(mortalityprog_agegroups,2,function(x){
  x*mortality_rate_noncvd
})
mortalityprog_cvd_agegroups<-mortalityprog_agegroups-mortalityprog_noncvd_agegroups

mortality_rates_prog_agegroups<-mortalityprog_agegroups/pop_matr_agegroup[,-ncol(pop_matr_agegroup)]
mortality_rates_prog_cvd_agegroups<-mortalityprog_cvd_agegroups/pop_matr_agegroup[,-ncol(pop_matr_agegroup)]
mortality_rates_prog_noncvd_agegroups<-mortalityprog_noncvd_agegroups/pop_matr_agegroup[,-ncol(pop_matr_agegroup)]

## realative risks # per 10mmHg SBP
relative_risks=list()
rr_x=c(40,50,60,70,80,87)
rr_xextr<-age_groups_means
rr_y=c(1.68,1.56,1.45,1.33,1.26,1.14)
rr_yextr<-approx(rr_x,rr_y,xout=rr_xextr)
relative_risks[[disease_names[1]]]<-rr_yextr$y
rr_y=c(2.05,1.83,1.63,1.44,1.29,1.10)
rr_yextr<-approx(rr_x,rr_y,xout=rr_xextr)
relative_risks[[disease_names[2]]]<-rr_yextr$y
rr_y=c(2.86,2.49,2.16,1.88,1.63,1.37)
rr_yextr<-approx(rr_x,rr_y,xout=rr_xextr)
relative_risks[[disease_names[3]]]<-rr_yextr$y

## Reduction Effect
groups_parameters<-list()
groups_parameters$age_mean<-matrix(c(rep(45,4),rep(55,4),rep(65,4),rep(75,4),rep(83,4)),ncol=5,nrow=4)
groups_parameters$sbp_mean<-matrix(c(rep(115,5),rep(122.5,5),rep(135,5),rep(145,5)),ncol=5,nrow=4,byrow=TRUE)
                                                
sodium_red<-5 # a day
sbp_red_mean<- (sodium_red/2.30)*(-3.735-0.105*(groups_parameters$age_mean-50)-1.874*(1*(groups_parameters$sbp_mean>140) ) )

save.image(file=paste("Data/R/WorkingData",date(),".RData",sep="_"))



#### Model ####

## parameters
year_start=2013
year_end=2038

age_groups=c("40-49","50-59","60-69","70-79","80+")
age_groups_means<-c(45,55,65,75,83)

hypertension_prevalence=hypertension_prevalence
hypertension_prevalence
hypertension_treated=0.26

mort_rates_nocvd<-data.frame(mortality_rates_prog_noncvd_agegroups)
colnames(mort_rates_nocvd)<-2013:2049
mort_rates_cvd<-mortality_rates_prog_cvd_agegroups

relative_risks=relative_risks
instances_rate=instances_rate

jgp_data=jgp_data

cost_ht_treatment=623.28

interest_rate=0.03

popyear_prog_total<-data.frame(popyear_total)
colnames(popyear_prog_total)<-2013:2050
rownames(popyear_prog_total)<-0:99

## functions
sbp_reduce<-function(slt_reduce,age,hypertension){
  (slt_reduce/2.30)*(-3.735-0.105*(age-50)-1.874*(1*(hypertension) ) )
}

sbp_distribution<-function(pop,slt_reduce){
  
  x_nonhyper=c(0,20/64,44/64,1)
  y_nonhyper=c(90,120,130,140)
  
  x_hyper_nont=c(0,0.25,0.5,0.75,0.95,1)
  y_hyper_nont_4059=c(140,c(140,144,152,174)+5,200)
  y_hyper_nont_60p=c(140,c(145,152,164,189)+5,200)
  
  x_hyper_t=c(0,0.05,0.25,0.5,0.75,0.95,1)
  y_hyper_t_4059=c(90,c(102,116,127,139,163)+5,200)
  y_hyper_t_60p=c(90,c(107,123,136,150,180)+5,200)
  
  hyper<-hypertension_prevalence*pop

  pop_hyper<-cbind(
    mapply(function(x,y){
      u=runif(x)
      z=approx(x_hyper_nont,y_hyper_nont_4059,xout=u)$y+sbp_reduce(slt_reduce,y,1)
      hyper_false_obs<-z[z<140]
      hyper_real_obs<-z[z>=140]
      id_treated=sample(x=(1:length(hyper_real_obs)),size=floor(hypertension_treated*length(hyper_real_obs)),replace=FALSE)
      hyper_nont_obs<-hyper_real_obs[-id_treated]
      u=runif(length(id_treated))
      z=approx(x_hyper_t,y_hyper_t_4059,xout=u)$y+sbp_reduce(slt_reduce,y,0)      
      hyper_total=length(hyper_real_obs)
      zf<-c(hyper_false_obs,hyper_nont_obs,z)
      c(sum(zf<115),sum(zf>=115&zf<130),sum(zf>=130&zf<140),sum(zf>=140),mean(zf[(zf<115)],na.rm=TRUE),mean(zf[(zf>=115&zf<130)],na.rm=TRUE),mean(zf[(zf>=130&zf<140)],na.rm=TRUE),mean(zf[(zf>=140)]),length(id_treated),hyper_total)
    },hyper[1:2],age_groups_means[1:2]),
    mapply(function(x,y){
      u=runif(x)
      z=approx(x_hyper_nont,y_hyper_nont_60p,xout=u)$y+sbp_reduce(slt_reduce,y,1)
      hyper_false_obs<-z[z<140]
      hyper_real_obs<-z[z>=140]
      id_treated=sample(1:length(hyper_real_obs),floor(hypertension_treated*length(hyper_real_obs)),replace=FALSE)
      hyper_nont_obs<-hyper_real_obs[-id_treated]
      u=runif(length(id_treated))
      z=approx(x_hyper_t,y_hyper_t_60p,xout=u)$y+sbp_reduce(slt_reduce,y,0)      
      hyper_total=length(hyper_real_obs)
      zf<-c(hyper_false_obs,hyper_nont_obs,z)
      c(sum(zf<115),sum(zf>=115&zf<130),sum(zf>=130&zf<140),sum(zf>=140),mean(zf[(zf<115)],na.rm=TRUE),mean(zf[(zf>=115&zf<130)],na.rm=TRUE),mean(zf[(zf>=130&zf<140)],na.rm=TRUE),mean(zf[(zf>=140)]),length(id_treated),hyper_total)  
    },hyper[3:5],age_groups_means[3:5])
  )
    
  nonhyper=pop-hyper
  
  pop_nonhyper<-mapply(function(x,y){
    u=runif(x)
    y=approx(x_nonhyper,y_nonhyper,xout=u)$y+sbp_reduce(slt_reduce,y,0)
    c(sum(y<115),sum(y>=115&y<130),sum(y>=130&y<140),sum(y>=140),mean(y[(y<115)]),mean(y[(y>=115&y<130)]),mean(y[(y>=130&y<140)]),mean(y[(y>=140)],na.rm=TRUE))
  },nonhyper,age_groups_means)
  
  pop_hyper[is.na(pop_hyper)]<-0
  pop_nonhyper[is.na(pop_nonhyper)]<-0
  
  out<-list()
  out$dist<-pop_nonhyper[1:4,]+pop_hyper[1:4,]
  out$means<-rbind( rep(115,5),pop_nonhyper[2:4,]/out$dist[2:4,]*pop_nonhyper[6:8,]+pop_hyper[2:4,]/out$dist[2:4,]*pop_hyper[6:8,])
  out$ht_treated<-pop_hyper[9,]
  out$hyper_total<-pop_hyper[10,]
  out
}

cvd_prob_fun<-function(inst_rates,rr,mean_sbp){
  coeff<-apply(mean_sbp,2,function(z){(z-115)/10})
  out<-mapply(function(x,y){
    tmp_rates<-matrix(unlist(mapply(function(v,w) v^w,as.list(y),as.data.frame(coeff),SIMPLIFY=FALSE)),nrow=4)
    prob=matrix(rep(x,4),nrow=4,byrow=TRUE)*apply(tmp_rates,2,function(z) z/sum(z))
  },inst_rates,rr,SIMPLIFY=FALSE)
  out
}

cvd_prob_intervention_fun<-function(cvd_prob,rr,mean_sbp){
  coeff<-apply(mean_sbp,2,function(z){(z-115)/10})
  out<-mapply(function(x,y){
    tmp_rates<-matrix(unlist(mapply(function(v,w) v^w,as.list(y),as.data.frame(coeff),SIMPLIFY=FALSE)),nrow=4)
    prob=matrix(rep(x[1,],4),nrow=4,byrow=TRUE)*tmp_rates
  },cvd_prob,rr,SIMPLIFY=FALSE)
  out
}


### Testing
pop_tmp=rep(10000,5)
test_dist1<-sbp_distribution(pop_tmp,0)
cvd_prob<-cvd_prob_fun(instances_rate,relative_risks,test_dist1$means)
test_dist2<-sbp_distribution(pop_tmp,2.3)
cvd_prob_int<-cvd_prob_intervention_fun(cvd_prob,relative_risks,test_dist2$means)

## objects
model0_pop_init=list()
model0_group_init=list()

model0_cvd_prob<-list()

model0_cost_hyper=list()

model0_deaths_noncvd=list()
model0_cvd_events=list()

model0_cvd_events_deathoutside=list()
model0_cvd_events_deathinside=list()
model0_cvd_events_hosp=list()

model0_cost_hosp=list()

model0_total_costs=list()

model0_total_deaths_cvd=list()
model0_total_deaths=list()

model0_pop_end=list()

## model RUN ####

model0_pop_init[["2013"]]<-pop2013

# ceteris paribus
for (r in as.character(year_start:year_end)){
  
  dist_temp<-sbp_distribution(model0_pop_init[[r]],0)
  model0_group_init[[r]]<-dist_temp$dist
  model0_cost_hyper[[r]]<-dist_temp$ht_treated*cost_ht_treatment
  
  model0_deaths_noncvd[[r]]=model0_group_init[[r]]*matrix(rep(mort_rates_nocvd[[r]],4),byrow=TRUE,nrow=4)
  
  if (r=="2013") {
    cvd_prob_base<-cvd_prob_fun(instances_rate,relative_risks,dist_temp$means)
    model0_cvd_prob[[r]]<-cvd_prob_base
  }
  if (r!="2013"){model0_cvd_prob[[r]]<-cvd_prob_intervention_fun(cvd_prob_base,relative_risks,dist_temp$means) }
  
  model0_cvd_events[[r]]=lapply(model0_cvd_prob[[r]],function(x) model0_group_init[[r]]*x)
  
  model0_cvd_events_deathoutside[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_deaths_outside_rate),model0_cvd_events[[r]],SIMPLIFY=FALSE)
  
  model0_cvd_events_hosp[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_hosp_rate),model0_cvd_events[[r]],SIMPLIFY=FALSE)
  
  model0_cvd_events_deathinside[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_deaths_inside_rate),model0_cvd_events_hosp[[r]],SIMPLIFY=FALSE)
  
  model0_cost_hosp[[r]]=mapply(function(x,y){sum(x*y) }
                          ,model0_cvd_events_hosp[[r]],lapply(jgp_data,function(z){ matrix(rep(z$mean_cost,4),byrow=TRUE,nrow=4) })  ,SIMPLIFY=FALSE)
  
  model0_total_deaths_cvd[[r]]<-mapply(function(x,y) x+y,model0_cvd_events_deathinside[[r]],model0_cvd_events_deathoutside[[r]],SIMPLIFY=FALSE)
  model0_total_deaths[[r]]<-sapply(data.frame(Reduce("+",model0_total_deaths_cvd[[r]]) + model0_deaths_noncvd[[r]]),sum)
  
  model0_total_costs[[r]]<-sum(model0_cost_hyper[[r]])+Reduce("+",model0_cost_hosp[[r]])
  
  model0_pop_end[[r]]=model0_pop_init[[r]] - model0_total_deaths[[r]]
  rp=as.character(as.numeric(r)+1)
  model0_pop_init[[rp]]=model0_pop_end[[r]] + c(popyear_prog_total[[rp]][41] - popyear_prog_total[[rp]][51],
                                                 popyear_prog_total[[rp]][51] - popyear_prog_total[[rp]][61],
                                                 popyear_prog_total[[rp]][61] - popyear_prog_total[[rp]][71],
                                                 popyear_prog_total[[rp]][71] - popyear_prog_total[[rp]][81],
                                                 popyear_prog_total[[rp]][81]) 
}


## objects model1
model1_pop_init=list()
model1_group_init=list()

model1_cvd_prob<-list()

model1_cost_hyper=list()

model1_deaths_noncvd=list()
model1_cvd_events=list()

model1_cvd_events_deathoutside=list()
model1_cvd_events_deathinside=list()
model1_cvd_events_hosp=list()

model1_cost_hosp=list()

model1_total_deaths_cvd=list()
model1_total_deaths=list()

model1_total_costs=list()

model1_pop_end=list()

reference_sodium=2
current_sodium=4.2
sodium_reduction=current_sodium-reference_sodium

model1_pop_init[["2013"]]<-pop2013

# intervention
for (r in as.character(year_start:year_end)){
  
  dist_temp<-sbp_distribution(model1_pop_init[[r]],sodium_reduction)
  model1_group_init[[r]]<-dist_temp$dist
  model1_cost_hyper[[r]]<-dist_temp$ht_treated*cost_ht_treatment
  
  model1_deaths_noncvd[[r]]=model1_group_init[[r]]*matrix(rep(mort_rates_nocvd[[r]],4),byrow=TRUE,nrow=4)
  
  model1_cvd_prob[[r]]<-cvd_prob_intervention_fun(cvd_prob_base,relative_risks,dist_temp$means)
  model1_cvd_events[[r]]=lapply(model1_cvd_prob[[r]],function(x) model1_group_init[[r]]*x)
  
  model1_cvd_events_deathoutside[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_deaths_outside_rate),model1_cvd_events[[r]],SIMPLIFY=FALSE)
  
  model1_cvd_events_hosp[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_hosp_rate),model1_cvd_events[[r]],SIMPLIFY=FALSE)
  
  model1_cvd_events_deathinside[[r]]=mapply(function(x,y){
    matrix(rep(x,4),nrow=4,byrow=TRUE)*y
  }, data.frame(instances_deaths_inside_rate),model1_cvd_events_hosp[[r]],SIMPLIFY=FALSE)
  
  model1_cost_hosp[[r]]=mapply(function(x,y){sum(x*y) }
                               ,model1_cvd_events_hosp[[r]],lapply(jgp_data,function(z){ matrix(rep(z$mean_cost,4),byrow=TRUE,nrow=4) })  ,SIMPLIFY=FALSE)
  
  model1_total_deaths_cvd[[r]]<-mapply(function(x,y) x+y,model1_cvd_events_deathinside[[r]],model1_cvd_events_deathoutside[[r]],SIMPLIFY=FALSE)
  model1_total_deaths[[r]]<-sapply(data.frame(Reduce("+",model1_total_deaths_cvd[[r]]) + model1_deaths_noncvd[[r]]),sum)
  
  model1_total_costs[[r]]<-sum(model1_cost_hyper[[r]])+Reduce("+",model1_cost_hosp[[r]])
  
  model1_pop_end[[r]]=model1_pop_init[[r]] - model1_total_deaths[[r]]
  rp=as.character(as.numeric(r)+1)
  model1_pop_init[[rp]]=model1_pop_end[[r]] + c(popyear_prog_total[[rp]][41] - popyear_prog_total[[rp]][51],
                                                popyear_prog_total[[rp]][51] - popyear_prog_total[[rp]][61],
                                                popyear_prog_total[[rp]][61] - popyear_prog_total[[rp]][71],
                                                popyear_prog_total[[rp]][71] - popyear_prog_total[[rp]][81],
                                                popyear_prog_total[[rp]][81]) 
}
