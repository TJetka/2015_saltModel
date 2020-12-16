library(stringr)
library(xlsx)
library(Hmisc)

setwd("~/Documents/R/InSalt")

## Initial ####

age_groups=c("40-49","50-59","60-69","70-79","80+")
age_groups_2=c("35-44","45-54","55-64","65-74")
age_groups_means<-c(45,55,65,75,83)
age_pension_mean<-c(rep(64,6),rep(65,8),rep(66,8),rep(67,4))
names(age_pension_mean)<-year_start:year_end
pop0013_df<-read.table(file="Data/Pop/pop0013.csv",sep=",",header=TRUE,colClasses=c("numeric"))
rownames(pop0013_df)<-age_groups
colnames(pop0013_df)<-2000:2013

disease_names=c("ihd","stroke","hypertension","stroke_total")
pop_names=c("noncvd","ihd","stroke","hypertension")

##
icd_codes=list()
icd_codes[[disease_names[1] ]]=c(paste("I",20:25,sep="") )
icd_codes[[disease_names[2] ]]=c(paste("I",c(60:63,65:67,69),sep="") )
icd_codes[[disease_names[4] ]]=c(paste("I",c(60:69),sep="") )
icd_codes[[disease_names[3] ]]=c(paste("I",11:13,sep="") )

year_start=2013
year_end=2038
model_years=year_start:year_end

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

## Mortality data ####

mort_data_list<-list()
mort_years<-c(2002:2012)
mort_gus_gryears<-c("40 - 44|45 - 49","50 - 54|55 - 59","60 - 64|65 - 69","70 - 74|75 - 79","80 - 84|85 - 89|90 - 95|95 lat\ni\nwięcej")

for (i in 1:length(mort_years)){
  mort_data_list[[i]]<-read.xlsx(file=paste("Data/Mortality/pl_zgo_",mort_years[i],"_00_3.xls",sep=""),sheetIndex=1)
}

mort_abs<-lapply(disease_names[1:3],function(x) {
  outx<-sapply(mort_data_list,function(y){
    rowid_temp<-str_detect(as.character(y[,1]),paste(icd_codes[[x]],collapse="|") )
    rowid_temp[is.na(rowid_temp)]<-FALSE
    outy<-sapply(mort_gus_gryears,function(z){
      colid_temp<-str_detect(as.character(unlist(y[6,])),z)
      colid_temp[is.na(colid_temp)]<-FALSE
      df<-y[rowid_temp,colid_temp]
      val<-c(sapply(df,function(x) as.character(x)))
      val[val=="-"]<-0
      val<-as.numeric(val)
      outz<-sum(val)
      outz
    },simplify=TRUE)
    names(outy)<-age_groups
    outy
  },simplify=TRUE)
  colnames(outx)<-mort_years
  outx
})
names(mort_abs)<-disease_names[1:3]

mort_rates<-lapply(mort_abs,function(x){
  outx<-x/pop0013_df[,c(as.character(mort_years))]
})

mort_abs_total<-sapply(mort_data_list,function(y){
  rowid_temp<-str_detect(as.character(y[,1]),"OGÓŁEM" )
  rowid_temp[is.na(rowid_temp)]<-FALSE
  outy<-sapply(mort_gus_gryears,function(z){
    colid_temp<-str_detect(as.character(unlist(y[6,])),z)
    colid_temp[is.na(colid_temp)]<-FALSE
    df<-y[rowid_temp,colid_temp]
    val<-c(sapply(df,function(x) as.character(x)))
    val[val=="-"]<-0
    val<-as.numeric(val)
    outz<-sum(val)
    outz
  },simplify=TRUE)
  names(outy)<-age_groups
  outy
},simplify=TRUE)
colnames(mort_abs_total)<-mort_years
mort_rates_total<-mort_abs_total/pop0013_df[,as.character(mort_years)]

mort_abs_noncvd<-mort_abs_total-Reduce("+",mort_abs)
mort_rates_noncvd<-mort_abs_noncvd/pop0013_df[,as.character(mort_years)]

save.image(file=paste("Data/R/WorkingData",date(),".RData",sep="_"))

## JGP Data 2013####
jgp_dir=dir("Data/JGP/2013")
jgp_data_list<-list()

pop2013_jgp=c(sum(pop0013_df[1:2,14]),sum(pop0013_df[3:4,14]),pop0013_df[5,14])

for (i in 1:length(jgp_dir)){
  sheetxls1=read.xlsx(file=paste("Data/JGP/2013/",jgp_dir[i],sep=""),sheetIndex=1,colClasses=c("character","character","numeric"))
  sheetxls4=read.xlsx(file=paste("Data/JGP/2013/",jgp_dir[i],sep=""),sheetIndex=4,colClasses=c("character","character","numeric","numeric","numeric"))
  sheetxls5=read.xlsx(file=paste("Data/JGP/2013/",jgp_dir[i],sep=""),sheetIndex=5,colClasses=c("character","character","numeric","numeric","numeric"))
  sheetxls9=read.xlsx(file=paste("Data/JGP/2013/",jgp_dir[i],sep=""),sheetIndex=9,colClasses=c("character","character","numeric","numeric","numeric"))
  
  mean_price<-sheetxls1[str_detect(sheetxls1[,1],"(zł)"),3][2]
  num_instances<-sheetxls1[str_detect(sheetxls1[,1],"Liczba wystąpień"),3][1]
  death_rate<-sheetxls4[str_detect(sheetxls4[,1],"zgon pacjenta"),4][1]/100
  
  jgp_data_list[[i]]<-list()
  jgp_data_list[[i]]$mean_price<-mean_price
  jgp_data_list[[i]]$death_rate<-death_rate
  
  jgp_data_list[[i]]$details=lapply(disease_names[1:3],function(x){
    pop_icd_i=c()
    
    #instances extrapolation
    prc_icd_i<-sum( sheetxls9[str_detect(sheetxls9[,1], paste(icd_codes[[x]],collapse="|") ),4] )/100
#     x_reg<-c(50,70,83)
    y_reg<-prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1:3]/pop2013_jgp
#     if ( all(diff(y_reg)>0)|all(diff(y_reg)<0)  ) {
#       y_extr=spline(x=x_reg,y=y_reg,xout=c(45,55,65,75,83),method="hyman")$y
#       } else {
#       y_extr=approxExtrap(x=x_reg,y=y_reg,xout=c(45,55,65,75,83))$y
#       }
#     y_extr=y_extr*(1*(y_extr>0))
#     
#     if (sum(y_extr[1:2])>0) {pop_icd_i[1]=y_extr[1]/sum(y_extr[1:2])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1]} else {pop_icd_i[1]=0}
#     if (sum(y_extr[1:2])>0) {pop_icd_i[2]=y_extr[2]/sum(y_extr[1:2])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][1]} else {pop_icd_i[2]=0}
#     if (sum(y_extr[1:2])>0) {pop_icd_i[3]=y_extr[3]/sum(y_extr[3:4])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][2]} else {pop_icd_i[3]=0}
#     if (sum(y_extr[1:2])>0) {pop_icd_i[4]=y_extr[4]/sum(y_extr[3:4])*prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][2]} else {pop_icd_i[4]=0}
#     pop_icd_i[5]=prc_icd_i*sheetxls5[str_detect(sheetxls5[,1],"41 - 60|61 - 80|81 i więcej"),3][3]
    
    pop_icd_i=c()
    y_instances_jgppop<-y_reg*pop2013_jgp
    ages_comparison<-apply(apply(mort_abs[[x]],2,function(y) { 
   c(y[1]/(y[1]+y[2]),
         y[2]/(y[1]+y[2]),
         y[3]/(y[3]+y[4]),
         y[4]/(y[3]+y[4]))
   }),1,mean)

    pop_icd_i[1]<-y_instances_jgppop[1]*ages_comparison[1]
    pop_icd_i[2]<-y_instances_jgppop[1]*ages_comparison[2]
    pop_icd_i[3]<-y_instances_jgppop[2]*ages_comparison[3]
    pop_icd_i[4]<-y_instances_jgppop[2]*ages_comparison[4]
    pop_icd_i[5]<-y_instances_jgppop[3]
    
    #deaths extrapolation
    death_icd_i=c()
    y_death_jgppop<-y_reg*pop2013_jgp*death_rate
    death_icd_i[1]<-y_death_jgppop[1]*ages_comparison[1]
    death_icd_i[2]<-y_death_jgppop[1]*ages_comparison[2]
    death_icd_i[3]<-y_death_jgppop[2]*ages_comparison[3]
    death_icd_i[4]<-y_death_jgppop[2]*ages_comparison[4]
    death_icd_i[5]<-y_death_jgppop[3]
    
    out=data.frame(age=age_groups,
                   instances=pop_icd_i,
                   cost=pop_icd_i*mean_price,
                   deaths=death_icd_i)
    
    out
  })
  names(jgp_data_list[[i]]$details)<-disease_names[1:3]
}

jgp_data<-lapply(disease_names,function(y){
  Reduce("+",lapply(jgp_data_list,function(x){
    x$details[[y]] }))
})

jgp_data<-lapply(jgp_data,function(x){
  x$age<-age_groups
  x$mean_cost<-x$cost/x$instances
  x$death_rate<-x$deaths/x$instances
  x$instances_rate<-x$instances/pop0013_df[,14]
  x
})

names(jgp_data)<-disease_names[1:3]


## Instances Data ####
instances_abs<-mapply(function(x,y){
  x$instances+(y[,c("2012")]-x$deaths)
},jgp_data[1:3],mort_abs,SIMPLIFY=FALSE)
instances_rates<-lapply(instances_abs,function(x){
  outx<-x/pop0013_df[,c("2012")]
})

instances_hosp_rates<-mapply(function(x,y){
  x$instances/y
},jgp_data[1:3],instances_abs,SIMPLIFY=FALSE)

mort_inst_rate<-mapply(function(x,y){
  x[,11]/y
},mort_abs,instances_abs,SIMPLIFY=FALSE)

mort_hosp_rate<-mapply(function(x,y){
  x$deaths/y[,11]
},jgp_data[1:3],mort_abs,SIMPLIFY=FALSE)

## mort rates extrapolation ####
x_reg=2002:2012
x_out<-model_years

mort_rates_model<-lapply(mort_rates,function(x){
  out<-t(apply(x,1,function(y){
    y_reg=y
    df_temp<-data.frame(x=x_reg,y=log(y_reg))
    reg_model<-lm(y~x,data=df_temp)
    y_extr<-exp(predict(reg_model, newdata=data.frame(x=x_out )))
    y_extr
  }))
  colnames(out)<-x_out
  out
})

mort_rates_total_model<-t(apply(mort_rates_total,1,function(y){
    y_reg=y
    df_temp<-data.frame(x=x_reg,y=log(y_reg))
    reg_model<-lm(y~x,data=df_temp)
    y_extr<-exp(predict(reg_model, newdata=data.frame(x=x_out )))
    y_extr
  }))
  colnames(mort_rates_total_model)<-x_out

mort_rates_noncvd_model<-t(apply(mort_rates_noncvd,1,function(y){
    y_reg=y
    df_temp<-data.frame(x=x_reg,y=log(y_reg))
    reg_model<-lm(y~x,data=df_temp)
    y_extr<-exp(predict(reg_model, newdata=data.frame(x=x_out )))
    y_extr
  }))
  colnames(mort_rates_noncvd_model)<-x_out


## mort rates prognosis ####
pop_prog_xls=read.xlsx(file="Data/Populacja/Prognoza_2014-50.xls",sheetIndex=4,colClasses=c("character","character",rep("numeric",9)))
pop_prog_total=matrix(pop_prog_xls[4:nrow(pop_prog_xls),3],nrow=102)[-c(1,102),]
mort_prog<-(pop_prog_total[1:(nrow(pop_prog_total)-1),1:(ncol(pop_prog_total)-1)]-pop_prog_total[2:(nrow(pop_prog_total)),2:(ncol(pop_prog_total))])
mort_prog_total<-apply(mort_prog,2,function(x){
  out=c()
  out[1]<-sum(x[41:50])
  out[2]<-sum(x[51:60])
  out[3]<-sum(x[61:70])
  out[4]<-sum(x[71:80])
  out[5]<-sum(x[81:99])
  out
})
colnames(mort_prog_total)<-2013:2049
rownames(mort_prog_total)<-age_groups

mort_prog_rates_total<-apply(rbind(mort_prog_total,pop_prog_total[1:(nrow(pop_prog_total)-1),1:(ncol(pop_prog_total)-1)]),2,function(x){
  out=c()
  out[1]<-sum(x[1])/sum(x[46:55])
  out[2]<-sum(x[2])/sum(x[56:65])
  out[3]<-sum(x[3])/sum(x[66:75])
  out[4]<-sum(x[4])/sum(x[76:85])
  out[5]<-sum(x[5])/sum(x[86:104])
  out
})
colnames(mort_prog_rates_total)<-2013:2049
rownames(mort_prog_rates_total)<-age_groups

mort_prog_rates_cvd<-lapply(mort_rates,function(x) {
  x[,c("2012")]*(mort_prog_rates_total/mort_prog_rates_total[,1])
})

mort_prog_rates_noncvd<-mort_prog_rates_total-Reduce("+",mort_prog_rates_cvd)

## Instances prognosis ####
instances_prog_rates_cvd<-lapply(instances_rates,function(x) {
  x*(mort_prog_rates_total/mort_prog_rates_total[,1])
})

## Hypertension prevalence
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


## realative risks # per 10mmHg SBP ####
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
relative_risks_prior_cvd<-list()
relative_risks_prior_cvd[[disease_names[1]]]<-1
relative_risks_prior_cvd[[disease_names[2]]]<-2
relative_risks_prior_cvd[[disease_names[3]]]<-1
relative_risks_dis<-list()
relative_risks_dis[[disease_names[1]]]<-c(1,1.67,1.67,1.67)
relative_risks_dis[[disease_names[2]]]<-c(1,1.67,1.67,1.67)
relative_risks_dis[[disease_names[3]]]<-c(1,1.67,1.67,1.67)

relative_risks_prior_cvd_nocvd<-c(1,2,1)
names(relative_risks_prior_cvd_nocvd)<-disease_names[1:3]

## CVD prediction form 2000-2013 model ####
premodel_years<-colnames(mort_rates[[1]])
premodel_pop<-pop0013_df[,premodel_years]
premodel_simulation<-list()
premodel_simulation[[1]]<-list()
premodel_simulation[[1]]$cvd_pop<-list()
premodel_simulation[[1]]$cvd_pop[[disease_names[1]]]<-rep(0,5)
premodel_simulation[[1]]$cvd_pop[[disease_names[2]]]<-rep(0,5)
premodel_simulation[[1]]$cvd_pop[[disease_names[3]]]<-rep(0,5)
premodel_simulation[[1]]$cvd_deaths<-list()
premodel_simulation[[1]]$cvd_deaths[[disease_names[1]]]<-rep(0,5)
premodel_simulation[[1]]$cvd_deaths[[disease_names[2]]]<-rep(0,5)
premodel_simulation[[1]]$cvd_deaths[[disease_names[3]]]<-rep(0,5)
premodel_simulation[[1]]$noncvd_deaths<-rep(0,5)
for (i in 2:(length(premodel_years))){
  premodel_simulation[[i]]<-list()
  
  premodel_simulation[[i]]$cvd_events<-lapply(instances_rates,function(x) {
    x*premodel_pop[,premodel_years[i-1]]
  })  
  premodel_simulation[[i]]$cvd_deaths<-lapply(mort_rates,function(x) {
    x[,premodel_years[i]]*premodel_pop[,premodel_years[i-1]]
  })  
  
  premodel_simulation[[i]]$noncvd_deaths<-mort_rates_noncvd[,premodel_years[i]]*premodel_pop[,premodel_years[i-1]]
  
  premodel_simulation[[i]]$noncvd_deaths_hcvd<-lapply(disease_names[1:3],function(x){
    premodel_simulation[[i]]$noncvd_deaths * relative_risks_prior_cvd_nocvd[x] * premodel_simulation[[i-1]]$cvd_pop[[x]] /( (premodel_pop[,premodel_years[i-1]]-Reduce("+",premodel_simulation[[i-1]]$cvd_pop)) + apply(do.call(rbind,premodel_simulation[[i-1]]$cvd_pop)*relative_risks_prior_cvd_nocvd,2,sum)  )
  })
  names(premodel_simulation[[i]]$noncvd_deaths_hcvd)<-disease_names[1:3]
  
  premodel_simulation[[i]]$cvd_deaths_hcvd<-lapply(disease_names[1:3],function(x){
    premodel_simulation[[i]]$cvd_deaths[[x]] * relative_risks_prior_cvd[[x]] * Reduce("+",premodel_simulation[[i-1]]$cvd_pop) /( (premodel_pop[,premodel_years[i-1]]-Reduce("+",premodel_simulation[[i-1]]$cvd_pop)) + Reduce("+",premodel_simulation[[i-1]]$cvd_pop)*relative_risks_prior_cvd[[x]] )  
  })
  names(premodel_simulation[[i]]$cvd_deaths_hcvd)<-disease_names[1:3]
  
  premodel_simulation[[i]]$cvd_events_hcvd<-lapply(disease_names[1:3],function(x){
    premodel_simulation[[i]]$cvd_events[[x]] * relative_risks_prior_cvd[[x]] * Reduce("+",premodel_simulation[[i-1]]$cvd_pop) /( (premodel_pop[,premodel_years[i-1]]-Reduce("+",premodel_simulation[[i-1]]$cvd_pop)) + Reduce("+",premodel_simulation[[i-1]]$cvd_pop)*relative_risks_prior_cvd[[x]] )  
  })
  names(premodel_simulation[[i]]$cvd_events_hcvd)<-disease_names[1:3]
  
  premodel_simulation[[i]]$cvd_pop<-lapply(disease_names[1:3],function(x){
    premodel_simulation[[i-1]]$cvd_pop[[x]]+premodel_simulation[[i]]$cvd_events[[x]]-premodel_simulation[[i]]$cvd_events_hcvd[[x]]-premodel_simulation[[i]]$cvd_deaths[[x]]-premodel_simulation[[i]]$noncvd_deaths_hcvd[[x]]
  })
  names(premodel_simulation[[i]]$cvd_pop)<-disease_names[1:3]
}

qaly=(read.table(file="Data/qaly.csv",sep="\t",header=TRUE))
zus=1000*(read.table(file="Data/zus.csv",sep=",",header=TRUE))[,2:6]
absencja=(read.table(file="Data/absencja.csv",sep=",",header=TRUE))[,2:6]
niezdolnosc=(read.table(file="Data/niezdolnosc.csv",sep=",",header=TRUE))[,2:6]

save.image(file="Data/R/premodel_final.RData")


#### Model ####

## parameters
year_start=2013
year_end=2038

age_groups=c("40-49","50-59","60-69","70-79","80+")
age_groups_means<-c(45,55,65,75,83)

relative_risks=relative_risks
cost_ht_treatment=623.28
cost_hosp<-lapply(jgp_data,function(x) x$mean_cost)
interest_rate=0.03
hypertension_treated_eff=0.26
hypertension_treated_all=0.62

qaly=(read.table(file="Data/qaly.csv",sep="\t",header=TRUE))
zus=(read.table(file="Data/zus.csv",sep=",",header=TRUE))[,2:6]
absencja=(read.table(file="Data/absencja.csv",sep=",",header=TRUE))[,2:6]
niezdolnosc=(read.table(file="Data/niezdolnosc.csv",sep=",",header=TRUE))[,2:6]
colnames(zus)<-age_groups
colnames(absencja)<-age_groups
colnames(niezdolnosc)<-age_groups

working_days=251
value_year_work=3650*12
value_day_work=value_year_work/working_days

popyear_prog_total<-data.frame(pop_prog_total)
colnames(popyear_prog_total)<-2013:2050
rownames(popyear_prog_total)<-0:99



#### Model 2 ####

## parameters
year_start=2013
year_end=2038

age_groups=c("40-49","50-59","60-69","70-79","80+")
age_groups_means<-c(45,55,65,75,83)

age_pension_mean<-c(rep(64,6),rep(65,8),rep(66,8),rep(67,4))
names(age_pension_mean)<-year_start:year_end

relative_risks=relative_risks
cost_ht_treatment=623.28
cost_hosp<-lapply(jgp_data,function(x) x$mean_cost)
interest_rate=0.03
hypertension_treated_eff=0.26
hypertension_treated_all=0.62

qaly=(read.table(file="Data/qaly.csv",sep="\t",header=TRUE))
zus=1000*(read.table(file="Data/zus.csv",sep=",",header=TRUE))[,2:6]
absencja=(read.table(file="Data/absencja.csv",sep=",",header=TRUE))[,2:6]
niezdolnosc=(read.table(file="Data/niezdolnosc.csv",sep=",",header=TRUE))[,2:6]

func_zus<-function(x){
  colnames(x)<-age_groups
  x=split(x,1:nrow(x))
  names(x)<-disease_names[1:3]
  x
}

zus<-func_zus(zus)
absencja<-func_zus(absencja)
niezdolnosc<-func_zus(niezdolnosc)

working_days=251
value_year_work=3650*12
value_day_work=value_year_work/working_days

popyear_prog_total<-data.frame(pop_prog_total)
colnames(popyear_prog_total)<-2013:2050
rownames(popyear_prog_total)<-0:99