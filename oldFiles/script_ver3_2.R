load(file="Data/R/premodel_final.RData")

#### Model ####

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
      id_treated=sample(x=(1:length(hyper_real_obs)),size=floor(hypertension_treated_eff*length(hyper_real_obs)),replace=FALSE)
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
      id_treated=sample(1:length(hyper_real_obs),floor(hypertension_treated_eff*length(hyper_real_obs)),replace=FALSE)
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
  out$ht_treated_eff<-pop_hyper[9,]
  out$ht_treated_all<-(62/26)*pop_hyper[9,]
  out$hyper_total<-pop_hyper[10,]
  out
}

cvd_prob_fun<-function(rates,rr_sbp,rr_dis,mean_sbp,pop){
  
  pop_prc<-apply(do.call(rbind,pop),2,function(x){
    x/sum(x)
  })
  
  coeff<-lapply(mean_sbp,function(x) {apply(x,2,function(z){(z-115)/10})})
  out<-mapply(function(x,y,z){
    tmp_rates<-lapply(coeff, function(xx) {
      outxx<-list()
      outxx<-matrix(unlist(mapply(function(v,w) v^w,as.list(y),as.data.frame(xx),SIMPLIFY=FALSE)),nrow=4)
      outxx
    })
    
    de_tmp= apply(do.call(rbind,tmp_rates) * pop_prc * c(t(matrix(rep(1,4*4),nrow=4) * z)),2,sum)
    p1=x/de_tmp
    prob=list()
    for (jj in 1:length(z)){
      prob[[jj]]<-tmp_rates[[jj]]*matrix(rep(p1*z[jj],4),nrow=4, byrow=TRUE)
    }
    names(prob)<-names(mean_sbp)
    prob
  },rates,rr_sbp,rr_dis,SIMPLIFY=FALSE)
  names(out)<-names(rates)
  out
}

cvd_prob_intervention_fun<-function(cvd_prob,rr_sbp,rr_dis,mean_sbp){
  coeff<-lapply(mean_sbp,function(x) {apply(x,2,function(z){(z-115)/10})})
  out<-mapply(function(x,y,z){
    tmp_rates<-lapply(coeff, function(xx) {
      outxx<-matrix(unlist(mapply(function(v,w) v^w,as.list(y),as.data.frame(xx),SIMPLIFY=FALSE)),nrow=4)
      outxx
    })
    prob=list()
    for (jj in 1:length(z)){
      prob[[jj]]<-tmp_rates[[jj]]*matrix(rep( x[[1]][1,]*z[jj],4),nrow=4, byrow=TRUE)
    }
    names(prob)<-names(mean_sbp)
    prob
  },cvd_prob,rr_sbp,rr_dis,SIMPLIFY=FALSE)
  names(out)<-names(cvd_prob)
  out
}

## objects
model0_pop_nocvd_init=list()
model0_pop_cvd_init=list()
model0_pop_init=list()

model0_group_nocvd_init=list()
model0_group_cvd_init=list()
model0_group_init=list()

model0_cvd_prob_deaths<-list()
model0_cvd_prob_instances<-list()

model0_deaths_cvd<-list()
model0_instances_cvd<-list()
model0_deaths_noncvd<-list()
model0_total_deaths=list()

model0_instances_hosp_cvd<-list()

model0_cost_hyper=list()
model0_cost_hosp=list()
model0_cost_hyper_sum=list()
model0_cost_hosp_sum=list()
model0_cost_zus=list()
model0_cost_absencja=list()
model0_cost_niezdolnosc=list()

model0_qaly=list()
model0_total_costs=list()

model0_instances_cvd_total=list()
model0_survive_cvd=list()
model0_survive_total=list()
model0_pop_groups_end=list()
model0_pop_end=list()

model0_pop_end=list()
cvd_prob_base<-list()

## model RUN ####

#init
model0_pop_nocvd_init[["2013"]]<-list(noncvd=(pop2013-Reduce("+",premodel_simulation[[length(premodel_years)]]$cvd_pop)))
model0_pop_cvd_init[["2013"]]<-premodel_simulation[[length(premodel_years)]]$cvd_pop
model0_pop_init[["2013"]]<-c(model0_pop_nocvd_init[["2013"]],model0_pop_cvd_init[["2013"]])

  cost_unit_zus<-mapply(function(x,y){y/x},model0_pop_cvd_init[["2013"]],zus,SIMPLIFY=FALSE)
  cost_unit_absencja<-mapply(function(x,y){ (y/x)*value_day_work },model0_pop_cvd_init[["2013"]],absencja,SIMPLIFY=FALSE)
  cost_unit_niezdolnosc<-mapply(function(x,y){(y/x)},model0_pop_cvd_init[["2013"]],niezdolnosc,SIMPLIFY=FALSE)
  
  # ceteris paribus
  for (r in as.character(year_start:year_end)){
    
    dist_temp<-lapply(model0_pop_init[[r]], function(x) sbp_distribution(x,0))
    
    model0_group_init[[r]]<-lapply(dist_temp,function(x) x$dist)
    
    model0_deaths_noncvd[[r]]=lapply(model0_group_init[[r]],function(x) x*matrix(rep(mort_prog_rates_noncvd[,r],4),byrow=TRUE,nrow=4) )
    
    model0_cvd_prob_deaths[[r]]<-cvd_prob_fun(lapply(mort_prog_rates_cvd,function(x) x[,r]),relative_risks,relative_risks_dis,lapply(dist_temp,function(x) x$means),do.call(rbind,lapply(dist_temp,function(x) apply(x$dist,2,sum))))
    
    model0_deaths_cvd[[r]]=lapply(model0_cvd_prob_deaths[[r]], function(z) {mapply(function(x,y) x*y ,model0_group_init[[r]],z,SIMPLIFY=FALSE )})
    
    model0_instances_cvd[[r]]=mapply(function(x,y){
      lapply(x, function(z){ z/matrix(rep(y,4),byrow=TRUE,nrow=4) })
    },model0_deaths_cvd[[r]],mort_inst_rate,SIMPLIFY=FALSE)
    
    model0_instances_hosp_cvd[[r]]=mapply(function(x,y){
      lapply(x, function(z){ z*matrix(rep(y,4),byrow=TRUE,nrow=4) })
    },model0_instances_cvd[[r]],instances_hosp_rates,SIMPLIFY=FALSE)
    
    model0_cost_hosp[[r]]=mapply(function(x,y){ lapply(x, function(z) { z*matrix(rep(y,4),byrow=TRUE,nrow=4) }) }
                                 ,model0_instances_hosp_cvd[[r]],cost_hosp[1:3] ,SIMPLIFY=FALSE)
    
    model0_cost_hyper[[r]]<-(Reduce("+", lapply(dist_temp,function(y) y$ht_treated_all) ))*cost_ht_treatment
    
    model0_cost_hosp_sum[[r]]<-sum(unlist(model0_cost_hosp[[r]]))
    model0_cost_hyper_sum[[r]]<-sum(model0_cost_hyper[[r]])
    
    model0_total_costs[[r]]<-model0_cost_hosp_sum[[r]]+model0_cost_hyper_sum[[r]]  
    
    model0_instances_cvd_total[[r]] = lapply(pop_names, function(x) { model0_instances_cvd[[r]][[disease_names[1]]][[x]]+model0_instances_cvd[[r]][[disease_names[2]]][[x]]+model0_instances_cvd[[r]][[disease_names[3]]][[x]]  }  )
    model0_survive_cvd[[r]] = mapply(function(x,y){ mapply(function(w,z) w-z ,x,y,SIMPLIFY=FALSE) },model0_instances_cvd[[r]],model0_deaths_cvd[[r]],SIMPLIFY=FALSE)
    model0_survive_total[[r]] = c(list(noncvd = matrix(0,nrow=4,ncol=5)),lapply(model0_survive_cvd[[r]],function(x) { Reduce("+",x) }) )
    
    model0_pop_groups_end[[r]]=mapply(function(x,y,z,w){x-y-z+w},model0_group_init[[r]], model0_deaths_noncvd[[r]], model0_instances_cvd_total[[r]] , model0_survive_total[[r]],SIMPLIFY=FALSE)
    model0_pop_end[[r]]=lapply(model0_pop_groups_end[[r]],function(x){ apply(x,2,sum) })
    pop_frac_tmp<-sapply(model0_pop_end[[r]],function(x) {
      sum(x)
    })
    pop_frac_tmp<-pop_frac_tmp/sum(pop_frac_tmp)
    
    rp=as.character(as.numeric(r)+1)
    model0_pop_init[[rp]]=mapply(function(x,y){
      x + y*c(popyear_prog_total[[rp]][41] - popyear_prog_total[[rp]][51],
              popyear_prog_total[[rp]][51] - popyear_prog_total[[rp]][61],
              popyear_prog_total[[rp]][61] - popyear_prog_total[[rp]][71],
              popyear_prog_total[[rp]][71] - popyear_prog_total[[rp]][81],
              popyear_prog_total[[rp]][81])
    },model0_pop_end[[r]],pop_frac_tmp,SIMPLIFY=FALSE)
    
    model0_qaly[[r]] <- sum(mapply(function(x,y){
      sum(x)*(1-y)
    },model0_pop_end[[r]],qaly,SIMPLIFY=TRUE))
    
    model0_cost_zus[[r]]<-mapply(function(x,y){
        sum(x*y)
    },model0_pop_init[[r]][2:4],cost_unit_zus,SIMPLIFY=FALSE)
    
    model0_cost_absencja[[r]]<-mapply(function(x,y){
      sum(x*y)
    },model0_pop_init[[r]][2:4],cost_unit_absencja,SIMPLIFY=FALSE)
    
    model0_cost_niezdolnosc[[r]]<-mapply(function(x,y){
      sum(x*y*value_year_work*(age_pension_mean[r]-age_groups_means))
    },model0_pop_init[[r]][2:4],cost_unit_niezdolnosc,SIMPLIFY=FALSE)
    
}



## objects
model1_pop_nocvd_init=list()
model1_pop_cvd_init=list()
model1_pop_init=list()

model1_group_nocvd_init=list()
model1_group_cvd_init=list()
model1_group_init=list()

model1_cvd_prob_deaths<-list()
model1_cvd_prob_instances<-list()

model1_deaths_cvd<-list()
model1_instances_cvd<-list()
model1_deaths_noncvd<-list()
model1_total_deaths=list()

model1_instances_hosp_cvd<-list()

model1_cost_hyper=list()
model1_cost_hosp=list()
model1_cost_hyper_sum=list()
model1_cost_hosp_sum=list()
model1_cost_zus=list()
model1_cost_absencja=list()
model1_cost_niezdolnosc=list()

model1_qaly=list()
model1_total_costs=list()

model1_instances_cvd_total=list()
model1_survive_cvd=list()
model1_survive_total=list()
model1_pop_groups_end=list()
model1_pop_end=list()

model1_pop_end=list()
cvd_prob_base<-list()

## model RUN ####

reference_sodium=c(1.52, 1.52, 2.00, 2.00, 2.22, 2.95, 3.70)
current_sodium=c(  3.90, 5.40, 5.40, 3.90, 3.90, 3.90, 5.40)

for (ii in 1:length(reference_sodium)){
sodium_reduction=current_sodium[ii]-reference_sodium[ii]

#init
model1_pop_nocvd_init[["2013"]]<-list(noncvd=(pop2013-Reduce("+",premodel_simulation[[length(premodel_years)]]$cvd_pop)))
model1_pop_cvd_init[["2013"]]<-premodel_simulation[[length(premodel_years)]]$cvd_pop
model1_pop_init[["2013"]]<-c(model1_pop_nocvd_init[["2013"]],model1_pop_cvd_init[["2013"]])

# ceteris paribus
for (r in as.character(year_start:year_end)){
  
  dist_temp<-lapply(model1_pop_init[[r]], function(x) sbp_distribution(x,sodium_reduction))
  
  model1_group_init[[r]]<-lapply(dist_temp,function(x) x$dist)
  
  model1_deaths_noncvd[[r]]=lapply(model1_group_init[[r]],function(x) x*matrix(rep(mort_prog_rates_noncvd[,r],4),byrow=TRUE,nrow=4) )
  
  model1_cvd_prob_deaths[[r]]<-cvd_prob_intervention_fun( model0_cvd_prob_deaths[[r]],relative_risks,relative_risks_dis,lapply(dist_temp,function(x) x$means) )
  
  model1_deaths_cvd[[r]]=lapply(model1_cvd_prob_deaths[[r]], function(z) {mapply(function(x,y) x*y ,model1_group_init[[r]],z,SIMPLIFY=FALSE )})
  
  model1_instances_cvd[[r]]=mapply(function(x,y){
    lapply(x, function(z){ z/matrix(rep(y,4),byrow=TRUE,nrow=4) })
  },model1_deaths_cvd[[r]],mort_inst_rate,SIMPLIFY=FALSE)
  
  model1_instances_hosp_cvd[[r]]=mapply(function(x,y){
    lapply(x, function(z){ z*matrix(rep(y,4),byrow=TRUE,nrow=4) })
  },model1_instances_cvd[[r]],instances_hosp_rates,SIMPLIFY=FALSE)
  
  model1_cost_hosp[[r]]=mapply(function(x,y){ lapply(x, function(z) { z*matrix(rep(y,4),byrow=TRUE,nrow=4) }) }
                               ,model1_instances_hosp_cvd[[r]],cost_hosp[1:3] ,SIMPLIFY=FALSE)
  
  model1_cost_hyper[[r]]<-(Reduce("+", lapply(dist_temp,function(y) y$ht_treated_all) ))*cost_ht_treatment
  
  model1_cost_hosp_sum[[r]]<-sum(unlist(model1_cost_hosp[[r]]))
  model1_cost_hyper_sum[[r]]<-sum(model1_cost_hyper[[r]])
  
  model1_total_costs[[r]]<-model1_cost_hosp_sum[[r]]+model1_cost_hyper_sum[[r]]  
  
  model1_instances_cvd_total[[r]] = lapply(pop_names, function(x) { model1_instances_cvd[[r]][[disease_names[1]]][[x]]+model1_instances_cvd[[r]][[disease_names[2]]][[x]]+model1_instances_cvd[[r]][[disease_names[3]]][[x]]  }  )
  model1_survive_cvd[[r]] = mapply(function(x,y){ mapply(function(w,z) w-z ,x,y,SIMPLIFY=FALSE) },model1_instances_cvd[[r]],model1_deaths_cvd[[r]],SIMPLIFY=FALSE)
  model1_survive_total[[r]] = c(list(noncvd = matrix(0,nrow=4,ncol=5)),lapply(model1_survive_cvd[[r]],function(x) { Reduce("+",x) }) )
  
  model1_pop_groups_end[[r]]=mapply(function(x,y,z,w){x-y-z+w},model1_group_init[[r]], model1_deaths_noncvd[[r]], model1_instances_cvd_total[[r]] , model1_survive_total[[r]],SIMPLIFY=FALSE)
  model1_pop_end[[r]]=lapply(model1_pop_groups_end[[r]],function(x){ apply(x,2,sum) })
  pop_frac_tmp<-sapply(model1_pop_end[[r]],function(x) {
    sum(x)
  })
  pop_frac_tmp<-pop_frac_tmp/sum(pop_frac_tmp)
  
  rp=as.character(as.numeric(r)+1)
  model1_pop_init[[rp]]=mapply(function(x,y){
    x + y*c(popyear_prog_total[[rp]][41] - popyear_prog_total[[rp]][51],
            popyear_prog_total[[rp]][51] - popyear_prog_total[[rp]][61],
            popyear_prog_total[[rp]][61] - popyear_prog_total[[rp]][71],
            popyear_prog_total[[rp]][71] - popyear_prog_total[[rp]][81],
            popyear_prog_total[[rp]][81])
  },model1_pop_end[[r]],pop_frac_tmp,SIMPLIFY=FALSE)
  
  model1_qaly[[r]] <- sum(mapply(function(x,y){
    sum(x)*(1-y)
  },model1_pop_end[[r]],qaly,SIMPLIFY=TRUE))
  
  model1_cost_zus[[r]]<-mapply(function(x,y){
    sum(x*y)
  },model1_pop_init[[r]][2:4],cost_unit_zus,SIMPLIFY=FALSE)
  
  model1_cost_absencja[[r]]<-mapply(function(x,y){
    sum(x*y)
  },model1_pop_init[[r]][2:4],cost_unit_absencja,SIMPLIFY=FALSE)
  
  model1_cost_niezdolnosc[[r]]<-mapply(function(x,y){
    sum(x*y*value_year_work*(age_pension_mean[r]-age_groups_means))
  },model1_pop_init[[r]][2:4],cost_unit_niezdolnosc,SIMPLIFY=FALSE)
}

## Comparison ####

# deaths
comparison_deaths<-mapply(function(x,y){
  sum(mapply(function(w,z){
    out=sum(Reduce("+",w)-Reduce("+",z))
    out
  },x,y,SIMPLIFY=TRUE))
},model0_deaths_cvd,model1_deaths_cvd,SIMPLIFY=TRUE)

# costs direct
comparison_direct<-mapply(function(x,y){
 x-y
},model0_total_costs,model1_total_costs,SIMPLIFY=TRUE)

# qaly
comparison_qaly<-mapply(function(x,y){
  x-y
},model0_qaly,model1_qaly,SIMPLIFY=TRUE)

# public
comparison_public<-mapply(function(x,y,z){
  x+Reduce("+",y)-Reduce("+",z)
},comparison_direct,model0_cost_zus,model1_cost_zus,SIMPLIFY=TRUE)

# indirect cost
comparison_indirect<-mapply(function(x,y,z,w){
  Reduce("+",x)+Reduce("+",z)-Reduce("+",y)-Reduce("+",w)
},model0_cost_absencja,model1_cost_absencja,model0_cost_niezdolnosc,model1_cost_niezdolnosc,SIMPLIFY=TRUE)

save(comparison_direct,comparison_deaths,comparison_indirect,comparison_qaly,comparison_public,file=paste("Data/Output/_",reference_sodium[ii],"_",current_sodium[ii],".RData",sep="") )
}

## Comparisons

for (i in 1:length(reference_sodium)){
filename=paste("Data/Output/_",reference_sodium[i],"_",current_sodium[i],".RData",sep="")
load(filename)

mean_death=mean(comparison_deaths)
total_death=sum(comparison_deaths)

total_cost_direct=sum(comparison_direct*(1/(1+interest_rate)^(0:(length(comparison_direct)-1))))
total_qualy=sum(comparison_qaly*(1/(1+interest_rate)^(0:(length(comparison_direct)-1))))
total_cost_public=sum(comparison_public*(1/(1+interest_rate)^(0:(length(comparison_direct)-1))))
total_cost_indirect=sum(comparison_indirect*(1/(1+interest_rate)^(0:(length(comparison_direct)-1))))

campaign_cost_annualy<-5*10^7
total_campaign_cost<-sum(5*10^7*(1/(1+interest_rate)^(0:(length(comparison_direct)-1))))

write.table(data.frame(total_cost_direct,total_cost_indirect,total_cost_public,total_death,mean_death,total_qualy,total_campaign_cost),
            file=paste("Data/Output/Output_",reference_sodium[i],"_",current_sodium[i],".txt",sep=""),col.names=TRUE)
}

