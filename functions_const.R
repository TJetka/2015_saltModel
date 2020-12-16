## constant functions
sbp_reduce<-function(slt_reduce,age,hypertension){
  (slt_reduce/2.30)*(-3.735-0.105*(age-50)-1.874*(1*(hypertension) ) )
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

## function 0 ####
##################
saltApp_main_0<-function(model_in_const,model_in_var){

model_out<-list()  

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

## initial
model0_pop_nocvd_init[[model_in_const$year_start_ch]]<-list(noncvd=(model_in_const$pop2013-Reduce("+",model_in_const$premodel_simulation[[length(model_in_const$premodel_years)]]$cvd_pop)))
model0_pop_cvd_init[[model_in_const$year_start_ch]]<-model_in_const$premodel_simulation[[length(model_in_const$premodel_years)]]$cvd_pop
model0_pop_init[[model_in_const$year_start_ch]]<-c(model0_pop_nocvd_init[[model_in_const$year_start_ch]],model0_pop_cvd_init[[model_in_const$year_start_ch]])

model_in_const$cost_unit_zus<-mapply(function(x,y){y/x},model0_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$zus,SIMPLIFY=FALSE)
model_in_const$cost_unit_absencja<-mapply(function(x,y){ (y/x)*model_in_var$value_day_work },model0_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$absencja,SIMPLIFY=FALSE)
model_in_const$cost_unit_niezdolnosc<-mapply(function(x,y){(y/x)},model0_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$niezdolnosc,SIMPLIFY=FALSE)
  
  # ceteris paribus
  for (r in as.character(model_in_const$year_start:model_in_var$year_end)){
    
    dist_temp<-lapply(model0_pop_init[[r]], function(x) sbp_distribution(x,0))
    
    model0_group_init[[r]]<-lapply(dist_temp,function(x) x$dist)
    
    model0_deaths_noncvd[[r]]=lapply(model0_group_init[[r]],function(x) x*matrix(rep(model_in_const$mort_prog_rates_noncvd[,r],4),byrow=TRUE,nrow=4) )
    
    model0_cvd_prob_deaths[[r]]<-cvd_prob_fun(lapply(model_in_const$mort_prog_rates_cvd,function(x) x[,r]),model_in_const$relative_risks,model_in_const$relative_risks_dis,lapply(dist_temp,function(x) x$means), model0_group_init[[r]])
    
    model0_deaths_cvd[[r]]=lapply(model0_cvd_prob_deaths[[r]], function(z) {mapply(function(x,y) x*y ,model0_group_init[[r]],z,SIMPLIFY=FALSE )})
    
    model0_instances_cvd[[r]]=mapply(function(x,y){
      lapply(x, function(z){ z/matrix(rep(y,4),byrow=TRUE,nrow=4) })
    },model0_deaths_cvd[[r]],model_in_const$mort_inst_rate,SIMPLIFY=FALSE)
    
    model0_instances_hosp_cvd[[r]]=mapply(function(x,y){
      lapply(x, function(z){ z*matrix(rep(y,4),byrow=TRUE,nrow=4) })
    },model0_instances_cvd[[r]],model_in_const$instances_hosp_rates,SIMPLIFY=FALSE)
    
    model0_cost_hosp[[r]]=mapply(function(x,y){ lapply(x, function(z) { z*matrix(rep(y,4),byrow=TRUE,nrow=4) }) }
                                 ,model0_instances_hosp_cvd[[r]],model_in_const$cost_hosp[1:3] ,SIMPLIFY=FALSE)
    
    model0_cost_hyper[[r]]<-(Reduce("+", lapply(dist_temp,function(y) y$ht_treated_all) ))*model_in_var$cost_ht_treatment
    
    model0_cost_hosp_sum[[r]]<-sum(unlist(model0_cost_hosp[[r]]))
    model0_cost_hyper_sum[[r]]<-sum(model0_cost_hyper[[r]])
    
    model0_total_costs[[r]]<-model0_cost_hosp_sum[[r]]+model0_cost_hyper_sum[[r]]  
    
    model0_instances_cvd_total[[r]] = lapply(model_in_const$pop_names, function(x) { model0_instances_cvd[[r]][[model_in_const$disease_names[1]]][[x]]+model0_instances_cvd[[r]][[model_in_const$disease_names[2]]][[x]]+model0_instances_cvd[[r]][[model_in_const$disease_names[3]]][[x]]  }  )
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
      x + y*c(model_in_const$popyear_prog_total[[rp]][41] - model_in_const$popyear_prog_total[[rp]][51],
              model_in_const$popyear_prog_total[[rp]][51] - model_in_const$popyear_prog_total[[rp]][61],
              model_in_const$popyear_prog_total[[rp]][61] - model_in_const$popyear_prog_total[[rp]][71],
              model_in_const$popyear_prog_total[[rp]][71] - model_in_const$popyear_prog_total[[rp]][81],
              model_in_const$popyear_prog_total[[rp]][81])
    },model0_pop_end[[r]],pop_frac_tmp,SIMPLIFY=FALSE)
    
    model0_qaly[[r]] <- sum(mapply(function(x,y){
      sum(x)*(1-y)
    },model0_pop_end[[r]],model_in_const$qaly,SIMPLIFY=TRUE))
    
    model0_cost_zus[[r]]<-mapply(function(x,y){
        sum(x*y)
    },model0_pop_init[[r]][2:4],model_in_const$cost_unit_zus,SIMPLIFY=FALSE)
    
    model0_cost_absencja[[r]]<-mapply(function(x,y){
      sum(x*y)
    },model0_pop_init[[r]][2:4],model_in_const$cost_unit_absencja,SIMPLIFY=FALSE)
    
    model0_cost_niezdolnosc[[r]]<-mapply(function(x,y){
      sum(x*y*model_in_var$value_year_work*(model_in_const$age_pension_mean[r]-model_in_const$age_groups_means))
    },model0_pop_init[[r]][2:4],model_in_const$cost_unit_niezdolnosc,SIMPLIFY=FALSE)
    
}

model_out$instances<-model0_instances_cvd
model_out$instances<-model0_instances_hosp_cvd
model_out$pop<-model0_pop_init
model_out$deaths_cvd<-model0_deaths_cvd
model_out$total_costs <-model0_total_costs 
model_out$qaly<-model0_qaly
model_out$cost_zus<-model0_cost_zus
model_out$cost_absencja<-model0_cost_absencja
model_out$cost_niezdolnosc<-model0_cost_niezdolnosc
model_out$cvd_prob_deaths<-model0_cvd_prob_deaths

model_out
} #end of function saltApp_main


## function 1 ####
##################
saltApp_main_1<-function(model_in_const,model_in_var,model0_out){

model_out<-list()
  
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
## model RUN ####

model_in_var$sodium_reduction=model_in_var$current_sodium-model_in_var$reference_sodium

#init
model1_pop_nocvd_init[["2013"]]<-list(noncvd=(model_in_const$pop2013-Reduce("+",model_in_const$premodel_simulation[[length(model_in_const$premodel_years)]]$cvd_pop)))
model1_pop_cvd_init[["2013"]]<-model_in_const$premodel_simulation[[length(model_in_const$premodel_years)]]$cvd_pop
model1_pop_init[["2013"]]<-c(model1_pop_nocvd_init[["2013"]],model1_pop_cvd_init[["2013"]])

model_in_const$cost_unit_zus<-mapply(function(x,y){y/x},model1_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$zus,SIMPLIFY=FALSE)
model_in_const$cost_unit_absencja<-mapply(function(x,y){ (y/x)*model_in_var$value_day_work },model1_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$absencja,SIMPLIFY=FALSE)
model_in_const$cost_unit_niezdolnosc<-mapply(function(x,y){(y/x)},model1_pop_cvd_init[[model_in_const$year_start_ch]],model_in_const$niezdolnosc,SIMPLIFY=FALSE)


# ceteris paribus
for (r in as.character(model_in_const$year_start:model_in_var$year_end)){
  
  dist_temp<-lapply(model1_pop_init[[r]], function(x) sbp_distribution(x,model_in_var$sodium_reduction))
  
  model1_group_init[[r]]<-lapply(dist_temp,function(x) x$dist)
  
  model1_deaths_noncvd[[r]]=lapply(model1_group_init[[r]],function(x) x*matrix(rep(model_in_const$mort_prog_rates_noncvd[,r],4),byrow=TRUE,nrow=4) )
  
  model1_cvd_prob_deaths[[r]]<-cvd_prob_intervention_fun( model0_out$cvd_prob_deaths[[r]],model_in_const$relative_risks,model_in_const$relative_risks_dis,lapply(dist_temp,function(x) x$means) )
  
  model1_deaths_cvd[[r]]=lapply(model1_cvd_prob_deaths[[r]], function(z) {mapply(function(x,y) x*y ,model1_group_init[[r]],z,SIMPLIFY=FALSE )})

  model1_instances_cvd[[r]]=mapply(function(x,y){
    lapply(x, function(z){ z/matrix(rep(y,4),byrow=TRUE,nrow=4) })
  },model1_deaths_cvd[[r]],model_in_const$mort_inst_rate,SIMPLIFY=FALSE)
  
  model1_instances_hosp_cvd[[r]]=mapply(function(x,y){
    lapply(x, function(z){ z*matrix(rep(y,4),byrow=TRUE,nrow=4) })
  },model1_instances_cvd[[r]],model_in_const$instances_hosp_rates,SIMPLIFY=FALSE)
  
  model1_cost_hosp[[r]]=mapply(function(x,y){ lapply(x, function(z) { z*matrix(rep(y,4),byrow=TRUE,nrow=4) }) }
                               ,model1_instances_hosp_cvd[[r]],model_in_const$cost_hosp[1:3] ,SIMPLIFY=FALSE)
  
  model1_cost_hyper[[r]]<-(Reduce("+", lapply(dist_temp,function(y) y$ht_treated_all) ))*model_in_var$cost_ht_treatment
  
  model1_cost_hosp_sum[[r]]<-sum(unlist(model1_cost_hosp[[r]]))
  model1_cost_hyper_sum[[r]]<-sum(model1_cost_hyper[[r]])

  model1_total_costs[[r]]<-model1_cost_hosp_sum[[r]]+model1_cost_hyper_sum[[r]]  
  
  model1_instances_cvd_total[[r]] = lapply(model_in_const$pop_names, function(x) { model1_instances_cvd[[r]][[model_in_const$disease_names[1]]][[x]]+model1_instances_cvd[[r]][[model_in_const$disease_names[2]]][[x]]+model1_instances_cvd[[r]][[model_in_const$disease_names[3]]][[x]]  }  )
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
    x + y*c(model_in_const$popyear_prog_total[[rp]][41] - model_in_const$popyear_prog_total[[rp]][51],
            model_in_const$popyear_prog_total[[rp]][51] - model_in_const$popyear_prog_total[[rp]][61],
            model_in_const$popyear_prog_total[[rp]][61] - model_in_const$popyear_prog_total[[rp]][71],
            model_in_const$popyear_prog_total[[rp]][71] - model_in_const$popyear_prog_total[[rp]][81],
            model_in_const$popyear_prog_total[[rp]][81])
  },model1_pop_end[[r]],pop_frac_tmp,SIMPLIFY=FALSE)
  
  model1_qaly[[r]] <- sum(mapply(function(x,y){
    sum(x)*(1-y)
  },model1_pop_end[[r]],model_in_const$qaly,SIMPLIFY=TRUE))
 
  model1_cost_zus[[r]]<-mapply(function(x,y){
    sum(x*y)
  },model1_pop_init[[r]][2:4],model_in_const$cost_unit_zus,SIMPLIFY=FALSE)
  
  model1_cost_absencja[[r]]<-mapply(function(x,y){
    sum(x*y)
  },model1_pop_init[[r]][2:4],model_in_const$cost_unit_absencja,SIMPLIFY=FALSE)
  
  model1_cost_niezdolnosc[[r]]<-mapply(function(x,y){
    sum(x*y*model_in_var$value_year_work*(model_in_const$age_pension_mean[r]-model_in_const$age_groups_means))
  },model1_pop_init[[r]][2:4],model_in_const$cost_unit_niezdolnosc,SIMPLIFY=FALSE)
}

model_out$instances<-model1_instances_cvd
model_out$instances<-model1_instances_hosp_cvd
model_out$pop<-model1_pop_init
model_out$deaths_cvd<-model1_deaths_cvd
model_out$total_costs <-model1_total_costs 
model_out$qaly<-model1_qaly
model_out$cost_zus<-model1_cost_zus
model_out$cost_absencja<-model1_cost_absencja
model_out$cost_niezdolnosc<-model1_cost_niezdolnosc
model_out$cvd_prob_deaths<-model1_cvd_prob_deaths  

model_out
} # end of function saltApp_main_1


## Comparison ####
saltApp_main_comp<-function(model0_out,model1_out,model_in_const){

  comp_out<-list()
  
# deaths
comparison_deaths<-mapply(function(x,y){
  sum(mapply(function(w,z){
    out=sum(Reduce("+",w)-Reduce("+",z))
    out
  },x,y,SIMPLIFY=TRUE))
},model0_out$deaths_cvd,model1_out$deaths_cvd,SIMPLIFY=TRUE)

# costs direct
comparison_direct<-mapply(function(x,y){
 x-y
},model0_out$total_costs,model1_out$total_costs,SIMPLIFY=TRUE)

# qaly
comparison_qaly<-mapply(function(x,y){
  x-y
},model0_out$qaly,model1_out$qaly,SIMPLIFY=TRUE)

# public
comparison_public<-mapply(function(x,y,z){
  x+Reduce("+",y)-Reduce("+",z)
},comparison_direct,model0_out$cost_zus,model1_out$cost_zus,SIMPLIFY=TRUE)

# indirect cost
comparison_indirect<-mapply(function(x,y,z,w){
  Reduce("+",x)+Reduce("+",z)-Reduce("+",y)-Reduce("+",w)
},model0_out$cost_absencja,model1_out$cost_absencja,model0_out$cost_niezdolnosc,model1_out$cost_niezdolnosc,SIMPLIFY=TRUE)

campaign_cost_annualy <- c(rep(model_in_var$campaign_cost1,min(5,length(comparison_direct)) ),rep(model_in_var$campaign_cost2, max(0,length(comparison_direct)-5) ))
## Comparisons

##TUTAJ##
comp_out$mean_death=mean(comparison_deaths)
comp_out$total_death=sum(comparison_deaths)
comp_out$total_cost_direct=sum(comparison_direct*(1/(1+model_in_var$interest_rate)^(0:(length(comparison_direct)-1))))
comp_out$total_qaly=sum(comparison_qaly*(1/(1+model_in_var$interest_rate)^(0:(length(comparison_direct)-1))))
comp_out$total_cost_public=sum(comparison_public*(1/(1+model_in_var$interest_rate)^(0:(length(comparison_direct)-1))))
comp_out$total_cost_indirect=sum(comparison_indirect*(1/(1+model_in_var$interest_rate)^(0:(length(comparison_direct)-1))))
comp_out$total_campaign_cost<-sum( campaign_cost_annualy*(1/(1+model_in_var$interest_rate)^(0:(length(comparison_direct)-1))))

comp_out$model0<-model0_out
comp_out$model1<-model1_out

comp_out
}

output_write<-function(model_in_var,comp_out){
  write.table(data.frame(comp_out$total_cost_direct,
                         comp_out$total_cost_indirect,
                         comp_out$total_cost_public,
                         comp_out$total_death,
                         comp_out$mean_death,
                         comp_out$total_qaly,
                         comp_out$total_campaign_cost),
              file=paste("Data/Output/Comparison_",model_in_var$reference_salt,"_",model_in_var$current_salt,".txt",sep=""),col.names=TRUE)

save(comp_out,file=paste("Data/Output/All_",model_in_var$reference_salt,"_",model_in_var$current_salt,".RData",sep=""))
}
