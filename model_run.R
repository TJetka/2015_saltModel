load(file="Data/R/model_initial.RData")
 
# ## Constant Parameters ####
#############################

# model_in_const$age_groups<-age_groups
# model_in_const$disease_names<-disease_names
# func_zus<-function(x){
#   colnames(x)<-model_in_const$age_groups
#   x=split(x,1:nrow(x))
#   names(x)<-model_in_const$disease_names[1:3]
#   x
# }
# model_in_const$year_start<-2013
# model_in_const$year_start_ch<-as.character(model_in_const$year_start)
# model_in_const$cost_hosp<-lapply(jgp_data,function(x) x$mean_cost)
# model_in_const$zus<-func_zus(zus)
# model_in_const$absencja<-func_zus(absencja)
# model_in_const$niezdolnosc<-func_zus(niezdolnosc)
# model_in_const$popyear_prog_total<-data.frame(pop_prog_total)
# colnames(model_in_const$popyear_prog_total)<-2013:2050
# rownames(model_in_const$popyear_prog_total)<-0:99
# model_in_const$hypertension_prevalence<-hypertension_prevalence
# model_in_const$age_groups_means<-age_groups_means
# model_in_const$premodel_simulation<-premodel_simulation
# model_in_const$premodel_years<-premodel_years
# model_in_const$mort_prog_rates_noncvd<-mort_prog_rates_noncvd
# model_in_const$mort_prog_rates_cvd<-mort_prog_rates_cvd
# model_in_const$relative_risks<-relative_risks
# model_in_const$relative_risks_dis<-relative_risks_dis
# model_in_const$mort_inst_rate<-mort_inst_rate
# model_in_const$instances_hosp_rates<-instances_hosp_rates
# model_in_const$cost_hosp<-lapply(jgp_data,function(x) x$mean_cost)
# model_in_const$pop_names<-pop_names
# model_in_const$age_pension_mean<-age_pension_mean
# model_in_const$pop2013<-pop2013
# model_in_const$qaly<-qaly

source("functions_const.R")

## MODEL RUN ####
#################

cur_salt<-c( 9.6, 13.5)#c( 9.6, 13.5,  9.6, 13.5, 9.6, 13.5, 9.6, 13.5)
ref_salt<-c( 7.3, 9.25)#c( 3.8,  3.8,  5.0,  5.0, 6.7, 8.65, 7.3, 9.25)

for (i in 1:length(cur_salt)){

# ## Variable Parameters ####
#############################

model_in_var$current_salt <- cur_salt[i]
model_in_var$reference_salt  <- ref_salt[i]

model_in_var$year_end=2038
model_in_var$cost_ht_treatment=623.28
model_in_var$interest_rate=0.03
model_in_var$hypertension_treated_eff=0.26
model_in_var$hypertension_treated_all=0.62
model_in_var$working_days=251
model_in_var$value_year_work=3650*12
model_in_var$value_day_work=model_in_var$value_year_work/model_in_var$working_days
model_in_var$reference_sodium=model_in_var$reference_salt*0.4
model_in_var$current_sodium=model_in_var$current_salt*0.4
model_in_var$campaign_cost1<-5*10^7
model_in_var$campaign_cost2<-2.5*10^7

source("functions_var.R")

## Main Functions ####
######################

model0_out<-saltApp_main_0(model_in_const,model_in_var)
model1_out<-saltApp_main_1(model_in_const,model_in_var,model0_out)
comp_out<-saltApp_main_comp(model0_out,model1_out,model_in_const)
output_write(model_in_var,comp_out)

}
