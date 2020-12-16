library(shiny)

load("data/model_initial.RData", envir=.GlobalEnv)
source("data/functions_const.R")

var_names<-c("year_end","cost_ht_treatment","interest_rate","hypertension_treated_eff",
             "hypertension_treated_all","working_days","value_year_work","reference_salt",
             "current_salt","campaign_cost1","campaign_cost2")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$df<-renderTable({

    for (i in 1:length(var_names)){
    model_in_var[[var_names[i]]]<-input[[var_names[i]]]
    }
    model_in_var$value_day_work=model_in_var$value_year_work/model_in_var$working_days
    model_in_var$reference_sodium=model_in_var$reference_salt*0.4
    model_in_var$current_sodium=model_in_var$current_salt*0.4
    source("data/functions_var.R")
    
    withProgress(message = 'Calculating Model', value = 0, {
        incProgress(1/10, detail = paste("Calculating model 0"))
        model0_out<-saltApp_main_0(model_in_const,model_in_var)
        incProgress(4/10, detail = paste("Calculating model 1"))
        model1_out<-saltApp_main_1(model_in_const,model_in_var,model0_out)
        incProgress(5/10, detail = paste("Summarizing results"))
    }) 

   comp_out<-data.frame(saltApp_main_comp(model0_out,model1_out,model_in_const))
  })
} )
