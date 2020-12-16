## variable functions
sbp_distribution<-function(pop,slt_reduce){
  
  x_nonhyper=c(0,20/64,44/64,1)
  y_nonhyper=c(90,120,130,140)
  
  x_hyper_nont=c(0,0.25,0.5,0.75,0.95,1)
  y_hyper_nont_4059=c(140,c(140,144,152,174)+5,200)
  y_hyper_nont_60p=c(140,c(145,152,164,189)+5,200)
  
  x_hyper_t=c(0,0.05,0.25,0.5,0.75,0.95,1)
  y_hyper_t_4059=c(90,c(102,116,127,139,163)+5,200)
  y_hyper_t_60p=c(90,c(107,123,136,150,180)+5,200)
  
  hyper<-model_in_const$hypertension_prevalence*pop
  
  pop_hyper<-cbind(
    mapply(function(x,y){
      u=runif(x)
      z=approx(x_hyper_nont,y_hyper_nont_4059,xout=u)$y+sbp_reduce(slt_reduce,y,1)
      hyper_false_obs<-z[z<140]
      hyper_real_obs<-z[z>=140]
      id_treated=sample(x=(1:length(hyper_real_obs)),size=floor(model_in_var$hypertension_treated_eff*length(hyper_real_obs)),replace=FALSE)
      hyper_nont_obs<-hyper_real_obs[-id_treated]
      u=runif(length(id_treated))
      z=approx(x_hyper_t,y_hyper_t_4059,xout=u)$y+sbp_reduce(slt_reduce,y,0)      
      hyper_total=length(hyper_real_obs)
      zf<-c(hyper_false_obs,hyper_nont_obs,z)
      c(sum(zf<115),sum(zf>=115&zf<130),sum(zf>=130&zf<140),sum(zf>=140),mean(zf[(zf<115)],na.rm=TRUE),mean(zf[(zf>=115&zf<130)],na.rm=TRUE),mean(zf[(zf>=130&zf<140)],na.rm=TRUE),mean(zf[(zf>=140)]),length(id_treated),hyper_total)
    },hyper[1:2],model_in_const$age_groups_means[1:2]),
    mapply(function(x,y){
      u=runif(x)
      z=approx(x_hyper_nont,y_hyper_nont_60p,xout=u)$y+sbp_reduce(slt_reduce,y,1)
      hyper_false_obs<-z[z<140]
      hyper_real_obs<-z[z>=140]
      id_treated=sample(1:length(hyper_real_obs),floor(model_in_var$hypertension_treated_eff*length(hyper_real_obs)),replace=FALSE)
      hyper_nont_obs<-hyper_real_obs[-id_treated]
      u=runif(length(id_treated))
      z=approx(x_hyper_t,y_hyper_t_60p,xout=u)$y+sbp_reduce(slt_reduce,y,0)      
      hyper_total=length(hyper_real_obs)
      zf<-c(hyper_false_obs,hyper_nont_obs,z)
      c(sum(zf<115),sum(zf>=115&zf<130),sum(zf>=130&zf<140),sum(zf>=140),mean(zf[(zf<115)],na.rm=TRUE),mean(zf[(zf>=115&zf<130)],na.rm=TRUE),mean(zf[(zf>=130&zf<140)],na.rm=TRUE),mean(zf[(zf>=140)]),length(id_treated),hyper_total)  
    },hyper[3:5],model_in_const$age_groups_means[3:5])
  )
  
  nonhyper=pop-hyper
  
  pop_nonhyper<-mapply(function(x,y){
    u=runif(x)
    y=approx(x_nonhyper,y_nonhyper,xout=u)$y+sbp_reduce(slt_reduce,y,0)
    c(sum(y<115),sum(y>=115&y<130),sum(y>=130&y<140),sum(y>=140),mean(y[(y<115)]),mean(y[(y>=115&y<130)]),mean(y[(y>=130&y<140)]),mean(y[(y>=140)],na.rm=TRUE))
  },nonhyper,model_in_const$age_groups_means)
  
  pop_hyper[is.na(pop_hyper)]<-0
  pop_nonhyper[is.na(pop_nonhyper)]<-0
  
  out<-list()
  out$dist<-pop_nonhyper[1:4,]+pop_hyper[1:4,]
  out$means<-rbind( rep(115,5),pop_nonhyper[2:4,]/out$dist[2:4,]*pop_nonhyper[6:8,]+pop_hyper[2:4,]/out$dist[2:4,]*pop_hyper[6:8,])
  out$ht_treated_eff<-pop_hyper[9,]
  out$ht_treated_all<-(model_in_var$hypertension_treated_all/model_in_var$hypertension_treated_eff)*pop_hyper[9,]
  out$hyper_total<-pop_hyper[10,]
  out
}