


##### WHATS THE BEST TIMELAG????

lang="nl"
start_date="auto"
end_date="auto"
type_of_outcome="Influenza_AB_lab_incidence"
country = "netherlands"
type_of_input = c("wiki_primary")
method="lm.simple"
start_date_eval="2015-01-01"
end_date_eval="2015-12-31"
training_period=60
eval_period=28
detrending=1
detrend_window=14
detrend_robust=T 
time_lag= -7
wiki_normalization=1
status=1


best_lag<-list()
for(l in 1:60){
  temp_model<-pft_model(lang=lang,
                        start_date=start_date,
                        end_date=end_date,
                        type_of_outcome=type_of_outcome,
                        country = country,
                        type_of_input = type_of_input,
                        method=method,
                        start_date_eval=start_date_eval,
                        end_date_eval=end_date_eval,   
                        training_period=training_period, 
                        eval_period=eval_period, 
                        detrending=detrending,
                        detrend_window=detrend_window,
                        detrend_robust=detrend_robust, 
                        time_lag= l-30, 
                        wiki_normalization=wiki_normalization,
                        status=status)
  temp_rsqr<-NULL;r<-NULL
  for(i in 1:length(temp_model)){
    r<-summary(temp_model[[i]]$lm.model)$adj.r.square
    temp_rsqr<-c(temp_rsqr,r)
  }
  best_lag[[l]]<-temp_rsqr}

mean.r<-NULL;rt<-NULL
for(s in 1:length(best_lag)){
  rr<-mean(best_lag[[s]],na.rm=T,trim=0.05)
  mean.r<-c(mean.r,rr)
}

plot(x=-29:30,y=mean.r,type="l",main="mean adj.r2 in relation to timelag")

















# additional ideas for systematic analyses of predictive models

lang="nl"
start_date="auto"
end_date="auto"
type_of_outcome="Influenza_AB_lab_incidence"
country = "netherlands"
type_of_input = c("wiki_primary")
method="lm.simple"
start_date_eval="auto"
end_date_eval="2015-01-31"
training_period=60
eval_period=28
detrending=1
detrend_window=14
detrend_robust=T 
time_lag= -7
wiki_normalization=1
status=1

example.1<-pft_model(lang=lang,
                       start_date=start_date,
                       end_date=end_date,
                       type_of_outcome=type_of_outcome,
                       country = country,
                       type_of_input = type_of_input,
                       method=method,
                       start_date_eval=start_date_eval,
                       end_date_eval=end_date_eval,   
                       training_period=training_period, 
                       eval_period=eval_period, 
                       detrending=detrending,
                       detrend_window=detrend_window,
                       detrend_robust=detrend_robust, 
                       time_lag= time_lag, 
                       wiki_normalization=wiki_normalization,
                       status=status)


training_period=180

example.2<-pft_model(lang=lang,
                     start_date=start_date,
                     end_date=end_date,
                     type_of_outcome=type_of_outcome,
                     country = country,
                     type_of_input = type_of_input,
                     method=method,
                     start_date_eval=start_date_eval,
                     end_date_eval=end_date_eval,   
                     training_period=training_period, 
                     eval_period=eval_period, 
                     detrending=detrending,
                     detrend_window=detrend_window,
                     detrend_robust=detrend_robust, 
                     time_lag= time_lag, 
                     wiki_normalization=wiki_normalization,
                     status=status)

detrend_window=7
training_period=60
time_lag=-14

example.3<-pft_model(lang=lang,
                     start_date=start_date,
                     end_date=end_date,
                     type_of_outcome=type_of_outcome,
                     country = country,
                     type_of_input = type_of_input,
                     method=method,
                     start_date_eval=start_date_eval,
                     end_date_eval=end_date_eval,   
                     training_period=training_period, 
                     eval_period=eval_period, 
                     detrending=detrending,
                     detrend_window=detrend_window,
                     detrend_robust=detrend_robust, 
                     time_lag= time_lag, 
                     wiki_normalization=wiki_normalization,
                     status=status)
training_period=365*2
time_lag=-21


example.4<-pft_model(lang=lang,
                     start_date=start_date,
                     end_date=end_date,
                     type_of_outcome=type_of_outcome,
                     country = country,
                     type_of_input = type_of_input,
                     method=method,
                     start_date_eval=start_date_eval,
                     end_date_eval=end_date_eval,   
                     training_period=training_period, 
                     eval_period=eval_period, 
                     detrending=detrending,
                     detrend_window=detrend_window,
                     detrend_robust=detrend_robust, 
                     time_lag= time_lag, 
                     wiki_normalization=wiki_normalization,
                     status=status)


time_lag=0


example.5<-pft_model(lang=lang,
                     start_date=start_date,
                     end_date=end_date,
                     type_of_outcome=type_of_outcome,
                     country = country,
                     type_of_input = type_of_input,
                     method=method,
                     start_date_eval=start_date_eval,
                     end_date_eval=end_date_eval,   
                     training_period=training_period, 
                     eval_period=eval_period, 
                     detrending=detrending,
                     detrend_window=detrend_window,
                     detrend_robust=detrend_robust, 
                     time_lag= time_lag, 
                     wiki_normalization=wiki_normalization,
                     status=status)

time_lag=-14

example.6<-pft_model(lang=lang,
                     start_date=start_date,
                     end_date=end_date,
                     type_of_outcome=type_of_outcome,
                     country = country,
                     type_of_input = type_of_input,
                     method=method,
                     start_date_eval=start_date_eval,
                     end_date_eval=end_date_eval,   
                     training_period=training_period, 
                     eval_period=eval_period, 
                     detrending=detrending,
                     detrend_window=detrend_window,
                     detrend_robust=detrend_robust, 
                     time_lag= time_lag, 
                     wiki_normalization=wiki_normalization,
                     status=status)

example.1.e<-eval_pft_model(pft_model_df=example.1, method="plot_mean_performance")
example.2.e<-eval_pft_model(pft_model_df=example.2, method="plot_mean_performance")
example.3.e<-eval_pft_model(pft_model_df=example.3, method="plot_mean_performance")
example.4.e<-eval_pft_model(pft_model_df=example.4, method="plot_mean_performance")
example.5.e<-eval_pft_model(pft_model_df=example.5, method="plot_mean_performance")
example.6.e<-eval_pft_model(pft_model_df=example.5, method="plot_mean_performance")


eval_pft_model(pft_model_df=example.1, method="plot1",eval_date="20160101")
eval_pft_model(pft_model_df=example.2, method="plot1",eval_date="20160101")
eval_pft_model(pft_model_df=example.3, method="plot1",eval_date="20160101")
eval_pft_model(pft_model_df=example.4, method="plot1",eval_date="20160101")
eval_pft_model(pft_model_df=example.5, method="plot1",eval_date="20160101")
eval_pft_model(pft_model_df=example.6, method="plot1",eval_date="20160101")


ggplot(data=example.1.e$nowcast_diff,aes(x=date,y=outcome)) +
  geom_line(size=3) +
 # geom_line(data=example.1.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="red") + # too many outliers
 # geom_line(data=example.2.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="blue") + # too late, too bad
 geom_line(data=example.3.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="darkgreen") +  
  geom_line(data=example.4.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="orange") +
  geom_line(data=example.5.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="lightblue") +
  geom_line(data=example.6.e$nowcast_eval,aes(x=date,y=pred.day.21),size=2,col="gray") +
  xlim(c(as.Date("2011-07-01"), as.Date("2014-12-01")))









