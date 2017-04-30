# dutch ili data, first look

# from weekly data to daily data
nl_ili<-data.frame(read.csv( "/Users/waqr/Documents/Project Flu Trend/data/nl_ili_unformatted.csv"))
str(nl_ili)
require(ISOweek)
nl_ili$kw<-gsub("-","-1-",nl_ili$kw) ### INTERESTING! TAKE 7TH DAY BETTER THAN 1st???
#nl_ili$kw<-gsub("53","52",nl_ili$kw) # now this seems unneccary?!
nl_ili$kw<-as.Date(nl_ili$kw,format="%Y-%u-%U") # each week gets a date
nl_ili<-wft_weeks_to_days(nl_ili$kw,nl_ili$incidentie_IAZ)
nl_ili<-nl_ili[complete.cases(nl_ili),]
# a first plot
plot(nl_ili,type="l")

# store outcome
list_of_outcomes<- store_outcomes(code="nl",
               type_of_outcome = "ili_incidence",
               outcome_data =  nl_ili$count,
               dates=nl_ili$date)

# a first model
# 1. build df
nl.ili.df<- pft_build_up(country="netherlands",
             lang="nl",
             type_of_outcome = "ili_incidence",
             type_of_input = "wiki_primary",
             start_date = "auto",
             end_date = "auto",
             input_df=list_of_inputs,
             outcome_df=list_of_outcomes,
             status=1)
nl.ili.df<-nl.ili.df[complete.cases(nl.ili.df),]

# plot ili vs Griep page count
par(mfrow=c(2,1))
plot(nl.ili.df$date,scale(nl.ili.df$ili_incidence),type="l")
lines(nl.ili.df$date,scale(nl.ili.df$Griep),col="gray",lwd=0.5)
lines(nl.ili.df$date, scale(pft_simple_trend( nl.ili.df$Griep ))  ,col="darkorange",lwd=2)

# a simple lm
lm.simple.fit<-lm(ili_incidence~pft_simple_trend( Griep), data=nl.ili.df)

# plotting lm, anyhting better?
plot(nl.ili.df$date,nl.ili.df$ili_incidence,type="l")
lines(nl.ili.df$date,predict(lm.simple.fit),col="orange",lwd=2)
summary(lm.simple.fit)

# as comparison, lab data
nl.IAB.df<- pft_build_up(country="netherlands",
                         lang="nl",
                         type_of_outcome = "Influenza_AB_lab_incidence",
                         type_of_input = "wiki_primary",
                         start_date = "auto",
                         end_date = "auto",
                         input_df=list_of_inputs,
                         outcome_df=list_of_outcomes,
                         status=1)
nl.IAB.df<-nl.IAB.df[complete.cases(nl.IAB.df),]

plot(nl.IAB.df$date,scale(nl.IAB.df$Influenza_AB_lab_incidence),type="l")
lines(nl.IAB.df$date,scale(nl.IAB.df$Griep),col="gray",lwd=0.5)
lines(nl.IAB.df$date, scale(pft_simple_trend( nl.IAB.df$Griep ))  ,col="darkorange",lwd=2)
# interestingly, model fits a bit better?!
lm.simple.fit2<-lm(Influenza_AB_lab_incidence~pft_simple_trend( Griep), data=nl.IAB.df)
plot(nl.IAB.df$date,nl.IAB.df$Influenza_AB_lab_incidence,type="l")
lines(nl.IAB.df$date,predict(lm.simple.fit2),col="orange")
summary(lm.simple.fit2)




# combining ili and lab data and wiki data
par(mfrow=c(1,1))
plot(nl.ili.df$date,scale(nl.ili.df$ili_incidence),type="l",lwd=3)
lines(nl.IAB.df$date,scale(nl.IAB.df$Influenza_AB_lab_incidence),col="blue",lwd=3)
lines(nl.ili.df$date, scale(pft_simple_trend( nl.ili.df$Griep ))  ,col="darkorange",lwd=2)

######
# building pft_model


nl_ili_model<-pft_model(lang="nl",
                        start_date="auto", # auto will get min date
                        end_date="auto",   # auto will get max date
                        type_of_outcome="ili_incidence",
                        country = "netherlands",
                        type_of_input = c("wiki_primary","wiki_linked"), # + wiki_relared? + wiki_random?!
                        
                        # lasso regression + cross validation
                        method="cv", # "cv" or "simple.lm"
                        
                        cv_fold=3, # "M", "Y", or 3 to sample size
                        cv_lambda="min", #  "min" or "1se"... a number is possible but nonsense!
                        grid=10^seq(10,-2,length=250), # a grid for cv, default!
                        start_date_eval="auto",  # can also be a Date, must be compatible with training period !!!
                        end_date_eval="auto",   # can also be a Date, "auto" gets the maximum, must be compatible with...?!
                        training_period=365*2, # training period in days or "past", must be compatible with min and max dates!!!
                        # evaluation_intervall=1, # not sure .... interval for modelling
                        eval_period=28, # this is nor related to cv, is it?
                        
                        # do you want decomposed independent variables?
                        detrending=1, # Seasonal Decomposition of Time Series by Loess
                        detrend_window=7,
                        detrend_robust=T, # or F
                        
                        time_lag= 0, # how many days is official data behind? -7 for 1 week behind page view data
                        wiki_normalization=0,
                        status=1)

eval.nl<-eval_pft_model(pft_model_df=nl_ili_model,
                        method="plot_mean_performance" )

eval.nl$plots$test_actual_vs_pred_d28 +
  ylim(0,30)



eval_pft_model(pft_model_df=nl_ili_model,method="plot1",eval_date="20140101")



