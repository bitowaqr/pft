# Demo 27 April 2017

# Assume we have outcome data for the Netherlands.
# Loading a data set with dates and daily incidence data
nl_influenza_data<-read.csv(file="/Users/waqr/Documents/Project Flu Trend/data/nl_influenza_data.csv")
nl_influenza_data$date<-as.Date(nl_influenza_data$date) # dates must be class "Date"
View(nl_influenza_data)
plot(nl_influenza_data,type="l")

# store outcome data in the outcome list
list_of_outcomes<-store_outcomes(
                      outcome_data=nl_influenza_data$Influenza_AB_lab_incidence , 
                      dates=nl_influenza_data$date,
                      country="netherlands", 
                      code=NA ,
                      type_of_outcome="Influenza_AB_lab_incidence")


# What to download? 
# predefined list of terms:
head(lookup_terms$nl$primary)
head(lookup_terms$nl$linked)

# japanese
head(lookup_terms$ja$primary) 
head(lookup_terms$ja$linked) 

# Manually adding the related term "Keelpijn"
lookup_terms<-pft_add_terms(lang="nl", type="related",terms=c("Keelpijn"), add=0)
head(lookup_terms$nl$related)


# Download data for the Netherlands from Wikipedia
               X<-pft_initiate(lang="nl")

# shortcut:
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/list_of_inputs.RData")

# Combine inputs + outputs in a data frame
# Illustrate why decomposition is important
# Set up a test data.frame for the seasons 2010-2011, 2011-2012
test_nl<-pft_build_up(country = "netherlands", # Outcome data from the Netherlands
             lang="nl", # Wikipedia page views from Dutch Wikipedia
             start_date = as.Date("2010-01-01"), # from 2010
             end_date=as.Date("2012-05-31"), # to mid-2012
             type_of_input = c("wiki_primary","wiki_linked"), # types of wikipedia page view data
             type_of_outcome="Influenza_AB_lab_incidence", #Standardized number of lab Influenza A+B cases 
             status=0)

# Influence incidence 2010-id 2012 standardized
plot(test_nl$date, scale(test_nl$Influenza_AB_lab_incidence),type="l",ylim=c(-2,5))
# ... versus "griep" page views
# - Be aware of the seasonal effects! 
lines(test_nl$date, scale(test_nl$Griep), col="darkgreen",lwd=0.5,lty=1); points(test_nl$date, scale(test_nl$Griep), col="darkgreen",pch="."); 
# close up look
plot(test_nl$date, scale(test_nl$Griep), col="darkgreen",lwd=0.5,type="o",xlim=c(as.Date("2010-01-01"),as.Date("2010-04-01")),ylim=c(-1,3))

# "Seasonal Decomposition of Time Series by Loess"
lines(test_nl$date, scale(pft_simple_trend(test_nl$Griep)),lwd=3, col="darkgreen") #detrend

# ignoring weekly patterns
plot(test_nl$date, scale(test_nl$Influenza_AB_lab_incidence),type="l",ylim=c(-2,5))
lines(test_nl$date, scale(test_nl$Griep), col="darkgreen",lwd=0.5,lty=1); points(test_nl$date, scale(test_nl$Griep), col="darkgreen",pch="."); 
lines(test_nl$date, scale(pft_simple_trend(test_nl$Griep)),lwd=3, col="darkgreen") #detrend

# other wiki pages
# Epidemie
lines(test_nl$date, scale(pft_simple_trend(test_nl$Epidemie)),lwd=3, col="cyan") #detrend
# Hoesten
lines(test_nl$date, scale(pft_simple_trend(test_nl$Hoesten)),lwd=3, col="darkorange") #detrend


# Demonstrating Overfitting
test_nl2<-test_nl[test_nl$date<as.Date("2012-01-01"),]

overfit<-data.frame(date=test_nl2$date,
                    Influenza_AB_lab_incidence=test_nl2$Influenza_AB_lab_incidence, 
                    data.frame(lapply(test_nl2[,-c(1,2)],pft_simple_trend)))
overfit.test<-data.frame(date=test_nl$date,
                         Influenza_AB_lab_incidence=test_nl$Influenza_AB_lab_incidence, 
                         data.frame(lapply(test_nl[,-c(1,2)],pft_simple_trend)))

fit.over<-lm(data=overfit, Influenza_AB_lab_incidence~.)

plot(overfit$date, overfit$Influenza_AB_lab_incidence, type="l",lwd=3,xlim=c(as.Date("2010-01-01"),as.Date("2012-06-01")),ylim=c(0,100))
lines(overfit$date,  predict(fit.over),col="orange",lwd=2)
summary(fit.over)

# Testing the model
lines(test_nl$date,test_nl$Influenza_AB_lab_incidence,lwd=2)
lines(test_nl$date, predict(fit.over,newdata = overfit.test),col="orange",lwd=3)


#####------------------------------######
#####---------- MODEL -------------######
#####------------------------------######

# Model Set-Up
        X <- pft_model(country="netherlands",
                       type_of_outcome="Influenza_AB_lab_incidence",
                       
                       lang="nl",
                       type_of_input = c("wiki_primary"), # input: "Griep" only
          
                        method="simple.lm", # also lasso + cv possible
                       # default time period: all dates with complete data
                       # simulation period: total-training period
                        training_period=365*2, # also "past" possible
                        eval_period=28,
                        time_lag= 0, # no delay between wiki page view and reporting
                       
                        detrending=1,  
                        detrend_window=21, 
                        detrend_robust=T,
                        wiki_normalization=0,
                        status=1)

# shortcut
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/demo1_test_model.RData")

# whats the result?
names(test_model)
names(test_model$t20120104)
names(test_model$t20120104$model.stats) # What is most useful ???

# evaluate
eval.1<-eval_pft_model(test_model,"plot_mean_performance")
# shortcut
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/demo1_eval.1.RData")

# Evaluation functions 1: 
# look at 2015-2016 influenza epidemic
eval_pft_model(test_model,method="plot1",eval_date="20160128") +
  xlim(as.Date("2015-07-01"),as.Date("2016-04-01")) +
  geom_vline(xintercept = as.numeric(as.Date("2016-01-28")), linetype=4)

eval_pft_model(test_model,method="plot1",eval_date="20160314") +
  xlim(as.Date("2015-07-01"),as.Date("2016-04-01")) +
  geom_vline(xintercept = as.numeric(as.Date("2016-03-14")), linetype=4)


# Evaluation function 2:
# evaluate training data
eval.1$plots$training_actual_vs_pred

# evaluate test data - predictions on day 7
eval.1$plots$test_actual_vs_pred_d7 +
  xlim(c(as.Date("2015-06-01"),as.Date("2016-06-01")))

# mean error plot (broken?)
eval.1$plots$mean.sqr.error.plot

# How wrong is the model over the time?
# predicting 7,14,21,28 days without data
eval.1$plots$test_diff_plot 

# Real-time Forecast of Influenza activity...


# ... 













