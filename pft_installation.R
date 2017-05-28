#----------------------------------#
# PROJECT FLU TREND PLATFORM.alpha #
#                                  #
# by Paul Schneider                #
# Schneider.paulpeter@gmail.com    #
#----------------------------------#

# (Install and) load dependent packages
pft_packages <- function(package){
  for(i in 1:length(package)){
  if(eval(parse(text=paste("require(",package[i],")")))==0) {
  install.packages(package)}}
  return (eval(parse(text=paste("require(",package,")"))))
}
required_packages<-c("ISOweek","wikipediatrend","RCurl","RJSONIO","RCurl","shiny","prophet","dplyr","ggplot2","RJSONIO","purrr","xtable","glmnet","devtools")
pft_packages(required_packages)

# load pft toolbox
source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package

  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  if(unlink.tmp.certs) unlink("cacert.pem")
  
  # parase lines and evealuate in the global environement
  eval(parse(text = script), envir= .GlobalEnv)
}
source_https("https://raw.githubusercontent.com/projectflutrend/pft/master/pft_toolbox.R")

# PROJECT FLU TREND REFERENCE LISTS
country_list<-read.csv(file= "https://raw.githubusercontent.com/projectflutrend/pft/master/country_list.csv")[,-c(1,2,4)]
language_table<-read.csv(file="https://raw.githubusercontent.com/projectflutrend/pft/master/language_list.csv")
lookup_terms<-load_url("https://github.com/projectflutrend/pft/blob/master/lookup_terms.RData?raw=true")
list_of_inputs<- load_url("https://github.com/projectflutrend/pft/blob/master/list_of_inputs.RData?raw=true")
list_of_outcomes<- load_url("https://github.com/projectflutrend/pft/blob/master/list_of_outcomes.rdata?raw=true")

# DOWNLOAD INPUT DATA
# language_table # ---> consult language table
list_of_inputs<- load_input(c("de","nl","vi","it","ja","zh","cs","fa","fr","he","id","ko","no","pl","sv","th","en"))

# ADD OUTCOME DATA
# country_list # ---> consult country list
# list_of_outcomes<-pft_store_outcomes(country= ,source_df= )
list_of_outcomes<- load_flunet(country = c("netherlands","germany","viet nam","japan","china","indonesia","france","israel","italy","malaysia","norway","thailand","sweden","poland"))
list_of_outcomes<- load_flunet(country = c("iran","korea","czechia","usa"),name_in_list=c("iran, islamic republic of","korea, republic of","czech republic","united states"))
# list_of_outcomes<-load_local(file.choose())


# CREATE EXAMPLES to start shiny app with... 
if(0==1){
m1<-pft_model(
  lang="nl",
  start_date=as.Date("2015-01-01"), 
  end_date=as.Date("2015-05-01"),
  type_of_outcome="Influenza_AB_lab_incidence",
  country = "netherlands",
  type_of_input = c("wiki_primary"),
  method="simple.lm",
  cv_fold=30, 
  cv_lambda=as.character("1se"),     #cv_lambda,
  grid=10^seq(10,-2,length=100),
  start_date_eval="auto",  
  end_date_eval="auto",   
  training_period="past",
  eval_period=28,
  detrending=1,
  detrend_window=21,
  detrend_robust=T,
  time_lag= 0, 
  wiki_normalization=0,
  status=1)
e1<-eval_pft_model(m1,method = "plot_mean_performance")
info1<-data.frame(country="netherlands",
                  outcome="ili_incidence",
                  lang="Dutch; Flemish",
                  inputs="wiki_primary",
                  method="simple.lm",
                  training_period="past",
                  time_lag="0")
list1<-list(e1=e1,m1=m1,info1=info1)
example1a<-list1
list2<-list(e2=e1,m2=m1,info2=info1)
example1b<-list2
}
list1<-list(e1=NULL,m1=NULL,info1=NULL)
example1a<-list1
list2<-list(e2=NULL,m2=NULL,info2=NULL)
example1b<-list2

# RUN SHINY APP FROM GITHUB
runGitHub("pft", "projectflutrend", subdir = "shiny")




# ... voila

