#----------------------------------#
# PROJECT FLU TREND PLATFORM.alpha #
#                                  #
# by Paul Schneider                #
# Schneider.paulpeter@gmail.com    #
#----------------------------------#


# This file installs and loads all packages, internal functions and 
# data neccessary to start WikipediaFluTrend.alpha app from the browser. 
# When launched for the first time, it may take a few minutes. 
# For each session data needs to be downloaded only once. 
# (If an error occurs in "source_https", the alternative "getURL" should be used)

if(exists("first_start")==0){
# (Install and) load dependent packages
pft_packages <- function(package){
  for(i in 1:length(package)){
  if(eval(parse(text=paste("require(",package[i],")")))==0) {
  install.packages(package)}}
  return (eval(parse(text=paste("require(",package,")"))))
}
required_packages<-c("pageviews","ISOweek","wikipediatrend","RCurl","RJSONIO","RCurl","shiny","prophet","dplyr","ggplot2","RJSONIO","purrr","xtable","glmnet","devtools")
pft_packages(required_packages)

# load pft toolbox
source_https <- function(u, unlink.tmp.certs = FALSE) {
  # load package

  # read script lines from website using a security certificate
  if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
  #script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
  script <- getURL(u)
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
list_of_outcomes<- load_flunet(country = c("netherlands","germany","viet nam","japan","china","indonesia","france","israel","italy","malaysia","norway","thailand","sweden","poland"))
list_of_outcomes<- load_flunet(country = c("iran","korea","czechia","usa","uk"),name_in_list=c("iran, islamic republic of","korea, republic of","czech republic","united states","united kingdom"))

# You can also use your own data. Easiest way is to use "pft_store_outcome" function (see below)
# The data will remain on your computer and will not be uploaded
# Example:
# temp_df<-load_local("/Users/waqr/Documents/Project Flu Trend/data/nl.ili.rdata")
# temp_df<-data.frame(date=temp_df$date,ili_incidence=temp_df$ili_incidence)
# temp_df=temp_df[!is.na(temp_df$ili_incidence),]
# list_of_outcomes=pft_store_outcomes(country="netherlands",source_df = temp_df)

# CREATE EXAMPLES to start shiny app with... 
list1<-list(e1=NULL,m1=NULL,info1=NULL)
list2<-list(e2=NULL,m2=NULL,info2=NULL)}

# All neccessary files have been downloaded
first_start=1

# RUN SHINY APP FROM GITHUB
runGitHub("pft", "projectflutrend", subdir = "shiny")

# ... voila

