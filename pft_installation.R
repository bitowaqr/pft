# PROJECT FLU TREND FUNCTIONS

# (Install and) load dependent packages
pft_packages <- function(package){
  for(i in 1:length(package)){
  if(eval(parse(text=paste("require(",package[i],")")))==0) {
  install.packages(package)}}
  return (eval(parse(text=paste("require(",package,")"))))
}
required_packages<-c("RCurl","shiny","prophet","dplyr","ggplot2","RJSONIO","purrr","xtable","glmnet")
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
# language_table # consult language table
list_of_inputs<- load_input(c("de","nl","vi","it","ja","zh","cs","fa","fr","he","id","ko","no","pl","sv","th"))

# ADD OUTCOME DATA
# list_of_outcomes<-pft_store_outcomes(country= ,source_df= )
list_of_outcomes<- load_flunet(country = c("netherlands","viet nam","japan","china","indonesia","france","israel","italy","malaysia","norway","thailand","sweden","poland"))
list_of_outcomes<- load_flunet(country = c("iran","korea","czechia"),name_in_list=c("iran, islamic republic of","korea, republic of","czech republic"))
# list_of_outcomes<-load_local(file.choose())

#
#-----> VIRGIN ISLANDS???


# RU shiny app
runGitHub("pft", "projectflutrend", subdir = "shiny")
