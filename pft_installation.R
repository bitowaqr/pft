###########################################################
#                WIKIPEDIA FLU TREND.alpha                #
#                                                         #
#                   Maastricht University &               #
#   Netherlands institute for health services research    #
#                                                         #
#                   Contact: Paul Schneider               #
#               schneider.paulpeter@gmail.com             #
###########################################################
#
# This is the installation for the Wikipedia Flu Trend.alpha platform (WFT)
#
# Running the script will install and launch the Wikipedia Flu Trend.alpha platform (WFT)
# All required required R-packages, data and WFT functions will be downloaded 
# When launched for the first time, this process may take a few minutes. 
# Afterwards, the platform should automatically start in your browser. 
#
#
# ------ INSTALLATION DETAILS AND QUICK MANUAL -------
#
# 1. Install and load all required R-packages
# 
# 2. Load WFT functions from github
#
# 3. Load the infrastrucutre of WFT from github:
#     - country_list: provides names and ISO codes for all countries
#     - language_table: provides names and ISO codes for all languages 
#     - lookup_terms: contains all primary, linked and related terms to look up on Wikipedia in most languages
#     - list_of_inputs: this is the list where all the data from Wikipedia will be stored, under the ISO code of the language
#     - list_of_outcomes: this is the list where all outcome data will be stored, under the name of the country
#
# 4. Download a sample collection of wikipedia inputs and WHO-Flunet outcome data from github
#    You can use several functions to add more inputs or outcomes. 
#    You can look up country and language ISO codes in 'country_list' and 'language_table'
#    The data that you load using these functions will remain on your computer
#        - 'pft_initiate(lang="es")' will download spanish Wikipedia data from 2010-01-01 until today
#        - 'pft_add_related_terms(type="wiki_related",terms=c("Fieber","Husten"), lang="de")' will add "Fieber" 
#            and "Husten" as terms in  the wiki_related category within the German wiki input data. 
#            Data will be downloaded automatically.
#        - 'update_input_wiki(lang="nl")' will update the Wikipedia data within the Dutch list
#        - 'pft_store_outcomes(country = "netherlands", source_df = example.df)' this function can be used to
#           store local outcome data. source_df must be a type "data.frame" with one date vector (named "date")
#           and one numeric vector with a names that fits a category within list_of_outcomes
#
# 5. Download and run shiny app in your browser
# 
#############################################################

# INSTALL WFT
if(exists("first_start")==FALSE) {
  # 1. INSTALL AND LOAD REQUIRED R-PACKAGES
  cat("INSTALL AND LOAD REQUIRED R-PACKAGES \n")
  pft_packages <- function(package){
    for(i in 1:length(package)){
      if(eval(parse(text=paste("require(",package[i],")")))==0) {
        install.packages(package)}}
    return (eval(parse(text=paste("require(",package,")"))))
  }
  required_packages<-c("pageviews","ISOweek","wikipediatrend","RCurl","RJSONIO","RCurl","shiny","prophet","dplyr","ggplot2","RJSONIO","purrr","xtable","glmnet","devtools")
  pft_packages(required_packages)
  
  # 2. LOAD WFT FUNCTIONS FROM GITHUB
  cat("LOAD WFT FUNCTIONS FROM GITHUB \n")
  source_https <- function(u, unlink.tmp.certs = FALSE) {
    # load package
    
    # read script lines from website using a security certificate
    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    # alternatively use
    # script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    script <- getURL(u)
    if(unlink.tmp.certs) unlink("cacert.pem")
    
    # parase lines and evealuate in the global environement
    eval(parse(text = script), envir= .GlobalEnv)
  }
  source_https("https://raw.githubusercontent.com/projectflutrend/pft/master/pft_toolbox.R")
  
  # 3. LOAD WFT INFRASTRUCTURE FROM GTIHUB
  cat("LOAD WFT INFRASTRUCTURE FROM GTIHUB \n")
  cat("loading country_list  \n")
  country_list<-read.csv(file= "https://raw.githubusercontent.com/projectflutrend/pft/master/country_list.csv")[,-c(1,2,4)]
  cat("loading language_table \n")
  language_table<-read.csv(file="https://raw.githubusercontent.com/projectflutrend/pft/master/language_list.csv")
  cat("loading lookup_terms \n")
  lookup_terms<-load_url("https://github.com/projectflutrend/pft/blob/master/lookup_terms.RData?raw=true")
  cat("loading empty list_of_inputs \n")
  list_of_inputs<- load_url("https://github.com/projectflutrend/pft/blob/master/list_of_inputs.RData?raw=true")
  cat("loading empty list_of_outcomes \n")
  list_of_outcomes<- load_url("https://github.com/projectflutrend/pft/blob/master/list_of_outcomes.rdata?raw=true")

  # 4. LOAD INPUT AND OUTCOME DATA
  # DOWNLOAD SAMPLE INPUT DATA
  cat("DOWNLOAD SAMPLE INPUT DATA \n")
  list_of_inputs<- load_input(c("de","nl","vi","it","ja","zh","cs","fa","fr","he","id","ko","no","pl","sv","th","en"))
  list_of_outcomes<- load_flunet(country = c("netherlands","germany","viet nam","japan","china","indonesia","france","israel","italy","malaysia","norway","thailand","sweden","poland"))
  list_of_outcomes<- load_flunet(country = c("iran","korea","czechia","usa","uk"),name_in_list=c("iran, islamic republic of","korea, republic of","czech republic","united states","united kingdom"))
  # + Load your own data...
  
  first_start="First start completed!"
  cat("Everything is set up \nWFT should be ready to go! \n")
}

# 5. RUN SHINY APP FROM GITHUB
cat("Launching Wikipedia Flu Trend.alpha \n \n")
if(exists("list1")==FALSE){list1=list(e1=NULL,m1=NULL,info1=NULL)}
if(exists("list2")==FALSE){list2=list(e2=NULL,m2=NULL,info2=NULL)}
runGitHub("pft", "projectflutrend", subdir = "shiny", ref="shiny")

# ... Voila

