###-----------------------------------------------------------------###
###-----------------------------------------------------------------###
###                                                                 ###
###  S  E  T  U  P  G  L  O  B  A  L  E  N  V  I  R  O  M  E  N  T  ###
###                                                                 ###
###-----------------------------------------------------------------###
###-----------------------------------------------------------------###

####---->  load reference lists
country_list<-read.csv(file= "/Users/waqr/Documents/Wikipedia Flu Trend/outcome_data/country_list.csv")[,-c(1,2,4)]
language_table<-read.csv(file= "/Users/waqr/Documents/Wikipedia Flu Trend/outcome_data/language_list.csv")

###---setting up a lookup list for search terms-
new_terms_lookup<-0
if(new_terms_lookup==1){
  require(RJSONIO)
  require(RCurl)
  
  # wiki_primaries
  # searching terms automatically linked to english "Influenza" in other languages
  primary<-wft_lang_titles(page="Influenza",origin_lang="en",filter_lang=c(as.character(language_table$ISO_639_1)))
  names(primary)<-c("ISO_639_1" ,"lang", "page") # solving a problem with the original language ("en" was missing/at the wrong place in the table)
  primary<-merge(language_table,primary[-39,],by="ISO_639_1")
  primary<-data.frame(lang=primary$ISO_639_1,page=as.character(primary$page))
  
  # wiki_related
  # setting a list of related terms, CHOSEN INDIVIDUALLY?
  related<-sapply(language_table$ISO_639_1,function(x) NULL)
  names(related)<-as.factor(language_table$ISO_639_1)
  
  # wiki_random
  # Setting up a list for random wiki pages to control general weikipedia trends
  random_pages<-sapply(language_table$ISO_639_1,function(x) NULL)
  names(random_pages)<-as.factor(language_table$ISO_639_1)
  number_of_random_pages<-200 # CAVE: SHOULD BE MUCH HIGHER!!!
  for(i in 1:length(language_table$ISO_639_1)){ 
    r1<-NULL; rp<-NULL
    cat(i,"/",length(language_table$ISO_639_1),"\n")
    if(!is.na(primary$page[i])){
      for(j in 1:number_of_random_pages){
        cat("generating random pages for",as.character(language_table$ISO_639_1[i]),j,"/",number_of_random_pages,"\n")
        r1<-as.character(data.frame(fromJSON(getURL(paste("https://",language_table$ISO_639_1[i],".wikipedia.org/w/api.php?action=query&list=random&rnnamespace=0&rnlimit=1&format=json",sep=""))))$query.random.title[1])
        rp<-c(rp,r1)
        }
      random_pages[[i]]<-rp
       }}
   
  # wiki_linked
  # create a look up table for terms linked to the primary wiki page ("Influenza"), in the repsective language
  linked_terms<-sapply(language_table$ISO_639_1,function(x) NULL)
  names(linked_terms)<-as.factor(language_table$ISO_639_1)
  for(i in 1:length(language_table$ISO_639_1)){ 
    cat(i,"/",length(language_table$ISO_639_1),"\n")
    if(!is.na(primary$page[i])){
      linked_terms[[i]]<-wft_lp_names(page=primary$page[i],lang=primary$lang[i])
    }
    if(is.na(primary$page[i])){ linked_terms[[i]]<-"no match for Influenza found for this language" }
  } # getting pages linked to primary wiki page for every language
  
  # combining everything 
  # a big look up list
  lookup_terms <- sapply(language_table$ISO_639_1,function(x) NULL)
  names(lookup_terms)<-as.factor(language_table$ISO_639_1)  
  for(i in 1:length(language_table$ISO_639_1)){  
    lookup_terms[[i]]<-list(primary=as.character(primary$page[i]),  
                            related=related[[i]], 
                            linked=list(linked_terms[[i]]),
                            random_pages=list(random_pages[[i]]))} # combining primary, related, linked
  save(lookup_terms,file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/lookup_terms.RData")
}
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/lookup_terms.RData")
rm(new_terms_lookup)

####----> LIST OF OUTCOMES
set_up_new_list_of_outcomes<-1
if(set_up_new_list_of_outcomes==1){
  list_of_outcomes<-list()
  for(i in 1:length(country_list$country_code_lowcaps)){
    list_of_outcomes[[i]]<-list(info=data.frame(country=country_list$description[i],
                                                code=country_list$country_code_lowcaps[i],
                                                code_uppercaps=as.character(country_list$country_code[i])),
                                dependents=data.frame(date=seq(from=as.Date("2010-01-01"),to=as.Date(Sys.Date()),by=1),
                                                      Influenza_AB_lab_incidence=NA,
                                                      ili_incidence=NA))
  }
  names(list_of_outcomes)<-country_list$description
  
  nl_ili1<-read.csv(file="/Users/waqr/Documents/Wikipedia Flu Trend/outcome_data/nl_1.csv")
  nl_ili1$date<-as.Date(nl_ili1$date)
  list_of_outcomes<-store_outcomes(code="nl",type_of_outcome = "Influenza_AB_lab_incidence",outcome_data = nl_ili1$incidence,dates=nl_ili1$date)
  list_of_outcomes<-store_outcomes(code="nl",type_of_outcome = "Influenza_AB_lab_incidence_std",outcome_data = scale(nl_ili1$incidence),dates=nl_ili1$date)
  
  de_ili1<-read.csv(file="/Users/waqr/Documents/Wikipedia Flu Trend/outcome_data/de_1.csv")
  de_ili1$temp_days<-as.Date(de_ili1$temp_days)
  list_of_outcomes<-store_outcomes(code="de",type_of_outcome = "Influenza_AB_lab_incidence",outcome_data = de_ili1$daily_count_temp,dates=de_ili1$temp_days)
  list_of_outcomes<-store_outcomes(code="de",type_of_outcome = "Influenza_AB_lab_incidence_std",outcome_data = scale(de_ili1$daily_count_temp),dates=de_ili1$temp_days)
  save(list_of_outcomes,file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/list_of_outcomes.RData")
}
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/list_of_outcomes.RData")
rm(set_up_new_list_of_outcomes)

####----> Set up a fresh, new list_of_inputs 
set_up_new_list_of_inputs<-0
if(set_up_new_list_of_inputs==1){
list_of_inputs <- sapply(language_table$ISO_639_1,function(x) NULL)
names(list_of_inputs)<-as.factor(language_table$ISO_639_1)
for(i in 1:length(language_table$ISO_639_1)){
  TO_DATE<-as.Date("2017-01-01")
  list_of_inputs[[i]]<-list(wiki_primary=data.frame(date=seq(from=as.Date("2010-01-01"),to=TO_DATE,by=1)),
                            wiki_linked=data.frame(date=seq(from=as.Date("2010-01-01"),to=TO_DATE,by=1)),
                            wiki_related=data.frame(date=seq(from=as.Date("2010-01-01"),to=TO_DATE,by=1)),
                            wiki_random=data.frame(date=seq(from=as.Date("2010-01-01"),to=TO_DATE,by=1)),
                            gtrend_primary=data.frame(date=seq(from=as.Date("2010-01-01"),to=TO_DATE,by=1)),
                            wiki_lookup_searches=list(wiki_primary_term=lookup_terms[[i]]$primary,
                                                      wiki_related_terms=lookup_terms[[i]]$related[[1]],
                                                      wiki_linked_terms=lookup_terms[[i]]$linked[[1]],
                                                      wiki_random_page=lookup_terms[[i]]$random_pages[[1]])
  )
  
}
save(list_of_inputs,file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/list_of_inputs.RData")
}
load(file="/Users/waqr/Documents/Wikipedia Flu Trend/data sets/list_of_inputs.RData")
rm(set_up_new_list_of_inputs)

  
