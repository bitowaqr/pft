###-----------------------------------------------------------------###
###-----------------------------------------------------------------###
###                                                                 ###
###                 Project FLu Trend - Main functions              ###
###                                                                 ###
###-----------------------------------------------------------------###
###-----------------------------------------------------------------###

#
wft_pc<-function(page="Influenza",lang="de",start_date=Sys.time()-24*60*60,end_date=Sys.time(),timezone="GMT",status=0){
  require(RJSONIO)
  require(RCurl)
  require(pageviews)
  start_date<-format(start_date, tz=timezone,usetz=TRUE); end_date<-format(end_date, tz=timezone,usetz=TRUE)
  start_date_old_method=start_date;end_date_old_method=end_date;
  start_date_new_method=ISOdatetime(2016,01,01,0,0,01,tz=timezone); end_date_new_method=NULL
  
  if(start_date>=ISOdatetime(2016,01,01,0,0,01,tz=timezone)) {
    start_date_old_method=NULL
    end_date_old_method=NULL
    start_date_new_method=start_date
    end_date_new_method=end_date}
  
  if(end_date>=ISOdatetime(2016,01,01,0,0,01,tz=timezone)) {
    end_date_old_method=ISOdatetime(2015,12,31,23,59,59,tz=timezone)
    end_date_new_method=end_date
  }
  
  df<-list(); df_temp<-list()
  
  if(as.Date(start_date)<as.Date("2016-01-01")) {
    time_temp_days<-list()
    time_temp_days$date<-seq(as.Date(start_date_old_method),as.Date(end_date_old_method), "days") #generate day strings-> lookup table for page view results
    time_temp_days$date<-substr(time_temp_days$date, 1, 10) ### ! NEEDS TO BE IMPLEMENTED toavoid 31st February etc!!!
    time_temp_days<-as.data.frame(time_temp_days)
    t1<-paste(format(as.Date(start_date_old_method),format="%Y-%m"),"-01",sep="")
    t2<-paste(format(as.Date(end_date_old_method),format="%Y-%m"),"-01",sep="")
    time_temp_months<-seq(as.Date(t1,format="%Y-%m-%d"),as.Date(t2,format="%Y-%m-%d"), "months") #generate months strings-> input for download
    time_temp_months<-substr(time_temp_months, 1, 7)
    time_temp_months<-sub("-","",time_temp_months)
    
    
    for(p in 1:length(page)){
      temp_page<-page[p]
      pageview_month<-NULL
      
      for(i in 1:length(time_temp_months)){
        month_year<-time_temp_months[i]
        pageview_temp_month<-getURL(paste("http://stats.grok.se/json/",lang,"/",month_year, "/",temp_page,  sep=""))
        pageview_temp_month<-data.frame(fromJSON(pageview_temp_month))
        pageview_temp_month$date<-rownames(pageview_temp_month )
        pageview_month<-rbind(pageview_month,pageview_temp_month)
        pageview_month<-pageview_month[order(pageview_month$date),]
        pageview_month<-merge(time_temp_days,pageview_month,by="date")
        df_temp<-data.frame(date=as.Date(pageview_month$date),views=pageview_month$daily_views,page=pageview_month$title,lang=pageview_month$project)
        df_temp<-unique(df_temp)
        if(status==1){ print(paste("site=", p,"/",length(page)," date=",i,"/",length(time_temp_months) ) )}
        
      }#month loop closing <2016
      df[[p]]<-df_temp
    }#page loop closing <2016
    
    names(df)<-c(page)
  }#if <2016 included closing
  
  if(as.Date(end_date)>=as.Date("2016-01-01") & length(df)>=1) {
    for(p in 1:length(page)){
      tryCatch({
        temp_16<-article_pageviews(project = paste(lang,".wikipedia",sep=""),
                                   article = page[p], platform = "all",
                                   user_type = "all", 
                                   start = pageview_timestamps(as.Date(start_date_new_method,tz=timezone)), 
                                   end = pageview_timestamps(as.Date(end_date_new_method,tz=timezone)), 
                                   reformat = TRUE)
        df_16<-data.frame(date=temp_16$date,views=temp_16$views,page=temp_16$article,lang=lang)
        df[[p]]<-rbind(df[[p]],df_16)
        
        if(status==1){ print(paste("site=", p,"/",length(page),"2016-" ) )}
      },
      error=function(e) {})}
  }#2016 page loop closing
  
  if(as.Date(end_date)>=as.Date("2016-01-01") & !length(df)>=1) {
    for(p in 1:length(page)){
      tryCatch({
        temp_16<-article_pageviews(project = paste(lang,".wikipedia",sep=""),
                                   article = page[p], platform = "all",
                                   user_type = "all", 
                                   start = pageview_timestamps(as.Date(start_date_new_method,tz=timezone)), 
                                   end = pageview_timestamps(as.Date(end_date_new_method,tz=timezone)), 
                                   reformat = TRUE)
        df_16<-data.frame(date=temp_16$date,views=temp_16$views,page=temp_16$article,lang=lang)
        
        df[[p]]<-df_16
        if(status==1){ print(paste("site=", p,"/",length(page),"2016-" ) )}
      }  ,
      error=function(e) {})}
  }#2016 page loop closing
  
  
  
  # if 2016 included closing
  names(df)<-page
  return(df) 
}# function closing
wft_lp<-function(page="Influenza",lang="de") { # the API of wikipedia takes a query and gives infrmation in json format
  require(RJSONIO)
  require(RCurl)
  require(purrr)
  
  page=gsub(" ","_",page)
  temp_page<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,  sep="")
  linked_pages<-getURL(temp_page) # this query asks for links on the respective page
  linked_pages<-(fromJSON(linked_pages)) #json format
  names(linked_pages$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
  links<-map_df(linked_pages$query$pages$name$links, as.list)$title # map_df can nicely trnasform the weird structure of the data 
  
  temp_page_cont<-NULL;temp_page_cont1<-NULL
  if(!is.null(linked_pages$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",linked_pages$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont1<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # if links>500, multiple queries are neccessary
  temp_page_cont2<-NULL
  if(!is.null(temp_page_cont$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",temp_page_cont$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont2<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # max number of links = 1500
  temp_page_cont3<-NULL
  if(!is.null(temp_page_cont$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",temp_page_cont$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont3<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # max number of links = 2000
  
  links<-c(links,temp_page_cont1,temp_page_cont2,temp_page_cont3) 
  check_the_links<-wft_pc(links,lang=lang,start_date = "2017-01-01",end_date = "2017-01-01",status=1)
  check_list<-t(data.frame(lapply(check_the_links,length)))
  which(check_list==0)
  links<-links[- which(check_list==0)]
  return(links)  
}
wft_lp_names<-function(page="Influenza",lang="de") { # the API of wikipedia takes a query and gives infrmation in json format
  require(RJSONIO)
  require(RCurl)
  require(purrr)
  
  page=gsub(" ","_",page)
  temp_page<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,  sep="")
  linked_pages<-getURL(temp_page) # this query asks for links on the respective page
  linked_pages<-(fromJSON(linked_pages)) #json format
  names(linked_pages$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
  links<-map_df(linked_pages$query$pages$name$links, as.list)$title # map_df can nicely trnasform the weird structure of the data 
  
  temp_page_cont<-NULL;temp_page_cont1<-NULL
  if(!is.null(linked_pages$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",linked_pages$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont1<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # if links>500, multiple queries are neccessary
  temp_page_cont2<-NULL
  if(!is.null(temp_page_cont$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",temp_page_cont$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont2<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # max number of links = 1500
  temp_page_cont3<-NULL
  if(!is.null(temp_page_cont$continue[1])){ # the problem was that the wikipediR(?) page_links function did only give 500
    temp_page_cont<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&prop=links&pllimit=500&format=json&titles=",page,"&plcontinue=",temp_page_cont$continue[1],  sep="")
    temp_page_cont<-getURL(temp_page_cont)
    temp_page_cont<-(fromJSON(temp_page_cont))
    names(temp_page_cont$query$pages)<-"name" # this solves a problem that arises from different names assigned by wikipedia
    temp_page_cont3<-map_df(temp_page_cont$query$pages$name$links, as.list)$title
  } # max number of links = 2000
  
  links<-c(links,temp_page_cont1,temp_page_cont2,temp_page_cont3) 
  return(links)  
}
wft_lang_titles<-function(page="Influenza",origin_lang="en",filter_lang=c("de","it","nl")){
  require( wikipediatrend)
  temp<-data.frame(code=origin_lang,lang=language_table[c( match(origin_lang,language_table$ISO_639_1)),2] ,page=page)
  titles <- data.frame(wp_linked_pages( page= page,lang=origin_lang))
  lang_titles<-data.frame(code=filter_lang,lang=language_table[c( match(filter_lang,language_table$ISO_639_1)),2])
  lang_titles<-cbind(lang_titles,page=titles[c( match(lang_titles$code,titles$lang)),3])
  lang_titles<-rbind(temp,lang_titles)
  return(lang_titles)
}
store_outcomes<-function(country=NA,code=NA,type_of_outcome,outcome_data,dates){
  
    # I should make input checks on double dates and stuff?
  is_it_daily<-seq(from=min(dates),to=max(dates),by=1)
  #if(length(is_it_daily)!=length(dates)){stop("something wrong with the dates, must be daily") }
  if(sum(is.na(type_of_outcome))+sum(is.na(outcome_data))>0){stop("NAs not allowed in outcome data.")}
  if(class(dates)!="Date"){stop("dates must be class Date")}
  if(!is.na(country)&!is.na(code)){ stop("error: give me only a code or a country, not both")}
  if(!is.na(country)){country=country}
  if(!is.na(code)){country=country_list$description[which(country_list$country_code_lowcaps==code)] }
  if(min(dates)<min(list_of_outcomes$andorra$dependents$date)){ 
    outcome_data<-outcome_data[dates>=as.Date("2010-01-01")]
    dates<-dates[dates>=as.Date("2010-01-01")]
  }

  list_item<-which(country_list$description==country)
  column<-which(names(list_of_outcomes$andorra$dependents)==type_of_outcome)
  rows<-match(dates,list_of_outcomes$andorra$dependents$date)
  list_of_outcomes[[list_item]][[2]][rows,column]<-c(outcome_data)
  
  length( list_of_outcomes[[list_item]][[2]][rows,column])
  sum(is.na(list_of_outcomes[[list_item]][[2]][rows,column]))
  return(list_of_outcomes)
}
update_input_wiki<-function(type_of_inputs=c("wiki_primary","wiki_linked","wiki_related"),lang=lang,df=list_of_inputs){
  
  item<-which(names(list_of_inputs)==lang)
  type<-which(names(list_of_inputs[[item]])==type_of_inputs)
  end.at<-as.Date(Sys.time())
  
  if(sum(type_of_inputs=="wiki_primary")==1){
    start.at=max(df[[item]]$wiki_primary[[1]]$date[!is.na(  df[[item]]$wiki_primary[[1]][,2])],na.rm=T) # last date for which wiki_primary has view data
    cat("update wiki_primary \n")
    update_wiki_primary=wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_primary_term,
                               lang=lang,
                               start_date=start.at,
                               end_date=end.at,
                               status=1)
    
    df[[item]]$wiki_primary[[1]]<-rbind(df[[item]]$wiki_primary[[1]],update_wiki_primary[[1]]) # combine old and new
  }
  
  
  if(sum(type_of_inputs=="wiki_linked")==1){
    start.at=max(df[[item]]$wiki_linked[[1]]$date[!is.na(  df[[item]]$wiki_linked[[1]][,2])],na.rm=T) # last date for which wiki_primary has view data
    cat("update wiki_linked \n")
    update_wiki_linked=wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_linked_terms,
                              lang=lang,
                              start_date=start.at,
                              end_date=end.at,
                              status=1)
    for(i in 1:length(update_wiki_linked)){
      df[[item]]$wiki_linked[[i]]<-rbind(df[[item]]$wiki_linked[[i]],update_wiki_linked[[i]]) # combine old and new
    }}
  
  if(sum(type_of_inputs=="wiki_related")==1){
    start.at=max(df[[item]]$wiki_related[[1]]$date[!is.na(  df[[item]]$wiki_related[[1]][,2])],na.rm=T) # last date for which wiki_primary has view data
    cat("update wiki_related \n")
    update_wiki_related=wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_related_terms,
                               lang=lang,
                               start_date=start.at,
                               end_date=end.at,
                               status=1)
    for(i in 1:length(update_wiki_related)){
      df[[item]]$wiki_related[[i]]<-rbind(df[[item]]$wiki_related[[i]],update_wiki_related[[i]]) # combine old and new
    }}
  
  return(df)
}
reformat_wiki<-function(df,start_date=min(df[[1]]$date),end_date=max(df[[1]]$date),status=0){ # input is df= a list from wft_pc and source_list=which was "page" in wft_pc
  source_list=names(df)
  temp_df<-NULL; ref_dates<-data.frame(date=as.Date(seq(start_date,end_date,by="day")))
  big_temp_df<-matrix(nrow=length(seq(start_date,end_date,by="day")) ,ncol=c(length(df )+1) )
  big_temp_df[,1]<-as.Date(seq(start_date,end_date,by="day"))
  for(i in 1:length(source_list)){ #
    if(status==1){ cat(i,"\n")}
    if(length(data.frame(df[[i]]))!=0){
      temp_df<-data.frame(df[[i]])
      names(temp_df)<-c("date","views",names(df)[i] ,"lang")
      temp_df$date<-as.Date(temp_df$date,origin="1970-01-01")
      #    temp_df$date<-as.Date(temp_df$date)
      
      temp_df<-merge(ref_dates,temp_df,by="date",all=FALSE)
      temp_df<-merge(ref_dates,temp_df,by="date",all=TRUE)
      
      temp_df$views[is.na(temp_df$views)]<-0; temp_df$lang[is.na(temp_df$lang)]<-temp_df$lang[!is.na(temp_df$lang)][1]; 
      temp_df[which(is.na(temp_df[,3])),3]<-temp_df[which(!is.na(temp_df[,3])),3][1]
      big_temp_df[,i+1]<-temp_df[,2]
    }}
  big_temp_df<-data.frame(big_temp_df)
  names(big_temp_df)<-c("date",source_list)
  big_temp_df$date<-as.Date(big_temp_df$date,origin=as.Date("1970-01-01"))
  for(j in 1:(length(source_list))+1){
    rows_NA<-which(is.na(big_temp_df[,j]))
    big_temp_df[rows_NA,j]<--1 # when views are NA, -1 is inserted instead to keep functionality of the whole df
  }
  
  
  return(big_temp_df)
}
pft_add_terms<-function(type="related",terms=c("Husten","Schnupfen"), add=1, lang="de"){
  item<-which(names(lookup_terms)==lang)
  list_type<-which(names(lookup_terms[[lang]])==type)
  if(sum(lookup_terms[[lang]][[list_type]] %in% terms)==0){
    if(add==1){
      if(class(lookup_terms[[lang]][[list_type]])=="character"){lookup_terms[[lang]][[list_type]]<-  c(lookup_terms[[lang]][[list_type]],terms) }  
      if(class(lookup_terms[[lang]][[list_type]])=="list"){ lookup_terms[[lang]][[list_type]]<-  c(as.character(data.frame(lookup_terms[[lang]][[list_type]])[,1]),terms) } 
      if(class(lookup_terms[[lang]][[list_type]])=="NULL"){lookup_terms[[lang]][[list_type]]<-terms }  
    }
    if(add==0){
      lookup_terms[[lang]][[list_type]]<-terms}}
  else( cat("one or more terms already exist"))
  cat("Last terms for lang=",lang,"&type=",list_type,":\n",paste("...", tail(lookup_terms[[lang]][[list_type]],sep="")))
  return(lookup_terms)
}
pft_random_wiki_pages<-function(lang="en",number="1",status=1){
  require(RCurl)
  require(RJSONIO)
  list_of_random_wiki_pages<-NULL
  for(i in 1:number){
    if(status==1){cat(i,"/",number,"\n")}
    temp_page<-paste("https://",lang,".wikipedia.org/w/api.php?action=query&generator=random&grnnamespace=0&rnlimit=1&format=json",  sep="")
    x<-getURL(temp_page)
    x<-fromJSON(x)
    x<-data.frame(x$query$pages)
    x<-as.character(x[[3]])
    list_of_random_wiki_pages<-c(list_of_random_wiki_pages,x)
  }
  return(list_of_random_wiki_pages)
}
pft_simple_trend<-function(x,s.window=7, t.window=NULL){
  temp.stl=stl(ts(x,freq=7),s.window=7,t.window=t.window)
  return(temp.stl$time.series[,2])
}

# Working Horses


#### NEED REVISIN
wft_weeks_to_days<-function(weekly_counts,  # vector with outcome ata
                            week_dates=NULL, # vector with weekly dates
                            format="%Y-%u", # dates can have several formats:'%Y-%u' (2010-01),'%Y-W%V-%u'(2010-W01) and '%Y-%U-%u'(2010-01-07) 
                            special.fun="53to1" # special functions to deal with specialitirs of certain countries or datasets
                            ){ 
  require(ISOweek)
  
  
  
  df<-data.frame(week_dates=week_dates,weekly_counts=weekly_counts)
  #df_ref<-data.frame(week_dates=seq(from=min(week_dates),to=max(week_dates),by=7))
  #merge(df_ref,df,by="week_dates")
  if(sum(is.na(weekly_counts))>0){stop("Missing incidence counts are not allowed")}
  if(format!="%Y-%u" & format!="%Y-W%V-%u"& format!="%Y-%U-%u" ){stop("Function can only deal with formats '%Y-%u' (2010-01),'%Y-W%V-%u'(2010-W01) and '%Y-%U-%u'(2010-01-07) \n")}
  if(length(df$week_dates)>length(df[complete.cases(df),]$week_dates)){  # missing data?
    cat("Warning: Some cases have been removed due to NAs \n") 
    df<-df[complete.cases(df),]} # missing data will be removed
  if(format=="%Y-%u"){
    if(special.fun=="53to1"){ # a special.fun for vietnam data in which week 53 is present and week 1 is NA or missing
      df$week_dates<-gsub("53","01",df$week_dates)}
    df$week_dates<-ISOweek2date( paste(gsub("-","-W",df$week_dates),"-7",sep="")) #"2017-01->2017-W01-07->2017-01-05"
  } 
  if(format=="%Y-W%V-%u"){
    df$week_dates<-ISOweek2date(week_dates)
  }  
  if(format=="%Y-%U-%u"){
    df$week_dates<-as.Date(week_dates,format="%Y-%U-%u")
  }
  
  # creating a vector with daily dates from weekly dates 
  temp_days<-NULL; y<-0:6
  for(i in 1:length(df$week_dates)){ 
    x<-rep(df$week_dates[i],times=7)
    x<-x-6;z<-x+y # x-6 OR x-7 ???
    temp_days<-as.Date(c( temp_days,z),origin="1970-01-01")}
  
  # computing daily counts by assuming linear changes 
  daily_count_temp<-rep(df$weekly_counts[1]/7,7)
  for(i in 1:(length(df$weekly_counts)-1)) {
    y<-seq(df$weekly_counts[i],df$weekly_counts[i+1],length.out = 7)/7
    daily_count_temp<-c(daily_count_temp,y)}
  
  # combine daily dates and counts
  df_daily<-data.frame(date=as.Date(temp_days),count=daily_count_temp)
  df_ref<-data.frame(date=seq(from=min(df_daily$date),to=max(df_daily$date),by=1))
  df_daily<-merge(df_ref,df_daily,all=T)
  if(sum(is.na(df_daily))>0){cat("WARNING: NAs HAVE BEEN REPLACED WITH 0 !!!")}
  df_daily$count[is.na(df_daily$count)]<-0
  return(df_daily)
} 

pft_build_up<-function(country="germany",
                       lang="de",
                       type_of_outcome="Influenza_AB_lab_incidence_std",
                       type_of_input=c("wiki_primary", "wiki_linked","wiki_related"), 
                       start_date=start_date, 
                       end_date=end_date,
                       input_df=list_of_inputs,
                       outcome_df=list_of_outcomes,
                       status=1){
  # if statements needed...
  
  outcome_country<-  which(names(outcome_df)==  country)
  if(length(outcome_country)!=1){stop("something wrong with outcome country")}
  outcome_cols<-names(outcome_df[[outcome_country]]$dependents) %in% c(type_of_outcome, "date")
  if(sum(outcome_cols)<2){stop("something is wrong with the outcome df, needs a date and a valid type")}
  
  input_lang<-which(names(list_of_inputs)== lang)
  input_lists<-which( names(input_df[[input_lang]]) %in% (type_of_input))
  
  # time frame
  if(class(start_date)=="Date" & class(end_date)=="Date"){ # when 2 dates are provided
    df_temp<-data.frame(date=seq(as.Date(start_date),as.Date(end_date),by=1))}
  
  if(class(start_date)=="Date" & as.character(end_date)=="auto"){ # when start date is provided
    end=max(input_df[[input_lang]][[input_lists[1]]][[1]]$date)
    df_temp<-data.frame(date=seq(as.Date(start_date),as.Date(end),by=1))}
  
  if(as.character(start_date)=="auto" & class(end_date)=="Date"){ # when end date is provided
    start=min(input_df[[input_lang]][[input_lists[1]]][[1]]$date)
    df_temp<-data.frame(date=seq(as.Date(start),as.Date(end_date),by=1))}
  
  if(as.character(start_date)=="auto" & as.character(end_date)=="auto"){ # when no dates are provided
    end=max(input_df[[input_lang]][[input_lists[1]]][[1]]$date)
    start=min(input_df[[input_lang]][[input_lists[1]]][[1]]$date)
    df_temp<-data.frame(date=seq(as.Date(start),as.Date(end),by=1))}
  df_temp<-merge(df_temp,outcome_df[[outcome_country]]$dependents[,outcome_cols],by="date",all=F)
  
  for(i in 1:length(input_lists)){
    df_temp<-merge(df_temp,reformat_wiki(input_df[[input_lang]][[input_lists[i]]]),by="date",all=F)
  }
  return(df_temp)
  if(status==1){ cat("We have",sum(is.na(df_temp))," missing values in the dataframe \n 
                     min date, max date, type in, type out")}
  }

pft_initiate<-function(lang="nl",
                       start_date=as.Date("2010-01-01"),
                       end_date=as.Date("2016-12-31"),
                       status=1,
                       df=list_of_inputs){
  item<-which(names(list_of_inputs)==lang)
  x<-NULL
  if(!is.null(df[[item]]$wiki_lookup_searches$wiki_primary_term)){
    cat("initiate wiki_primary \n")
    df[[item]]$wiki_primary<-wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_primary_term,
                                    lang=lang,
                                    start_date=start_date,
                                    end_date=end_date,
                                    status=status)}
  
  if(!is.null(df[[item]]$wiki_lookup_searches$wiki_linked_terms)){
    cat("initiate wiki_linked \n")
    df[[item]]$wiki_linked<-NULL
    for(i in 1:length(df[[item]]$wiki_lookup_searches$wiki_linked_terms)){
      cat("linked page",i,"of",length(df[[item]]$wiki_lookup_searches$wiki_linked_terms),"\n")
      tryCatch({
        x<-length(df[[item]]$wiki_linked)+1
        df[[item]]$wiki_linked[[x]]<-wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_linked_terms[i],
                                            lang=lang,
                                            start_date=start_date,
                                            end_date=end_date,
                                            status=0)[[1]]
        names(df[[item]]$wiki_linked)[x]<-df[[item]]$wiki_lookup_searches$wiki_linked_terms[i]
      },
      error=function(e) {cat("wiki page not found \n")})}
    
  }
  
  
  if(!is.null(df[[item]]$wiki_lookup_searches$wiki_related_terms)){
    cat("initiate wiki_related \n")
    df[[item]]$wiki_related<-NULL
    for(i in 1:length(df[[item]]$wiki_lookup_searches$wiki_related_terms)){
      cat("related page",i,"of",length(df[[item]]$wiki_lookup_searches$wiki_related_terms),"\n")
      tryCatch({
        x<-length(df[[item]]$wiki_related)+1
        df[[item]]$wiki_related[[x]]<-wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_related_terms[i],
                                             lang=lang,
                                             start_date=start_date,
                                             end_date=end_date,
                                             status=0)[[1]]
        names(df[[item]]$wiki_related)[x]<-df[[item]]$wiki_lookup_searches$wiki_related_terms[i]
      },
      error=function(e) {cat("wiki page not found \n")})}
  }
  
  if(!is.null(df[[item]]$wiki_lookup_searches$wiki_random_page)){
    cat("initiate wiki_random \n")
    df[[item]]$wiki_random=NULL
    for(i in 1:length(df[[item]]$wiki_lookup_searches$wiki_random_page)){
      cat("random page",i,"of",length(df[[item]]$wiki_lookup_searches$wiki_random_page),"\n")
      tryCatch({
        x<-length(df[[item]]$wiki_random)+1
        df[[item]]$wiki_random[[x]]<-wft_pc(page=df[[item]]$wiki_lookup_searches$wiki_random_page[i],
                                            lang=lang,
                                            start_date=start_date,
                                            end_date=end_date,
                                            status=0)[[1]]
        names(df[[item]]$wiki_random)[x]<-df[[item]]$wiki_lookup_searches$wiki_random_page[i]
        
      },
      error=function(e) {cat("wiki page not found \n")})}
    
  }
  
  #  df[[item]]$  ...more?
  
  return(df)
}



eval_pft_model<-function(pft_model_df, # a list, created by pft_model
                         method="plot_mean_performance", # "plot_mean_performance","nowcast" or "plot1"
                         eval_date="20160801", # for method = "plot1"
                         lang="nl", detrending=1,detrend_window=21,detrend_robust=T,wiki_normalization=0      # for method= "nowcast"
                          ){ 
  require(ggplot2)
  if(method=="plot1"){
    where.to.look<-which(names(pft_model_df)== paste("t",eval_date,sep=""))
    train.data=pft_model_df[[where.to.look]]$train.data
    test.data=pft_model_df[[where.to.look]]$test.data
    all.data=pft_model_df[[where.to.look]]$combined.data
    lm.trained=pft_model_df[[where.to.look]]$lm.model
    
    eval_from_date_plot=ggplot(data=all.data,aes(x=date,y=outcome)) +
      geom_line() +
      geom_line(data=train.data, aes(x=date,y=predict(lm.trained,newdata = train.data) )  ,col="blue") +
      geom_line(data=test.data, aes(x=date,y=predict(lm.trained,newdata = test.data)),col="red") +
      annotate("text",x=min(all.data$date)+100, y =max(all.data$outcome) , 
               label = paste("trained r2=", round(summary(lm.trained)$adj.r.squared,3) ) ,col="blue") +
      annotate("text",x=min(all.data$date)+100, y =max(all.data$outcome)-1 , 
               label = paste("tested r2=", round(cor(predict(lm.trained,newdata = test.data), test.data$outcome)^2,3) ) ,col="red")
    return(eval_from_date_plot)
    
  } 
  
  if(method=="plot_mean_performance"){ 
    
    # getting the predcited values per predcition day for each day
    nowcast_eval<-NULL;date<-list(); dates.temp<-date()
    for(d in 1:length(pft_model_df)){
      for(f in 1:length(pft_model_df[[1]]$model.stats$predicted.lm)){
        no.nas<-try(as.numeric(pft_model_df[[d]]$model.stats$predicted.lm[f]),silent=T)
        date[[f]]<-ifelse(is.na(no.nas),0,no.nas)}
      nowcast_eval<-rbind(nowcast_eval,date)} # this is a confusing list with all the predictions for days 1-28
    if(length(nowcast_eval[1,])<28){stop("plot_mean_performance needs at least 28 models")}
    for(s in 1:length(nowcast_eval[1,])){ # predcition days need to be shifted along the time axis to be in the right column
      nowcast_eval[ ,s]    = c( rep(NA,times=s-1),nowcast_eval[ c(seq(from=1,to=  length(nowcast_eval[,2]) -s+1) ),s])}
    
    row.names(nowcast_eval)<-as.character(seq(from=1,to=length(row.names(nowcast_eval))))
    nowcast_eval<-data.frame(nowcast_eval)
    nowcast_eval<-data.frame(lapply(nowcast_eval,as.numeric))
    for(n in 1:length(nowcast_eval[,1])){ # now rows show the dates for which the prediciton was made
      row.names(nowcast_eval)[n]<-as.character(pft_model_df[[n]]$test.data$date[1])}
    
    for(a in 1:length(nowcast_eval[1,])){ # and now the columns give the names: how far away from the training data set the predictionw as made
      names(nowcast_eval)[a]<-paste("pred.day.",a,sep="")} 
    
    nowcast_eval<-data.frame(date=as.Date(row.names(nowcast_eval)),data.frame(nowcast_eval))
    
    
    
    
    ### Plotting per day
    # creating reference data.frame in a complicated way...
    temp1<-NULL; temp2<-NULL; temp3<-NULL; 
    source_df<-data.frame(date=pft_model_df[[1]]$combined.data$date,outcome=pft_model_df[[1]]$combined.data$outcome)
    train_df<-data.frame(date=pft_model_df[[1]]$train.data$date,outcome=pft_model_df[[1]]$train.data$outcome,trained.predicted=predict(pft_model_df[[1]]$lm.model))
    test_df<-data.frame(date=pft_model_df[[1]]$test.data$date,outcome=pft_model_df[[1]]$test.data$outcome)
    
    for(i in 1:length(pft_model_df)){ # retriving dates and actual outcomes from pft_model, unneccassarily difficult!?
      temp1<-data.frame(date= pft_model_df[[i]]$combined.data$date,
                        outcome=pft_model_df[[i]]$combined.data$outcome)
      source_df=rbind(source_df,temp1[-which(temp1$date %in% source_df$date),])
      
      temp2<-data.frame(date= pft_model_df[[i]]$train.data$date,
                        outcome=pft_model_df[[i]]$train.data$outcome,
                        trained.predicted=predict(pft_model_df[[i]]$lm.model))
      train_df=rbind(train_df,temp2[-which(temp2$date %in% train_df$date),])
      
      temp3<-data.frame(date= pft_model_df[[i]]$test.data$date,
                        outcome=pft_model_df[[i]]$test.data$outcome)
      test_df=rbind(test_df,temp3[-which(temp3$date %in% test_df$date),])
    } # stupid way to do this probably...
    
    
    # Actual versus Predicted per day of prediciton
    test_actual_rate=ggplot(data=test_df,aes(x=date,y=outcome)) +
      geom_line() 
    
    test_actual_vs_pred_all= test_actual_rate + 
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.1,col=paste("pred.day.1")) ) +
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.7,col=paste("pred.day.7")) ) +
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.14,col=paste("pred.day.14")) ) +
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.21,col=paste("pred.day.21")) ) +
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.28,col=paste("pred.day.28")) ) 
    
    test_actual_vs_pred_d7= test_actual_rate + 
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.7,col=paste("pred.day.7")) )
    
    test_actual_vs_pred_d14= test_actual_rate + 
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.14,col=paste("pred.day.14")) )
    
    test_actual_vs_pred_d21= test_actual_rate + 
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.21,col=paste("pred.day.21")) )
    
    test_actual_vs_pred_d28= test_actual_rate + 
      geom_line(data=nowcast_eval,aes(x=date,y=pred.day.28,col=paste("pred.day.28")) )
    
    
    nowcast_diff<-merge(nowcast_eval,test_df,by="date",all=F)
    nowcast_diff[,-c(1,30)]<-nowcast_eval[,-1]-   nowcast_diff$outcome
    
    test_diff_plot= ggplot(data=nowcast_diff,aes(x=date,y=pred.day.1)) +
      geom_line() +
      geom_line(data=nowcast_diff,aes(x=date,y=pred.day.7,col=paste("pred.day.7")) ) +
      geom_line(data=nowcast_diff,aes(x=date,y=pred.day.14,col=paste("pred.day.14")) ) +
      geom_line(data=nowcast_diff,aes(x=date,y=pred.day.21,col=paste("pred.day.21")) ) +
      geom_line(data=nowcast_diff,aes(x=date,y=pred.day.28,col=paste("pred.day.28")) ) +
      geom_line(aes(y=0))
    
    # How did the training period go?
    training_actual_rate=ggplot(data=train_df,aes(x=date,y=outcome)) +
      geom_line() 
    
    training_actual_vs_pred= training_actual_rate + 
      geom_line(data=train_df,aes(x=date,y=trained.predicted,col=paste("last training prediction")) ) 
    
    training_diff_plot= ggplot(data=train_df,aes(x=date,y=trained.predicted-outcome)) +
      geom_line() +
      geom_line(aes(y=0))
    
    # Evaluating the mean error per prediction for day 1-28
    mean.error.per.day=as.numeric(colMeans(nowcast_diff[,-c(1,29)],na.rm=T))
    mean.sqr.error.per.day=as.numeric(colMeans(nowcast_diff[,-c(1,29)]^2,na.rm=T))
    
    mean.error.plot=ggplot(data=NULL,aes(x=1:28,y=mean.error.per.day))+
      geom_line()
    mean.sqr.error.plot=ggplot(data=NULL,aes(x=1:28,y=mean.sqr.error.per.day))+
      geom_line()
    
    performance_eval<-list(nowcast_eval=nowcast_eval,
                           nowcast_diff=nowcast_diff,
                           mean.error.per.day=mean.error.per.day,
                           mean.sqr.error.per.day=mean.sqr.error.per.day,
                           plots=list(test_actual_rate=test_actual_rate,
                                      test_actual_vs_pred_all=test_actual_vs_pred_all,
                                      test_actual_vs_pred_d7=test_actual_vs_pred_d7,
                                      test_actual_vs_pred_d14=test_actual_vs_pred_d14,
                                      test_actual_vs_pred_d21=test_actual_vs_pred_d21,
                                      test_actual_vs_pred_d28=test_actual_vs_pred_d28,
                                      test_diff_plot=test_diff_plot,
                                      mean.error.plot=mean.error.plot,
                                      mean.sqr.error.plot=mean.sqr.error.plot,
                                      training_actual_rate=training_actual_rate,
                                      training_actual_vs_pred=training_actual_vs_pred,
                                      training_diff_plot=training_diff_plot))
    return( performance_eval  )
  } # if plot_mean_performance bracket end
  
  
  if(method=="nowcast"){ 
    latest.lm=pft_model_df[[length(pft_model_df)]] # get latest model from pft_model
    today= Sys.Date() # what date is today?
    start.at=max(latest.lm$train.data$date)   # looking 6 months back?!
    start.at<-as.Date(ifelse(length(seq(from=start.at,to=today,by=1))>60,start.at,today-60),origin=as.Date("1970-01-01"))
    pages=row.names(data.frame(latest.lm$lm.model$coefficients)) # names for looking up recent wikipedia data
    pages=pages[!pages  %in% c("date","(Intercept)")] # eliminating date and intercepts from page nams
    new_wiki_data=tryCatch({ wft_pc(page=pages,  # look up recent wikipedia page views
                         lang=lang,
                         start_date = start.at,
                         end_date = today)},error=function(e){stop("Sorry, some of the pages that are used in the model can not be found on today's Wikipedia")})
    new_wiki_data=reformat_wiki(df=new_wiki_data[which(lapply(new_wiki_data, length)>0)]) # eliminate all items with length = 0 (intercept, date,...)
    
    if(detrending==1){
      names<-names(new_wiki_data)
      for(o in 1:length(names(new_wiki_data)[-1]))
        new_wiki_data[,o+1]<-as.numeric(stl(ts(new_wiki_data[,o+1],freq=7),s.window = 7,t.window=detrend_window,robust=detrend_robust)$time.series[,2])
    }
    nowcast.predictions=predict(latest.lm$lm.model, newdata=new_wiki_data) # make new predictions with recent wiki data
    nowcast.df=data.frame(date=new_wiki_data$date, prediction=nowcast.predictions)
    
    # plotting things
    now.plot<-ggplot(data=nowcast.df, aes(x=date, y=prediction)) +
      geom_line(linetype=1,col="lightblue",lwd=1.5) +
      geom_line(data=latest.lm$lm.model$model, aes(x=date,y=predict(latest.lm$lm.model)),col="blue",lwd=1.5) +
      geom_line(data=latest.lm$lm.model$model, aes(x=date,y=outcome),col="black",lwd=1.5) +
      geom_vline(xintercept = as.numeric(start.at))
    return(now.plot)
  } # if noewcast bracket end
  
}  # eval_date="20160801")# format="YYYYMMDD") or NULL




pft_model<-function(lang="nl",
                    start_date="auto", # auto will get min date
                    end_date="auto",   # auto will get max date
                    type_of_outcome="Influenza_AB_lab_incidence",
                    country = "netherlands",
                    type_of_input = c("wiki_primary","wiki_linked"), # + wiki_relared? + wiki_random?!
                    
                    # lasso regression + cross validation
                    method="cv", # "cv" or "simple.lm"
                    
                    cv_fold="M", # "M", "Y", or a number indicating the number of combined days to be sampled to sampled? size 
                    cv_lambda="min", #  "min" or "1se"... a number is possible but nonsense!
                    grid=10^seq(10,-2,length=100), # a grid for cv, default!
                    start_date_eval="auto",  # can also be a Date, must be compatible with training period !!!
                    end_date_eval="auto",   # can also be a Date, "auto" gets the maximum, must be compatible with...?!
                    training_period=365*2, # training period in days or "past", must be compatible with min and max dates!!!
                    # evaluation_intervall=1, # not sure .... interval for modelling
                    eval_period=28, # this is nor related to cv, is it?
                    
                    # do you want decomposed independent variables?
                    detrending=1, # Seasonal Decomposition of Time Series by Loess
                    detrend_window=21,
                    detrend_robust=T, # or F
                    
                    time_lag= 0, # how many days is official data behind? -7 for 1 week behind page view data
                    wiki_normalization=0,
                    status=1){ 
  # Loading required packages
  require(glmnet)
  require(ISOweek)
  require(ggplot2)
  
  # setting up the working df
  df<-pft_build_up(start_date = start_date, 
                   end_date = end_date, 
                   country = country,
                   type_of_outcome = type_of_outcome,
                   lang= lang,
                   type_of_input = type_of_input)
  # raw.data<-df # makes a copy for forensic purposes, could be retrieved afterwards to compare results?
  df<-df[complete.cases(df),]
  
  if(time_lag!=0){
    names<-names(df)
    lag.df<-data.frame(date=df$date, df[,which(names(df)==type_of_outcome)])
    lag.df[,1]<-lag.df[,1]+time_lag
    df<-merge(lag.df,df[,-which(names(df)==type_of_outcome)],by="date",all=F)
    names(df)<-names
  } # the outcome is moved x days on the time axis (x=time_lag), dates with NA are deleted from the data.frame!!!
  if(wiki_normalization==1){
    random_wiki_usage<-pft_build_up(start_date = start_date, 
                                    end_date = end_date, 
                                    country = country,
                                    type_of_outcome = type_of_outcome,
                                    lang= lang,
                                    type_of_input = "wiki_random",
                                    status=0)
    normalizing_factor.1<-as.numeric()
    for(w in 1:length(random_wiki_usage[,1])){
      normalizing_factor.1[w]<-mean(as.numeric(random_wiki_usage[w,-1]),na.rm=T,trim=0.05)
    }} # introduces a normalization factor from "wiki_random"
  if(detrending==1){
    if(length(df$date)<detrend_window){stop("Training time frame is too short for these settings.")}
    # This needs to be validated: Does it really work properly?
    names<-names(df)
    for(o in 1:(length(names(df))-2)+2)
      df[,o]<-as.numeric(stl(ts(df[,o],freq=7),s.window = 7,t.window=detrend_window,robust=detrend_robust)$time.series[,2])
    if(wiki_normalization==1){
      norm<-data.frame( date=random_wiki_usage$date, normalizing_factor.weekly=as.numeric(stl(robust = T,ts(normalizing_factor.1,freq=7),t.window = 7,s.window = 7)$time.series[,2]),
                        normalizing_factor.monthly=as.numeric(stl(robust = T,ts(normalizing_factor.1,freq=7),t.window = 30,s.window = 30)$time.series[,2]),
                        normalizing_factor.yearly=as.numeric(stl(robust = T,ts(normalizing_factor.1,freq=7),t.window = 365,s.window = 60)$time.series[,2]))
      df=merge(df,norm,by="date",all=F)
    }} # detrending the whole df except date and outcome, alos creatinf weekly, monthly and yearly normalization detrends
  
  
  
  # evaluation dates, training start, stop etc 
  start.eval.at<-NULL
  if(as.character(training_period)=="past"){train.period=30}
  if(is.numeric(training_period)==T){train.period=training_period}
  
  if(as.character(start_date_eval)=="auto"){start.eval.at<-min(df$date)+train.period}
  if(class(start_date_eval)=="Date"){start.eval.at<-start_date_eval}
  
  if(as.character(end_date_eval)=="auto"){end.eval.at<-max(df$date)-eval_period}
  if(class(end_date_eval)=="Date"){end.eval.at<-end_date_eval}
  
  eval.steps<-seq(from=start.eval.at,to=end.eval.at,by=1) # instead of by=1, by=evaluation_intervall possible ?!
  n.eval.steps<-seq(from=1,to=length(eval.steps))

  training.start.at<-start.eval.at-train.period
  
  # Big empty list for storing all evaluation results
  PREDICTED<-list()
  
  if(method=="cv"){
    # Lasso glm + cross validation
    for(i in seq(1:max(n.eval.steps))){ # evaluation_intervall?? -1??? or 0???
      tryCatch({
      if(as.character(training_period)=="past"){d=0; cv_fold=(train.period+i)/5} # for "past" the training period icreases
      if(is.numeric(training_period)==T){d=i}
      train.start.ith=training.start.at+d # for "past" the training period icreases
      train.end.ith=training.start.at+train.period+i
      
      test.start.ith=train.end.ith+1
      test.end.ith=test.start.ith+eval_period
      
      # set up training and test data set
      train.data.ith<-df[df$date>=train.start.ith & df$date<=train.end.ith,]
      train.x.ith<-data.matrix(train.data.ith[,-which(names(df)==type_of_outcome)])
      train.y.ith<-train.data.ith[,which(names(df)==type_of_outcome)]
      
      test.data.ith<-df[df$date>=test.start.ith & df$date<test.end.ith,]
      test.x.ith<-data.matrix(test.data.ith[,-which(names(df)==type_of_outcome)])
      test.y.ith<-test.data.ith[,which(names(df)==type_of_outcome)]
      
      # lasso GLM
      lasso.mod=glmnet( x=train.x.ith,y=train.y.ith ,alpha=1,lambda=grid)
      
      # runing cv, depending on validation method year, months, or n?
      dates.for.validation=seq(from=train.start.ith,to=train.end.ith,by=1)
      if(cv_fold=="Y"){
        if(length(dates.for.validation)<365*3){stop("Training period too short for cv method")}
        id=as.factor(format(dates.for.validation[dates.for.validation %in% train.x.ith[,1]],format="%Y"));nfolds=length(levels(id));
      cv.out=cv.glmnet(x=train.x.ith,y=train.y.ith, nfolds = nfolds, foldid = as.numeric(id))    }
      if(cv_fold=="M"){
        if(length(dates.for.validation)<90){stop("Training period too short for cv method")}
        id=as.factor(format(dates.for.validation[dates.for.validation %in% train.x.ith[,1]],format="%Y-%m"));nfolds=length(levels(id))
      cv.out=cv.glmnet(x=train.x.ith,y=train.y.ith, nfolds = nfolds, foldid = as.numeric(id))     }
      
      if(is.numeric(cv_fold)){split.size=length(dates.for.validation)/cv_fold;id=cut(dates.for.validation,split.size); nfolds=split.size
      cv.out=cv.glmnet(x=train.x.ith,y=train.y.ith, nfolds = nfolds)   }
      
      # setting lambda
      if(cv_lambda=="1se"){lambda=as.numeric(cv.out[10])}
      if(cv_lambda=="min"){lambda=as.numeric(cv.out[9]) }
      
      # getting coefficient names
      lasso.coef_temp=predict(lasso.mod,type="coefficients",s=lambda) 
      lasso.coef<-as.numeric(lasso.coef_temp) 
      names(lasso.coef)<-row.names(lasso.coef_temp) 
      coefs<-names(lasso.coef[lasso.coef!=0])[-1] #takes away the intercept
      
      # train and test dataframes for predciting values in lm and glm
      model.train.data.ith<-data.frame(train.y.ith,train.data.ith[,c(1,which(names(train.data.ith) %in% coefs))])
      names(model.train.data.ith)<-c("outcome","date",coefs)
      model.test.data.ith<-data.frame(test.y.ith,test.data.ith[,c(1,which(names(train.data.ith) %in% coefs))])
      names(model.test.data.ith)<-c("outcome","date",coefs)
      model.all.data.ith<-rbind(model.train.data.ith, model.test.data.ith)
      
      # the lm model
      lm.trained.ith<-lm(outcome~. ,data=model.train.data.ith[,-2])
     
      # A big list with all relevant values + plots
      PREDICTED[[i]]=list(model.stats=data.frame( # lasso glm results, not sure why they are different form lm...  
        predicted.lasso=as.numeric(predict(lasso.mod,s=lambda,newx=test.x.ith,type="response")),    
        sqr.difference.lasso=(as.numeric(predict(lasso.mod,s=lambda,newx=test.x.ith))-test.y.ith)^2,
        abs.difference.lasso=sqrt( (as.numeric(predict(lasso.mod,s=lambda,newx=test.x.ith))-test.y.ith)^2 ),
        mean.sqr.difference.lasso=mean((as.numeric(predict(lasso.mod,s=lambda,newx=test.x.ith))-test.y.ith)^2),
        mean.abs.difference.lasso=mean(sqrt( (as.numeric(predict(lasso.mod,s=lambda,newx=test.x.ith))-test.y.ith)^2)),
        # adjusted r2 possible. need to look up formula!
        mod.trained.r.sqr.lasso=cor(as.numeric(predict.glmnet(lasso.mod,s=log(lambda),newx=train.x.ith)),train.y.ith )^2,
        mod.tested.r.sqr.lasso=cor(as.numeric(predict.glmnet(lasso.mod,s=log(lambda),newx=test.x.ith)),test.y.ith )^2,
        
        # lm results
        predicted.lm=as.numeric(predict(lm.trained.ith,newdata = model.test.data.ith)),    
        sqr.difference.lm=(as.numeric(predict(lm.trained.ith,newdata = model.test.data.ith))-test.y.ith)^2,
        abs.difference.lm=sqrt( (as.numeric(predict(lm.trained.ith,newdata = model.test.data.ith))-test.y.ith)^2 ),
        mean.sqr.difference.lm=mean((as.numeric(predict(lm.trained.ith,newdata = model.test.data.ith))-test.y.ith)^2),
        mean.abs.difference.lm=mean(sqrt( (as.numeric(predict(lm.trained.ith,newdata = model.test.data.ith))-test.y.ith)^2 )),
        mod.trained.adj.r.sqr.lm=summary(lm.trained.ith)$adj.r.squared,
        # adjusted r2 for prediction is possible. need to look up formula!
        mod.tested.r.sqr.lm=cor(predict(lm.trained.ith,newdata = model.test.data.ith), model.test.data.ith$outcome)^2
      ),  
      
      lm.model=lm.trained.ith, 
      train.data=model.train.data.ith,
      test.data=model.test.data.ith,
      combined.data=model.all.data.ith) # end list PREDICTED  
      
      
      names(PREDICTED)[i]<-paste("t",format(test.start.ith,format="%Y%m%d"),sep="")     
      }, # trycatch function bracket
      error=function(e){cat("Uups, something went wrong here - ") # error is called when tryCatch catches an error
                          PREDICTED[[i]]<-paste("something went wrong")})   #end trycatch bracket
      if(status==1){ cat("status:",round((i)/length(n.eval.steps)*100,2), "% \n") }
      
    } # CROSS VALIDATION PREDCITED loop bracket
  } # ###METHOD="cv" brackets end here
  
  if(method=="simple.lm"){ 
    for(i in seq(1:max(n.eval.steps))){ # evaluation_intervall?? -1??? or 0???
      
      if(as.character(training_period)=="past"){d=0} # for "past" the training period icreases
      if(is.numeric(training_period)==T){d=i}
      train.start.ith=training.start.at+d # for "past" the training period icreases
      train.end.ith=training.start.at+train.period+i
      
      test.start.ith=train.end.ith+1
      test.end.ith=test.start.ith+eval_period
      
      # set up training and test data set
      train.data.ith<-df[df$date>=train.start.ith & df$date<=train.end.ith,]
      names(train.data.ith)[which( names(train.data.ith)==type_of_outcome)]<-"outcome"
      
      test.data.ith<-df[df$date>=test.start.ith & df$date<test.end.ith,]
      names(test.data.ith)[which( names(test.data.ith)==type_of_outcome)]<-"outcome"
      
      all.data.ith<-rbind(train.data.ith, test.data.ith)
      
      # the lm model
      lm.trained.ith<-lm(outcome~. ,data=train.data.ith)
      
      # A big list with all relevant values + plots
      PREDICTED[[i]]=list(
        model.stats=data.frame( # lm results
          predicted.lm=as.numeric(predict(lm.trained.ith,newdata = test.data.ith)),    
          sqr.difference.lm=(as.numeric(predict(lm.trained.ith,newdata = test.data.ith))-test.data.ith$outcome)^2,
          abs.difference.lm=sqrt( (as.numeric(predict(lm.trained.ith,newdata = test.data.ith))-test.data.ith$outcome)^2 ),
          mean.sqr.difference.lm=mean((as.numeric(predict(lm.trained.ith,newdata = test.data.ith))-test.data.ith$outcome)^2),
          mean.abs.difference.lm=mean(sqrt( (as.numeric(predict(lm.trained.ith,newdata = test.data.ith))-test.data.ith$outcome)^2 )),
          mod.trained.adj.r.sqr.lm=summary(lm.trained.ith)$adj.r.squared,
          # adjusted r2 for prediction is possible. need to look up formula!
          mod.tested.r.sqr.lm=cor(predict(lm.trained.ith,newdata = test.data.ith), test.data.ith$outcome)^2),  
        
        lm.model=lm.trained.ith, # end list PREDICTED 
        train.data=train.data.ith,
        test.data=test.data.ith,
        combined.data=all.data.ith
      )
      
      names(PREDICTED)[i]<-paste("t",format(test.start.ith,format="%Y%m%d"),sep="")      
      
      
      
      if(status==1){ cat("status:",round((i)/length(n.eval.steps)*100,2), "% \n") }
      
    } # lm simple PREDCITED loop bracket
    
  } # ### Mehtod="simple.lm" bracket ends ###
  
  return(PREDICTED)
} 



