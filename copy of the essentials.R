# essentials copy

# load all functions

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
wft_pc<-function(page="Influenza", lang="de",start_date=Sys.time()-24*60*60, end_date=Sys.time(),  timezone="GMT",status=0){
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




wft_weeks_to_days<-function(week_dates,weekly_counts){
  if(class(week_dates)=="Date"){ # daily dates
    temp_days<-NULL
    for(i in 1:length(week_dates)){
      x<-rep(week_dates[i],times=7)
      x<-x-6;y<-0:6;z<-x+y # x-6 OR x-7 ???
      temp_days<-as.Date(c( temp_days,z),origin="1970-01-01")
    }}
  else{ print("weekly data must be class Date")}
  # daily count
  daily_count_temp<-rep(weekly_counts[1],7)
  for(i in 1:(length(weekly_counts)-1)) {
    y<-seq(weekly_counts[i],weekly_counts[i+1],length.out = 7)/7
    daily_count_temp<-c(daily_count_temp,y)
  }
  df<-data.frame(date=as.Date(temp_days),count=daily_count_temp)
  return(df)
} 
store_outcomes<-function(country=NA,code=code,type_of_outcome,outcome_data,dates){
  
  # no weekly data allowed?!? if weekly, than transform?
  # I should make input checks on double dates and stuff?
  is_it_daily<-seq(from=min(dates),to=max(dates),by=1)
  is_it_weekly<-seq(from=min(dates),to=max(dates),by=7)
  # if(length(is_it_daily)!=length(dates)){stop("something wrong with the dates, must be daily") }
  if(length(is_it_weekly)==length(dates)){stop("data must be daily counts, try use the transform function") }
  if(min(dates)<min(list_of_outcomes$andorra$dependents$date)){ 
    outcome_data<-outcome_data[dates>=as.Date("2010-01-01")]
    dates<-dates[dates>=as.Date("2010-01-01")]
  }
  
  if(class(dates)!="Date"){stop("dates must be class Date")}
  if(!is.na(country)&!is.na(code)){ stop("error: give me only a code or a country, not both")}
  if(!is.na(code)){country<-country_list$description[which(country_list$country_code_lowcaps==code)] }
  
  list_item<-which(country_list$description==country)
  column<-which(names(list_of_outcomes$andorra$dependents)==type_of_outcome)
  rows<-match(dates,list_of_outcomes$andorra$dependents$date)
  list_of_outcomes[[list_item]][[2]][rows,column]<-c(outcome_data)
  
  length( list_of_outcomes[[list_item]][[2]][rows,column])
  sum(is.na(list_of_outcomes[[list_item]][[2]][rows,column]))
  return(list_of_outcomes)
}
first_store_input<-function(lang=lang,type,input_list,columnnames=names(input_list)){
  df<-input_list
  if(class(input_list)!="list"){stop("Input must be a list with df1(date=... ,input1=...), df2(date...")}
  if(is.na(min(df[[1]]$date))) {stop("df[[1]]$date must be a date - list(df1=data.frame(date=..., input1=...),df2(date=...")}
  
  list_to_df<-reformat_wiki(df=input_list,
                            #source_list=names(input_list),
                            start_date=   start_date<-min(input_list[[1]]$date),
                            end_date=max(input_list[[1]]$date))
  
  list_item<-which(names(list_of_inputs)==lang)
  type<-which(names(list_of_inputs[[1]])== type)
  
  list_of_inputs[[list_item]][[type]]<-merge(  list_of_inputs[[list_item]][[type]],list_to_df,by="date",all=TRUE)
  return(list_of_inputs)
}
update_input_wiki<-function(type=c("wiki_primary","wiki_linked","wiki_related","gtrend_primary"),lang=lang){
  list_item<-which(names(list_of_inputs)==lang)
  type_items<-which(names(list_of_inputs$aa)  %in% type)
  
  for(i in 1:length(type_items)){
    
    # update the dates to be uo to sys.date
    if(max(list_of_inputs[[list_item]][[type_items[i]]]$date)!=Sys.Date() ){
      max_date<-max(list_of_inputs[[list_item]][[type_items[i]]]$date)
      update_dates<-seq(from=max_date,to=Sys.Date(),by=1)
      vars<-names(list_of_inputs[[list_item]][[type_items[i]]])[-1]
      m <- matrix(NA, ncol = length(vars)+1, nrow = length(update_dates))
      m<-data.frame(m)
      names(m)<-c("date",vars)
      m[,1]<-update_dates
      list_of_inputs[[list_item]][[type_items[i]]]<-rbind(list_of_inputs[[list_item]][[type_items[i]]],m)
    }
    
    # see where NAs are
    rows<-which(is.na(list_of_inputs[[list_item]][[type_items[i]]][,2]))
    dates_with_missing_data<-list_of_inputs[[list_item]][[type_items[i]]]$date[c(rows-1)]
    pages<-names(list_of_inputs[[list_item]][[type_items[i]]])[-1]
    lang<-lang
    start_date<-min(dates_with_missing_data)
    end_date<-Sys.Date()
    
    look_up_missing_wiki_data<-wft_pc(page=pages,lang =lang,start_date=start_date,end_date=end_date,status=1)
    names(look_up_missing_wiki_data)<-pages
    
    
    list_to_df<-reformat_wiki(df=look_up_missing_wiki_data,
                              #source_list=names(look_up_missing_wiki_data),
                              start_date= as.Date(min(look_up_missing_wiki_data[[1]]$date)),
                              end_date=as.Date(max(look_up_missing_wiki_data[[1]]$date+2)))
    
    list_item<-which(names(list_of_inputs)==lang)
    type<-which(names(list_of_inputs[[1]])== type)
    list_of_inputs[[list_item]][[i]][rows,]<-list_to_df
    
    return(list_of_inputs)
  }
  
  
  
  
  list_of_inputs<-store_input(lang=lang,type = type[i], input_list = look_up_missing_wiki_data)
  
}
auto_update_wiki_primary_part1<-function(lang_code=lang){
  xxx<-as.character(wft_lang_titles(page="Influenza",origin_lang = "en",filter_lang= lang_code)[2,3])
  list_item<-which(names(list_of_inputs)==lang_code)
  
  temp<-wft_pc(page=xxx,lang =lang_code ,start_date=as.Date("2010-01-01"),end_date=as.Date("2010-01-31"),status=1)
  list_of_inputs<-first_store_input(lang=lang_code ,type = "wiki_primary", input_list = temp)
  
  return(list_of_inputs)
  
}
auto_update_wiki_primary_part2<-function(lang_code=lang){
  list_of_inputs<-update_input_wiki(type="wiki_primary",lang=lang_code)
}
wft_update_any_type<-function(lang, type_of_input="wiki_linked"){ 
  select_list<-which(names(list_of_inputs)==lang)
  select_type<-which(names(list_of_inputs[[select_list]])==type_of_input)
  df_temp<-list_of_inputs[[select_list]][[select_type]]
  if(sum(is.na(list_of_inputs[[select_list]][[select_type]][,2]))>0){ #[,2] is used as reference for all, probably ok?!
    start_update_from<-min(list_of_inputs[[select_list]][[select_type]][which(is.na(list_of_inputs[[select_list]][[select_type]][,2])),1] )}
  if(sum(is.na(list_of_inputs[[select_list]][[select_type]][,2]))==0){
    start_update_from<-max(list_of_inputs[[select_list]][[select_type]]$date)+1}
  update_end=Sys.Date()
  pages<-names(list_of_inputs[[select_list]][[select_type]])[-1]
  temp_list<-wft_pc(page=pages,lang=lang,start_date = start_update_from, end_date = update_end, status=1)
  #names(temp_list)<-c(pages)
  temp_df<-reformat_wiki(df=temp_list)
  list_of_inputs[[select_list]][[select_type]]<-rbind(list_of_inputs[[select_list]][[select_type]],temp_df)
  return(list_of_inputs)
} 






