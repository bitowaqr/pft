





# CORRUPTED??!?!?!










###--------------------------------------------------###
###------------pft_build_df--------------------------###
###--------------------------------------------------###

# set start-end
# scale? -> adjust start-end
# type -> std?
# type2 -> adjusted for regular?
# locate inputs 
# start-end inputs
# locate outcomes
pft_build_df(start_date=start_date,  
             end_date=end_date,
             country="germany",
             type_of_outcome="Influenza_AB_lab_incidence_std",
             outcome_df=list_of_outcomes,
             type_of_input=c("wiki_primary", "wiki_related", "wiki_linked","wiki_random"), 
             lang="de",
             input_df=list_of_inputs,
             scaled=TRUE,
             centered=TRUE,
             decomposed=TRUE,
             decompose_days=14) # adjust dates, needs +7 in the beginning and end


build_working_df<-function(start_date=start_date, end_date=end_date,country="germany",type_of_outcome="Influenza_AB_lab_incidence_std",lang="de",type_of_input=c("wiki_primary", "wiki_related"), input_df=list_of_inputs,outcome_df=list_of_outcomes){
  # if statements needed...
  df_temp<-data.frame(date=seq(as.Date(start_date),as.Date(end_date),by=1))
  
  outcome_country<-  which(names(outcome_df)==  country)
  if(length(outcome_country)!=1){stop("something wrong with outcome country")}
  outcome_cols<-names(outcome_df[[outcome_country]]$dependents) %in% c(type_of_outcome, "date")
  if(sum(outcome_cols)<2){stop("something is wrong with the outcome df, needs a date and a valid type")}
  df_temp<-merge(df_temp,outcome_df[[outcome_country]]$dependents[,outcome_cols],by="date",all=F)
  
  input_lang<-which(names(list_of_inputs)== lang)
  input_lists<-which( names(input_df[[input_lang]]) %in% (type_of_input))
  
  
  for(i in 1:length(input_lists)){
    df_temp<-merge(df_temp,input_df[[input_lang]][[input_lists[i]]],by="date",all=F)
  }
  return(df_temp)
  cat("We have",sum(is.na(df_temp))," missing values in the dataframe \n 
      min date, max date, type in, type out")
}









wft_eliminate_zeros_in_loi<-function(lang,type_of_input,status=1){
  select_list<-which(names(list_of_inputs)==lang)
  select_type<-which(names(list_of_inputs[[select_list]])==type_of_input)
  df_temp<-list_of_inputs[[select_list]][[select_type]]
  for(i in 1:(length(df_temp)-1)+1){
    if(status==1){
      cat("status=", i,"/",(length(df_temp)-1)+1,"\n")
    }
    zeros<-NULL
    zeros<-which( df_temp[,i] ==0)
    for(j in 1:length(zeros)){
      z<-zeros[j]
      if(z>2 & z< length(df_temp[,i])-2){
        df_temp[z,i]<-round(mean(c(df_temp[z+1,i],df_temp[z+2,i], df_temp[z-1,i],df_temp[z-2,i]),trim=1,na.rm=TRUE),0)
      }
    }
  }
  list_of_inputs[[select_list]][[select_type]]<-df_temp
  return(list_of_inputs)
}
wft_eliminate_zeros_in_df<-function(lang,df){
  df_temp<-df
  for(i in 1:(length(df_temp)-1)+1){
    zeros<-NULL
    zeros<-which( df_temp[,i] ==0)
    for(j in 1:length(zeros)){
      z<-zeros[j]
      if(z>2 & z< length(df_temp[,i])-2){
        df_temp[z,i]<-round(mean(c(df_temp[z+1,i],df_temp[z+2,i], df_temp[z-1,i],df_temp[z-2,i]),trim=1,,na.rm=TRUE),0)
      }
    }
  }
  return(df_temp)
}


