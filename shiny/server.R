#server
require(xtable)
require(shiny)

available_inputs=NULL; available_outcomes=NULL
for(i in 1:length(list_of_inputs)){
  available_inputs[i]=length(list_of_inputs[[i]][[1]][[1]])==4
}
for(i in 1:length(list_of_outcomes)){
  available_outcomes[i]=sum(!is.na(list_of_outcomes[[i]]$dependents[,-1]))>0
}
list_of_inputs<-list_of_inputs[available_inputs]
list_of_outcomes<-list_of_outcomes[available_outcomes]

language_table<-language_table[language_table$ISO_639_1 %in% names(list_of_inputs),]
country_list<-country_list[country_list$description %in% names(list_of_outcomes),]


shinyServer(function(input, output,session) { 
  # 1. Setting up the data that will be used for this
  
 
  #if(!exists("example1a")) {
  #example1a<-NULL; example1b<-NULL
  #example2a<-NULL; example2b<-NULL
  #example3a<-NULL; example3b<-NULL
  #e1<-NULL; e2<-NULL;list1<-NULL; list2<-NULL}


  get_country <- reactive({
    country.selection<-input$pft_model.country # what country was selected?
  })    # Update type_of_outcome selector
  lang<-reactive({
    lang<-as.character(language_table$ISO_639_1[which(language_table$language ==input$pft_model.lang)])
  })             # which language selected?
  get_outcome <- reactive({
    outcome.selection<-input$pft_model.type_of_outcome # what country was selected?
  })    # which type_of_outcome selected?
  
  observe({   # Update type_of_outcome selector

    selected_country=which(names(list_of_outcomes)==get_country()) # pass get_country to the update function
    available_outcome=which(!as.logical(lapply(list_of_outcomes[[selected_country]]$dependents,is.logical)))[-1]
    limit.country.choices=names(list_of_outcomes[[selected_country]]$dependents)[c(available_outcome)]
    updateSelectInput(session, "pft_model.type_of_outcome", choices = limit.country.choices)
  })  # Update type_of_outcome selector
  observe({   # Update type_of_input selector

    selected_lang=which(names(list_of_inputs)==lang()) # pass get_country to the update function
    available_inputs=which(lapply(list_of_inputs[[selected_lang]],is.data.frame)!=1)
    available_inputs=available_inputs[!names(available_inputs) %in% "wiki_lookup_searches"]
    limit.lang.choices=names(list_of_inputs[[selected_lang]])[c(available_inputs)]
    updateSelectInput(session, "pft_model.type_of_input", choices = limit.lang.choices,  selected = limit.lang.choices[1])
  })  # Update type_of_input selector
  observe({    # Update type_of_input selector
    selected_country=which(names(list_of_outcomes)==get_country()) # pass get_country to the update function
    selected_lang=which(names(list_of_inputs)==lang()) # pass get_country to the update function
    available_inputs=which(lapply(list_of_inputs[[selected_lang]][-6],is.data.frame)!=1)
    selected.outcome=which(names(list_of_outcomes[[selected_country]]$dependents)==get_outcome())
    available.date.outcome=list_of_outcomes[[selected_country]]$dependents$date[!is.na(list_of_outcomes[[selected_country]]$dependents[[selected.outcome]])]
    available.date.inputs=list_of_inputs[[selected_lang]][[1]][[1]]$date[!is.na(list_of_inputs[[selected_lang]][[1]][[1]][2])]
    min.date=max(min(available.date.inputs),min(available.date.outcome))
    max.date=min(max(available.date.inputs),max(available.date.outcome))
  
    updateDateRangeInput(session, "pft_model.start.end.dates", 
                         min=min.date,max=max.date,
                         start=min.date,end=max.date)
  })  # Update type_of_input selector end
  observe({ # Update zoom + pickdate start
    x.1<-which(names(list1$e1$plots) %in% input$plot.selection)
    p.1<-list1$e1$plots[[x.1]]
    min<-min(p.1$data$date); max<-max(p.1$data$date); val=c(min,max)
    # # Control the value, min, max, and step.
    ## Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "zoom", value = val,
                      min = min, max = max)
    updateSliderInput(session, "pickdate", value = val,
                      min = min, max = max-28)
  }) # update zoom + pickdate end
  observe({     # update zoom 2
    x.2<-which(names(list2$e2$plots) %in% input$plot.selection2)
    p.2<-list2$e2$plots[[x.2]]
    min<-min(p.2$data$date); max<-max(p.2$data$date); val=c(min,max)
    # # Control the value, min, max, and step.
    ## Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "zoom2", value = val,
                      min = min, max = max)
    updateSliderInput(session, "pickdate2", value = val,
                      min = min, max = max-28)
  })     # update zoom 2 end
  
  
 
  
  ### MODEL 1
  observeEvent(input$do, { 
    model1 <- reactive({ 
      input$refresh
      m1<-pft_model(
        lang=lang(),
        start_date=input$pft_model.start.end.dates[1], # auto will get min date
        end_date=input$pft_model.start.end.dates[2],   # auto will get max date
        type_of_outcome=input$pft_model.type_of_outcome,
        country = input$pft_model.country,
        type_of_input = input$pft_model.type_of_input, # + wiki_relared? + wiki_random?!
        method=input$pft_model.method, # "cv" or "simple.lm"
        cv_fold=ifelse(length(grep("M", input$pft_model.cv_fold))==1,3,ifelse(length(grep("Y", input$pft_model.cv_fold))==1,cv_fold,as.numeric(input$pft_model.cv_fold))), # "M", "Y", or a number indicating the number of combined days to be sampled to sampled? size 
        cv_lambda="1se", #  "min" or "1se"... a number is possible but nonsense!
        grid=10^seq(10,-2,length=100), # a grid for cv, default!
        start_date_eval="auto",  # can also be a Date, must be compatible with training period !!!
        end_date_eval="auto",   # can also be a Date, "auto" gets the maximum, must be compatible with...?!
        training_period=ifelse(input$pft_model.training_period=="past",input$pft_model.training_period,as.numeric(input$pft_model.training_period)), # training period in days or "past", must be compatible with min and max dates!!!
        eval_period=28, # this is nor related to cv, is it?
        detrending=as.numeric(input$pft_model.detrending), # Seasonal Decomposition of Time Series by Loess
        detrend_window=ifelse(input$pft_model.detrend_window %in% seq(from=7,to=7*10000,by=7),input$pft_model.detrend_window,NULL),
        detrend_robust=input$pft_model.detrend_robust, # or F
        time_lag= input$pft_model.time_lag, # how many days is official data behind? -7 for 1 week behind page view data
        wiki_normalization=0,
        status=1)
      
      
      return(m1)
    }) 
    m1<-reactiveValues()
    m1<-model1()
    
    e1<-reactiveValues()
    e1<-eval_pft_model(m1,method = "plot_mean_performance")
    
    info1<-reactiveValues()
    get.info<-reactive({ data.frame(country=input$pft_model.country,
                                       outcome=input$pft_model.type_of_outcome,
                                       lang=input$pft_model.lang,
                                       inputs=input$pft_model.type_of_input,
                                       method=input$pft_model.method,
                                       training_period=input$pft_model.training_period,
                                       time_lag=input$pft_model.time_lag
                                       )}) 
   info1<-get.info()
   
    list1<-reactiveValues()
    get.list<-reactive({list(e1=e1,m1=m1,info1=info1)})
    list1<<-get.list()
    
  }) # end do button 1

    output$e1_plot <- renderPlot({ # PLOT EVAL 1
      x<-which(names(list1$e1$plots) %in% input$plot.selection)
      # dont show the maximum outliers
      ymax<- ifelse((max(list1$e1$plots[[x]]$data$outcome)+100)>layer_scales(list1$e1$plots[[x]] )$y$range$range[[2]],
                    layer_scales(list1$e1$plots[[x]] )$y$range$range[[2]],
                    max(list1$e1$plots[[x]]$data$outcome)+100)
      ymin<-ifelse((min(list1$e1$plots[[x]]$data$outcome)-100)<layer_scales(list1$e1$plots[[x]] )$y$range$range[[1]],
                   layer_scales(list1$e1$plots[[x]] )$y$range$range[[1]],
                   min(list1$e1$plots[[x]]$data$outcome)-100)

      p<-list1$e1$plots[[x]] +
        ylim(ymin,ymax) +
        xlim(as.Date(input$zoom[1]),as.Date(input$zoom[2])) +
        ggtitle(paste(" COUNTRY=",list1$info1$country,
                      " OUTCOME=",list1$info1$outcome,
                      "\n LANGUAGE=",list1$info1$lang,
                      " INPUT=",list1$info1$inputs,
                      "\n METHOD=",list1$info1$method,
                      " TRAIN T=",list1$info1$training_period,
                      " LAG=",list1$info1$time_lag, sep="")) 
      p})# plot eval 1
    output$pick <- renderPlot({
      eval_pft_model(list1$m1,method = "plot1",eval_date=format(input$pickdate,format="%Y%m%d")) }) # PLOT PICK 1
    observeEvent(input$nowcast, {  
    output$nowcast.plot<-renderPlot({
      
      eval_pft_model(list1$m1,method = "nowcast") }) # PLOT PICK 2
    }) # PLOT NOWCAST
    observeEvent(input$forecast, {  
      output$forecast.plot<-renderPlot({
        eval_pft_model(list1$m1,method = "prophet",days_forecast = as.numeric(input$days.forecast), forecast_from = NULL) }) # PLOT forecast
    }) # PLOT forecast 
    output$summary.stats1 <- renderTable({
      Listener2 <- input$do
      isolate(backgroundchange())      
      xtable( data.frame( days_without_data=c(1:28),
                          mean.errors=round(list1$e1$mean.error.per.day,3),
                          sqr.errors=round(list1$e1$mean.sqr.error.per.day,3)))}) #summary stats text1
    
#####    #####   #####    #####   #####    #####   #####    #####   #####    #####   
    
  observeEvent(input$do2, { 
      model2 <- reactive({ 
        input$refresh
        m2<-pft_model(
          lang=lang(),
          start_date=input$pft_model.start.end.dates[1], # auto will get min date
          end_date=input$pft_model.start.end.dates[2],   # auto will get max date
          type_of_outcome=input$pft_model.type_of_outcome,
          country = input$pft_model.country,
          type_of_input = input$pft_model.type_of_input, # + wiki_relared? + wiki_random?!
          method=input$pft_model.method, # "cv" or "simple.lm"
          cv_fold=ifelse(length(grep("M", input$pft_model.cv_fold))==1,3,ifelse(length(grep("Y", input$pft_model.cv_fold))==1,cv_fold,as.numeric(input$pft_model.cv_fold))), # "M", "Y", or a number indicating the number of combined days to be sampled to sampled? size 
          cv_lambda="1se", #  "min" or "1se"... a number is possible but nonsense!
          grid=10^seq(10,-2,length=100), # a grid for cv, default!
          start_date_eval="auto",  # can also be a Date, must be compatible with training period !!!
          end_date_eval="auto",   # can also be a Date, "auto" gets the maximum, must be compatible with...?!
          training_period=ifelse(input$pft_model.training_period=="past",input$pft_model.training_period,as.numeric(input$pft_model.training_period)), # training period in days or "past", must be compatible with min and max dates!!!
          eval_period=28, # this is nor related to cv, is it?
          detrending=as.numeric(input$pft_model.detrending), # Seasonal Decomposition of Time Series by Loess
          detrend_window=ifelse(input$pft_model.detrend_window %in% seq(from=7,to=7*10000,by=7),input$pft_model.detrend_window,NULL),
          detrend_robust=input$pft_model.detrend_robust, # or F
          time_lag= input$pft_model.time_lag, # how many days is official data behind? -7 for 1 week behind page view data
          wiki_normalization=0,
          status=1)
        
        return(m2)
      }) 
      m2<-reactiveValues()
      m2<-model2()
      
      e2<-reactiveValues()
      e2<-eval_pft_model(m2,method = "plot_mean_performance")
      
      info2<-reactiveValues()
      get.info<-reactive({ data.frame(country=input$pft_model.country,
                                      outcome=input$pft_model.type_of_outcome,
                                      lang=input$pft_model.lang,
                                      inputs=input$pft_model.type_of_input,
                                      method=input$pft_model.method,
                                      training_period=input$pft_model.training_period,
                                      time_lag=input$pft_model.time_lag
      )}) 
      info2<-get.info()
      
      list2<-reactiveValues()
      get.list<-reactive({list(e2=e2,m2=m2,info2=info2)})
      list2<<-get.list()
    }) # end do button 2

    output$e2_plot <- renderPlot({ # PLOT EVAL 2
      x<-which(names(list2$e2$plots) %in% input$plot.selection2)
      ymax2<- ifelse((max(list2$e2$plots[[x]]$data$outcome)+100)>layer_scales(list2$e2$plots[[x]] )$y$range$range[[2]],
                    layer_scales(list2$e2$plots[[x]] )$y$range$range[[2]],
                    max(list2$e2$plots[[x]]$data$outcome)+100)
      ymin2<-ifelse((min(list2$e2$plots[[x]]$data$outcome)-100)<layer_scales(list2$e2$plots[[x]] )$y$range$range[[1]],
                   layer_scales(list2$e2$plots[[x]] )$y$range$range[[1]],
                   min(list2$e2$plots[[x]]$data$outcome)-100)
    
      p<-list2$e2$plots[[x]] +
        ylim(ymin2,ymax2) +
        xlim(as.Date(input$zoom2[1]),as.Date(input$zoom2[2]))+
        ggtitle(paste(" COUNTRY=",list2$info2$country,
                      " OUTCOME=",list2$info2$outcome,
                      "\n LANGUAGE=",list2$info2$lang,
                      " INPUT=",list2$info2$inputs,
                      "\n METHOD=",list2$info2$method,
                      " TRAIN T=",list2$info2$training_period,
                      " LAG=",list2$info2$time_lag, sep="")) 
      p}) # plot eval 2
    output$pick2 <- renderPlot({
      eval_pft_model(list2$m2,method = "plot1",eval_date=format(input$pickdate2,format="%Y%m%d")) }) # PLOT PICK 2
    observeEvent(input$nowcast2, { 
    output$nowcast.plot2<-renderPlot({
      eval_pft_model(list2$m2,method = "nowcast" )}) # PLOT PICK 2
    }) # Plot Nowcast 2
    observeEvent(input$forecast2, {  
      input$refresh
      output$forecast.plot2<-renderPlot({
       
        eval_pft_model(list2$m2,method = "prophet",days_forecast = as.numeric(input$days.forecast2), forecast_from = NULL) }) # PLOT forecast
    }) # PLOT forecast 
    
    output$summary.stats2 <- renderTable({
      Listener2 <- input$do2
      isolate(backgroundchange())      
      xtable( data.frame( days_without_data=c(1:28),
                          mean.errors=round(list2$e2$mean.error.per.day,3),
                          sqr.errors=round(list2$e2$mean.sqr.error.per.day,3)))}) #summary stats text2

    backgroundchange <- reactive({
      invalidateLater(1000, session)
      
      runif(1)
    }) # a refresh button
    
    
    
#    observeEvent(input$example.1, {
 #     list1<<-example1a
  #    list2<<-example1b})
#    observeEvent(input$example.2, {
 #     list1<<-example2a
  #    list2<<-example2b})
#    observeEvent(input$example.3, {
 #     list1<<-example3a
  #    list2<<-example3b})
     
    
    

    
    
    
    
    
    
    
}) # shiny server close



