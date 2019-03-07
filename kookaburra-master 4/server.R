server <- function(input,output,session){
  # BADGES TABLES #
  output$badges_table_1 <- DT::renderDataTable({prepare_awards(awards_data,c('Weekly','2-Week Streak','4-Week Streak'),
                                                               c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),
                                                               c(3,1,2),dvals$data,
                                                               F)}) #'date',input$date,
  output$badges_table_2 <- DT::renderDataTable({prepare_awards(awards_data,c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),c(3,2,5,1,6,8,7,4,9),dvals$data,F)})
  output$badges_table_3 <- DT::renderDataTable({prepare_awards(awards_data,c('50 Minutes Milestone','100 Minutes Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone'),
                                                               c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),
                                                               c(5,1,2,3,4),dvals$data,F)})
  output$badges_table_4 <- DT::renderDataTable({prepare_badge_count(awards_data,c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  output$subtot_1 <- DT::renderDataTable({prepare_awards(awards_data,c('Weekly','2-Week Streak','4-Week Streak'),
                                                         c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),
                                                        c(1),dvals$data,T)})
  output$subtot_2 <- DT::renderDataTable({prepare_awards(awards_data,c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),c(1),dvals$data,T)})
  output$subtot_3 <- DT::renderDataTable({prepare_awards(awards_data,c('50 Minutes Milestone','100 Minutes Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone'),
                                                         c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),
                                                         c(1),dvals$data,T)})
  # BADGES TEXT #
  output$text_badges_1 <- renderText(format(sum(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$count),big.mark=","))
  output$text_badges_2 <- renderText(format(len(unique(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$customer_id)),big.mark=","))
  output$text_badges_3 <- renderText(round(sum(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$count)/len(unique(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$customer_id)),2))
  output$text_badges_4 <- renderText(len(setdiff(unique(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$customer_id),unique(awards_data[awards_data$date >= dvals$data[1]-(dvals$data[2]-dvals$data[1]) && awards_data$date <= dvals$data[2]-(dvals$data[2]-dvals$data[1]),]$customer_id))))
  output$text_badges_5 <- renderText((sum(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$count)-sum(awards_data[awards_data$date >= dvals$data[1]-30 && awards_data$date <= dvals$data[2]-30,]$count))/sum(awards_data[awards_data$date >= dvals$data[1]-30 && awards_data$date <= dvals$data[2]-30,]$count))
  output$text_badges_6 <- renderText((sum(awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]$count)-sum(awards_data[awards_data$date >= dvals$data[1]-90 && awards_data$date <= dvals$data[2]-90,]$count))/sum(awards_data[awards_data$date >= dvals$data[1]-90 && awards_data$date <= dvals$data[2]-90,]$count))
  
  output$badgeText <- renderUI({
    diff <- 1+dvals$data[2]-dvals$data[1]
    f_list <- c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level)
    a1 <- awards_data[awards_data$date >= dvals$data[1] && awards_data$date <= dvals$data[2],]
    a2 <- awards_data[awards_data$date >= dvals$data[1]-diff && awards_data$date <= dvals$data[2]-diff,]
    a3 <- awards_data[awards_data$date >= dvals$data[1]-7 && awards_data$date <= dvals$data[2]-7,]
    a4 <- awards_data[awards_data$date >= dvals$data[1]-30 && awards_data$date <= dvals$data[2]-30,]
    a1 <- applyFilters(a1,f_list)
    a2 <- applyFilters(a2,f_list)
    a3 <- applyFilters(a1,f_list)
    a4 <- applyFilters(a2,f_list)
    a2 <- len(unique(a2$customer_id))
    count <- sum(a1$count)
    count7 <- sum(a3$count)
    count30 <- sum(a4$count)
    diff7 <- ifelse(count7 < 1,'N/A',paste0(format(round((count-count7)/count7,2),nsmall=2),'%'))
    diff30 <- ifelse(count30 < 1,'N/A',paste0(format(round((count-count30)/count30,2),nsmall=2),'%'))
    cid <- len(unique(a1$customer_id))
    fluidRow(class='top_metrics',tags$div(class='metric_box',format(count,big.mark=',')),
    tags$div(class='metric_box',format(cid,big.mark=',')),
    tags$div(class='metric_box',format(round(count/cid,2),nsmall=2)),
    tags$div(class='metric_box',format(setdiff(len(unique(a1$customer_id)),a2),big.mark=',')),
    tags$div(class='metric_box',diff7),
    tags$div(class='metric_box metric_last',diff30))
  })
  #REAWRDS TABLES#
  output$rewards_table_1<-DT::renderDataTable({prepare_rewards_new(rewards_data,c('Weekly Challenge Completed','2-Week Streak','4-Week Streak'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter),dvals$data)})
  output$rewards_table_2<-DT::renderDataTable({prepare_rewards_new(rewards_data,c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter),dvals$data)})
  
  #output$rewards_table_2 <- DT::renderDataTable({prepare_awards(rewards_data,c('Onboarding Complete','Health Data Synced','Push Notifications Enabled','Answered Questions','Read 5 Articles','Watched 5 Videos','Share on Social','Opened App','You Took 10!'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  #output$rewards_table_3 <- DT::renderDataTable({prepare_awards(rewards_data,c('50 Minutes Milestone','100 Minutes Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone','300 Minutes Milestone','350 Minute Milestone','400 Minute Milestone','450 Minute Milestone','500 Minute Milestone','550 Minutes Milestone','600 Minute Milestone','650 Minute Milestone','700 Minute Milestone','750 Minute Milestone','800 Minute Milestone','850 Minute Milestone','900 Minute Milestone','950 Minute Milestone'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  #output$rewards_table_4 <- DT::renderDataTable({prepare_badge_count(rewards_data,c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  
  # ACTIVITY TABLES #
  output$activity_table_1 <- DT::renderDataTable({prepare_activity(awards_data,NULL,dvals$data)})
  output$activity_table_2 <- DT::renderDataTable({prepare_awards(awards_data,c('50 Minutes Milestone','100 Minutes Milestone','150 Minute Milestone','200 Minute Milestone','250 Minute Milestone'),c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  
  # APP TABLES #
  #output$app_table_1 <- DT::renderDataTable({prepare_app(app_data,NULL,dvals$data)})

  # CHARTS #
  output$badges_chart_1 <- renderHighchart({prepare_chart(awards_data,c('gender',input$gender,'age',input$age,'hour',input$hour,'month',input$month,'quarter',input$quarter,'Province',input$province,'answerValue',input$level,'max_award_code',input$max_level),dvals$data)})
  #output$app_chart_1 <- renderHighchart({prepare_chart(app_data)})
  
  #metrics1 <- c('Total','Unique Users','New Users','Earned','Earned To Date','Remaining','% Remaining')
  #metrics2 <- c('Earned','Earned To Date')
  #metrics3 <- c('Rewards Won','Total Ballots','Answer Questions','App Open','Level A','Level B','Level C','Level D','Onboarding Completed','Share Social','Take Ten','View Articles','View Videos','Weekly 1','Weekly 2','Weekly 4')

  # TABLES FROM OLD SCOPE #
  #output$summary_table  <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description'),c('Count'),dvals$data,input$cbx_cartesian),dvals$data,c(input$split1,input$split2,input$split3),metrics1,NULL,10000,input$format,input$compare)})
  #output$summary_table2 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description'),c('Count'),dvals$data,input$cbx_cartesian),dvals$data,c(input$split1,input$split2,input$split3),metrics1,NULL,10000,input$format,input$compare)})
  #output$summary_table3 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description','Customer ID'),c('Count'),dvals$data,F),dvals$data,c(input$split4,input$split5,input$split6,input$split7),metrics2,NULL,10000,F,input$compare)})
  #output$summary_table4 <- DT::renderDataTable({makeSummaryTableSorting(prepareData(c('Date','Name','Description','Customer ID'),c('Count'),dvals$data,F),dvals$data,c(input$split4,input$split5,input$split6,input$split7),metrics2,NULL,10000,F,input$compare)})
  #output$summary_table5 <- DT::renderDataTable({makeSummaryTableSorting(prepare_award_data(c('Award Code','Customer ID','Date'),c('Ballots'),c(as.Date('2018-11-01'),as.Date('2019-02-01')),T,input$cbx_date),dvals$data,award_cols$data,metrics3,NULL,10000)})
  #output$summary_table6 <- DT::renderDataTable({makeSummaryTableSorting(prepare_award_data(c('Award Code','Customer ID','Date'),c('Ballots'),c(as.Date('2018-11-01'),as.Date('2019-02-01')),T,input$cbx_date),dvals$data,award_cols$data,metrics3,NULL,10000)})
  
  dvals <- reactiveValues(data = NULL)
  
  observeEvent(input$period,{
    if(input$period!='Custom'){dvals$data <- getDateRange(input$period,input$compare)}
    else{dvals$data <- c(input$dateRange,input$dateRange2)}
    session$sendCustomMessage('updatePeriod',input$period)
  })
  observeEvent(input$dateRange,{
    dvals$data <- c(input$dateRange)
  })
  output$dateDisplay <- renderUI({  
    dateRangeInput('dateRange', label='Date Range:',start=getDateRange('90')[1],end=getDateRange('90')[2],format='M dd, yyyy')
  })
  observe({
    updateDateRangeInput(session,'dateRange',start = dvals$data[1],end = dvals$data[2])
  })
  
  #client_time<-reactive(as.numeric(input$client_time)/1000)
  #output$timeDisplay<-renderUI({selectInput('time','Time',c('None',input$client_time),selected='None')})
  #observeEvent(input$dates_qtr,{
  #  dvals$data <- getDateRange('q',input$compare)
  #})
}
