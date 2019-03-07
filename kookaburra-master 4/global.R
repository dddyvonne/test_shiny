#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LOAD PACKAGES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c('shiny','shinydashboard','shinythemes','shinyjs','highcharter','data.table','reshape2','ggplot2','stringr','plotly','tools','tidyr','scales','anytime',
              'readr','plyr','rowr','DT','V8','tibble','gsubfn','lubridate','shinyWidgets','leaflet','openintro','stringr','RMariaDB','odbc','dbplyr','sp','bigQueryR','dplyr')
lapply(packages,library,character.only=TRUE)

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GTM
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jsCode <- paste0("shinyjs.init = function(){(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                 j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src='https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);})(window,document,'script','dataLayer','GTM-W6CQG8K');}")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  SQL DATABASE CONNECTION & BIGQUERY AUTH
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
con <- dbConnect(
  drv = RMariaDB::MariaDB(), 
  username = 'dev',
  password = 'dev', 
  host = '127.0.0.1',
  dbname = 'paction_db_qa',
  port = 3306
)
#bqr_auth()

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  HELPER FUNCTIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titlec <- function(x){
  s <- strsplit(x, ' ')[[1]]
  paste(toupper(substring(s,1,1)),tolower(substring(s, 2)),sep='',collapse=' ')
}

default_rows <- function(x){
  if(is.null(x)){x <- 15}
  x
}

calcDiff <- function(x,y){
  if(is.infinite(x)||is.infinite(y)||is.nan(x)||is.nan(y)||is.na(x)||is.na(y)||y==0){y=0}
  ifelse(y==0,na_val,paste0(format(round(100*(x-y)/y,2),nsmall=2),"%"))  #paste0(round(100*(x-y)/y,2),"%")) #percent_format()((x-y)/y))
}

getDateRange <- function(r,p='None'){
  if(grepl('^(m|q|y)',r)){
    m <- ifelse(grepl('^m',r),month(Sys.Date()),ifelse(grepl('^q',r),(3*quarter(Sys.Date()))-2,'01'))
    o <- ifelse(grepl('^m',r),1,ifelse(grepl('^q',r),3,12))
    date1 <- as.Date(paste(year(Sys.Date()),m,'01',sep='-')) %m-% months(ifelse(grepl('p$',r),o,0))
    date2 <- date1 %m+% months(o)-1
    date3 <- date1 %m-% months(ifelse(grepl('^(N|P)',p),o,12))
    date4 <- date3 %m+% months(o)-1
  }
  else if(grepl('^w',r)){
    date1 <- as.Date(Sys.Date()+1-as.POSIXlt(Sys.Date())$wday-ifelse(grepl('p$',r),7,0))
    date2 <- date1+6
    date3 <- (date1-ifelse(grepl('^(N|P)',p),7,0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+6
  }
  else if(grepl('[0-9]{1}',r)){
    date1 <- Sys.Date() - as.numeric(r)
    date2 <- Sys.Date()-1
    date3 <- (date1 - ifelse(grepl('^(N|P)',p),as.numeric(r),0)) %m-% years(ifelse(grepl('^(N|P)',p),0,1))
    date4 <- date3+as.numeric(r)-1
  }
  c(date1,date2,date3,date4)
}

roundval <- function(x,n){
  print(x)
  if(x==na_val||is.na(x)||is.infinite(x)||is.nan(x)){x=0}
  round(x,n)
}

addSortColumns <- function(s){
  cols <- c()
  cols_new <- c()
  for (i in 1:ncol(s)){
    eval(parse(text=paste0('s$`',names(s)[i],'_rank` <- reFormat(s$`',names(s)[i],'`)')))
    cols <- c(cols,names(s)[i],paste0(names(s)[i],'_rank'))
    cols_new <- c(cols_new,paste0(names(s)[i],'_rank'),names(s)[i])
  }
  s <- s[,cols,with=F]
  setnames(s,old=cols,new=cols_new)
  s
}

reFormat <- function(c){
  c
}

applyFilters <- function(d,f){
  for(i in 1:(len(f)/2)){
    if(!grepl('^All',f[2*i])){
      d <- eval(parse(text=paste0('d[d$`',f[(2*i)-1],'` == "',f[2*i],'",]')))
    }
  }
  d
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  GLOBAL CONSTANTS & DEFINITIONS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
min_date <- as.Date('2018-11-01')
max_date <- Sys.Date()
na_val <- '--'

periods <- c('This Year'='y','Last Year'='yp','This Quarter'='q','Last Quarter'='qp','This Month'='m','Last Month'='mp','This Week'='w','Last Week'='wp',
             'Last 90 Days'='90','Last 30 Days'='30','Last 14 Days'='14','Last 7 Days'='7','Custom')

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  READ IN DATA
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mypath = '' #/Users/harveymanhood/Documents/clients/participaction/kookaburra/'
province_data <- fread(paste0(mypath,'data/provinces.csv'))
awards_data <- fread(paste0(mypath,'data/award_backup.csv'))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  QUERIES
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myquery_awards_old <- "SELECT customer_id, clearbridge_appuser_id, award_short_description, CONCAT(UCASE(SUBSTRING(gender,1,1)),LOWER(SUBSTRING(gender,2))) AS gender,
                  CASE WHEN age < 5 THEN '0-4' ELSE (CASE WHEN age < 18 THEN '5-17' ELSE (CASE WHEN age < 25 THEN '18-24' ELSE (CASE WHEN age < 35 THEN '25-34' ELSE (CASE WHEN age < 45 THEN '35-44' ELSE (CASE WHEN age < 55 THEN '45-54' ELSE (CASE WHEN age < 65 THEN '55-64' ELSE '65+' END) END) END) END) END) END) END AS age, answerValue,
                  CONCAT('Q',QUARTER(created_at),' ',YEAR(created_at)) as quarter, CONCAT(SUBSTRING(MONTHNAME(created_at),1,3),' ',YEAR(created_at)) AS month, DATE(created_at) AS date, CASE WHEN hour(created_at) > 17 THEN 'Evening' ELSE (CASE WHEN hour(created_at) > 11 THEN 'Afternoon' ELSE (CASE WHEN hour(created_at) > 5 THEN 'Morning' ELSE 'Night' END) END) END as hour, COUNT(award_short_description) AS count,
                  UPPER(SUBSTRING(postal_code,1,1)) AS province_postcode
                  FROM (SELECT * FROM participaction_core_customeraward A
                  INNER JOIN (SELECT _uid AS award_id2,award_type,award_code,sequence_number,start_timestamp,end_timestamp,days_valid,num_ballots,program_id,earn_multiple_allowed,award_class,minutes_required FROM participaction_core_award) B ON B.award_id2 = A.award_id
                  INNER JOIN (SELECT master_id,id AS award_num,language_code,name AS award_name,short_description AS award_short_description,long_description AS award_long_description FROM participaction_core_award_translation WHERE language_code = 'en') C ON C.master_id = A.award_id
                  INNER JOIN (SELECT _uid AS customer_id2,clearbridge_appuser_id,postal_code,gender,age FROM participaction_core_customer) D ON A.customer_id = D.customer_id2) X
                  LEFT JOIN (SELECT userId, answerValue FROM paction_db_qa.QA WHERE type = 'onboarding' AND questionKey = 'activityLevel') Y ON X.clearbridge_appuser_id = Y.userId
                  GROUP BY customer_id, award_short_description, gender, age, answerValue, quarter, month, date, hour, province_postcode;"
                  #LIMIT 1000;'

myquery_awards <- "SELECT customer_id, clearbridge_appuser_id, award_code, max_award_code, award_short_description, CONCAT(UCASE(SUBSTRING(gender,1,1)),LOWER(SUBSTRING(gender,2))) AS gender,
                   CASE WHEN age < 5 THEN '0-4' ELSE (CASE WHEN age < 18 THEN '5-17' ELSE (CASE WHEN age < 25 THEN '18-24' ELSE (CASE WHEN age < 35 THEN '25-34' ELSE (CASE WHEN age < 45 THEN '35-44' ELSE (CASE WHEN age < 55 THEN '45-54' ELSE (CASE WHEN age < 65 THEN '55-64' ELSE '65+' END) END) END) END) END) END) END AS age, user_selected_level,
                   CONCAT('Q',QUARTER(created_at),' ',YEAR(created_at)) as quarter, CONCAT(SUBSTRING(MONTHNAME(created_at),1,3),' ',YEAR(created_at)) AS month, DATE(created_at) AS date, CASE WHEN hour(created_at) > 17 THEN 'Evening' ELSE (CASE WHEN hour(created_at) > 11 THEN 'Afternoon' ELSE (CASE WHEN hour(created_at) > 5 THEN 'Morning' ELSE 'Night' END) END) END as hour, COUNT(award_short_description) AS count,
                   UPPER(SUBSTRING(postal_code,1,1)) AS province_postcode FROM (SELECT * FROM participaction_core_customeraward A
                   INNER JOIN (SELECT _uid AS award_id2,award_type,award_code,sequence_number,start_timestamp,end_timestamp,days_valid,num_ballots,program_id,earn_multiple_allowed,award_class,minutes_required FROM participaction_core_award) B ON B.award_id2 = A.award_id
                   INNER JOIN (SELECT master_id,id AS award_num,language_code,name AS award_name,short_description AS award_short_description,long_description AS award_long_description FROM participaction_core_award_translation WHERE language_code = 'en') C ON C.master_id = A.award_id
                   INNER JOIN (SELECT _uid AS customer_id2,clearbridge_appuser_id,user_selected_level,postal_code,gender,age FROM participaction_core_customer) D ON A.customer_id = D.customer_id2) X
                   INNER JOIN (SELECT customer_id AS customer_id4,award_code AS max_award_code FROM(SELECT customer_id,MAX(minutes) AS minutes FROM (SELECT * FROM participaction_core_customeraward) AA
                   INNER JOIN (SELECT master_id, CAST(SUBSTRING(short_description,1,LOCATE(' ',short_description)-1) AS UNSIGNED) AS minutes FROM participaction_core_award_translation WHERE language_code = 'en' AND LOCATE('Milestone',short_description) > 0) BB ON BB.master_id = AA.award_id
                   GROUP BY customer_id) CC
                   INNER JOIN (SELECT award_code,minutes2 FROM (SELECT * FROM participaction_core_award) DD
                   INNER JOIN (SELECT master_id,CAST(SUBSTRING(short_description,1,LOCATE(' ',short_description)-1) AS UNSIGNED) AS minutes2 FROM participaction_core_award_translation WHERE language_code = 'en' AND LOCATE('Milestone',short_description) > 0) EE ON EE.master_id = DD._uid
                   GROUP BY award_code,minutes2) FF ON CC.minutes = FF.minutes2) QQ ON X.customer_id = QQ.customer_id4               
                   GROUP BY customer_id, award_code, max_award_code, user_selected_level, award_short_description, gender, age, quarter, month, date, hour, province_postcode;"

myBigQuery <- paste0("SELECT user_pseudo_id,event_date,CAST(event_timestamp AS STRING) AS ts,CASE WHEN CAST(HOUR(event_timestamp) AS INTEGER) > 17 THEN 'Evening' ELSE (CASE WHEN CAST(HOUR(event_timestamp) AS INTEGER) > 11 THEN 'Afternoon' ELSE (CASE WHEN CAST(HOUR(event_timestamp) AS INTEGER) > 5 THEN 'Morning' ELSE 'Night' END) END) END as hour,event_name,device.category,device.operating_system,geo.country,geo.region FROM (TABLE_DATE_RANGE([participaction-mobile:analytics_192216464.events_],TIMESTAMP('2019-02-27'),TIMESTAMP('",as.character(Sys.Date()),"'))) WHERE event_name = 'first_open' GROUP BY user_pseudo_id,event_date,ts,hour,event_name,device.category,device.operating_system,geo.country,geo.region")

myBigQuery2 <- "SELECT * FROM(SELECT EXTRACT(DATE FROM TIMESTAMP_MICROS(event_timestamp) AT TIME ZONE
  (CASE WHEN region IN('Ontario','Quebec','Nunavut') THEN 'America/Toronto' ELSE
(CASE WHEN region IN('Manitoba','Saskatchewan') THEN 'America/Winnipeg' ELSE
(CASE WHEN region IN('Northwest Territories','Alberta') THEN 'America/Yellowknife' ELSE
(CASE WHEN region IN('British Columbia','Yukon Territory') THEN 'America/Los_Angeles' ELSE 'America/Moncton'
END)
END)
END)
END)
) AS date,SUM(first_open) AS first_open,category,
CASE WHEN operating_system = 'ANDROID' THEN 'Android' ELSE 'iOS' END as operating_system,
CASE WHEN day_part = 0 THEN 'Night' ELSE 
(CASE WHEN day_part = 1 THEN 'Morning' ELSE 
(CASE WHEN day_part = 2 THEN 'Afternoon' ELSE 'Evening'
END)
END)
END AS time,
CASE WHEN region = 'Newfoundland and Labrador' THEN 'Newfoundland & Labrador' ELSE 
(CASE WHEN region = 'Quebec' THEN 'Québec' ELSE 
(CASE WHEN region IN('Northwest Territories','Nunavut') THEN 'NW Territories & Nunavut' ELSE region
END)
END)
END AS region
FROM (SELECT event_date AS date,
CAST(1+FLOOR(EXTRACT(HOUR FROM TIMESTAMP_MICROS(event_timestamp) AT TIME ZONE
(CASE WHEN geo.region IN('Ontario','Quebec','Nunavut') THEN 'America/Toronto' ELSE
(CASE WHEN geo.region IN('Manitoba','Saskatchewan') THEN 'America/Winnipeg' ELSE
(CASE WHEN geo.region IN('Northwest Territories','Alberta') THEN 'America/Yellowknife' ELSE
(CASE WHEN geo.region IN('British Columbia','Yukon Territory') THEN 'America/Los_Angeles' ELSE 'America/Moncton'
END)
END)
END)
END)
)/6) AS INT64) AS day_part,event_timestamp,COUNT(event_name) AS first_open,device.category,device.operating_system,geo.region
FROM `participaction-mobile.analytics_192216464.events_*`
WHERE
_TABLE_SUFFIX BETWEEN '20190227' AND '20190303' AND event_name = 'first_open' AND geo.country = 'Canada' GROUP BY date,day_part,event_timestamp,event_name,device.category,device.operating_system,geo.country,geo.region) GROUP BY date,category,operating_system,time,region

UNION ALL

SELECT EXTRACT(DATE FROM TIMESTAMP_MICROS(event_timestamp) AT TIME ZONE
(CASE WHEN region IN('Ontario','Quebec','Nunavut') THEN 'America/Toronto' ELSE
(CASE WHEN region IN('Manitoba','Saskatchewan') THEN 'America/Winnipeg' ELSE
(CASE WHEN region IN('Northwest Territories','Alberta') THEN 'America/Yellowknife' ELSE
(CASE WHEN region IN('British Columbia','Yukon Territory') THEN 'America/Los_Angeles' ELSE 'America/Moncton'
END)
END)
END)
END)
) AS date,SUM(first_open) AS first_open,category,
CASE WHEN operating_system = 'ANDROID' THEN 'Android' ELSE 'iOS' END as operating_system,
CASE WHEN day_part = 0 THEN 'Night' ELSE 
(CASE WHEN day_part = 1 THEN 'Morning' ELSE 
(CASE WHEN day_part = 2 THEN 'Afternoon' ELSE 'Evening'
END)
END)
END AS time,
CASE WHEN region = 'Newfoundland and Labrador' THEN 'Newfoundland & Labrador' ELSE 
(CASE WHEN region = 'Quebec' THEN 'Québec' ELSE 
(CASE WHEN region IN('Northwest Territories','Nunavut') THEN 'NW Territories & Nunavut' ELSE region
END)
END)
END AS region
FROM (SELECT event_date AS date,
CAST(1+FLOOR(EXTRACT(HOUR FROM TIMESTAMP_MICROS(event_timestamp) AT TIME ZONE
(CASE WHEN geo.region IN('Ontario','Quebec','Nunavut') THEN 'America/Toronto' ELSE
(CASE WHEN geo.region IN('Manitoba','Saskatchewan') THEN 'America/Winnipeg' ELSE
(CASE WHEN geo.region IN('Northwest Territories','Alberta') THEN 'America/Yellowknife' ELSE
(CASE WHEN geo.region IN('British Columbia','Yukon Territory') THEN 'America/Los_Angeles' ELSE 'America/Moncton'
END)
END)
END)
END)
)/6) AS INT64) AS day_part,event_timestamp,COUNT(event_name) AS first_open,device.category,device.operating_system,geo.region
FROM `participaction-mobile.analytics_192216464.events_intraday_*`
WHERE
_TABLE_SUFFIX BETWEEN '20190227' AND '20190303' AND event_name = 'first_open' AND geo.country = 'Canada' GROUP BY date,day_part,event_timestamp,event_name,device.category,device.operating_system,geo.country,geo.region) GROUP BY date,category,operating_system,time,region) ORDER BY date"

#awards_data <- dbGetQuery(con,myquery_awards)
#write.csv(awards_data,'award_backup.csv')
awards_data$award_short_description <- gsub("Weekly Challenge Complete", "Weekly", awards_data$award_short_description)
awards_data <- setDT(merge(awards_data,province_data,by='province_postcode',all.x=T))
#app_data <- setDT(bqr_query('participaction-mobile','analytics_192216464',myBigQuery2,useLegacySql=F))

write.csv(awards_data,'award_backup.csv')

provinces <- unique(awards_data$Province)
unique_users <- len(unique(awards_data$customer_id))
total_awards <- sum(awards_data$count)

prepare_awards <- function(d,a,f,to_order,dates,t){
  d$date <- as.Date(d$date)
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  d <- applyFilters(d,f)
  s <- setDT(filter(d,d$award_short_description %in% a))
  
  if(t==T){
  #s <-  s[ , -which(names(s) %in% ("award_short_description"))]
  #s$award_short_description <- NULL
  s$award_short_description <- "Total"
  s1 <- s[,c('award_short_description','count')][,lapply(.SD,sum),by=c('award_short_description')]
  s2 <- s[,c('customer_id','award_short_description','count')][,lapply(.SD,sum),by=c('customer_id','award_short_description')]
  s2$unique <- 1
  s2 <- s2[,c('award_short_description','unique')][,lapply(.SD,sum),by=c('award_short_description')]
  s <- merge(s1,s2,by='award_short_description',all.x=T)
  s$`Distribution` <- 100*s$count/total_awards
  s$`Penetration` <- 100*s$unique/unique_users
  s$`Per Average User` <- round(s$Total/s$Unique,2)
  setnames(s,old=c('award_short_description','count','unique'),new=c('Award Description','Total','Unique'))
  s <- addSortColumns(s)
  s$`Distribution_rank` <- paste0(format(round(s$`Distribution_rank`,2),nsmall=2),"%")
  s$`Penetration_rank` <- paste0(format(round(s$`Penetration_rank`,2),nsmall=2),"%")
  
  s <- setorder(s[, .r := order(to_order)], .r)[, .r := NULL]
  datatable(s,options=list(pageLength=default_rows(50)),escape=F,rownames=F)
  } else {
  s <- setDT(filter(d,d$award_short_description %in% a))
  s1 <- s[,c('award_short_description','count')][,lapply(.SD,sum),by=c('award_short_description')]
  s2 <- s[,c('customer_id','award_short_description','count')][,lapply(.SD,sum),by=c('customer_id','award_short_description')]
  s2$unique <- 1
  s2 <- s2[,c('award_short_description','unique')][,lapply(.SD,sum),by=c('award_short_description')]
  s <- merge(s1,s2,by='award_short_description',all.x=T)
  s$`Distribution` <- 100*s$count/total_awards
  s$`Penetration` <- 100*s$unique/unique_users
  setnames(s,old=c('award_short_description','count','unique'),new=c('Award Description','Total','Unique'))
  s <- addSortColumns(s)
  s$`Distribution_rank` <- paste0(format(round(s$`Distribution_rank`,2),nsmall=2),"%")
  s$`Penetration_rank` <- paste0(format(round(s$`Penetration_rank`,2),nsmall=2),"%")
  s$`Per Average User` <- round(s$Total/s$Unique,2)
  s <- setorder(s[, .r := order(to_order)], .r)[, .r := NULL]
  datatable(s,options=list(pageLength=default_rows(50)),escape=F,rownames=F)
  }
}



#prepare_awards_delta <- function(d,a,f,to_order,dates){
#  t1 <- prepare_awards(d,a,f,to_order,dates,F)
#  t2 <- prepare_awards(d,a,f,to_order,dates-7,F)
#  merge(t1,t2[short_award_description,awrd_desc_rank,total,total_rank],)
#}

prepare_badge_count <- function(d,f,dates){
  d$date <- as.Date(d$date)
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  d <- applyFilters(d,f)
  d$Count <- 1
  s <- d[,c('customer_id','Count')][,lapply(.SD,sum),by=c('customer_id')]
  c_count <- nrow(s)
  s$Users <- 1
  s <- s[,c('Count','Users')][,lapply(.SD,sum),by=c('Count')][order(Count)]
  s2=data.table(Count=c(0:max(s$Count)))
  s2 <- merge(s2,s,by='Count',all.x=T)
  s2[is.na(s2)] <- 0
  s2$`Distribution` <- 100*s2$Users/sum(s2$Users)
  s2$`Penetration` <- 100*s2$Users/sum(c_count)
  s2$`Cum Distribution` <- cumsum(s2$`Distribution`)
  s2$`Cum Penetration` <- cumsum(s2$`Penetration`)
  s2 <- addSortColumns(s2)
  s2$`Distribution_rank` <- paste0(format(round(s2$`Distribution_rank`,2),nsmall=2),"%")
  s2$`Penetration_rank` <- paste0(format(round(s2$`Penetration_rank`,2),nsmall=2),"%")
  s2$`Cum Distribution_rank` <- paste0(format(round(s2$`Cum Distribution_rank`,2),nsmall=2),"%")
  s2$`Cum Penetration_rank` <- paste0(format(round(s2$`Cum Penetration_rank`,2),nsmall=2),"%")
  datatable(s2,options=list(pageLength=default_rows(15)),escape=F,rownames=F)
}

prepare_activity <- function(d,f,dates){
  d$date <- as.Date(d$date)
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  #for(i in 1:(len(f)/2)){
  #  if(!grepl('^All',f[2*i])){
  #    d <- eval(parse(text=paste0('d[d$`',f[(2*i)-1],'` == "',f[2*i],'",]')))
  #  }
  #}
  d$Count <- 1
  s <- d[,c('user_selected_level','Count')][,lapply(.SD,sum),by=c('user_selected_level')][order(user_selected_level)]
  s$`% Total` <- 100*s$user_selected_level/sum(s$user_selected_level)
  s$`Cum Total` <- cumsum(s$`% Total`)
  setnames(s,old=c('user_selected_level'),new=c('Onboarding Level'))
  s$`% Total` <- paste0(format(round(s$`% Total`,2),nsmall=2),"%")
  s$`Cum Total` <- paste0(format(round(s$`Cum Total`,2),nsmall=2),"%")
  datatable(s,options=list(pageLength=default_rows(10)),escape=F,rownames=F)
}

prepare_chart <- function(d,f,dates){
  d$date <- as.Date(d$date)
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  d <- applyFilters(d,f)
  if(!'count' %in% names(d)){d$count <- 1}
  d <- setDT(d)
  d <- d[,c('date','count')][,lapply(.SD,sum),by = c('date')]
  highchart() %>% 
  hc_chart(type = 'spline') %>% #,color = '#FF0000') %>%
  hc_subtitle(enabled = F) %>%
  hc_legend(enabled = F) %>% #, align = "left", verticalAlign = "top") %>%
  hc_xAxis(categories = c(d$date), labels = list(rotation = 0, style = list(fontSize= '8px')), tickInterval = 2) %>% 
  hc_yAxis(opposite=T,labels=list(align = 'left')) %>%
  hc_plotOptions(
    dataLabels = list(enabled = F),
    marker = list(enabled = F)
  ) %>% 
  hc_series(
    list(
      name = 'Badges Earned',
      data = round(as.numeric(d$count),2),
      color = '#FF948B'
    )
  )
}

#prepare_subtotal <- function(d,f,a,dates,t=T){
#  d$date <- as.Date(d$date)
#  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
#  d <- applyFilters(d,f)
#  s <- setDT(filter(d,d$award_short_description %in% a))
#  s1 <- s[,c('award_short_description','count')][,lapply(.SD,sum),by=c('award_short_description')]
#  s2 <- s[,c('customer_id','award_short_description','count')][,lapply(.SD,sum),by=c('customer_id','award_short_description')]
#  s2$unique <- 1
#  s2 <- s2[,c('award_short_description','unique')][,lapply(.SD,sum),by=c('award_short_description')]
#  s <- merge(s1,s2,by='award_short_description',all.x=T)
#  s$`Distribution` <- 100*s$count/total_awards
#  s$`Penetration` <- 100*s$unique/unique_users
#  setnames(s,old=c('award_short_description','count','unique'),new=c('Award Description','Total','Unique'))
  #s <- addSortColumns(s)
  #s$`Distribution_rank` <- paste0(format(round(s$`Distribution_rank`,2),nsmall=2),"%")
  #s$`Penetration_rank` <- paste0(format(round(s$`Penetration_rank`,2),nsmall=2),"%")
  #s <- setorder(s[, .r := order(to_order)], .r)[, .r := NULL]
 # s3 <- data.table(
 #   ID = c("Subtotal"),
 #   setDT(s)[, lapply(.SD, sum)]
#  )
#  datatable(s,options=list(pageLength=default_rows(50)),escape=F,rownames=F)
#}




prepare_app <- function(d,f,dates){
  s <- d[,c('operating_system','first_open')][,lapply(.SD,sum),by = c('operating_system')]
  datatable(s,options=list(pageLength=default_rows(15)),escape=F,rownames=F)
}
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  QUERIES FOR REWARDS
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myquery_rewards <- "SELECT D.customer_id, D.created_at, D.user_selected_level, D.gender,D.age,D.postal_code, 
participaction_core_award_translation.short_description, D.num_ballots, D.cycle_type
FROM (SELECT C.*, participaction_core_customer.user_selected_level, participaction_core_customer.gender, participaction_core_customer.postal_code,participaction_core_customer.age
FROM (SELECT B.*, participaction_core_award.award_code, participaction_core_award.num_ballots
FROM (SELECT  participaction_core_customeraward.customer_id, participaction_core_customeraward.award_id,A.* 
FROM (SELECT  participaction_core_customerawardballot.*, X.cycle_type
FROM participaction_core_customerawardballot
LEFT JOIN (SELECT  participaction_core_rewardcycle.cycle_type, participaction_core_rewardcycleinstance.*
FROM participaction_core_rewardcycleinstance
LEFT JOIN participaction_core_rewardcycle
ON participaction_core_rewardcycle._uid = participaction_core_rewardcycleinstance.reward_cycle_id) X
ON participaction_core_customerawardballot.reward_cycle_instance_id=X._uid) A
LEFT JOIN participaction_core_customeraward
ON participaction_core_customeraward._uid = A.customer_award_id) B
LEFT JOIN participaction_core_award
ON participaction_core_award._uid = B.award_id) C
LEFT JOIN participaction_core_customer
ON participaction_core_customer._uid = C.customer_id) D
LEFT JOIN participaction_core_award_translation
ON D.award_id = participaction_core_award_translation.master_id
AND participaction_core_award_translation.language_code != 'fr';
"
##LIMIT 1000;'

#dbDisconnect(con)

rewards_data <- dbGetQuery(con,myquery_rewards)
p_code <- c("A","B","C","E","G","H","J","K","L","M","N","P","R","S","T","V","X","Y")
province <- c("Newfoundland and Labrador","Nova Scotia","Prince Edward Island","New Brunswick", "Eastern Quebec",
              "Metropolitan Montréal","Western Quebec","Eastern Ontario","Central Ontario","Metropolitan Toronto",
              "Southwestern Ontario","Northern Ontario","Manitoba","Saskatchewan","Alberta","British Columbia",
              "Northwest Territories and Nunavut","Yukon")
rewards_data$Province <- province[match(toupper(substr(rewards_data$postal_code, 1, 1)), p_code)]

#Date
rewards_data$Date <- as.Date(rewards_data$created_at, format = "%m/%d/%Y")
rewards_data$Quarter <- paste(quarters(as.Date(rewards_data$created_at, format = "%m/%d/%Y")),year(rewards_data$created_at))
rewards_data$Month <- paste(substring(format(as.Date(rewards_data$created_at, format="%m/%d/%Y"),"%B"),1,3), year(rewards_data$created_at))
rewards_data$Day <- weekdays(as.Date(rewards_data$created_at, format="%m/%d/%Y"))

#Time
rewards_data$Time[6 <= hour(rewards_data$created_at) & hour(rewards_data$created_at) < 12] = "Morning"
rewards_data$Time[12 <= hour(rewards_data$created_at) & hour(rewards_data$created_at) < 17] = "Afternoon"
rewards_data$Time[17 <= hour(rewards_data$created_at) & hour(rewards_data$created_at) < 22] = "Evening"
rewards_data$Time[22 <= hour(rewards_data$created_at) & hour(rewards_data$created_at) <=24 ] = "Night"
rewards_data$Time[0 <= hour(rewards_data$created_at) & hour(rewards_data$created_at) <=5 ] = "Night"
rewards_data$count <- 1
prepare_rewards_new <- function(d,a,f,dates){
  d$date <- as.Date(d$Date)
  print(dates[1])
  print(dates[2])
  d <- d[(d$date >= dates[1]) & (d$date <= dates[2]),]
  for(i in 1:(len(f)/2)){
    if(!grepl('^All',f[2*i])){
      d <- eval(parse(text=paste0('d[d$`',f[(2*i)-1],'` == "',f[2*i],'",]'))) #names(d),with=F]')))
    }
  }  
  s <- setDT(filter(d,d$short_description %in% a))
  s1 <- s %>% dplyr::count(short_description,cycle_type,sort = TRUE)
  s2 <- s1 %>% spread(cycle_type,n)
  datatable(s2,options=list(pageLength=default_rows(50)),escape=F,rownames=F)
}










#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  LAYOUT
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
badges <- tabPanel('Badges',
  mainPanel(
    fluidRow(
     tags$h2('Summary of Badges Earned')
    ),
    fluidRow(class='top_metric_names',
        tags$div(class='metric_name_box','BADGES CLAIMED'),
        tags$div(class='metric_name_box','UNIQUE CUSTOMERS'),
        tags$div(class='metric_name_box','BADGES/CUSTOMER'),
        tags$div(class='metric_name_box','NEW USERS'),
        tags$div(class='metric_name_box','7-DAY GROWTH'),
        tags$div(class='metric_name_box metric_last','30-DAY GROWTH')   
    ),
    uiOutput('badgeText'),
    fluidRow(
      tags$div(class='table_holder',
        tags$h3(class='h3-float h3-text h3-first','Engagement Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_2')),
        tags$div(class='badges-table table-total',DT::dataTableOutput('subtot_2')),
        tags$h3(class='h3-float h3-text h3-first','Activity Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_1')),
        tags$div(class='badges-table table-total',DT::dataTableOutput('subtot_1')),
        tags$h3(class='h3-float h3-text h3-first','Level Badges'),
        tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_3')),
        tags$div(class='badges-table table-total',DT::dataTableOutput('subtot_3')),
        tags$h3(class='h3-float h3-text h3-first','Badges Earned per Unique User'),
        tags$div(class='badges-table table-6 table-expanded pagination-true',DT::dataTableOutput('badges_table_4'))
      )
      #tags$div(class='chart_holder',
      #  tags$h3(class='h3-float h3-text h3-first','Badges Earned: All Badge Types'),
       # tags$div(class='chart_top',highchartOutput('badges_chart_1'))
      #)
    )
  )
)

rewards <- tabPanel('Rewards',
  mainPanel(
    fluidRow(
      tags$h2('Summary of Rewards Claimed')
    ),
    tags$div(class='table_holder',
             tags$h3(class='h3-float h3-text h3-first','User Onboarding Levels'),
             tags$div(class='app-table table-expanded',DT::dataTableOutput('rewards_table_1')),
             tags$h3(class='h3-float h3-text h3-first','Engagement Badges')
             #tags$div(class='badges-table table-expanded',DT::dataTableOutput('badges_table_2'))
  )
  )
)

activity_report <- tabPanel('Activity Report',
  mainPanel(
    fluidRow(
      tags$h2('Activity Level')
    ),
    fluidRow(
      tags$div(class='table_holder',
               tags$h3(class='h3-float h3-text h3-first','User Onboarding Levels'),
               tags$div(class='app-table table-expanded',DT::dataTableOutput('activity_table_1')),
               tags$h3(class='h3-float h3-text h3-first','Activity Level Goals'),
               tags$div(class='badges-table table-expanded',DT::dataTableOutput('activity_table_2'))
      )
      #tags$div(class='chart_holder',
      #         tags$h3(class='h3-float h3-text h3-first','App Metrics'),
      #         tags$div(class='chart_top',highchartOutput('app_chart_1'))
      #
    )
  )
)

app_report <- tabPanel('App Behavior',
  mainPanel(
    fluidRow(
      tags$h2('App Behavior Report')
    ),
    fluidRow(
      tags$div(class='table_holder',
        tags$h3(class='h3-float h3-text h3-first','First-Time App Opens')
        #tags$div(class='app-table table-expanded',DT::dataTableOutput('app_table_1'))
      ),
      tags$div(class='chart_holder',
        tags$h3(class='h3-float h3-text h3-first','App Metrics')
        #tags$div(class='chart_top',highchartOutput('app_chart_1'))
      )
    )
  )
)

geo_report <- tabPanel('Geo Data',
  mainPanel(
    fluidRow(
      tags$h2('If we have time :)')
    )
  )
)