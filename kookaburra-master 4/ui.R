ui <- fluidPage(
  titlePanel('T-Mobile: Powered by Alchemy'),
  useShinyjs(),
  extendShinyjs(text = jsCode),
  theme = 'style.css',
  tags$head(
    tags$script(src='js.js'),
    tags$link(href='https://fonts.googleapis.com/css?family=M+PLUS+1p|Noto Sans|Raleway|Rubik|Nunito+Sans:300,400,600,700',rel='stylesheet')
  ),
  
  #HTML('<input type="text" id="client_time" name="client_time" style="display: none;"> '),
  #HTML('<input type="text" id="client_time_zone_offset" name="client_time_zone_offset" style="display: none;"> '),
  #tags$script('$(function(){var time_now = new Date()
  #            $("input#client_time").val(time_now.getTime())
  #            $("input#client_time_zone_offset").val(time_now.getTimezoneOffset())});'),
  
  tags$img(class='left-logo',src='logo_alchemy_white.svg'),
  tags$div(class='tr-mask'),
  tags$img(class='right-logo logo-1',src='logo_paction_white.svg'),
  tags$img(class='right-logo logo-2',src='logo_paction_notext_white.svg'),
  tags$div(class='loading-overlay',
    tags$div(class='loading-overlay-alchemy'
      #tags$img(src = 'alchemy.svg', class = 'alchemy-overlay-image'),
      #tags$p(class='loading-title','Powering Your Experience....'),
      #tags$div(class='loading-progress')
    )
  ),
  
  column(3,
    tags$div(class='filterset',
      tags$div(class='filterbox filter-period',selectInput('period','Period:',periods,selected = '90')),
      tags$div(class='filterbox filter-split filter-group1 filter-date',uiOutput('dateDisplay')), #selectInput('date','Date:',c('All Dates',unique(as.Date(awards_data$date))),selected = 'All Dates')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('age','Age:',c('All Ages',unique(awards_data$age)),selected = 'All Ages')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('month','Month:',c('All Months',unique(awards_data$month)),selected = 'All Months')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('quarter','Quarter:',c('All Quarters',unique(awards_data$quarter)),selected = 'All Quarters')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('gender','Gender:',c('All Genders',unique(awards_data$gender)),selected = 'All Genders')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('hour','Hour:',c('All Times','Morning','Afternoon','Evening','Night'),selected = 'All Times')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('province','Province:',c('All Provinces',sort(provinces[!provinces %in% NA])),selected = 'All Provinces')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('level','Active Level:',c('All Levels',sort(unique(awards_data$user_selected_level))),selected = 'All Levels')),
      tags$div(class='filterbox filter-split filter-group1',selectInput('max_level','L Reached:',c('All Levels',sort(unique(awards_data$max_award_code))),selected = 'All Levels'))
    )
  ),
  column(9,navbarPage('',position='fixed-top',badges,rewards,activity_report,app_report,geo_report))
)