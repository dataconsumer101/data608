library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(shiny)
library(leaflet)
library(purrr)


### Get Data

# get raw data
df <- read.csv('https://raw.githubusercontent.com/dataconsumer101/data608/main/NYPD_Shooting_Incident_Data__Historic_.csv')

# convert column names to lowercase
colnames(df) <- lapply(colnames(df), tolower)

# select only the fields that are going to be used in order to minimize memory usage
df <- df %>%
  select(occur_date, boro, statistical_murder_flag, 
         perp_sex, perp_age_group, perp_race,
         vic_sex, vic_age_group, vic_race,
         latitude, longitude)

# convert to date type
df$occur_date <- as.Date(df$occur_date, format = '%m/%d/%Y')


### Data Clean up
### check all possible values for each field
### then replace empty/null/others with 'unknown'

# create not in function by negating %in%
`%notin%` <- Negate(`%in%`)

## check boro
# unique(df$boro)

## check age groups
# unique(df$perp_age_group)
df$perp_age_group[df$perp_age_group %notin% c('<18','18-24','25-44','45-64','65+')] <- 'UNKNOWN'
df$vic_age_group[df$vic_age_group %notin% c('<18','18-24','25-44','45-64','65+')] <- 'UNKNOWN'

## check gender
# unique(df$perp_sex)
df$perp_sex[df$perp_sex %notin% c('M','F')] <- 'U'
df$vic_sex[df$vic_sex %notin% c('M','F')] <- 'U'

## check race
# unique(df$perp_race)
df$perp_race[df$perp_race == ''] <- 'UNKNOWN'
df$vic_race[df$vic_race == ''] <- 'UNKNOWN'


### add fields and edit formatting

# add field - truncate to month
df$month <- cut(as.Date(df$occur_date), 'month') %>%
  as.Date(format = '%Y-%m-%d')

# add field - year number
df$yr <- year(df$occur_date)

# change format style of strings to title
df$boro <- str_to_title(df$boro)
df$perp_age_group <- str_to_title(df$perp_age_group)
df$vic_age_group <- str_to_title(df$vic_age_group)
df$perp_race <- str_to_title(df$perp_race)
df$vic_race <- str_to_title(df$vic_race)

# date ranges for title
srange <- format(min(df$occur_date), format = '%B %Y')
erange <- format(max(df$occur_date), format = '%B %Y')

# list of boroughs for dropdowns
boro_list <- append('All', unique(df$boro)) %>%
  sort()

# list of years for dropdowns
year_list <- append('All', unique(df$yr)) %>%
  sort()

# total shootings by year -- denominator for next step
share_yr <- df %>%
  group_by(yr) %>%
  summarise(annual_count = n(),
            .groups = 'drop')

# calculate annual share of shootings by borough
share <- df %>%
  group_by(yr, boro) %>%
  summarise(count = n(),
            .groups = 'drop') %>%
  inner_join(share_yr, by = c('yr' = 'yr')) %>%
  mutate(share = count / annual_count)

# remove denomiator data
rm(share_yr)



### Shiny App

# 3 tabs with different inputs
ui <- fluidPage(
  titlePanel(str_c('NYC Shooting Statistics, ',srange, ' - ', erange)),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = 'input.tabs < 3',
                       selectInput(inputId = 'boro', 'Borough', choices = boro_list, selected = 'ALL')
                       ),
      conditionalPanel(condition = 'input.tabs == 2',
                       radioButtons(inputId = 'actor', label = 'Perspective', choices = c('Perpetrator','Victim'), selected = 'Perpetrator'),
                       checkboxGroupInput(inputId = 'checks', label = 'Demographic Variables', choices = c('Gender', 'Age', 'Race'))
                       ),
      conditionalPanel(condition = 'input.tabs == 3',
                       selectInput(inputId = 'yr', 'Year', choices = year_list, selected = 'All'),
                       checkboxInput(inputId = 'check_murders', 'Statistical Murders Only', value = T)
                       )
    ),
    
    mainPanel(
      tabsetPanel(
        type = 'tabs',
        id = 'tabs',
        tabPanel('Summary', value = 1, 
                 fluidRow(plotOutput('plot1a')),
                 fluidRow(plotOutput('plot1b'))
                 ),
        tabPanel('People Involved', value = 2, plotOutput('plot2', height = '750px')),
        tabPanel('Map', value = 3, leafletOutput('map1', height = '750px'))
      )
    )
  )  
)

# Render Plots
server <- function(input, output) {
  
  # count of shootings by month
  output$plot1a <- renderPlot({
    
    if (toupper(input$boro) == 'ALL') {

      df %>%
        group_by(month) %>%
        summarize(count = n(),
                  .groups = 'drop') %>%
        ggplot(aes(x = month, y = count)) +
        geom_line(color = 'darkred') +
        stat_smooth(geom='line', alpha = .3) +
        labs(x = element_blank(),
             y = '# of Shootings',
             title = "Monthly Reported Shootings - All Boroughs") +
        theme_bw() +
        scale_x_date(date_breaks = '1 year', date_labels = '%Y')

    } else { # if a borough is selected
      
      df %>%
        filter(boro == input$boro) %>%
        group_by(month) %>%
        summarize(count = n(),
                  .groups = 'drop') %>%
        ggplot(aes(x = month, y = count)) +
        geom_line(color = 'darkred') +
        stat_smooth(geom='line', alpha = .3) +
        labs(x = element_blank(),
             y = '# of Shootings',
             title = str_c("Monthly Reported Shootings - ", input$boro)) +
        theme_bw() +
        scale_x_date(date_breaks = '1 year', date_labels = '%Y')
      
    }
      
  })
  
  # annual share of shootings by borough
  output$plot1b <- renderPlot({
    
    if (input$boro == 'All') {
      
      ggplot(share, aes(x = yr, y = count, fill = boro)) +
        geom_col(position = 'fill') +
        labs(x = 'Year',
             y = '% of Shootings',
             title = 'Annual Share of All NYC Shootings',
             fill = 'Borough') +
        theme_bw() +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = 'bottom') +
        scale_x_continuous(breaks = seq(min(df$yr), max(df$yr), 1)) +
        scale_fill_manual(values = c('#DDCC77','#88CCEE','#44AA99','#117733','#332288'))
      
    } else { # if a borough is selected
      
      color_key <- NULL
      
      if (input$boro == 'Bronx') {
        color_key <- '#DDCC77'
      } else if (input$boro == 'Brooklyn') {
        color_key <- '#88CCEE'
      } else if (input$boro == 'Manhattan') {
        color_key <- '#44AA99'
      } else if (input$boro == 'Queens') {
        color_key <- '#117733'
      } else if (input$boro == 'Staten Island') {
        color_key <- '#332288'
      } else {
        color_key <- 'red4'  
      }
      
      filter(share, boro == input$boro) %>%
      ggplot(aes(x = yr, y = share)) +
        geom_line(color = color_key, size = 2) +
        stat_smooth(geom='line', alpha = .3) +
        labs(x = 'Year',
             y = '% of Shootings',
             title = str_c('Annual Share of All NYC Shootings - ', input$boro)) +
        theme_bw() +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_continuous(breaks = seq(min(df$yr), max(df$yr), 1))
      
    }
      
  })
  
  
  # rank of share of shootings by demographic
  output$plot2 <- renderPlot({
    
    # number of checkboxes selected
    checksum <- length(input$checks)
    
    # create flags for each check
    check_gender <- ifelse('Gender' %in% input$checks, T, F)
    check_age <- ifelse('Age' %in% input$checks, T, F)
    check_race <- ifelse('Race' %in% input$checks, T, F)
    
    # column index vector
    ci <- NULL
    
    # label vector
    lv <- NULL
    
    # find columns that match selections
    # and setup the label that will be used for the axis title
    for (i in 1:length(names(df))) {
      
      if (input$actor == 'Perpetrator') {
        
        if ((check_gender == T | checksum == 0) & names(df)[i] == 'perp_sex') {
          ci <- append(ci, i)
          lv <- append(lv, 'Gender')
          }
        
        if ((check_age == T | checksum == 0) & names(df)[i] == 'perp_age_group') {
          ci <- append(ci, i)
          lv <- append(lv, 'Age')
        }
        
        if ((check_race == T | checksum == 0) & names(df)[i] == 'perp_race') {
          ci <- append(ci, i)
          lv <- append(lv, 'Race')
          }
        
      } else { # if Victim
        
        if ((check_gender == T | checksum == 0) & names(df)[i] == 'vic_sex') {
          ci <- append(ci, i)
          lv <- append(lv, 'Gender')
        }
        
        if ((check_age == T | checksum == 0) & names(df)[i] == 'vic_age_group') {
          ci <- append(ci, i)
          lv <- append(lv, 'Age')
        }
        
        if ((check_race == T | checksum == 0) & names(df)[i] == 'vic_race') {
          ci <- append(ci, i)
          lv <- append(lv, 'Race')
        }
        
      }
      
    }
    
    # combine label vector into one label for selected demographics
    demo_label <- paste(lv, collapse = ', ')
    
    # add new field - create segment based on selections
    df$seg <- reduce(.x = df[ci], .f = paste)
    
    # create new dataframe, grouping by selected segment
    if (toupper(input$boro) == 'ALL') {
      
      df2 <- df %>%
        group_by(seg) %>%
        summarize(count = n(),
                  share = n() / nrow(df),
                  .groups = 'drop')    
      
    } else { # if a borough is selected
      
      df2 <- df %>%
        filter(boro == input$boro)
      
      df2 <- df2 %>%
        group_by(seg) %>%
        summarize(count = n(),
                  share = n() / nrow(df2),
                  .groups = 'drop')    
      
    }

    # plot based on selections
    df2 %>%
      top_n(30, share) %>%
      ggplot(aes(x = reorder(seg, count), y = share)) +
      geom_col(fill = 'darkred', alpha = .8) +
      coord_flip() +
      labs(y = 'Probability',
           x = demo_label,
           title = str_c('Probability Rank of ', input$actor, ' by Demographic')) +
      theme_bw() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1))
    

  })
  
  
  # map of murders based on latitude and longitutde
  output$map1 <- renderLeaflet({
    
    # filter data depending on checkbox
    if (input$check_murders == T) {
      dfm <- df %>%
        filter(statistical_murder_flag == 'true')
    } else { # all reported shootings
      dfm <- df
    }
    
    # filter data based on year selection
    # if only one year is selected
    if (input$yr != 'All') { 
      dfm <- dfm %>%
        filter(yr == as.integer(input$yr))
    }
    
    # render map
    dfm %>%
      leaflet() %>%
      addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png') %>%
      addCircles(lat=~latitude, lng=~longitude, radius = 5, color = 'maroon') %>%
      addCircleMarkers(clusterOptions = markerClusterOptions())
    
  })
  
}

shinyApp(ui = ui, server = server)












