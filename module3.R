library(dplyr)
library(ggplot2)
library(shiny)

# load the data
df <- read.csv('https://raw.githubusercontent.com/charleyferrari/CUNY_DATA_608/master/module3/data/cleaned-cdc-mortality-1999-2010-2.csv')

# start the weighted average calculation
df$weighted <- df$Crude.Rate * df$Population

# rank the diseases by the crude rate for every state in a given year
df <- df %>%
  group_by(ICD.Chapter, Year) %>%
  mutate(rank = dense_rank(desc(Crude.Rate)))

# concatenate the rank along with the state
df$state_rank <- paste0(df$rank, ' - ', df$State)

# calculated the national weighted average per year
us_avg <- df %>%
  group_by(ICD.Chapter, State = 'National Average', Year) %>%
  summarize(Crude.Rate = sum(weighted) / sum(Population),
            .groups = 'drop')

# distinct cause of death list
cod_list <- unique(df$ICD.Chapter)

# distinct state list
state_list <- unique(df$State)


ui <- fluidPage(
  titlePanel('Data608 Module 3 Assignment'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'cause', 'Cause of Death', choices = cod_list, selected = 'Neoplasms'),
      conditionalPanel(condition = 'input.tabs == 2',
                         selectInput(inputId = 'state', 'State', choices = state_list, selected = 'NY'))
    ),
    mainPanel(
      tabsetPanel(
        type = 'tabs',
        id = 'tabs',
        tabPanel('2010 Mortality Rates by State', value = 1, plotOutput('plot_q1', height = '800px')),
        tabPanel('Mortality Rates vs National Average', value = 2, 
                 fluidRow(plotOutput('plot_q2')),
                 fluidRow(column(width = 1), tableOutput('tbl'))
                 )
      )
    )
  )  
)

server <- function(input, output) {
  
  output$plot_q1 <- renderPlot({
    
    filter(df, Year == '2010' & ICD.Chapter == input$cause) %>%
      ggplot(aes(x = Crude.Rate, y = reorder(state_rank, Crude.Rate))) +
      geom_col(alpha = .8, width = .7, fill = 'light blue') +
      ylab('State') +
      theme_bw()
  })
  
  output$plot_q2 <- renderPlot({
    
    # filter(df, State == input$state) %>%
    #   select(ICD.Chapter, Year, State, Crude.Rate) %>%
    #   bind_rows(us_avg) %>%
    #   filter(ICD.Chapter == input$cause) %>% 
    #   ggplot(aes(x = Year, y = Crude.Rate, color = State)) +
    #   geom_line() 
    
    us_avg_case <- filter(us_avg, ICD.Chapter == input$cause)
    
    filter(df, State == input$state & ICD.Chapter == input$cause) %>%
      ggplot(aes(x = Year, y = Crude.Rate, color = 'maroon')) +
      geom_line() +
      geom_line(data = us_avg_case, aes(y = Crude.Rate, color = 'black'), lty = 2) +
      scale_color_manual(name = element_blank(), values = c('maroon' = 'maroon','black' = 'black'), labels = c('National Average', input$state)) +
      theme_bw() +
      theme(legend.position = 'bottom') 
  })
  
  output$tbl <- renderTable({
    
    us_avg_tbl <- filter(us_avg, ICD.Chapter == input$cause) %>%
      mutate(National_Avg = Crude.Rate) %>%
      select(ICD.Chapter, Year, National_Avg)
    
    filter(df, State == input$state & ICD.Chapter == input$cause) %>%
      select(ICD.Chapter, Year, National_Rank = rank, Deaths, Population, Crude.Rate) %>%
      full_join(us_avg_tbl, by = c('ICD.Chapter' = 'ICD.Chapter', 'Year' = 'Year')) %>%
      ungroup() %>%
      select(-ICD.Chapter) %>%
      arrange(Year)

  })
}

shinyApp(ui = ui, server = server)




