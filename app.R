library(shinydashboard)
library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(wordcloud2)
library(tidytext)

# cleaned df
df <- read_rds("data/clean_df.rds")


## Globals for other panels
# wordcloud globals
df_title_1 <- read_rds("data/df_title_1.rds")
df_title_2 <- read_rds("data/df_title_2.rds")
df_title_3 <- read_rds("data/df_title_3.rds")

df_descript_1 <- read_rds("data/df_descript_1.rds")
df_descript_2 <- read_rds("data/df_descript_2.rds")
df_descript_3 <- read_rds("data/df_descript_3.rds")

# wordcloud logo shape that did not work
# figPath = system.file("data/youtubelogo.PNG", package = "wordcloud2")
# figPath
## globals for other panels
choices_df <- df %>%
  select(category) %>%
  mutate(category = as.character(category)) %>%
  count(category)
choices <- c("Select All", choices_df[["category"]])
choices_num <- paste0(choices_df[["category"]], " (", choices_df[["n"]], ")")
choices_num <- c("Select All", choices_num)


ui <- dashboardPage(skin = "blue",
                    
      dashboardHeader(title="YouTube Trending Analytics", titleWidth =300),
      
      dashboardSidebar(
        
        sidebarMenu(
          
          id = "tabs",
          
          #conditional panels allow side bar tabs to change with selection
          conditionalPanel("input.my_set == 'tab1_val'",
                           selectInput(
                             "engagement", "Type of Engagement", c("Views",
                                                                   "Likes",
                                                                   "Dislikes",
                                                                   "Comment Count"="Comment_Count"))
          ),
          conditionalPanel("input.my_set == 'tab2_val'",
                           selectInput(
                             "time", "Video Upload Time", c("Time of Day",
                                                            "Day of Week")
                           ),
                           # textOutput("Graph shows video upload times for trending YouTube videos. 
                           #          Users may use this a guideline for upload times of the most popular content creators"),
                           selectInput(
                             "category", "Category", choice = choices_num
                           )
                           
          ),
          
          conditionalPanel("input.my_set == 'tab3_val'",
                           radioButtons("text", "Choose Source", c("Title", "Description")),
                           selectInput("ngram", "1-2-3 Words", choice = c("Monogram", "Bigram", "Trigram")),
                           selectInput("categoryw", "Category", choice = choices_num)
                           
          )
          
        )
      ),
      
      # actual outputs for the plots
      dashboardBody(
        tabBox(
          title = "Trended YouTube Videos from Nov. 2017 - June 2018",
          # The id lets us use input$tabset1 on the server to find the current tab
          id = "my_set", height = "500px", width = "800px",
          tabPanel("Engagement by Category", id = "tab1",value='tab1_val', plotOutput("boxPlot")),
          
          tabPanel("Upload Date", id = "tab2", value='tab2_val', plotOutput("timePlot")),
          
          tabPanel("Popular Words", id = "tab3", value='tab3_val', wordcloud2Output('wordcloud2'))
          
        ) 
)
                    
                    
)

server <- function(input, output) {
  
  # output$barPlot <- renderPlot({
  #   df %>%
  #     group_by(category) %>%
  #     summarise(likes = sum(as.numeric(!!rlang::sym(str_to_lower(input$engagement)))),
  #               n = n(), avg = likes/n) %>%
  #     ggplot() +
  #     geom_boxplot(aes(fct_reorder(category, avg), fill = category)) +
  #     scale_y_continuous(labels = comma) +
  #     labs(x="", y=paste(input$engagement, "per Video")) +
  #     theme(legend.position = "none") +
  #     coord_flip()
  # })
  
  # boxplot outputs
  output$boxPlot <- renderPlot({
    df %>%
      ggplot() +
      geom_boxplot(aes(fct_reorder(category, !!rlang::sym(str_to_lower(input$engagement))),
                       !!rlang::sym(str_to_lower(input$engagement)),
                       fill = category)) +
      scale_y_log10(labels = comma) +
      labs(x="", y=paste(input$engagement, "per Video")) +
      theme(legend.position = "none") + 
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold")) +
      coord_flip()
  })
  
  # timeplot outputs
  output$timePlot <- renderPlot({
    
    if (input$category %in% "Select All") {
      selected_choice <- choices[-1]
    }
    else {
      selected_choice <- str_extract(input$category, choices)
    }
    
    if (input$time == "Day of Week") {
      df %>%
        select(publish_time, category) %>%
        filter(category %in% selected_choice) %>%
        ggplot() + geom_bar(aes(wday(publish_time, label = TRUE))) +
        labs(x="", y="Number of Videos Uploaded") + 
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14,face="bold")) +
        scale_y_continuous(labels = comma)
    }
    else {
      df %>%
        select(publish_time, category) %>%
        filter(category %in% selected_choice) %>%
        mutate(hours = hour(publish_time),
               minutes = minute(publish_time),
               seconds = second(publish_time),
               time = make_datetime(hour = hours, min = minutes, sec = seconds)) %>%
        ggplot() + geom_freqpoly(aes(time)) +
        scale_x_datetime(date_breaks = "3 hours", date_labels = "%H:%M") +
        labs(x="", y="Number of Videos Uploaded") + 
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14,face="bold")) +
        scale_y_continuous(labels = comma)
    }
    
  })
  
  # wordcloud filtering is added to remove stopwords
  output$wordcloud2 <- renderWordcloud2({
    
    if (input$categoryw %in% "Select All") {
      selected_choicew <- choices[-1]
    }
    else {
      selected_choicew <- str_extract(input$categoryw, choices)
    }
    
    if(input$text == "Title") {
      if (input$ngram == "Monogram") {
        df_title_1 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      } else if (input$ngram == "Bigram") {
        df_title_2 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      } else {
        df_title_3 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      }
    }
    else {
      if (input$ngram == "Monogram") {
        df_descript_1 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      } else if (input$ngram == "Bigram") {
        df_descript_2 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      } else {
        df_descript_3 %>%
          filter(category %in% selected_choicew) %>%
          count(ngram, sort=TRUE)%>%
          top_n(200, n) %>%
          wordcloud2(size=0.7)
      }
      
    }
  })
}

shinyApp(ui = ui, server = server)

