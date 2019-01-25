
library(tidyverse)
library(scales)
library(shiny)
library(wordcloud2)
library(tidytext)
library(shinythemes)
library(shinydashboard)

df <- read_rds("data/clean_df.rds")
dat <- df
# Global Variables

## Word Cloud
tidytitle <- dat %>%
  unnest_tokens(word, title)

my_stopwords <- data_frame(word = c(as.character(1:10), "nhttp", "http",
                                    "https", "nhttps", "bit.ly",
                                    "www.youtube.com", "youtube", "2017",
                                    "2018", "goo.gl", "nfollow", "video",
                                    "videos", "youtu.be", "facebook", "twitter",
                                    "ninstagram", "nfacebook", "ntwitter",
                                    "nsubscribe", "nwatch"))

df_title <- df %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords) %>%
  filter(!str_detect(word, "[^0-9a-zA-Z]"))

df_descript <- df %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words) %>%
  anti_join(my_stopwords) %>%
  filter(!str_detect(word, "[^0-9a-zA-Z]"))


## Globals for other panels
choices_df <- df %>%
  select(category) %>%
  mutate(category = as.character(category)) %>%
  count(category)
choices <- c("Select All", choices_df[["category"]])
choices_num <- paste0(choices_df[["category"]], " (", choices_df[["n"]], ")")
choices_num <- c("Select All", choices_num)

ui <- fluidPage(
  fluidPage(theme = shinytheme("yeti"),
   titlePanel(fluidRow("YouTube Trending Analytics", style = "height:50px;background-color:#36454f;color:white;line-height:20px;padding: 20px;")),
   
   sidebarLayout(position = "left",
                 sidebarPanel(
                   
       conditionalPanel(condition="input.conditionedPanels==1",
         selectInput(
           "engagement", "Type of engagement", c("Views",
                               "Likes",
                               "Dislikes",
                               "Comment Count"="Comment_Count")
                         ),
         helpText("Boxplot showcases distribution and median metrics by category")
                     ),
       
       conditionalPanel(condition="input.conditionedPanels==2",
              selectInput(
                "time", "Video Upload Time", c("Time of Day",
                                        "Day of Week")
                          ),
                          helpText("Graph shows video upload times for trending YouTube videos. Users may use this a guideline for upload times of the most popular content creators"),
               selectInput(
                 "category", "Category", choice = choices_num
                           ),
                           helpText("Category (Number of Trending Videos)")

                 
                     ),
       conditionalPanel(condition="input.conditionedPanels==3",
                        radioButtons(
                                         "text", "Choose Source:",
                                         c("Title", "Description")),
                                       selectInput(
                                         "categoryw", "Category", choice = choices_num
                                       ),
                                       helpText("Category (Number of Trending Videos)")
                 )),
       mainPanel(
         tabsetPanel(id = "conditionedPanels",
                     tabPanel("Engagement by Category", value=1, plotOutput("boxPlot")),
                     tabPanel("Trend in Time", value=2, plotOutput("timePlot")),
                     tabPanel("Popular Words", value=3, wordcloud2Output('wordcloud2'))
         )
       ))
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
        labs(x="", y="Videos Uploaded") + 
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
        labs(x="", y="Videos Uploaded") + 
        theme(axis.text=element_text(size=14),
              axis.title=element_text(size=14,face="bold")) +
        scale_y_continuous(labels = comma)
    }

  })

  output$wordcloud2 <- renderWordcloud2({

    if (input$categoryw %in% "Select All") {
      selected_choicew <- choices[-1]
    }
    else {
      selected_choicew <- str_extract(input$categoryw, choices)
    }

    if(input$text == "Title") {
      df_title %>%
        filter(category %in% selected_choicew) %>%
        count(word, sort=TRUE)%>%
        top_n(100, n) %>%
        wordcloud2(size=0.5, shape = "oval")
    }
    else {
      df_descript %>%
        filter(category %in% selected_choicew) %>%
        count(word, sort=TRUE)%>%
        top_n(100, n) %>%
        wordcloud2(size=0.5, shape = "oval")
    }
  })
}


# Run the application
shinyApp(ui = ui, server = server)

   
