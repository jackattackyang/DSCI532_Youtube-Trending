library(dplyr)
library(ggplot2)
library(stringr)
library(forcats)
library(readr)
library(lubridate)
library(scales)
library(shiny)
library(wordcloud2)
library(tidytext)

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

   titlePanel("YouTube Trending Analytics",
              windowTitle = "Trend Now"),

   fluidRow(
      column(4 ,
        wellPanel(
         selectInput(
           "engagement", "Type of engagement", c("Views",
                               "Likes",
                               "Dislikes",
                               "Comment Count"="Comment_Count")
           ),
         helpText("")
         ),
        div(style = "height:300px; background-color: white;"),
        wellPanel(h3("Trending Videos by Time"),
           selectInput(
             "time", "Time frame", c("Time of Day",
                           "Day of Week")
           ),
           helpText("Trending Videos from Nov. 2017 - June 2018"),

           selectInput(
             "category", "Category", choice = choices_num
           ),
           helpText("Category (Number of Trending Videos)")

           ),

        div(style = "height:125px; background-color: white;"),

        wellPanel(
          radioButtons("text", "Choose Source:",
                       c("Title",
                         "Description"))
        )
         ),

      mainPanel(
         plotOutput("boxPlot"),
         plotOutput("timePlot"),

           #numericInput('size', 'Size of wordcloud', n),


           # checkboxInput(inputId = "title",
           #               label = strong("Show title words"),
           #               value = FALSE),
           #
           # checkboxInput(inputId = "description",
           #               label = strong("Show description words"),
           #               value = FALSE),

           wordcloud2Output('wordcloud2')


      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$barPlot <- renderPlot({
    df %>%
      group_by(category) %>%
      summarise(likes = sum(as.numeric(!!rlang::sym(str_to_lower(input$engagement)))),
                n = n(), avg = likes/n) %>%
      ggplot() +
      geom_boxplot(aes(fct_reorder(category, avg), fill = category)) +
      scale_y_continuous(labels = comma) +
      labs(x="", y=paste(input$engagement, "per Video")) +
      theme(legend.position = "none") +
      coord_flip()
  })

  output$boxPlot <- renderPlot({
    df %>%
      ggplot() +
      geom_boxplot(aes(fct_reorder(category, !!rlang::sym(str_to_lower(input$engagement))),
                       !!rlang::sym(str_to_lower(input$engagement)),
                       fill = category)) +
      scale_y_log10(labels = comma) +
      labs(x="", y=paste(input$engagement, "per Video")) +
      theme(legend.position = "none") +
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

   
