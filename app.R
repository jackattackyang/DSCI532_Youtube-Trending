library(tidyverse)
library(lubridate)
library(scales)
library(shiny)

df <- read_rds("data/clean_df.rds")

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
                               "Comment_Count")
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
           
           )
         ),
      
      mainPanel(
         plotOutput("boxPlot"),
         plotOutput("timePlot")
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
}

# Run the application 
shinyApp(ui = ui, server = server)

