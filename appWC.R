#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if(require(shiny)){
    library(wordcloud2)
    library(tidytext)
    library(tidyr)
    library(dplyr)
    library(stringr)
# Global variables can go here
dat <- readRDS("data/clean_df.rds")

tidytitle <- dat %>%
    unnest_tokens(word, title)

my_stopwords <- data_frame(word = c(as.character(1:10), "nhttp", "http",
                                    "https", "nhttps", "bit.ly",
                                    "www.youtube.com", "youtube", "2017",
                                    "2018", "goo.gl", "nfollow", "video",
                                    "videos", "youtu.be", "facebook", "twitter",
                                    "ninstagram", "nfacebook", "ntwitter",
                                    "nsubscribe", "nwatch"))

tidytitle<- tidytitle %>%
    anti_join(stop_words)

tidytitle<- tidytitle %>%
    filter(!str_detect(word, "[^0-9a-zA-Z]")) %>%
    anti_join(my_stopwords)

titletokens <- tidytitle %>%
    count(word, sort=TRUE)

titletokens <- titletokens%>%
    top_n(100, n)
    #filter(n > 250)
    #wordcloud2(size = 1)

# Description dataframe
tidydescrip <- dat %>%
    unnest_tokens(word, description)

tidydescrip<- tidydescrip %>%
    anti_join(stop_words) 



tidydescrip<- tidydescrip %>%
    filter(!str_detect(word, "[^0-9a-zA-Z]")) %>%
    anti_join(my_stopwords)

descriptokens <- tidydescrip %>%
    count(word, sort=TRUE)

descriptokens<-descriptokens%>%
    top_n(100, n)
    #wordcloud2(size = 0.5, shape = "oval")

#n <- 1

# Define the UI
ui <- bootstrapPage(
    #numericInput('size', 'Size of wordcloud', n),
    radioButtons("text", "Choose Source:",
                 c("Title",
                   "Description")),
    
    # checkboxInput(inputId = "title",
    #               label = strong("Show title words"),
    #               value = FALSE),
    # 
    # checkboxInput(inputId = "description",
    #               label = strong("Show description words"),
    #               value = FALSE),
    
    wordcloud2Output('wordcloud2')
)


# Define the server code
server <- function(input, output) {
    
    
    output$wordcloud2 <- renderWordcloud2({
        # wordcloud2(demoFreqC, size=input$size)
        #wordcloud2(titletokens, size=input$size)
        text <- switch(input$text,
                       Title = titletokens,
                       Description = descriptokens)
        
        wordcloud2(text, size=0.7)
        
        # if (input$title) {
        #     wordcloud2(titletokens, size=1)
        # }
        # 
        # else if (input$description) {
        #     wordcloud2(descriptokens, size = 0.5, shape = "oval")
        # }
    
    })
}
# Return a Shiny app object
# Sys.setlocale("LC_CTYPE","chs") #if you use Chinese character
## Do not Run!
shinyApp(ui = ui, server = server)
}