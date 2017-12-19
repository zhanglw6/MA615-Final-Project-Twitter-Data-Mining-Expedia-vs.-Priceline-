#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(tidyverse)
library(flexdashboard)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(plyr)
library(stringr)
library(plotly)
library(wordcloud)
library(ggthemes)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(devtools)
library(bitops)
library(twitteR)
library(reshape2)
library(tidytext)
library(dplyr)

#map data
epp <- read.csv("expediamap.csv")
epp <- epp[,c(2,3)]
plp <- read.csv("pricelinemap.csv")
plp <- plp[,c(2,3)]
#timeline data
eptweets.df <- read.csv("expediatweets.csv")
pltweets.df <- read.csv("pricelinetweets.csv")
#wordcloud
ep_clean <- readRDS('ep_clean.rds')
pl_clean <- readRDS('pl_clean.rds')
#sentiment
eptweets.sentiment <- read.csv("epsenti.csv")
pltweets.sentiment <- read.csv("plsenti.csv")

ui <- shinyUI(fluidPage(
  
  titlePanel(
    fluidRow(
      column(8, 
             h3("Twitter Data Mining - Expedia vs. Priceline"),
             h5("Liwen (Sophie) Zhang. All the codes can be approached from", 
                a(href="https://github.com/zhanglw6/MA615-Final-Project-Twitter-Data-Mining-Expedia-vs.-Priceline-", 
                  "my Github"))
             ))),
  
  navbarPage(
    title= "", 
    tabPanel("Introduction", 
             br(), 
             p("Nowadays, social media are frequently used by people before, during and after a trip. 
               Twitter, as one of the best social media around, has become a more and more important 
               platform for travel brands to engage with their customers, boost brand favorability and 
               make influences on travelers. In this project, we are going to explore tweets about two 
               prominent travel sites, Expedia and Priceline, by plotting users location maps, timelines,
               word clouds and conducting sentiment analysis."),
             br(), 
             h4("Background on Expedia and Priceline"),
             sidebarLayout(position = "right",
                  sidebarPanel(img(src = "expedia-logo.png", height=100, width=100)),
                  mainPanel(p("- Expedia, founded in 1996, operates 200 additional travel booking websites, 
                      including Expedia.com, Hotels.com, Hotwire.com, Orbitz, Travelocity, Trivago, 
                       HomeAway, and Venere.com."))
                          ),#close sidebar
             sidebarLayout(position = "right",
                  sidebarPanel(img(src = "price-logo.png", height=100, width=100)),
                  mainPanel(
                            p("- Priceline, founded in 1997, is a subsidiary of The Priceline Group, 
                               which also owns other travel fare metasearch engines including Booking.com,
                               Agoda.com, Kayak.com, Cheapflights, Rentalcars.com, Momondo, and OpenTable."))
                  ) #close sidebar
    ), #close introduction tab
    
    
    tabPanel("Data Mining Process", 
             tabsetPanel(
               tabPanel("Maps",
                        br(),
                        p("Here are the maps of users in the United States who posted tweets about 
                          Expedia and Priceline. Both maps show that there are more tweets posted along 
                          east coast, and around California and Northwestern U.S, but only a few
                          tweets were posted in the Middle and Northern part of the U.S. We can see that popular
                          travel destinations tend to have more tweets regarding Expedia and Priceline, 
                          because those places usually have lot more travelers who would like to share either
                          their good experience or comments about the bad service they got."),
                        hr(),
                        splitLayout(leafletOutput("epmap"), leafletOutput("plmap"))
                        ),#close maps
               
               tabPanel("Tweet Time",
                         br(),
                         p("Here are the two Bubble Charts about time of tweets based on the number of
                           retweets regarding Expedia and Priceline. It is obvious that there are more
                           retweets about Expedia than Priceline. Also, they show that weekend is a 
                           popular time of retweets, which may due to more travel time for travelers 
                           than weekdays."),
                         p("From these information, Expedia and Priceline can understand the behavior of 
                           their audience better. Then, they can adjust their schedule to post at the 
                           personalized best time through the week and the day to optimize their reach 
                           and potential impressions."), 
                        hr(),
                         sidebarLayout(
                           sidebarPanel(radioButtons(inputId = "timelineinput",
                                                    label="Select a Campnay",
                                                    choices = c("Expedia","Priceline"))),
                           mainPanel(plotlyOutput("timeplot"))
                         )),#close time
               
               tabPanel("Word Cloud",
                        br(),
                        p("Here are the word cloud for Expedia and Priceline. The words were plotted 
                          with respect to their frequencies, the bigger the words, the higher the 
                          frequency."),
                        p("The most frequent word about Expedia is expediachat, which is a successful weekly event of Expedia. It is a Q&A session moderated by a travel expert every Wednesday and sees a reliable 3,000-5,000 tweets on Q&A days. This is a great example of travel company trying to make most out of twitter. The weekly event ‘builds brand engagement by creating a community of travel lovers, all eager to ask and answer questions, and share tips on globe-trotting adventures’. On the other hand, the most frequent word about Priceline is imagination, which is interesting but not actually related to Priceline. The reason it showed up is because of a talk about “there is no innovation without imagination” given by the founder of Priceline who already left the company. This will not necessarily help Priceline build its image on Twitter, but may relate Priceline to something innovative in travelers mind."),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "wordinput",
                                       label="Select a Company",
                                       choices = c("Expedia","Priceline")),
                            sliderInput("freq", "Minimum Frequency:",
                                        min = 1,  max = 50, value = 15),
                            sliderInput("max", "Maximum Number of Words:", 
                                        min = 1,  max = 300,  value = 100)),
                          mainPanel(plotOutput("wordcloud"))
                          )), #close word cloud
               
               tabPanel("Sentiment Analysis",
                        br(),
                        p("Here are the sentiment score for tweets about Expedia and Priceline. 
                          The orange line on the graph is the mean sentiment score. From the graph,
                          we can see that Expedia has a positive average sentiment score, while
                          Priceline has a negative average sentiment score. The word cloud of
                          Priceline tells the same story. Words like stop, bad, mistake are frequently
                          used by people who tweet about Priceline. Thus, Priceline may need to 
                          pay more attention regards how to respond to travelers on Twitter
                          about these negative messages."),
                        hr(),
                        splitLayout(plotlyOutput("epsenti"), plotlyOutput("plsenti")),
                        splitLayout(plotOutput("epword"), plotOutput("plword"))
                        ) #close sentiment
             )
             ),
    
    
    tabPanel("Conclusion",
             br(),
             p("Since Expedia and Priceline are the two prominent travel websites in the 
               United States, there is always going to be a battle between them. 
               Twitter as one of the most popular social media in modern life will of course 
               be a battleground. It is hard to say who win or loss, but both travel websites 
               definitely need to utilize the information provided to analyze and predict 
               customer behavior to make themselves heard by their target customer so that 
               they can better engage with them, boost brand favorability and make influences 
               on travelers to make more money."),
             br(), 
             h4("References"),
             p("https://audiense.com/6-ways-apply-twitter-data-analysis-findings/"),
             p("https://simplymeasured.com/blog/4-social-media-branding-lessons-we-can-learn-from-expediachat/#sm.0001jwfxrh9v0cqtrjy1jw86ejzrx"),
             p("https://www.newgenapps.com/blog/the-secret-way-of-measuring-customer-emotions-social-media-sentiment-analysis"),
             p("https://sites.google.com/site/miningtwitter/questions")
             )
             ) #close navbarPage
    )
)

                    
                             
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  output$epmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  
      setView(lng=-97, lat=40, zoom=4) %>%
      addCircleMarkers(lng=epp$x, lat=epp$y, radius = 2, weight = 3,
                       color="orange", stroke = TRUE, fillOpacity = 0.7) %>%
      addLegend("topright", colors = "orange", labels = "Expedia")
}) #close epleaflet

  output$plmap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%  
      setView(lng=-97, lat=40, zoom=4) %>%
      addCircleMarkers(lng=plp$x, lat=plp$y, radius = 2, weight = 3,
                       color="blue", stroke = TRUE, fillOpacity = 0.7)%>%
      addLegend("topright", colors = "blue", labels = "Priceline")
  }) #close plleaflet
  
  output$wordcloud<-renderPlot({
    if(input$wordinput=="Expedia"){
      set.seed(1234)
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
      par(mar=rep(0, 4))
      plot.new()
      text(x=0.5, y=0.5, "Word Cloud for Expedia")
      wordcloud(ep_clean, scale=c(5,0.2), min.freq = input$freq, max.words=input$max,
                random.order=FALSE, colors=brewer.pal(9,"Reds")[5:9])
    }
    else{
      set.seed(1234)
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
      par(mar=rep(0, 4))
      plot.new()
      text(x=0.5, y=0.5, "Word Cloud for Priceline")
      wordcloud(words = pl_clean, scale=c(5,0.2), min.freq = input$freq, max.words=input$max,
                random.order=FALSE, colors=brewer.pal(8,"Dark2")[5:9])
    }
  }) #close word cloud
  
  output$epsenti <- renderPlotly({
    p1 <- ggplot(na.omit(eptweets.sentiment)) +
      geom_bar(mapping=aes(x=score, fill = sentiment0)) +
      geom_vline(xintercept = mean(eptweets.sentiment$score), linetype="dashed", size=0.7, col="orange") +
      labs(x = "sentiment score", title = "Sentiment Score for Tweets about Expedia")
    ggplotly(p1)
      }) #close epsenti
    
  output$plsenti <- renderPlotly({
    p2 <- ggplot(na.omit(pltweets.sentiment)) +
      geom_bar(mapping=aes(x=score, fill = sentiment0)) +
      geom_vline(xintercept = mean(pltweets.sentiment$score), linetype="dashed", size=0.7, col="orange") +
      labs(x = "sentiment score", title = "Sentiment Score for Tweets about Priceline")
    ggplotly(p2)
      }) #close plsenti
  
  output$epword <- renderPlot({
    eptweets.sentiment %>%
      select(word, score, sentiment0) %>% 
      count(c("word", "score", "sentiment0")) %>% 
      group_by(sentiment0) %>%
      ungroup() %>%
      mutate(word = reorder(word, freq)) %>%
      acast(word ~ sentiment0, value.var = "freq", fill = 0) %>%
      comparison.cloud(colors = brewer.pal(3, "Set2"),
                       min.freq = 10, max.words=100)
    
  }) #close epword
  
  output$plword <- renderPlot({
    pltweets.sentiment %>%
      select(word, score, sentiment0) %>% 
      count(c("word", "score", "sentiment0")) %>% 
      group_by(sentiment0) %>%
      ungroup() %>%
      mutate(word = reorder(word, freq)) %>%
      acast(word ~ sentiment0, value.var = "freq", fill = 0) %>%
      comparison.cloud(colors = brewer.pal(3, "Set2"), scale=c(5,0.2), 
                       min.freq = 10, max.words=100)
  }) #close plword
  
  
  output$timeplot<-renderPlotly({
    if(input$timelineinput=="Expedia"){
      eptweets.df$Day <- weekdays(as.Date(eptweets.df$created))
      eptweets.df$Day <- factor(eptweets.df$Day, levels = c("Saturday", "Friday","Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))
      eptweets.df$Hour <- substring(eptweets.df$created,12,13)
      eptweets.df$Hour <- as.factor(eptweets.df$Hour)
      epp2 <- ggplot(eptweets.df, aes(x = eptweets.df$Hour, y = eptweets.df$Day, 
                                      size = retweetCount, color = Day)) +
        geom_point() +
        labs(x = "Hour", y = "Day of Week", title = "Time of Tweets about Expedia")
      ggplotly(epp2)
    }
    else{
      pltweets.df$Day <- weekdays(as.Date(pltweets.df$created))
      pltweets.df$Day <- factor(pltweets.df$Day, levels = c("Saturday", "Friday","Thursday", "Wednesday", "Tuesday", "Monday", "Sunday"))
      pltweets.df$Hour <- substring(pltweets.df$created,12,13)
      pltweets.df$Hour <- as.factor(pltweets.df$Hour)
      plp2 <- ggplot(pltweets.df, aes(x = pltweets.df$Hour, y = pltweets.df$Day, 
                                      size = retweetCount, color = Day)) +
        geom_point() +
        labs(x = "Hour", y = "Day of Week", title = "Time of Tweets about Priceline")
      ggplotly(plp2)
    }
  }) #close timeplot
  
  
  })

# Run the application 
shinyApp(ui = ui, server = server)
