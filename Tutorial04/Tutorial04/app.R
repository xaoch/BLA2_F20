#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(visNetwork)
library(igraph)
library(tidytext)
library(wordcloud2)

forum_data<-read.csv('ClimateForum15.csv',stringsAsFactors=FALSE)

nodes_authors =
    forum_data %>%
    group_by(author_id) %>%
    summarize(
        username=last(author_username),
        posts=n(),
        thread_started=length(X_type[X_type %in% c('CommentThread')]),
        votes_up=sum(votes_up_count),
        comments=sum(comment_count[!is.na(comment_count)])
    )

nodes_authors$thread_initiatior<-ifelse(nodes_authors$thread_started>0,"Initiator","Commenter")

links_posts =
    forum_data %>%
    filter(X_type %in% c('Comment')) %>%
    select(author_id,mongoid,parent_ids,comment_thread_id) %>%
    mutate(parent=ifelse(parent_ids=="",as.character(comment_thread_id),as.character(parent_ids))) %>%
    select(author_id,mongoid,parent)

get_user_from_post = function(post_id){
    forum_data[forum_data$mongoid==post_id,]$author_id
}

links_posts$author_parent<-sapply(links_posts$parent,get_user_from_post)

weighted_links = links_posts %>%
    group_by(author_id,author_parent) %>%
    summarize(
        weight=n()
    )


text_df = tibble(messageId = forum_data$mongoid, text = forum_data$body)

tokenized= text_df %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

wordFreq = tokenized %>%
    anti_join(stop_words) %>%
    count(word) %>%
    filter(n>30)

sentiments = 
    tokenized %>%
    inner_join(get_sentiments("bing")) %>% 
    count(messageId, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)

get_sentiment=function(id){
    author_messages<- forum_data %>%
        filter(author_id %in% c(id))
    author_sentiments<- sentiments %>%
        filter(messageId %in% author_messages) %>%
        summarize(
            count=sum(sentiment)
        )
    author_sentiments$count
}

nodes_authors$sentiment=sapply(nodes_authors$author_id,get_sentiment)
nodes_authors$sentiment = cut(nodes_authors$sentiment, breaks=c(-Inf, -5, 0, 5, +Inf), labels=c('Very Negative','Negative','Positive','Very Positive'))

g<-graph_from_data_frame(weighted_links, directed = TRUE, vertices = nodes_authors)

size_options = list("Number of Posts"="posts",
                    "Threads Started"="thread_started",
                    "Up-votes"="votes_up",
                    "Number of Comments"="comments")

color_options = list("None"="none",
                     "Thread Initiators"="thread_initiatior",
                     "Community"="community",
                     "Sentiment"="sentiment")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Discourse Analytics"),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
        tabBox(height = "1100px", width = "1000px",
               
               tabPanel(title = tagList(icon("project-diagram", 
                                             class = "fas fa-project-diagram"),
                                        "NETWORK"),
                        fluidRow(
                        box(title = "Controls", width=4, status = "primary", solidHeader=TRUE,
                            selectInput("size", label = "Size represents:", choices = size_options, selected = "posts"),
                            selectInput("color", label = "Color represents:", choices = color_options, selected = "none"),
                            sliderInput("posts", label = "Number of Posts", min = min(nodes_authors$posts), max = max(nodes_authors$posts), value = c(min(nodes_authors$posts), max(nodes_authors$posts)))
                            ),
                    
                        
                        box(title = "Network", width=8, status = "primary", visNetworkOutput("network"))
                        )
                        ),
               tabPanel(title = tagList(icon("cloud", 
                                             class="fas fa-cloud"),
                                        "WORD CLOUDS"),
                        fluidRow(
                            box(title = "Controls", width=4, status = "primary", solidHeader=TRUE,
                                selectInput("ngram", label = "Use N-grams of Size:", choices = c(1,2,3), selected = 1),
                                selectInput("author", label = "By author:", choices = c("All",nodes_authors[nodes_authors$posts>10,]$username), selected = "All")
                            ),
                            
                            
                            box(title = "Word Cloud", width=8, status = "primary", wordcloud2Output("wordcloud"))
                        )
                        
                        
               )
               
               
        )
        # Second tab content
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$network<- renderVisNetwork({
        
        selected_authors=nodes_authors[nodes_authors$posts %in% seq(input$posts[1],input$posts[2]),]$author_id
        g2 = induced_subgraph(g, as.character(selected_authors))
        cfg = cluster_fast_greedy(as.undirected(g2))
        V(g2)$community = cfg$membership
        graph_data <- toVisNetworkData(g2)
        
        graph_data$nodes$label=graph_data$nodes$username
        graph_data$nodes$value=graph_data$nodes[[input$size]]
        graph_data$nodes$group=graph_data$nodes[[input$color]]
        graph_data$edges$width=graph_data$edges$weight
        
        visNetwork(nodes = graph_data$nodes, edges = graph_data$edges, height = "500px") %>%
            visIgraphLayout(randomSeed = 123) %>%
            visNodes(color = list(background = "lightblue", 
                                  border = "darkblue",
                                  highlight = "yellow")) %>%
            visOptions(highlightNearest = list(enabled = T, degree=0, hover = F), 
                       nodesIdSelection = T) %>%
            visLegend()
    })
   
    output$wordcloud<-renderWordcloud2({
        
        if(input$author=="All"){
            text_df_selected=text_df
            limitOne=30
            limitTwo=10
            limitThree=5
        }
        else{
            author_messages= forum_data %>%
                filter(author_username %in% c(input$author))
            text_df_selected=text_df%>%filter(messageId %in% author_messages$mongoid)
            limitOne=1
            limitTwo=1
            limitThree=1
        }
        
        if(input$ngram==1){
            tok= text_df_selected %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words)
            wordFreq = tok %>%
                count(word) %>%
                filter(n>limitOne)
        }
        if(input$ngram==2){
            bigrams = text_df_selected %>%
                unnest_tokens(bigram, text, token = "ngrams", n = 2)
            
            bigrams %>%
                count(bigram, sort = TRUE)
            
            bigrams_separated = bigrams %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            
            bigrams_filtered = bigrams_separated %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word2 %in% stop_words$word)
            
            
            bigram_counts = bigrams_filtered %>% 
                count(word1, word2, sort = TRUE)
            
            tok = bigrams_filtered %>%
                unite(word, word1, word2, sep = " ")
            wordFreq = tok %>%
                count(word) %>%
                filter(n>limitTwo)
        }
        if(input$ngram==3){
            tok=text_df_selected %>%
                unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
                separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
                filter(!word1 %in% stop_words$word,
                       !word2 %in% stop_words$word,
                       !word3 %in% stop_words$word) %>%
                count(word1, word2, word3, sort = TRUE)
            tok = tok %>%
                unite(word, word1, word2, word3, sep = " ")
            wordFreq = tok %>%
                filter(n>limitThree)
        }
        
        
        wordcloud2(data = wordFreq, size = 1)
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
