library(ggplot2)
library("reshape2")
library(memoise)
source("helper.R")

#options(shiny.trace=TRUE)

shinyServer(function(input, output, session) {

  selectedAttributes <- reactive({
    c = if (is.null(input$attributes)) c("timedelta") else input$attributes
    return (c)
  })
  
  articleWithTypeFiltered <- reactive({
    #print(input$typeCheckGroup)
    return (filter(articles.reduced, type %in% input$typeCheckGroup))
  })
  
  output$plot_author <- renderPlot({
    
    ggplot(articles.top.authors, aes(factor(author), shares, fill = factor(author))) +
      geom_boxplot() + geom_point() +  scale_y_log10() + xlab("Author") + ylab("log10(shares)")
    
    #ggplot(articles.top.authors, aes(factor(author), fill = factor(author))) +
    #  geom_bar() 
    
  })
  
#   output$plot_author_by_shares <- renderPlot({
#     
#     ggplot(articles.top.authors.by.shares, aes(factor(author), shares, fill = factor(author))) +
#       geom_boxplot() + geom_point() +  scale_y_log10() + xlab("Author") + ylab("log10(shares)")
#     
#   })
  
#   article.shares.vs.preditive <- reactive({
#     #print(input$isScaled)
#     
#     if(input$isScaled)
#      return (articles.scaled)
#     else
#      return (articles.numeric)
#     
#   })
  
  
  output$plot_channels <- renderPlot({
    articles.filtered = articleWithTypeFiltered()
    
    articles.filtered$channel = factor(articles.filtered$channel, 
                                       levels=names(sort(table(articles.filtered$channel), decreasing=TRUE))
                                       )
    
    ggplot(articles.filtered, aes(channel, fill=channel)) + geom_bar() 
  })
  
  output$plot_shares_density <- renderPlot({
    
    # Histogram overlaid with kernel density curve
    ggplot(articles.reduced, aes(x = shares)) + 
      geom_histogram(aes(y=..density..),
                     #binwidth=10,
                     colour="black", fill="white") +
    geom_density(alpha=.5, fill="#FF6666") +
    xlim(0, 5000) +
    ggtitle("Shares Range and Density")
    
  })
  
  output$plot_shares_over_time <- renderPlot({
    ggplot(articles.reduced, aes(x = as.Date(post_date), y = shares)) + geom_point() + ggtitle("Shares Over Time")
  })
  
  output$plot_shares_vs_rest <- renderPlot({
    
      #plot_share = plot(article.shares.vs.preditive())
    
    cormat <- round(cor(articles.numeric),2)
    
    cormat <- reorder_cormat(cormat)
    upper_tri <- get_upper_tri(cormat)
    
    # Melt the correlation matrix
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    
    plot_share = ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Correlation") +
      theme_minimal()+ 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 12, hjust = 1))
      #coord_fixed()
    
    plot_share = plot_share + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))
    
#     plot_share = ggplot(article.shares.vs.preditive(), aes(y = shares))
# 
#     for(selectedAttribute in selectedAttributes()) {
#       if(selectedAttribute == 'content_sentiment_polarity')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = content_sentiment_polarity, colour = "content_sentiment_polarity"))
#       else if(selectedAttribute == 'content_subjectivity')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = content_subjectivity, colour = "content_subjectivity"))
#       else if(selectedAttribute == 'n_tokens_content')
#         plot_share = plot_share + geom_point( aes(x = n_tokens_content, colour = "n_tokens_content"))
#       else if(selectedAttribute == 'n_tokens_title')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = n_tokens_title, colour = "n_tokens_title"))
#       else if(selectedAttribute == 'num_hrefs')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = num_hrefs, colour = "num_hrefs"))
#       else if(selectedAttribute == 'num_imgs')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = num_imgs, colour = "num_imgs"))
#       else if(selectedAttribute == 'num_keywords')
#         plot_share = plot_share + geom_point(aes(x = num_keywords, colour = "num_keywords"))
#       else if(selectedAttribute == 'num_videos')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = num_self_hrefs, colour = "num_videos"))
#       else if(selectedAttribute == 'title_sentiment_polarity')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = title_sentiment_polarity, colour = "title_sentiment_polarity"))
#       else if(selectedAttribute == 'title_subjectivity')
#         plot_share = plot_share + geom_smooth(se=FALSE, aes(x = title_subjectivity, colour = "title_subjectivity"))
#     }
#     
#     plot_share = plot_share + guides(col = guide_legend("Other Variables"))
    #plot_share = plot_share + xlim(-1, 1) 
    
    return (plot_share)
  })
  
  # Using "memoise" to automatically cache the results
  getTermMatrix <- memoise(function(text) {
    #text <- wordCloundText
    
    myCorpus = Corpus(VectorSource(text))
    myCorpus = tm_map(myCorpus, content_transformer(tolower))
    myCorpus = tm_map(myCorpus, removePunctuation)
    myCorpus = tm_map(myCorpus, removeNumbers)
    myCorpus = tm_map(myCorpus, removeWords,
                      c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))
    
    myDTM = TermDocumentMatrix(myCorpus,
                               control = list(minWordLength = 1))
    
    m = as.matrix(myDTM)
    
    sort(rowSums(m), decreasing = TRUE)
  })
  
  
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    # Change when the "update" button is pressed...
    
    if(input$hotSelection == 'topic')
      wordCloundText =  all.topics.text
      #return (topic.counts)
    else
      wordCloundText = all.titles.text
    
    # ...but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(wordCloundText)
      })
    })
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$plot_topics <- renderPlot({
    
    v <- terms()
    
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = 1, max.words=100,
                  colors=brewer.pal(8, "Dark2"))
  })
  
  
  
  output$table_word_count <- DT::renderDataTable({
    
    wordCounts = as.data.frame(terms())
    wordCounts$word = rownames(wordCounts)
    wordCounts$total = wordCounts[,c(1)]   #select(wordCounts, word, total = wordCounts)
    row.names(wordCounts)<-NULL
    wordCounts = select(wordCounts, word, total)
    
    #print(head(wordCounts))

    DT::datatable(head(wordCounts, 20), options = list(searching = FALSE, paging = FALSE))
  })
  
   output$table <- DT::renderDataTable({
     DT::datatable(articles[,-4], options = list(searching = TRUE, paging = TRUE))
   })
  
})