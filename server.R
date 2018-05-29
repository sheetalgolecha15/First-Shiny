server <- shinyServer(function(input, output) {
  
  Dataset <- reactive({
    if (is.null(input$file)) { return(NULL) }
    else{
      if (input$type=="TXT"){
        Data <- readLines(input$file$datapath)}
      else{
        Data <- pdf_text(input$file$datapath)
      }
      Data1<-read_File(Data)
      Data1<-  subset( Data1, select = -sentence )
      colnames(Data1)[5] <- "word"
      return(Data1)
    }
  })
  
  output$DF = renderTable({ 
    X<-Dataset()
    
    head(X,100) 
    
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$file,"_Dataframe", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(Y, file, row.names = FALSE)
    }
  )
  
  WC_type <- reactive({
    all_word = Dataset() %>% subset(., upos %in% input$WC) 
    top_word = txt_freq(all_word$lemma)  # txt_freq() calcs noun freqs in desc order
    
    words_cloud<-top_word[top_word$freq>input$Freq,]
    cloud<-words_cloud  %>% with(wordcloud(words_cloud$key, words_cloud$freq, max.words = input$Size))
    return(cloud)
  })
  
  
  
  output$Word_Cloud = renderPlot({
    WC_type()
    #WC_type()  %>% with(wordcloud(WC_type()$key, WC_type()$freq, max.words = size_wc()))
    
  })
  
  Cooc <- reactive({
    nokia_cooc <- cooccurrence(   	# try `?cooccurrence` for parm options
      x = subset(Dataset(), upos %in% input$COOC), 
      term = "lemma", 
      group = c("doc_id", "paragraph_id", "sentence_id"))
    
    wordnetwork <- head(nokia_cooc, 50)
    wordnetwork <- igraph::graph_from_data_frame(wordnetwork) # needs edgelist in first 2 colms.
    
    cooc_graph<-ggraph(wordnetwork, layout = "fr") +  
      
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "orange") +  
      geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
      
      theme_graph(base_family = "Arial Narrow") +  
      theme(legend.position = "none") +
      
      labs(title = "Cooccurrences within 3 words distance", subtitle = input$COOC)
    return(cooc_graph)
  })
  
  output$Cooccurence = renderPlot({
    Cooc()
    #WC_type()  %>% with(wordcloud(WC_type()$key, WC_type()$freq, max.words = size_wc()))
    
  })
  
  output$Sentiment =renderPlot({
    Senti()
  })
  
  Senti <- reactive({
    
    bing_word_counts <- Dataset() %>%
      
      inner_join(bing) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    #bing_word_counts
    bing_word_counts %>%
      filter(n > input$word) %>%
      mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("Contribution to sentiment")
  })
  
})

# Now call shinyApp function
shinyApp(ui = ui, server = server)