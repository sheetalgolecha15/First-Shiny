library("shiny")
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
if (!require(pdftools)){install.packages("pdftools")}
if (!require(tidyverse)){install.packages("tidyverse")}
#install.packages("pdftools")
library(udpipe)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library(pdftools)
library(tidyverse)


read_File <- function(DF) {
  
  DF  =  str_replace_all(DF, "<.*?>", "") # get rid of html junk 
  DF  =  str_replace_all(DF, '"', "") 
  DF  =  str_replace_all(DF, 'x', "") 
 
  english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")  # file_model only needed
  
  x <- udpipe_annotate(english_model, x = DF) #%>% as.data.frame() %>% head()
  x <- as.data.frame(x)
  return(x)

}
# Define ui function
ui <- shinyUI(
  fluidPage(
    
    titlePanel("Text Analytics"),
    
    sidebarLayout( 
      
      sidebarPanel(  
        selectInput("type", "Choose a file type:",
                    choices = c("TXT", "PDF")),
        fileInput("file", "Upload data (txt or pdf file)"),
        selectInput("WC", "Choose word Cloud Type:",
                    choices = c("VERB", "NOUN","PROPN")),  
        
        downloadButton("downloadData", "Download the complete dataframe here"),
        numericInput('Freq', 'Frequency of words', 15,
                     min = 1, max = 100     ),
        numericInput('Size', 'Size of word cloud', 100,
                     min = 1, max = 300)    ,
        
        selectInput("COOC", "Parts OF Speech for Co-occurence", c("VERB","NOUN","ADJ","ADV","PROPN"), selected = c("VERB","ADJ","NOUN"), multiple = TRUE,
                    selectize = TRUE, width = NULL, size = NULL),
        numericInput('word', 'sentiment analysis word filter', 5,
                     min = 1, max = 100)   ),
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Overview",
                             h4(p("Data input")),
                             p("This apps only supports txt or pdf file which will help to read the text data and analyse it. once you upload the file it atleat takes 2-3 mins to process depending on file size",align="justify"),
                             p("Please refer to the link below for sample csv file."),
                             a(href="https://github.com/sheetalgolecha15/First-Shiny/blob/master/hp.txt"
                               ,"Sample data txt file\n"), 
                             br(),
                             a(href="https://github.com/sheetalgolecha15/First-Shiny/blob/master/G.pdf"
                               ,"Sample data pdf file"), 
                             br(),
                             h4('How to use this App'),
                             p('To use this app, click on', 
                               span(strong("Upload data (csv file with header)")),
                               'and uppload the csv data file. You can also change the number of clusters to fit in k-means clustering')),
                    tabPanel("Data Frame", 
                             tableOutput('DF')),
                    
                    tabPanel("Word Cloud",
                             plotOutput('Word_Cloud')),
                    
                    tabPanel("Co-occurance",
                             plotOutput('Cooccurence')),
                    tabPanel("Sentiment",
                             plotOutput('Sentiment'))
                    
        ) # end of tabsetPanel
      )# end of main panel
    ) # end of sidebarLayout
  )  # end if fluidPage
) # end of UI


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