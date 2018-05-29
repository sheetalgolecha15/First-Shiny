library("shiny")
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
if (!require(wordcloud)){install.packages("pdftools")}
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
  
  #nokia = readLines('/Users/sheetalgolecha/Documents/TA/Group Assignment Data files-20180501/hp.txt')
  DF  =  str_replace_all(DF, "<.*?>", "") # get rid of html junk 
  DF  =  str_replace_all(DF, '"', "") 
  DF  =  str_replace_all(DF, 'x', "") 
  # ?udpipe_download_model   # for langu listing and other details
  
  # load english model for annotation from working dir
  english_model = udpipe_load_model("./english-ud-2.0-170801.udpipe")  # file_model only needed
  
  # now annotate text dataset using ud_model above
  # system.time({   # ~ depends on corpus size
  x <- udpipe_annotate(english_model, x = DF) #%>% as.data.frame() %>% head()
  x <- as.data.frame(x)
  return(x)
  #	})  # 13.76 secs
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
                               ,"Sample data txt file"), 
                             a(href="https://github.com/sheetalgolecha15/First-Shiny/blob/master/hp.txt"
                               ,"Sample data txt file"), 
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
