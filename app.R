library(shinythemes)
library(RcppArmadillo)
library(shiny)
library(rvest)
library(httr)
library(tidyverse)
library(PerformanceAnalytics)
library(reshape2)
library(ggthemes)
library(GeneralizedHyperbolic)
library(forecast)
library(rsconnect)

options(scipen=999)
# Define UI 
ui <- shinyUI(fluidPage(theme=shinytheme("superhero"),
                        
                        # Application title
                        headerPanel("Time series data from coinmarketcap.com"),
                        
                        # Sidebar with a slider input for number of bins 
                        mainPanel(
                          textInput("cripto",label = h3("Type your favorite crypto coin :)"))
                          # Show a plot of the coin
                          ,
                          selectInput("select", label = h3("Variable select"), 
                                      choices = list("Highest price" = 3, "Lowest price" = 4, "Closing price" = 5,
                                                     "Volume" = 6, "Marketcap" = 7), 
                                      selected = 4),
                          
                          hr(),
                          fluidRow(column(3, verbatimTextOutput("value"))),
                          plotOutput("coinplot")
                        )
))

# Define server logic required to draw the plot
server <- function(input, output){
  
  output$coinplot <- renderPlot({
    titulo.moeda <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_node("title") %>%
      html_text()
    preco.dia <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr *.text-left") %>%
      html_text() %>%
      as.character() %>%
      gsub("Feb","Fev",.) %>%
      gsub("Apr","Abr",.) %>%
      gsub("May","Mai",.) %>%
      gsub("Aug","Ago",.) %>%
      gsub("Sep","Set",.) %>%
      gsub("Oct","Out",.) %>%
      gsub("Dec","Dez",.) %>%
      as.Date(.,format="%b %d, %Y")%>%
      as.data.frame()
    
    preco.abertura <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(2)") %>%
      html_text() %>%
      as.numeric()%>%
      as.data.frame()
    
    preco.maximo <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(3)") %>%
      html_text() %>%
      as.numeric()%>%
      as.data.frame()
    
    preco.minimo <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(4)") %>%
      html_text() %>%
      as.numeric()%>%
      as.data.frame()
    
    preco.fechamento <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(5)") %>%
      html_text() %>%
      as.numeric()%>%
      as.data.frame()
    
    volume.dia <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(6)") %>%
      html_text() %>%
      { gsub(",","", .) } %>%
      as.numeric()%>%
      as.data.frame()
    
    
    capitaliz.mercado <- read_html(paste0("https://coinmarketcap.com/currencies/",as.character(input$cripto),"/historical-data/?start=20130428&end=20180424"))%>%
      html_nodes("tbody tr td:nth-child(7)") %>%
      html_text() %>%
      { gsub(",","", .) } %>%
      as.numeric()%>%
      as.data.frame()
    
    moeda <- cbind(preco.dia,preco.abertura,preco.maximo,preco.minimo,preco.fechamento,volume.dia,capitaliz.mercado)%>%
      `colnames<-`(c("Day","Opening price(USD)", "Highest price(USD)", "Lowest price(USD)", "Closing price(USD)", "Volume(USD)", "Marketcap(USD)"))
    
    nomedacolunaescolhida <- as.character(colnames(moeda)[as.numeric(input$select)])
    ggplot(moeda, aes(Day)) + geom_line(aes(y=moeda[,as.numeric(input$select)])) + theme_economist() + theme(axis.title.x = element_blank())+labs(y=nomedacolunaescolhida)+ggtitle(titulo.moeda)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

