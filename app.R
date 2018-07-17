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
library(shinycssloaders)
library(rsconnect)

options(scipen=999)
# Define UI 
ui <- shinyUI(fluidPage(theme=shinytheme("superhero"),
                        
          # Application title
          headerPanel("Live webscrapper from coinmarketcap.com"),
                        
          # Sidebar with a slider input for number of bins 
          sidebarPanel(
          textInput("cripto",label = h3("Type your favorite crypto coin :)")),
          actionButton("runforrest", "Go!"),
          checkboxInput("log","Work with log-variables",value = T),
          textInput("interval",label = h3("Time interval (eg: 20160531-20160630, or all time)"),value="all time"),
          # Show a plot of the coin
          selectInput("select", label = h3("Variable select"), 
                      choices = list("Opening price"=2,"Highest price" = 3, "Lowest price" = 4, "Closing price" = 5,
                                                     "Volume" = 6, "Marketcap" = 7), 
                                      selected = 4),
                                      hr(),
                      fluidRow(column(3, verbatimTextOutput("value")))),
          mainPanel(
                    tabsetPanel(type="tabs",
                    tabPanel("Real data",withSpinner(plotOutput("coinplot"))),
                    tabPanel("Adjusted data",withSpinner(plotOutput("plotadjusted"))),
                    tabPanel("Adjusted vs. real",withSpinner(plotOutput("plotadjreal")))))))

# Define server logic required to draw the plot
server <- function(input, output){
  observeEvent(input$runforrest, {
  #building database
  paginalida<-reactive({if(paste(input$interval)=="all time"){
    read_html(paste0("https://coinmarketcap.com/currencies/",
                                 input$cripto,
                                 "/historical-data/?start=20130428&end=",format(Sys.Date(),"%Y%m%d")))}else{
    read_html(paste0("https://coinmarketcap.com/currencies/",
                     input$cripto,
                     "/historical-data/?start=",substr(input$interval,1,8),"&end=",substr(input$interval,10,17)))}})
    moeda<- reactive({data.frame('Day'=paginalida()%>%html_nodes("tbody tr *.text-left")%>%html_text() %>%
                      as.character() %>%
                      gsub("Feb","Fev",.) %>%
                      gsub("Apr","Abr",.) %>%
                      gsub("May","Mai",.) %>%
                      gsub("Aug","Ago",.) %>%
                      gsub("Sep","Set",.) %>%
                      gsub("Oct","Out",.) %>%
                      gsub("Dec","Dez",.) %>%
                      as.Date(.,format="%b %d, %Y"))%>%
    mutate('Opening price(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(2)") %>%html_text() %>%
             as.numeric()) %>%    
    mutate('Highest price(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(3)")   %>%html_text() %>%
             as.numeric()) %>%
    mutate('Lowest price(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(4)")   %>%html_text() %>%
             as.numeric())%>%
    mutate('Closing price(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(5)")%>%html_text() %>%
             as.numeric())%>%
    mutate('Volume(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(6)")      %>%html_text() %>%
             gsub(",","", .)%>%
             as.numeric())%>%
    mutate('Marketcap(USD)'=paginalida()%>%html_nodes("tbody tr td:nth-child(7)") %>%html_text() %>%
             gsub(",","", .)%>%
             as.numeric())%>%
        arrange(Day) %>%
        na.omit()
      })
    
  output$coinplot <- renderPlot({
                                 ggplot(data=moeda(), aes(Day)) + 
                                        geom_line(aes(y=moeda()[,as.numeric(input$select)])) + 
                                        theme_economist() + 
                                        theme(axis.title.x = element_blank())+
                                        labs(y="USD")+
                                        ggtitle(paste0("Time series of"," ",input$cripto)
                                                )
                                 })
  
  valores.ajustados <- reactive(if(input$log==FALSE){
                                hyperblm(`Closing price(USD)`~ `Highest price(USD)`+
                                                            `Lowest price(USD)`+
                                                            `Volume(USD)`+
                                                            `Marketcap(USD)`,
                                                             data=moeda()
                                          )$fitted.values
                                                     }else{
                                hyperblm(log(`Closing price(USD)`)~ log(`Highest price(USD)`)+
                                                                  log(`Lowest price(USD)`)+
                                                                  log(`Volume(USD)`)+
                                                                  log(`Marketcap(USD)`),
                                                                data=moeda()
                                         )$fitted.values                  
                                                           }
                                )
                      
                     
  DATA<-reactive(data.frame(
                            rbind(data.frame('dia'=1:nrow(moeda()),
                                             'id'=rep("reais",nrow(moeda())),
                                             'vetor'=if(input$log==F){moeda()$`Closing price(USD)`}else{
                                                                  log(moeda()$`Closing price(USD)`)
                                                                                                        }
                                                    )
                                             
                                  ,
                                  data.frame('dia'=1:nrow(moeda()),
                                             'id'=rep("ajustados",nrow(moeda())),
                                             'vetor'=valores.ajustados()
                                             )
                                  )
                            )
                  )
                 
                 
  
  output$plotadjusted <- renderPlot(
                                    ggplot(data=(filter(DATA(),id=="ajustados")), aes(x=dia, y=vetor, group=id, colour=id))+
                                           geom_line(size=1.15) + 
                                           theme_economist() + 
                                           labs(x="Number of days",y="Prices") + 
                                           scale_colour_manual(values = c("Yellow"),
                                                               labels = c("Adjusted")
                                                               )+
                                           ggtitle(paste0("Adjusted data:"," ",input$cripto))
                                              
                                    )
  output$plotadjreal <- renderPlot(
                                   ggplot(data=DATA(), aes(x=dia,y=vetor,group=id,colour=id)) + 
                                           geom_line(size=1.15) + 
                                           theme_economist() + 
                                           theme(axis.title.x = element_blank())+
                                           labs(y="",x="Number of days")+
                                           scale_colour_manual(values = c("Red","Yellow"), 
                                                         labels = c("Real", "Adjusted"))+
                                           ggtitle(paste0("Real data vs adjusted:"," ",input$cripto))
    
                                    
      )
  
})  }

# Run the application 
shinyApp(ui = ui, server = server)

