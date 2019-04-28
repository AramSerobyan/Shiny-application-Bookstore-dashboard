# load the required packages
library(RCurl)
library(shiny)
#install.packages("shinydashb")
library("DT")
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(gridExtra)

#install.packages('tidyverse')
library(tidyverse)
#install.packages('rvest')
library(rvest)
#install.packages('magrittr')
library(magrittr)
#install.packages('ggmap')
library(ggmap)
#install.packages('stringr')
#library(stringr)



varY <- getURL("https://raw.githubusercontent.com/AramSerobyan/Fork/master/publishers.csv")
books  <- read.csv(text = varY)


# # # A Rough Draft.

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Books: The State of the art")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar( 
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("To Be Changed", tabName = "dashboard", icon = icon("dashboard"))
  ), disable = TRUE
)


frow1 <- fluidRow(
  splitLayout( cellWidths = c( "80%","50%"),
               fluidRow( 
                 column(  width = 1,radioButtons("radio", "",
                      choices= c("month","year","all time"))),
                 column(  width = 4, 
                          plotOutput("revenuebyGenre", height = "250px")
                          ),
                 column(  width = 7, 
                          plotOutput("revenuebyTest", height = "250px")))
               #,plotOutput("revenuebyGenre", height = "250px")
               , column(  width = 12,  tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                          "Total Sale", fluidRow( width = 12,
                                    valueBoxOutput("value1")) ,
                          "Total Reads",
                          fluidRow( width = 12,
                                   valueBoxOutput("value2")),
                          "Reviews",
                          fluidRow( width = 12,
                                  valueBoxOutput("value3"))))
  )

frow2 <- fluidRow(
  
  column( width = 6,
    plotOutput("GeographicData", height = "300px")
  ),
  
  column( width = 6,
          fluidRow(
             column( width = 12,
                     radioButtons("TrendingRadio", "",
                                  choices = c("trending", "genres", "age"), inline = TRUE)),
                         # column(width = 12, 
                  # column(width = 12, 
                          #plotOutput("RevenueVsRanking", height = "125px")),
                          column(width = 12, 
                        plotOutput("RevenueVsRanking", height = "240px")))
          
  ) 
  
)


body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(title = 'Title', header, sidebar, body, skin='green')

server <- function(input, output) { 
  
  
  ##output$value2 <- renderValueBox({
    
    #valueBox(
    #  formatC(total.revenue, format="d", big.mark=',')
     # ,'Total Expected Revenue'
    #  ,icon = icon("gbp",lib='glyphicon')
   #   ,color = "green")
    
  #})
  
  output$value1 <- renderValueBox({
    
    valueBox(
        145 , "",color = "yellow", width = NULL)
    
  }) 
  
  output$value2 <- renderValueBox({
    
    valueBox(
      100 , "",color = "purple", width = 12)
    
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
       25 , "",color = "green", width = 12)
    
  })
  
  #creating the plotOutput content
  ## Currently a different plot to be replaced.
  output$RevenueVsRanking<- renderPlot({
    ggplot(data = books, 
           aes(x=amazon.revenue, y=average.rating, color=factor(genre))) + 
      geom_point() + ylab("amazon rating") + 
      xlab("amazon revenue") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue compared to rating") + labs(color = "Genre")
  })
  
  output$revenuebyGenre <- renderPlot({
    ggplot(data = books, 
           aes(x=genre, y=units.sold)) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Units Sold") + 
      xlab("Genre") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Genre") 
  })
  
  output$revenuebyTest <- renderPlot({
    ggplot(data = books, 
           aes(x=genre, y=units.sold)) + 
      geom_bar(position = "dodge", stat = "identity", fill = "green") + ylab("Units Sold") + 
      xlab("Genre") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=10, face="bold")) + 
      ggtitle("Trending Books") 
  })
  
  output$revenuebyTest2 <- renderPlot({
    ggplot(data = books, 
           aes(x=genre, y=units.sold)) + 
      geom_bar(position = "dodge", stat = "identity", fill = "blue") + ylab("Units Sold") + 
      xlab("Genre") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=10, face="bold")) + 
      ggtitle("Rating Rates") 
  })
  
  output$revenuebyTest3 <- renderPlot({
    ggplot(data = books, 
           aes(x=genre, y=units.sold)) + 
      geom_bar(position = "dodge", stat = "identity", fill = "blue") + ylab("Units Sold") + 
      xlab("Genre") + theme(legend.position="bottom" 
                            ,plot.title = element_text(size=10, face="bold")) + 
      ggtitle("User Stats") 
  })
  
  map.world <- map_data("world")
  
  df.country_points <- data.frame("lon" = c(135, -100, 110, 26), "lat" = c(-22, 49.815273, 58, 12))
  df.country_names <- data.frame("lon" = c(135, -100, 110, 26), "lat" = c(-30, 41.815273, 50, 4))
  
  output$GeographicData <- renderPlot({
    ggplot() +
     geom_polygon(data = map.world, aes(x = long, y = lat, group = group)) +
     geom_text(data = df.country_points, aes(x = lon, y = lat, label = c('20%', '11%', '25%', '2%')), col = 'red')+
      geom_text(data = df.country_names,
               aes(x = lon, y = lat, label = c('Australia', 'America', 'Russia', 'Africa')), col = 'red', size = 3)
    
  })

}


shinyApp(ui, server)
