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

library(gganimate)
library(DT)

varY <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/books_data-2.csv")
books  <- read.csv(text = varY)

genres <- books$genres
genres <- as.character(genres)
genre_unlist <- unlist(strsplit(genres, split = ","))
genre_unlist <- trimws(genre_unlist)
genre_unlist <- as.factor(genre_unlist)

genre_table <- sort(table(genre_unlist), decreasing = TRUE)

random_genres <- c("Journalism", "Cats", "Canada", "Epic", "Comics", "Biography", "Young Adult")

random_genres_table <- genre_table[random_genres]

df <- data.frame(random_genres_table)

plot <- ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title = "Genre popularity",
       x = "Genre",
       y = "Books in that genre") +
  geom_text(aes(label=Freq), vjust=-0.3, size=3.5) +
  theme_minimal()


## DAta Table
sorted_df <- books[order(-books$num_ratings), ]
sorted_df <- sorted_df[1:800, ]
sorted_df$top =seq(1, NROW(sorted_df), by=1)
#-----------------------------------------------------------------------------


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Trending Books")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar( 
  sidebarMenu(
    menuItem("", tabName = "", icon = icon("dashboard"))
    #  ,menuItem("To Be Changed", tabName = "dashboard", icon = icon("dashboard"))
  ), width = "60px", disable = FALSE
)


frow1 <- fluidRow(
  splitLayout( cellWidths = c( "80%","50%"),
               fluidRow( 
                 #column(  width = 1,radioButtons("radio", "",
                 #   choices= c("month","year","all time"))),
                 column( width = 1, ""),
                 column(  width = 7, 
                          div(dataTableOutput("dataTable"))
                 ),
                 column(  width = 4, 
                          plotOutput("GeographicData", height = "250px")))
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
frow2 <- fluidRow( column( width = 6, 
                           plotOutput("revenuebyGenre", height = "320px")),
                   
                   column( width = 6,
                           column( width = 12, radioButtons("TrendingRadio", "",
                                                            choices = c("trending", "genres"), inline = TRUE )),
                           column( width = 12, plotOutput("revenuebyGenre3", height = "132px")),
                           column( width = 12, plotOutput("revenuebyGenre2", height = "132px"))
                   )
)
#frow2 <- fluidRow(
# splitLayout( column( width = 6, plotOutput("RevenuebyGenre"),
#              column( width = 6, plotOutput("RevenuebyGenre2")   
#                      )
#)
#))
#frow2 <- fluidRow(
# column( width = 1, plotOutput("RevenuebyGenre")
#  ),
#column( width = 11,
#fluidRow( 
#column(width = 12, 
#       plotOutput("RevenuebyGenre"))
#column( width = 12, radioButtons("TrendingRadio", "",
#                  choices = c("trending", "genres", "Age"), inline = TRUE))
# column(width = 12, 
# column(width = 12, 
#plotOutput("RevenueVsRanking", height = "125px")),
#)
#)
#)


body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(title = 'Title', header, sidebar, body, skin='green')

#skin='green')

server <- function(input, output,session) { 
  
  
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
  
  
  
  #  render data table
  #-
  output$dataTable <- renderDT(
    data.frame(top =sorted_df$top,title= sorted_df$title, average_rating =sorted_df$avg_rating), # data
    rownames = FALSE,
    selection='single',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(lengthChange = FALSE,pageLength = 4, orderClasses=TRUE, dom = 't')
  )
  
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
  
  output$revenuebyGenre3 <- renderPlot({ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Genre popularity",
           x = "Genre",
           y = "Books in that genre") +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5)})
  
  output$revenuebyGenre2 <- renderPlot({ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Genre popularity",
           x = "Genre",
           y = "Books in that genre") +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5)})
  
  output$revenuebyGenre <- renderPlot({ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Genre popularity",
           x = "Genre",
           y = "Books in that genre") +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5)})
}


shinyApp(ui, server)