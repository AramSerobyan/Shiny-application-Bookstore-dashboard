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
sorted_df <- sorted_df[1:8, ]
sorted_df$top =seq(1, NROW(sorted_df), by=1)
#-----------------------------------------------------------------------------


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Trending Books")  

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
                 column(  width = 7, 
                          div(
                            
                            dataTableOutput("dataTable")
                            )
                 ),
                 column(  width = 4, 
                          plotOutput("revenuebyGenre", height = "250px")))
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
            column(width = 12, 
                   plotOutput("RevenuebyGenre", height = "242px")),
            column( width = 12,
                    radioButtons("TrendingRadio", "",
                                 choices = c("trending", "genres", "Age"), inline = TRUE))
            # column(width = 12, 
            # column(width = 12, 
            #plotOutput("RevenueVsRanking", height = "125px")),
          )
          
  ) 
  
)


body <- dashboardBody(frow1, frow2)

ui <- dashboardPage(title = 'Title', header, sidebar, body, skin='green')

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
  
  #  render data table
  #-
  output$dataTable <- renderDT(
    data.frame(top =sorted_df$top,title= sorted_df$title, average_rating =sorted_df$avg_rating), # data
    rownames = FALSE,
    selection='single',
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(lengthChange = FALSE, pageLength = 4, orderClasses=TRUE, dom = 't')
  
  )
  
  
}


shinyApp(ui, server)