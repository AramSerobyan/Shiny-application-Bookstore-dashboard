# load the required packages
library(RCurl)
library(shiny)
library("DT")
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(DT)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(XML)

library(tidyverse)
library(ggforce)
library(scales)
library(shinyWidgets)
library(ggpubr)
library(cowplot)

user_info_url <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/user-info.csv")
user_info <- read.csv(text = user_info_url, header = TRUE)

books_url <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/book-info.csv")
books <- read.csv(text = books_url, header = TRUE)

#user_info <- read.csv("user-info.csv", header = TRUE)
#books <- read.csv("book-info.csv", header = TRUE)

books$title <- as.character(books$title)
user_info$title <- as.character(user_info$title)


books$ratings_count <- books$rating_1 + books$rating_2 + books$rating_3 + books$rating_4 + books$rating_5

genres <- books$genres
genres <- as.character(genres)
books$genres <- genres
genre_unlist <- unlist(strsplit(genres, split = ","))
genre_unlist <- trimws(genre_unlist)
genre_unlist <- as.factor(genre_unlist)

genre_table <- sort(table(genre_unlist), decreasing = TRUE)

#remove $ sign
books$price <- gsub("\\$", "", books$price)
books$price <- gsub(" .*", "", books$price)
books$price <- as.numeric(books$price)


## DAta Table
sorted_books <- books[order(-books$ratings_count), ]
sorted_books$top =seq(1, NROW(sorted_books), by=1)
#-----------------------------------------------------------------------------




#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Trending Books")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar( 
  sidebarMenu(
    menuItem("", tabName = "", icon = icon("dashboard"))
    #  ,menuItem("To Be Changed", tabName = "dashboard", icon = icon("dashboard"))
  ), width = "60px", disable = TRUE
)


frow1 <- fluidRow(id = "firstRow",
    column(width = 5,
            fluidRow(id = "trending_books", width = 12, "Books"),
            div(dataTableOutput("dataTable"))),
    #column(width = 1, ""),
    column( width = 4, 
      plotOutput("review_plot")),
    column(width = 3,
           fluidRow(id = "info_name", width = 12, "Total Sales (USD Millions)"),
           fluidRow(id = "info_value", width = 12, valueBoxOutput("value1", 12)),  
           fluidRow(id = "info_name", width = 12, "Total Reads (Thousand)"),
           fluidRow(id = "info_value", width = 12,valueBoxOutput("value2", 12)),
           fluidRow(id = "info_name", width = 12, "Reviews (Thousand)"),
           fluidRow(id = "info_value", width = 12,valueBoxOutput("value3", 12)))
)
frow2 <- fluidRow( column( width = 5, 
                           plotOutput("country_plot", height = "320px")),
                   column( width = 7,
                           column( width = 12, plotOutput("age_plot", height = "160px")),
                           column( width = 12, plotOutput("genre_plot", height = "160px"))
                   )
)

body <- dashboardBody(frow1, frow2,
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
                      ))

ui <- dashboardPage( title = 'Title', header, sidebar, body, skin='green')

server <- function(input, output,session) { 
  
  output$value1 <- renderValueBox({
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    book = sorted_books[row, ]
    totalSale = book["ratings_count"] * book["price"]
    totalSale = format(round(totalSale/1000000, 3), nsmall = 2)
    valueBox(totalSale , "",color = "yellow", width = NULL)
  }) 
  
  output$value2 <- renderValueBox({
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    book = sorted_books[row, ]
    totalRead = book["ratings_count"]
    totalRead = format(round(totalRead/1000, 3), nsmall = 2)
    valueBox(totalRead , "",color = "purple", width = 12)
  })
  
  output$value3 <- renderValueBox({
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    book = sorted_books[row, ]
    totalReview = book["review"]
    totalReview = format(round(totalReview/1000, 3), nsmall = 2)
    valueBox(totalReview , "",color = "green", width = 12)
  })
  
  
  output$review_plot <- renderPlot({
    parlDiag <- function(Parties, shares, cols = NULL, repr=c("absolute", "proportion")) {
      repr = match.arg(repr)
      stopifnot(length(Parties) == length(shares))
      if (repr == "proportion") {
        stopifnot(sum(shares) == 1)
      }
      if (!is.null(cols)) {
        names(cols) <- Parties
      }
      
      # arc start/end in rads, last one reset bc rounding errors
      cc <- cumsum(c(-pi/2, switch(repr, "absolute" = (shares / sum(shares)) * pi, "proportion" = shares * pi)))
      cc[length(cc)] <- pi/2
      
      # get angle of arc midpoints
      meanAngles <- colMeans(rbind(cc[2:length(cc)], cc[1:length(cc)-1]))
      
      # unit circle
      labelX <- sin(meanAngles)
      labelY <- cos(meanAngles)
      
      # prevent bounding box < y=0
      labelY <- ifelse(labelY < 0.015, 0.015, labelY)
      
      p <- ggplot()+ 
        ggtitle("User Ratings") +
        theme_no_axes() + coord_fixed() +
        expand_limits(x = c(-1, 1), y = c(0, 1)) +
        theme(plot.title = element_text(size=25, hjust = 0.5, margin = margin(b = 85))) +
        theme(panel.border = element_blank(), 
              panel.background = element_blank()) +
        theme(legend.position = "none") +
        geom_arc_bar(colour = "grey", aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                         start = cc[1:length(shares)], 
                         end = c(cc[2:length(shares)], pi/2), fill = Parties)) +
        
        switch(is.null(cols)+1, scale_fill_manual(values = cols), NULL) 
      return(p)
    }
    
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    
      book = sorted_books[row, ]
      stars = c("1", "2", "3", "4", "5")
      reviews = c(book["rating_1"][1,], book["rating_2"][1,], book["rating_3"][1,], book["rating_4"][1,], book["rating_5"][1,])
      
      r <- data.frame(stars, reviews)
      r$stars <- factor(r$stars)
      r$Share <- r$reviews / sum(r$reviews)
      r$ymax <- cumsum(r$Share)
      r$ymin <- c(0, head(r$ymax, n= -1))
      
      bt <- data.frame(parties = stars,
                       seats   = reviews,
                       cols    = colorRampPalette(c("lightgreen", "darkgreen"))(5),
                       stringsAsFactors = FALSE)
      
      parlDiag(bt$parties, bt$seats, cols = bt$cols)
  })
  
  
  
  
  #  render data table
  #-
  output$dataTable <- renderDT(
    data.frame(Top=sorted_books$top, Title=sorted_books$title, Author=sorted_books$author, Raiting =sorted_books$avg_rating), # data
    rownames = FALSE,
    selection= list(mode = 'single', selected = 1 ),
    #selected = list( rows = 1, cols = 1),
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(lengthChange = FALSE, pageLength = 4, orderClasses=TRUE, dom = 't')
  )
  
  output$age_plot <- renderPlot({
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    
      book = sorted_books[row, ]
      book_age <- user_info[which(book$title == user_info$title),]
      
      
      ggplot(data=book_age,aes(book_age$Age)) +
        geom_histogram(fill = "orange") + labs(title = "Ratings by Age",
                                               x = "Reader's age",
                                               y = NULL) +
        labs(title = "Popularity By Reader Age" ) + theme_bw() + 
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        scale_y_continuous(breaks=NULL)   + 
        theme(plot.title = element_text(size=25, hjust = 0.5), 
              axis.title.x = element_text(size = 13, margin = margin(t = 10)))
    
  })
  
  output$genre_plot <- renderPlot({
    row = input$dataTable_rows_selected
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    
      book = sorted_books[row, ]
      book_genre <- book$genres
      book_genre_unlist <- unlist(strsplit(book_genre, split = ","))
      book_genre_unlist <- trimws(book_genre_unlist)

      random_genres_table <- sort(genre_table[book_genre_unlist], decreasing = TRUE)
      
      df <- data.frame(random_genres_table)
      
      p <- ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
        geom_bar(stat="identity", fill="slateblue3") +
        labs(title = "Books in Genre",
             x = "Genre",
             y = NULL) +
        geom_blank() + 
        geom_text(aes(label=Freq), vjust=-0.3, size=3.5) + theme_bw() + 
        theme(panel.border = element_blank(), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        scale_y_continuous(breaks=NULL)  + 
        theme(plot.title = element_text(size=25, hjust = 0.5, margin = margin(b = 15)), 
              axis.title.x = element_text(size = 13, margin = margin(t = 10)))
      
      p2 <- p + coord_cartesian(clip = "off")
      plot_grid(p2, align = "v", ncol = 1, vjust = -0.8)
    
    
    
    })
  
  output$country_plot <- renderPlot({
    row = input$dataTable_rows_selected
    print(row)
    if (is.null(row)) {
      row = input$dataTable_rows_current[1]
    }
    
      book = sorted_books[row, ]
      print(book)
      book_loc <- user_info[which(user_info$title == book$title), "Country"]
      print(book_loc)
      book_loc_df <- data.frame(table(book_loc))
      print(book_loc_df)
      layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
      par(mar=rep(0, 4))
      plot.new()
      text(x=0.5, y=0.75, "Popular in", cex = 2.1)
      wordcloud(words = book_loc_df$book_loc, freq = book_loc_df$Freq, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0, 
                colors=brewer.pal(8, "Dark2"), main = "Title", scale=c(10,2)) 
    
  })
}


shinyApp(ui, server)
