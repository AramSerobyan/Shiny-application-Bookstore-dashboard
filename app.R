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

books_loc_age_url <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/final_data.csv")
books <- read.csv(text = books_loc_age_url, header = TRUE)

books_genre_url <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/books_data-2.csv")
books_genre_data <- read.csv(text = books_genre_url, header = TRUE)

#book_info_url <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/book-info.csv")
#book_info <- read.csv(text = book_info_url, header = TRUE)

#book_info$title <- gsub("\\s*\\([^\\)]+\\)","",as.character(book_info$title))

#subset_book_genre_data <- books_genre_data[match(book_info$title, books_genre_data$title), ]

books_unique <- subset(books, !duplicated(books$isbn))[, 3:13]
books_unique$title <- gsub("\\s*\\([^\\)]+\\)","",as.character(books_unique$title))
books_unique$price <- gsub('([0-9]+) .*', '', books_unique$price)
books_unique$price <- gsub('Not available', '10', books_unique$price)
books_unique$price <- as.numeric(books_unique$price)
books_unique$price[which(is.na(books_unique$price))] <- 10 

genres <- books_genre_data$genres
genres <- as.character(genres)
genre_unlist <- unlist(strsplit(genres, split = ","))
genre_unlist <- trimws(genre_unlist)
genre_unlist <- as.factor(genre_unlist)

genre_table <- sort(table(genre_unlist), decreasing = TRUE)

random_genres <- c("Journalism", "Cats", "Canada", "Epic", "Comics", "Biography", "Young Adult")

random_genres_table <- genre_table[random_genres]

df <- data.frame(random_genres_table)



## DAta Table
sorted_books <- books_unique[order(-books_unique$ratings_count), ]
sorted_books$top =seq(1, NROW(sorted_books), by=1)
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
  splitLayout( cellWidths = c( "73%","75%"),
               fluidRow( 
                 column(  width = 6, div(dataTableOutput("dataTable"))),
                 column( width = 1, ""),
                 column(width = 5, plotOutput("GeographicData", height = "250px")))
               ,column(  width = 12,  tags$head(tags$style(HTML(".small-box {height: 50px}"))),
                         "Total Sales (USD)", fluidRow( width = 12,
                                                        valueBoxOutput("value1")) ,  "Total Reads",
                         fluidRow( width = 12,
                                   valueBoxOutput("value2")),
                         "Reviews",
                         fluidRow( width = 12,
                                   valueBoxOutput("value3"))))
)
frow2 <- fluidRow( column( width = 5, 
                           plotOutput("revenuebyGenre", height = "320px")),
                   #column( width = 1, ""),
                   column( width = 7,
                           column( width = 12, plotOutput("BooksByAge", height = "160px")),
                           column( width = 12, plotOutput("GenrePopularity", height = "160px"))
                   )
)

body <- dashboardBody(frow1, frow2, setBackgroundColor("white"))

ui <- dashboardPage(title = 'Title', header, sidebar, body, skin='green')

server <- function(input, output,session) { 
  
  renderPlot({
    s = input$x1_rows_selected
    par(mar = c(4, 4, 1, .1))
    plot(cars)
    if (length(s)) points(cars[s, , drop = FALSE], pch = 19, cex = 2)
  })
  
  output$value1 <- renderValueBox({
    row = input$dataTable_rows_selected
    book = sorted_books[row, ]
    totalSale = book["ratings_count"] * book["price"]
    valueBox(totalSale , "",color = "yellow", width = NULL)
  }) 
  
  output$value2 <- renderValueBox({
    row = input$dataTable_rows_selected
    book = sorted_books[row, ]
    totalRead = book["ratings_count"]
    valueBox(totalRead , "",color = "purple", width = 12)
  })
  
  output$value3 <- renderValueBox({
    valueBox(25 , "",color = "green", width = 12)
  })
  
  
  
  #  render data table
  #-
  output$dataTable <- renderDT(
    data.frame(Top=sorted_books$top, Title=sorted_books$title, Raiting =sorted_books$average_rating), # data
    rownames = FALSE,
    selection= list(mode = 'single', selected = 1 ),
    #selected = list( rows = 1, cols = 1),
    class = "display nowrap compact", # style
    filter = "top", # location of column filters
    options = list(lengthChange = FALSE, pageLength = 4, orderClasses=TRUE, dom = 't')
  )
  
  output$GeographicData <- renderPlot({
    
    
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
      
      p <- ggplot() + theme_no_axes() + coord_fixed() +
        expand_limits(x = c(-1.3, 1.3), y = c(0, 1.3)) + 
        theme(panel.border = element_blank()) +
        theme(legend.position = "none") +
        
        geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.5, r = 1,
                         start = cc[1:length(shares)], 
                         end = c(cc[2:length(shares)], pi/2), fill = Parties)) +
        
        switch(is.null(cols)+1, scale_fill_manual(values = cols), NULL) + 
        
        # for label and line positions, just scale sin & cos to get in and out of arc
        geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
                      group = rep(1:length(shares), 2)), colour = "white", size = 2) +
        geom_path(aes(x = c(0.9 * labelX, 1.15 * labelX), y = c(0.9 * labelY, 1.15 * labelY),
                      group = rep(1:length(shares), 2)), size = 1) +
        
        geom_label(aes(x = 1.15 * labelX, y = 1.15 * labelY, 
                       label = switch(repr,
                                      "absolute" = sprintf("%s\n%i", Parties, shares),
                                      "proportion" = sprintf("%s\n%i%%", Parties, round(shares*100)))), fontface = "bold", 
                   label.padding = unit(1, "points")) +
        
        geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY), colour = "white", size = 2) +
        geom_point(aes(x = 0.9 * labelX, y = 0.9 * labelY)) 
      #geom_text(aes(x = 0, y = 0, label = switch(repr, 
      #                                           "absolute" = (sprintf("Total: %i MPs", sum(shares))), 
      #                                           "proportion" = "")),
      #          fontface = "bold", size = 7) 
      
      return(p)
    }
    
    row = input$dataTable_rows_selected
    book = sorted_books[row, ]
    stars = c("1", "2", "3", "4", "5")
    reviews = c(book["ratings_1"][1,], book["ratings_2"][1,], book["ratings_3"][1,], book["ratings_4"][1,], book["ratings_5"][1,])
    
    r <- data.frame(stars, reviews)
    r$stars <- factor(r$stars)
    r$Share <- r$reviews / sum(r$reviews)
    r$ymax <- cumsum(r$Share)
    r$ymin <- c(0, head(r$ymax, n= -1))
    
    bt <- data.frame(parties = stars,
                     seats   = reviews,
                     cols    = c("white", 
                                 "snow2", 
                                 "palegreen2",
                                 "springgreen2", 
                                 "springgreen3"),
                     stringsAsFactors = FALSE)
    
    parlDiag(bt$parties, bt$seats, cols = bt$cols)
    
  })
  
  output$BooksByAge <- renderPlot({
    row = input$dataTable_rows_selected
    book = sorted_books[row, ]
    book_age <- books[which(books$isbn == book$isbn), "Age"]
    book_age_df <- data.frame(table(book_age))
    ggplot(data=book_age_df, aes(x=book_age_df$book_age, y=Freq, group=1)) +
      geom_line() + labs(title = "Ratings by Age",
                         x = "Reader's age",
                         y = "Rating") +
      theme( rect = element_rect(fill = "transparent") )
    
  })
  
  output$GenrePopularity <- renderPlot({ggplot(data=df, aes(x=genre_unlist, y=Freq)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Genre popularity",
           x = "Genre",
           y = "Books in that genre") +
      geom_text(aes(label=Freq), vjust=-0.3, size=3.5)})
  
  output$revenuebyGenre <- renderPlot({
    row = input$dataTable_rows_selected
    book = sorted_books[row, ]
    book_loc <- books[which(books$isbn == book$isbn), "Country"]
    book_loc_df <- data.frame(table(book_loc))
    print(book_loc_df)
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    par(mar=rep(0, 4))
    plot.new()
    text(x=0.5, y=0.5, "Popular in")
    wordcloud(words = book_loc_df$book_loc, freq = book_loc_df$Freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"), main = "Title") 
    
  })
}


shinyApp(ui, server)
