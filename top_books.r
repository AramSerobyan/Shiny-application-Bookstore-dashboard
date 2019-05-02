library(shiny) #  Shiny web app
library(DT)    #  for data tables
library(RCurl)

# user interface just shows the table
ui <- fluidPage(fluidRow(column(12, div(dataTableOutput("dataTable")))))

# server is where all calculations are done, tables are pre-rendered
server <- function(input, output, session) {
  
  # load CSV file
  varY <- getURL("https://raw.githubusercontent.com/AramSerobyan/Shiny-application-Bookstore-dashboard/master/books_data-2.csv")
  df  <- read.csv(text = varY)
  sorted_df <- df[order(-df$num_ratings), ]
  sorted_df <- sorted_df[1:8,]
  sorted_df$top =seq(1, NROW(sorted_df), by=1)
  #-----------------------------------------------------------------------------
  #  render data table
  
  #-----------------------------------------------------------------------------
  
  output$dataTable <- renderDT(
    data.frame(top =sorted_df$top,title= sorted_df$title, average_rating =sorted_df$avg_rating), # data
    selection='single',
    class = "display nowrap compact", # style
    filter = "none", # location of column filters
    options = list(lengthMenu = c(4,8), pageLength = 4, orderClasses=TRUE)
  )
  
}

# run the app
shinyApp(ui, server)

