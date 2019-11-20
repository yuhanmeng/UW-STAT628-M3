library(shiny)
library(leaflet)

ui = shinyUI(pageWithSidebar(
  headerPanel("Yelp Recommendation System"),
  
  sidebarPanel(
    numericInput("B_id", "Business ID:", min = 0, max = 4523, value = NA),
    helpText("Enter the business ID you want to search (from 0 to 4523)"),
    textInput("B_name", "Business Name:", value = NA),
    helpText("If you don't know business ID, you can search by business name"),
    selectInput("topic", "Topic:", 
                choices = c("","Brunch", "Bar", "Dessert","Fast food","Foreign flavor")),
    helpText("If you don't know business ID or name, you can search by topic"),
    submitButton("Search"),
    br(),
    br(),
    h4('Our team:'),
    h5('Chaoran Wang:cwang647@wisc.edu'),
    h5('Qiaoyu Wang:qwang382@wisc.edu'),
    h5('Yuhan Meng:meng46@wisc.edu'),
    h5('Lu Li:lli468@wisc.edu'),
    br(),
    a(actionButton(inputId = "email1", label = "Contact", 
                   icon = icon("envelope", lib = "font-awesome")),
      href="mailto:cwang647@wisc.edu")
    ),
  
  mainPanel(tabsetPanel(
    tabPanel(h4("Topic Distribution"),h4(htmlOutput("err1")),
             h3(htmlOutput("title1"),align="center"),
             column(6, h4(htmlOutput("top101title")), tableOutput("top101") ), 
             column(6, h4(htmlOutput("top102title")), tableOutput("top102") ), br(),
             column(6, h4(htmlOutput("top103title")), tableOutput("top103") ), 
             column(6, h4(htmlOutput("top104title")), tableOutput("top104") )
             ),
    tabPanel(h4("Recommendation for Customer"),h4(htmlOutput("err2")),
             tableOutput("info1"),tableOutput("info2"),tableOutput("info3"),
             plotOutput("plot")
             ),
    tabPanel(h4("Suggestions for Business"),h4(htmlOutput("err3")),
             h3(htmlOutput("title3_1")),br(),h4(htmlOutput("suggestion1")),
             h3(htmlOutput("title3_2")),br(),h4(tableOutput("suggestion2")),
             h3(htmlOutput("title3_3")),br(),h4(tableOutput("suggestion3"))
             ),
    tabPanel(h4("Business Nearby"),leafletOutput("map"))
  ))
))
