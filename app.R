library(shiny)
library(shinydashboard)
library(quantmod)
library(xts)
library(dygraphs)
library(leaflet)

ui<-dashboardPage(
  dashboardHeader(title = "STARBUCKS CASE STUDY"),
  ###################################################################### 
  dashboardSidebar(),
  ########################################################################
  dashboardBody(
    tabsetPanel(
      tabPanel(title  = "STOCK ANALYSIS",
               
               box(plotOutput("plot1"),width = 10),
               # box(plotOutput("plot2"))
              box( textOutput("text1"))
      ),
      tabPanel(title = "Stores",
               
               fluidRow(
                 
                 box(plotOutput("plot3"), width =15),
                 box(textOutput("text2"))
               )
      ),
      tabPanel(title ="Countries",
      #           
                fluidRow(
                  box(plotOutput("plot4"),width = 15),
                  box(textOutput("text3"))
                
                )
      ),
      tabPanel(title="location mapper",
        fluidRow(
          leafletOutput("mymap"),
          box(textOutput("text4"))
          #box(plotOutput("plot5"),width = 12)
        )       )
    )
  ))
######################backend###########################
server <- function(input,output){
  output$plot1<-renderPlot({
    sym<-("SBUX")
    getSymbols(sym)
    
    chart_Series(SBUX)
  })
  
  output$text1 <- renderText(
    "Its a starbucks case study using R and Shiny Dashboard. In 2008-2009 the revenue of starbucks went down as is visible from the falling stocks of Starbucks in those years depicted in the above graph. During this period, thousands of Starbucks stores mainly across USA and some other countries were closed down due to following reasons:
    1: Poor Demographic planning of Starbucks location
    2: Lack of different types of stores apart from company owned
    3: Small or weak presence in most of the other countries making Starbucks a weaker brand there.
    
    All these led to decrease in revenue of Starbucks which is evident through the stock graph above."
  )
  
  output$plot2<-renderPlot({
    sym<-("SBUX")
    dy <- getSymbols
    (sym)
    #dyxts<-xts(dy$Close)
    dyxtsbind<-cbind(dy$Close)
    dygraph(dyxtsbind)
  })
#   
   output$plot3<-renderPlot({
    sb <- read.csv("C:/Users/nbamb/Documents/R/starbuckscasestudyusingr/All_Starbucks_Locations_in_the_World.csv",header = TRUE)
  own<-table(sb$Ownership.Type)
  barplot(own,main = "Stores analysis",xlab = "types of stores",col = "cyan")
  
   })
   output$text2 <- renderText(
     "Previously Starbucks had only company operated stores limiting their presence everywhere. The Business Intelligence team then identified the need of other types of stores 
     to increase the brand awareness and the revenue of the company. So now Starbucks across the globe operates in many different forms as mentioned: 1. Comapny Owned stores 
     2. Joint Ventured Stores  3. Franchised stores  4. Licensed stores "
   )
   output$plot4<-renderPlot({
     sb <- read.csv("C:/Users/nbamb/Documents/R/starbuckscasestudyusingr/All_Starbucks_Locations_in_the_World.csv",header = TRUE)
     counts<-table(sb$Country)
     
     barplot(counts,main = "Country wise distribution",xlab="countries",col="red")
     
   })
   output$text3 <- renderText(
     "Maximum number of Starbucks stores can be found in USA as depicted above in the bar plot. But after the saturation of number of stores in USA during 2008 to 2010, the BI team 
     felt a need of expanding to other countries to increase the overall growth and revenue of the company. Now Starbucks covers almost all the countries in the world through its stores presence."
   )
    
  output$progressBox <- renderInfoBox({
    infoBox(
      "Progress", mylinear(), icon = icon("list"),
      color = "purple"
    )
  })
  output$mymap <- renderLeaflet({
    starbucks <- read.csv("starbucks.csv", stringsAsFactors = FALSE)
    str(starbucks)
    atlanta <- subset(starbucks, City == "Atlanta" & State == "GA")
    leaflet() %>% addTiles() %>% setView(-84.3847, 33.7613, zoom = 16) %>%
      addMarkers(data = atlanta, lat = ~ Latitude, lng = ~ Longitude,popup = atlanta$Name) %>%
      addPopups(-84.3847, 33.7616, 'Data journalists at work, <b>NICAR 2015</b>')
    
  })
  
  output$text4 <- renderText(
    " This is a map of Starbucks stores in the city of Atlanta. If you explore the map carefully, you can see many Starbucks stores clustered nearby which was a sign of poor demographic planning while setting up the stores. This was one of the major reasons why stores were shut down during 2008-2010. After the introduction of Business Intelligence as one of the departments in Starbucks, all Starbucks stores are now found in a strategic location to attract maximumn coffee drinkers."
  )

}

shinyApp(ui,server)