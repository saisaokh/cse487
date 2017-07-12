
## load packages for creating dashboard
library(shiny)
library(shinydashboard)
library(quantmod)

################# this section decides the layout and front end of the dashboard ###############
ui<-dashboardPage(
  dashboardHeader(title = "UNIVERSITY ANALYSIS AND VISUALIZATION"),
  ###################################################################### 
  dashboardSidebar(),
  ########################################################################
  dashboardBody(
    tabsetPanel(
      
      ############ this is the first tab in the dashboard
      tabPanel(title  = "UNIVERSITY ANALSIS",
               
              box( plotOutput("plot1"),height = 35,width = 25)
      
      ),
      
      ####### second tab #################
      tabPanel(title  = "HISTOGRAMS",
               
               box(plotOutput("plot2"),width = 10),
               box(plotOutput("plot3"),width = 10)
               
      
      ),
      
      ########## third tab ##################
      tabPanel(title = "PIECHARTS",
                box(plotOutput("plot4")),
                box(plotOutput("plot5"))
               
      ),
      
      ################ fourth tab #################
      tabPanel(title  = "PLOTS",
               fluidRow(
               plotOutput("plot6"),
               plotOutput("plot7")
              )      
      )
    )))


######################  this section decides the back end of the dashboard   ###########################
server <- function(input,output){
  
  university_ranks <- read.csv("university.csv",colClasses = "character",as.is = c("Ph.D.s.granted","Rank","School.name","Total.graduate.engineering.enrollment","Avg.GRE.Quant.Score.masters.and.PhD."))
  
  ####################### creating a subset of university data for top 20 universities
  univ_subset <- university_ranks[1:20,]
  ################ importing the university data file by converting .xslx into. csv
  output$plot1<-renderPlot({
    
   
    xy <- substr(univ_subset$Tuition,2,7)
    xy[11] <- substr(xy[11],1,3) 
    str(xy)
    x <- sub(",","",xy)
    y<-as.numeric(x)
    ################# creating the bar plot of tuition fees for top 20 univs
    par(mar=c(16,5,2,1))
    barplot(y,names.arg =univ_subset$School.name,las=2,col=c("black","grey","white","yellow"),xpd = TRUE,ylab="Tuition fees in dollars",main = "Average tuition fees of top 20 universities") 
  #  dotchart(as.numeric(univ_subset$Total.graduate.engineering.enrollment),labels = univ_subset$School.name)
    })
  
  ########### hitsograms on univ data #############
  output$plot2<-renderPlot({
  abc<-  as.numeric(univ_subset$Avg.GRE.Quant.Score.masters.and.PhD.)
  hist(abc,main = "Histogram of Average GRE score for masters and phd",col = c("black","grey"),xlab = "Avg GRE scores in Quant")
  })
  
  
  output$plot3<-renderPlot({
   
    str(univ_subset)
    abc1<-  as.numeric(univ_subset$Ph.D.s.granted)
    abc1
    hist(abc1,main = "Histogram of PHDs granted",col = c("grey","black"),xlab ="Number of PHDs granted")
    
  })
  
  ################### pie charts on univ data
  output$plot4<-renderPlot({
    
   pie(abs(as.numeric(univ_subset$Peer.assessment.score..out.of.5.)),labels = univ_subset$School.name,main = "Peer assessment score out of 5 for top 20 universities",col = c("black","grey","white","yellow","cyan"))
  })
  
  
  output$plot5<-renderPlot({
    pie(abs(as.numeric(univ_subset$Ph.D.s.granted)),labels = univ_subset$School.name,main = "PHDs granted by top 20 universities",col=c("black","grey","white","yellow","cyan"))
  })
  
  
  ############# plotting overall acceptance rate
    output$plot6 <- renderPlot(

            {
  
              par(mar=c(16,5,2,1))
              
            plot(factor(univ_subset$School.name),as.numeric(sub("%","",univ_subset$Overall.acceptance.rate))/100,las=2,main="Overall acceptance rate of top 20 universities", ylab="acceptane rate in percentages",xlab="universities",col=c("red","blue"))
      })
    
    
    ############# plotting faculty membership 
    output$plot7 <- renderPlot(
      {
        par(mar=c(16,5,2,1))
        plot(factor(univ_subset$School.name),as.numeric(sub("%","",univ_subset$Faculty.membership.in.National.Academy.of.Engineering))/100,las=2,main="Faculty membership in National Academy of Engineering from top 20 universities", ylab="Membership proportion in percentages",xlab="universities")
      })
    
}

############# function to create app for shiny dashboard
shinyApp(ui,server)
