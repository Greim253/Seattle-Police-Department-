
library(shiny)
library(lme4)
library(ggplot2)
library(arm)
library(stargazer)
library(dplyr)
library(lubridate)
library(ggvis)


x<-"https://gist.githubusercontent.com/Greim253/c12ea503529658f8a80696005b12012c/raw/e739483472e3c14fd8eb9b38689345850a398a34/UofFData.csv"
train_r<-read.csv(x)
train_r$count<-rep(1,length(nrow(train_r)))
train_r$Occured_date_time <- as.Date(train_r$Occured_date_time,"%m/%d/%Y")
train_r$month<-floor((as.numeric(train_r$Occured_date_time)-as.numeric(min(train_r$Occured_date_time)))/30)
train_r$d<-as.numeric(train_r$month)
r1_Precinct<-train_r %>% group_by(month=floor_date(Occured_date_time, "month"),Precinct=Precinct,Gender=Subject_Gender,Race=Subject_Race) %>% summarize(amount=sum(count))
r1_Sector<-train_r %>% group_by(month=floor_date(Occured_date_time, "month"),Sector=Sector,Gender=Subject_Gender,Race=Subject_Race) %>% summarize(amount=sum(count))
r1_Beat<-train_r %>% group_by(month=floor_date(Occured_date_time, "month"),Beat=Beat,Gender=Subject_Gender,Race=Subject_Race) %>% summarize(amount=sum(count))

ui <- fluidPage( 
  titlePanel("Breaking down Use of Force Incidents by Seattle Police Department Location Jurisdictions"),
  tabsetPanel(      
      tabPanel("By Precinct",  
               mainPanel(

              plotOutput("plot1a")
        
              ),
      
                sidebarPanel(
                  selectInput("Precinct","Choose a Precinct:",
                              unique(r1_Precinct$Precinct)
                              
                              ),
                  p("This tab subgroups the the SPD use of force data by Precinct selected above. After selecting the Precinct, it does a linear regression of incidents per day, factoring by Gender:Race."),
                  uiOutput("tab1")
                  )
      
              ), 
      
      tabPanel("By Sector", 
               
               mainPanel(
                 
                 plotOutput("plot2a")
                 
               ),
               
               sidebarPanel(
                    selectInput("Sector","Choose a Sector:",
                             unique(r1_Sector$Sector)
                 
                                ),
                    p("This tab subgroups the the SPD use of force data by Sector selected above. After selecting the Sector, it does a linear regression of incidents per day, factoring by Gender:Race."),
                    uiOutput("tab2")
                            )
      ),
      
      tabPanel("By Beat", 
               
               mainPanel(
                 
                 plotOutput("plot3a")
                 
               ),
               
               sidebarPanel(
                 selectInput("Beat","Choose a Beat:",
                             unique(r1_Beat$Beat)

                            ),
                 p("This tab subgroups the the SPD use of force data by Beat selected above. After selecting the Beat, it does a linear regression of incidents per day, factoring by Gender:Race."),
                 uiOutput("tab3")
                          )
               )
      )
  

  
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  url <- a("Use of Force Data", href="http://www.seattle.gov/police/information-and-data/use-of-force-data")
  output$tab1 <- renderUI({
    tagList("URL link:", url)
  })
  output$tab2 <- renderUI({
    tagList("URL link:", url)
  })
  output$tab3 <- renderUI({
    tagList("URL link:", url)
  })
  
  selectedData1 <- reactive({

    r1<-subset(r1_Precinct,Precinct==input$Precinct)
    return(r1)
  })
  
  selectedData2 <- reactive({
    
    r2<-subset(r1_Sector,Sector==input$Sector)
    return(r2)
    
  })
  
  selectedData3 <- reactive({
    
    r3<-subset(r1_Beat,Beat==input$Beat)
    return(r3)
    
  })
  
  output$plot1a <- renderPlot({
    
    r1a<-ggplot(data=selectedData1(),aes(x=month,y=amount,color=factor(Gender:Race)))+geom_point()+geom_smooth(method="lm",se=FALSE)+labs(x="Time (by month)",y="Use of Force Incidents per month",title=paste("Linear Regression of Precinct ",input$Precinct," data"))+theme(axis.title  = element_text(hjust = 0.5))
    r1a
  })
  
  
  output$plot2a <- renderPlot({
    
    r2a<-ggplot(data=selectedData2(),aes(x=month,y=amount,color=factor(Gender:Race)))+geom_point()+geom_smooth(method="lm",se=FALSE)+labs(x="Time (by month)",y="Use of Force Incidents per month",title=paste("Linear Regression of Sector ",input$Sector," data"))+theme(axis.title  = element_text(hjust = 0.5))
    r2a
  })
  

  
  
  output$plot3a <- renderPlot({
    
    r3a<-ggplot(data=selectedData3(),aes(x=month,y=amount,color=factor(Gender:Race)))+geom_point()+geom_smooth(method="lm",se=FALSE)+labs(x="Time (by month)",y="Use of Force Incidents per month",title=paste("Linear Regression of Beat ",input$Beat," data"))+theme(axis.title  = element_text(hjust = 0.5))
    r3a
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

