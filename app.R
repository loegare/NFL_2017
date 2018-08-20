#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sqldf)
library(scales)
drive<-readxl::read_xlsx('NFL.xlsx',sheet='Drive')
game<-readxl::read_xlsx('NFL.xlsx',sheet='Game')
play<-readxl::read_xlsx('NFL.xlsx',sheet='Player-Play')
#rsconnect::setAccountInfo(name='shnipes', token='6A6195725697F3D8AB463C0B34FC3209', secret='QGjr4Mn1z0nzrPlJct+DA86R2+axrTNO8uOHW6dm')

teamWin <- group_by(game, winner) %>%
     summarise(wins = n())
teamWin<-teamWin[order(teamWin$wins,decreasing=T),]
selectedTeam = teamWin$winner[1]
teamWin<-teamWin[order(teamWin$winner,decreasing=F),]

uniquestat<-play$cat
uniquestat<-unique(uniquestat)

play<- merge(play,game,by="gameid")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("NFL"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("week",
                     "Week:",
                     min = 1,
                     max = 17,
                     value = 17),
         selectInput("team",
                     "Team:",
                     choices = teamWin$winner,
                     multiple=FALSE,
                     selected = selectedTeam
         )
      ),selectInput("stat",
                    "Stat:",
                    choices = uniquestat,
                    multiple=FALSE
      )
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Team", plotOutput("winPlot"),plotOutput("statPlot")),
                    tabPanel("League Leader", plotOutput("llPlot"))
                   #, tabPanel("By Zip", plotOutput("table"))
        )
        
         
      )
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  wins<-reactive({
    game2<- group_by(game,week, winner) %>%
      summarise(wins = n())
    game2<-sqldf(paste('select * from game2 where week <= ',input$week,sep=' ' ))
    game2$highlight <- ifelse(game2$winner == input$team , '1', '0')
    game2
  })
  
  fbStats<-reactive({
    tackles<-play
    tackles<-sqldf(paste('select * from tackles where week <= ',input$week,sep=' ' ))
    
    tackles<- group_by(tackles,team, cat) %>%
      summarise(val = sum(val))
    tackles$highlight <- ifelse(tackles$team == input$team , '1', '0')
    tackles$selcat <- ifelse(tackles$cat == input$stat , '1', '0')
    tackles<-tackles %>% filter(selcat == '1') 
    

    tackles
  })
  
  leagueLeader <-reactive({
    LL<-sqldf(paste('select * from play where week <= ',input$week,sep=' ' ))
    LL <- group_by(LL,team,playername,cat) %>%
      summarise(val = sum(val))
    LL$selcat <- ifelse(LL$cat == input$stat , '1', '0')
    LL$highlight <- ifelse(LL$team == input$team , '1', '0')
    LL<-LL %>% filter(selcat == '1')
    
    LL<-LL[order(LL$val,decreasing=T),]
    LL = LL[1:10,]
    
    LL
  })
   
   output$winPlot <- renderPlot({
      ggplot(wins()[order(wins()$wins,decreasing=T),],aes(x=reorder(winner,-wins, function(x){ sum(x) }),wins))+
       geom_bar(stat='identity',aes(fill=highlight))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        scale_y_continuous(breaks=pretty_breaks(),limits=c(0,16))+
       geom_hline(yintercept=sum(wins()$wins)/32)
      
      
   })
   output$statPlot <- renderPlot({
     ggplot(fbStats(),aes(x=reorder(team,-val, function(x){ sum(x) }),val))+
       geom_bar(stat = 'identity',aes(fill=highlight))+
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
       ylab(input$stat)+
       scale_y_continuous(breaks=pretty_breaks())+
       geom_hline(yintercept=mean(fbStats()$val))
   })
    output$llPlot <- renderPlot({
      ggplot(leagueLeader(),aes(x=reorder(paste(playername,team,sep=' | '),-val, function(x){ sum(x) }),val))+
        geom_bar(stat = 'identity',aes(fill=highlight))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ylab(input$stat)+
        scale_y_continuous(breaks=pretty_breaks())+
        geom_hline(yintercept=mean(fbStats()$val))
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
#library(rsconnect)


#rsconnect::deployApp(appName='NFL_STATS')
