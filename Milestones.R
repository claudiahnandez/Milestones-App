library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(shinyjs)
source("helper_functions.R")

ui<-fluidPage(includeCSS("theme.CSS"),
              dashboardPage(skin="black",title="Milestones App",#58355E(right) #422c46(left)
                            dashboardHeader(title="ELAC",titleWidth="300px",dropdownMenuOutput('dropdown')),
                            dashboardSidebar(width="300px",collapsed=T,
                                             sidebarMenu(
                                               id = "tabs",
                                               menuItem("Milestones App", tabName = "main"),
                                               menuItem("Fall to Fall Retention", tabName="fall_to_fall"),
                                               menuItem("Tutorial",tabName = "tutorial")
                                             )),
                            dashboardBody(shinyjs::useShinyjs(),tags$script(HTML("function clickFunction(link){ 
                                                                                 Shiny.onInputChange('linkClicked',link);}")),#script used to link the notification status to help tab
                                          
                                          tabItems(tabItem(tabName = "main",
                                                           box(width=12,
                                                               box(title="Time Frame",status="primary",width=12,
                                                                   column(width=12,selectizeInput(inputId="years_tracked", label="Years to Track:",width="50%", choices=append("1 Year",paste(2:6,"Years")))),
                                                                   column(width=3,selectizeInput(inputId = "year", label="Academic Year", choices=c("2018-2019","2017-2018","2016-2017"))),
                                                                   column(width=4,offset=1,radioButtons(inputId = "semester",label="Semester", 
                                                                                                        choices=c("Summer","Fall","Spring"),inline=T))
                                                               ),
                                                               box(title="Student Demographics",solidHeader=T,status="primary",width=4,height=500,
                                                                   selectizeInput(inputId = "ethnicity",label="Ethnicity",
                                                                                  choices=c("ALL","Asian","Black/African American","Hawaiian/Pacific Islander",
                                                                                            "Hispanic/Latino","Native American","Two or more races","Unknown","White")),
                                                                   selectizeInput(inputId = "age", label="Age Group", choices=c("ALL","Under 18","18-19","20-24","25-29","30-39",
                                                                                                                                "40 or Older")),
                                                                   selectizeInput(inputId = "gender",label="Gender",choices=c("ALL","Female","Male"))
                                                               ), box(title="Affiliated Groups", solidHeader=T, width=4, status="primary",height=500,
                                                                      selectizeInput(inputId = "bogg",label="California Promise Grant",choices=c("ALL","Yes","No")),
                                                                      selectizeInput(inputId = "pell",label="Pell Grant",choices=c("ALL","Yes","No")),
                                                                      selectizeInput(inputId = "ab540",label="AB540 Status",choices=c("ALL","Yes","No")),
                                                                      selectizeInput(inputId = "dsps",label="DSPS",choices=c("ALL","Yes","No")),
                                                                      selectizeInput(inputId="eops", label="EOPS", choices=c("ALL","Yes","No"))),
                                                               
                                                               box(title="Placement", solidHeader = T, width=4,status="primary",
                                                                   selectizeInput(inputId = "english", label="English Placement",
                                                                                  choices=c("ALL","Transfer Level","1 Level Below Transfer", "2 Levels Below Transfer",
                                                                                            "3 Levels Below Transfer", "4 or more Levels Below Transfer",
                                                                                            "ESL","No Placement Found")),
                                                                   selectizeInput(inputId = "math", label="Math Placement",
                                                                                  choices=c("ALL","Transfer Level","1 Level Below Transfer", "2 Levels Below Transfer",
                                                                                            "3 Levels Below Transfer",
                                                                                            "No Placement Found"))
                                                               ),
                                                               box(title="Goals", solidHeader = T, width=4,status="primary",
                                                                   selectizeInput(inputId = "goals",label="Educational Goals", choices=c("ALL","Associate's Degree","Basic Skills","Career/Job Advancement",
                                                                                                                                     "GED","Maintain Licensure","Personal Development",
                                                                                                                                     "Transfer","UC/CSU Student","Undecided",
                                                                                                                                     "Unknown","Vocational Certificate","Vocational Degree"))
                                                               ),
                                                               column(width=1,offset=11,actionBttn(inputId = "submit", label = "Submit",block=T, no_outline=F, style = "jelly", color = "danger"))),
                                                           
                                                           valueBoxOutput("cohort_vb",width=3),
                                                           valueBoxOutput("demo_vb",width=4),
                                                           valueBoxOutput("groups_vb",width=2),
                                                           valueBoxOutput("placement_vb",width=3),
                                                           
                                                           column(12,plotlyOutput("milestones_graph",height=650))
                                                           
                                                           
                                          ),
                                          tabItem(tabName = "fall_to_fall", 
                                                  column(12,h2("Retention")),
                                                    column(6,box(width=12, solidHeader = T,title="Years",status="primary",
                                                                  column(5,selectInput(inputId = "f2f_year",label="From:",
                                                                              choices=paste0(paste("Fall", c( (as.integer(format(Sys.Date(), "%Y"))-1):( as.integer(format(Sys.Date(), "%Y"))-3))),
                                                                                             "/",paste("Fall", c( (as.integer(format(Sys.Date(), "%Y"))):( as.integer(format(Sys.Date(), "%Y"))-2))))
                                                                    )),
                                                                  column(width=2,offset=10, actionButton(inputId = "f2f_submit",label="Submit"))
                                                                  
                                                          )),br(),br(),
                                                  column(width=5,infoBoxOutput("results_f2f",width=12))
                                                  ),#fall_to_fall tab,
                                          tabItem(tabName = "tutorial",
                                                    actionBttn(inputId = "go_main",label="Go Back",style="stretch",color="primary",icon=icon("arrow-left"),size="sm",no_outline = FALSE),
                                                    includeHTML("Tutorial.txt"))
                                          ))#tabItems,dashboardBody
                            ))#dashboardPage ,fluidpage

server<-function(input, output, session) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                           badgeStatus = NULL,headerText="Help",get_noti2())})
  observeEvent(input$linkClicked,{
    
    if(grepl("Tutorial",input$linkClicked)){
      updateTabItems(session,"tabs",selected = "tutorial")
      output$dropdown=renderMenu({dropdownMenu(type="notifications",icon=icon("question-circle", "fa-2x"),
                                               badgeStatus = NULL,headerText="Help",get_noti2())})
    }
  })
  
  
  restrict_year<-reactiveVal(NULL)
  
  observeEvent(input$years_tracked,{
    restrict_year(get_possible_year_range(input$years_tracked)[1])
    updateSelectizeInput(session=session,inputId ="year", label="Academic Years", choices=get_possible_year_range(input$years_tracked))
    
    #if 7 and currently in summer or spring semester 
    
  })
  
  observeEvent(input$f2f_submit,{
    #get year 1  
    year_chosen<-isolate({input$f2f_year})
    
    data<-get_data(semester = NULL,year=NULL, years_wanted = NULL, 
                   year_semester =paste0(substr(year_chosen,6,9),"2"),
                   year_cap =paste0(substr(year_chosen,16,19),"2"))
    
    output$results_f2f<-renderInfoBox({
      infoBox(title="Persisted Fall to Fall",value=paste(length(unique(data[data$Year_Semester==Year_Cap,]$Student_Id))," Students"),
              subtitle=paste("Initial Students:", length(unique(data$Student_Id))))
    })
    
  
    
  })
  
  observeEvent(input$submit,{
    withProgress(message = 'Creating Milestones      ', value = 0, {
      n<-5
      incProgress(2/n, detail = paste("Getting the data"))
      get_data(input$semester, input$year, input$years_tracked)
      
      incProgress(3/n, detail = paste("Sorting through data"))
      
      result=thin_the_herd(input$ethnicity,input$age, input$gender, input$bogg,input$dsps,input$eops,input$english,input$math,input$goals,input$pell,input$ab540)
      
      
      if(is.null(result)){
        showNotification("Dissagregated too much! No data found.",type="error", duration=5)
      }else if(result==-1){
        showNotification("The categories selected resulted in less tan 15 students. Due to privacy reasons, data will not be displayed.",type="error", duration=5)
      }
      else{
        #more awards
        get_award_data(Raw_Data,substr(Year_Semester,1,4),substr(Year_Cap,1,4))
        
        
        incProgress(4/n, detail = paste("Rendering Graph"))
        colors <- c('rgb(94, 135, 156)','rgb(94, 156, 147)','rgb(63, 207, 207)',
                    'rgba(89, 199, 182,1)','rgb(82, 214, 136,1)',
                    'rgba(93, 237, 133,1)','rgb(138, 230, 12)',
                    'rgba(212, 255, 44,1)','rgba(255, 232, 0,1)',
                    'rgb(255, 195, 0)','rgb(229, 183, 27)',  'rgb(158, 137, 64)',
                    'rgb(107, 90, 86)','rgb(0, 0, 0)')
        
        
        output$cohort_vb<-renderValueBox(
          isolate(valueBox(value=tags$p(HTML((paste("Academic Year: ",input$year,br(),input$semester)))
                                        ,style="font-size:42%"),subtitle = "Cohort", icon=icon("calendar"),color="orange", width=3))
        )
        output$demo_vb<-renderValueBox(
          isolate(valueBox(value=tags$p(HTML((paste("Ethnicity:", input$ethnicity,br(),
                                                    "Gender: ",input$gender,br(),"Age: ",input$age))),style="font-size:42%")
                           ,subtitle = "Student Demographics", icon=icon("user"),color="orange", width=4))
        )
        
        output$groups_vb<-renderValueBox(
          isolate(valueBox(value=tags$p(HTML((paste("CA Promise Grant: ",input$bogg,br(),"Pell Grant: ",
                                                    input$pell,br(),"DSPS: ",input$dsps,br(),"EOPS: ",input$eops,
                                                    br(),"AB540: ",input$ab540))),style="font-size:42%")
                           ,subtitle = "Affiliated Groups", icon=icon("users"),color="orange", width=2))
        )
        
        output$placement_vb<-renderValueBox(
          isolate(valueBox(value=tags$p(HTML((paste("English: ",input$english,br(),"Math: ",input$math
                                                    ,br(),"Educational Goal:",input$goals))),style="font-size:42%")
                           ,subtitle = "Placement", icon=icon("book"),color="orange", width=3))
        )
        
        
        incProgress(5/n, detail = paste("Almost done..."))
        
        if(isolate(input$semester)=="Spring" )
          cohort_year=isolate(substr(input$year,6,9))
        else{
          cohort_year=isolate(substr(input$year,1,4))
        }
        #make plotly graph
        p<-data%>%plot_ly(x=~Categories,y=~Total, type="bar",
                          marker = list(color = colors),
                          text = ~paste(Total, " students"),
                          insidetextfont = list(color = '#000000'),
                          hoverinfo = 'text'
                          #The 'pull' attribute can also be used to create space between the sectors
        ) %>%
          layout(title =paste0('Student Achievement Milestones Over ',input$years_tracked,'<br>',cohort_year," ",input$semester," Cohort")
                      ,titlefont=list(family="Open Sans",size=30), margin = list(b=180,r=-2,t=80),
                 xaxis=list(title="",categoryorder = "trace"),yaxis=list(title=""),
                 annotations = list(x = ~Categories, y = ~Total, text =paste(round((data$Total/data$Total[1])*100,digits=2),"%") ,
                                    xanchor = 'center', yanchor = 'bottom',
                                    showarrow = FALSE))%>% config(displayModeBar = F)
        output$milestones_graph<-renderPlotly(p)
      }
    })
  })
  
  observe(if(!is.null(restrict_year()) & input$year==restrict_year()){
              if((as.integer(format(Sys.Date(), "%m"))>9  && as.integer(format(Sys.Date(), "%m"))<=12) ||  as.integer(format(Sys.Date(), "%m"))==1){#Fall
                updateRadioButtons(session=session, inputId = "semester",label="Semester", 
                                   choices=c("Summer","Fall"),inline=T)
                showNotification("Spring was removed because full year of data will not be achieved from that cohort",type="warning", duration=9)
              }
              else if(as.integer(format(Sys.Date(), "%m"))>=7  && as.integer(format(Sys.Date(), "%m"))<=9){#July-September
                updateRadioButtons(session=session, inputId = "semester",label="Semester", 
                                   choices=c("Summer"),inline=T)
                showNotification("Fall and Spring were removed because full year of data will not be achieved from that cohort",type="warning", duration=9)
                }
          }else{
          updateRadioButtons(session=session, inputId = "semester",label="Semester", 
                             choices=c("Summer","Fall","Spring"),inline=T)
        })
  
  observeEvent(input$go_main,{
    updateTabItems(session, "tabs","main")
  })
}


shinyApp(ui = ui, server = server)
