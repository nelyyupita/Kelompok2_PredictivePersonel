library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(DT)
library(echarts4r) 

data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
#view(data)

left <- c("left", "stay", "","","")
id <- c(1:1000)
ui <- dashboardPage(
  dashboardHeader(
    title = "HR Dashboard"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Employee Dashboard", tabName = "emp", icon = icon("user")),
      menuItem("Profile Employe", tabName = "profile", icon = icon("person")),
      menuItem("Employee", tabName = "empp", icon = icon("users"))
      
    )
  ),
  dashboardBody(
    bookmarkButton(),
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Ini menu Dashboard"),
              fluidRow(
                valueBox("2023-06-02","Date",icon = icon("calendar-days"), color = "light-blue",  width = 3),
                valueBox("3571","Existing Employees", icon = icon("users"),color = "teal",  width = 3),
                valueBox("169","Work Accidents",icon = icon("briefcase"), color = "orange",  width = 3),
                valueBox("3552","Unprometed Employees", icon = icon("info"),color = "fuchsia",  width = 3),
              ),
              fluidRow(
                #Diagram 1
                box(title = "Education Level", echarts4rOutput("total_education_level"), width = 3),
                box(
                  title = "Departement", 
                  width = 3,
                  echarts4rOutput("list_Department")
                ),
                box(
                  title = "Age Distribution", 
                  width = 3,
                  echarts4rOutput("Age_Distribution")
                ),
                box(
                  title = "Gender", 
                  width = 3,
                  echarts4rOutput("Gender")
                ),
              ),
              #Diagram 2
              fluidRow(
                box(title = "Stock Level Option", echarts4rOutput("StockOptionLevel"), width = 4),
                box(
                  title = "Education Field", 
                  width = 4,
                  echarts4rOutput("EducationField")
                ),
                box(
                  title = "Over Time", 
                  width = 4,
                  echarts4rOutput("Over_Time")
                ),
                box(
                  title = "Working Years",
                  width = 4,
                  echarts4rOutput("Working_Years")
                ),
                box(
                  title = "Job Level",
                  width = 4,
                  echarts4rOutput("Job_Level")
                ),
                box(
                  title = "Job Role",
                  width = 4,
                  echarts4rOutput("Job_Role")
                )
              ),
              #Diagram 3
              fluidRow(
                box(title = "Job Satisfaction", echarts4rOutput("Job_Satisfaction"), width = 4),
                box(
                  title = "Job Involvement", 
                  width = 4,
                  echarts4rOutput("Job_Involvement")
                ),
                box(
                  title = "Enviroment Satisfaction", 
                  width = 4,
                  echarts4rOutput("Environment_Satisfaction")
                ),
                box(
                  title = "Relationship Satisfaction",
                  width = 4,
                  echarts4rOutput("Relationship_Satisfaction")
                ),
                box(
                  title = "Work-Life Balance",
                  width = 4,
                  echarts4rOutput("Work_Life_Balance")
                ),
                box(
                  title = "Performance Rating",
                  width = 4,
                  echarts4rOutput("Performance_Rating")
                )
              ),
      ),
      tabItem(
        tabName = "emp",
        h1("Employee Dashboard"),
        selectInput("emp","Select Employee by ID", id),
        actionButton("btn","Filter"), 
        textOutput("textOutput") ,
        
        h1("Employee Data"),
        fluidRow(
          valueBox("sales","Employees Roles", icon = icon("users"),color = "teal",  width = 4),
          valueBox("2","Employees Number Project", icon = icon("star"),color = "fuchsia",  width = 4),
          valueBox("low","Employees Salary",icon = icon("plus"), color = "orange",  width = 4),
          
          box(width = 12,
              tags$div(
                class="container", 
                tags$h3("Other Information"),
                tags$p("Avarage Monthly House :"),
                tags$p("Number Project : 2"),
                tags$p("Time Spend Company : 3"),
                tags$p("Work Accident : 0"),
              )),
        ),
        
      ),
      #Update
      tabItem(
        tabName = "profile",
        h1("Employ Profile"),
        
        selectInput(inputId = "profile", label = "id employee", choices = data$EmployeeNumber),
        actionButton(inputId = "Btn", label = "search", style= "margin-bottom:14px"),
        
        uiOutput("coba"),
        fluidRow(
          valueBoxOutput("Yes", width = 3),
          valueBoxOutput("rate", width = 3),
          valueBoxOutput("Sales", width = 3),
          valueBoxOutput("roles", width = 3)
        ),
        
        box(width = 4,
            h3("Personal Information"),
            textOutput(outputId = "edu"),
            textOutput(outputId = "age"),
            textOutput(outputId = "gender"),
            textOutput(outputId = "edufild")),
        
        box(width = 4,
            h3("Job Related Information"),
            textOutput(outputId = "job"),
            textOutput(outputId = "month"),
            textOutput(outputId = "number"),
            textOutput(outputId = "travel")),
        
        box(width = 4,
            h3("Job Satisfaction Information"),
            textOutput(outputId = "satis"),
            textOutput(outputId = "jobsatis"),
            textOutput(outputId = "relation"),
            textOutput(outputId = "work")
        )
        
        
      ),
      
      tabItem(
        tabName = "empp",
        h1("Employee List"),
        dataTableOutput("dynamic"),
      )
    )
  )
)


server <- function(input, output) {
  
  ###########################################################################################
  filterData <- eventReactive(input$Btn,{
    data %>% filter(EmployeeNumber == input$profile)
  })
  
  
  output$roles <- renderValueBox({
    req(filterData())
    data <-  filterData()%>%
      select(JobRole)
    
    valueBox(value= data , subtitle = "Employee Roles", icon = icon("briefcase"))
  })
  
  #######################################
  filterData <- eventReactive(input$Btn,{
    data %>% filter(EmployeeNumber == input$profile)
  })
  
  
  output$Yes <- renderValueBox({
    req(filterData())
    data <-  filterData()%>%
      select(OverTime)
    
    valueBox(value= data$OverTime , subtitle = "Attantion status", icon = icon("user-times"), color = "red")
  })
  
  ########################################################################
  
  filterData <- eventReactive(input$Btn,{
    data %>% filter(EmployeeNumber == input$profile)
  })
  
  
  output$rate <- renderValueBox({
    req(filterData())
    data <-  filterData()%>%
      select(PerformanceRating)
    
    valueBox(value= data$PerformanceRating , subtitle = "Performance Rating", icon = icon("star"))
  })
  ##################################################
  
  filterData <- eventReactive(input$Btn,{
    data %>% filter(EmployeeNumber == input$profile)
  })
  
  
  output$Sales <- renderValueBox({
    req(filterData())
    data <-  filterData()%>%
      select(Department)
    
    valueBox(value= data$Department , subtitle = "Department", icon = icon("user"))
  })
  
  
  
  
  output$edu <- renderText({
    paste0("Education Level: Level ", filterData()$Education)
  })
  
  output$age <- renderText({
    paste0("Age: ", filterData()$Age)
  })
  
  output$gender <- renderText({
    paste0("Gender: ", filterData()$Gender)
  })
  
  output$edufild <- renderText({
    paste0("Education Field ", filterData()$EducationField)
  })
  
  output$job <- renderText({
    paste0("Job :", filterData()$JobRole)
  })
  
  output$month <- renderText({
    paste0("Monthly Income : ", filterData()$MonthlyIncome)
  })
  
  output$number <- renderText({
    paste0("Number of Companies Worked:  ", filterData()$NumCompaniesWorked)
  })
  
  output$travel <- renderText({
    paste0("Business Travel Frequency : ", filterData()$BusinessTravel)
  })
  
  
  output$satis <- renderText({
    paste0("Environment Satisfaction: ", filterData()$EnvironmentSatisfaction)
  })
  
  
  output$jobsatis <- renderText({
    paste0("Job Satisfaction: ", filterData()$JobSatisfaction)
  })
  
  output$relation <- renderText({
    paste0("Relationship Satisfaction: ", filterData()$RelationshipSatisfaction)
  })
  
  output$work <- renderText({
    paste0("Work Life Balance: ", filterData()$WorkLifeBalance)
  })
  
  ###########################################################################################
  
  
  
  
  
  
  
  output$coba <- renderUI({
    req(filterData())
    tagList(
      h1("Employe Data"))
    
  }
  )
  
  
  
  
  set.seed(122)
  output$total_education_level <- renderEcharts4r({
    total_education_level <- data %>% 
      group_by(Education) %>%
      summarise(count= n())%>%
      e_charts(Education) %>%
      e_bar(count, name = "Education") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$list_Department <- renderEcharts4r({
    list_Department <- data %>% 
      group_by(Department) %>%
      summarise(count= n())%>%
      count(Department) %>%
      e_charts(Department) %>%
      e_pie(n, name = "Department") %>%
      e_tooltip() %>%
      e_legend()
    
  })
  
  output$Age_Distribution <- renderEcharts4r({
    Age_Distribution <- data %>% 
      group_by(Age) %>%
      summarise(count= n())%>%
      count(Age) %>%
      e_charts(Age) %>%
      e_pie(n, name = "Age") %>%
      e_tooltip() %>%
      e_legend()
  })
  
  output$Gender <- renderEcharts4r({
    Gender <- data %>% 
      group_by(Gender) %>%
      summarise(count= n())%>%
      count(Gender) %>%
      e_charts(Gender) %>%
      e_pie(n, name = "Gender") %>%
      e_tooltip() %>%
      e_legend()
  })
  
  #DIAGRAM 2
  output$StockOptionLevel <- renderEcharts4r({
    Stock_Level_Option <- data %>% 
      group_by(StockOptionLevel) %>%
      summarise(count= n())%>%
      e_charts(StockOptionLevel) %>%
      e_bar(count, name = "StockOptionLevel") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$EducationField <- renderEcharts4r({
    Education_Field <- data %>% 
      group_by(EducationField) %>%
      summarise(count= n())%>%
      e_charts(EducationField) %>%
      e_bar(count, name = "EducationField") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Over_Time <- renderEcharts4r({
    Over_Time <- data %>% 
      group_by(OverTime) %>%
      summarise(count= n())%>%
      count(OverTime) %>%
      e_charts(OverTime) %>%
      e_pie(n, name = "Over_Time") %>%
      e_tooltip() %>%
      e_legend()
    
  })
  
  #DIAGRAM 3
  
  output$Working_Years <- renderEcharts4r({
    Working_Years <- data %>% 
      group_by(YearsAtCompany) %>%
      summarise(count= n())%>%
      count(YearsAtCompany) %>%
      e_charts(YearsAtCompany) %>%
      e_pie(n, name = "YearsAtCompany") %>%
      e_tooltip() %>%
      e_legend()
    
  })
  
  output$Job_Level <- renderEcharts4r({
    Job_Level <- data %>% 
      group_by(JobLevel) %>%
      summarise(count= n()) %>%
      e_charts(JobLevel) %>%
      e_bar(count, name = "JobLevel") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Job_Role <- renderEcharts4r({
    Job_Role <- data %>% 
      group_by(JobRole) %>%
      summarise(count= n())%>%
      e_charts(JobRole) %>%
      e_bar(count, name = "JobRole") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  #DIAGRAM 3
  
  output$Job_Satisfaction <- renderEcharts4r({
    Job_Satisfaction <- data %>% 
      group_by(JobSatisfaction) %>%
      summarise(count= n())%>%
      e_charts(JobSatisfaction) %>%
      e_bar(count, name = "JobSatisfaction") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Job_Involvement <- renderEcharts4r({
    Job_Involvement <- data %>% 
      group_by(JobInvolvement) %>%
      summarise(count= n())%>%
      e_charts(JobInvolvement) %>%
      e_bar(count, name = "JobInvolvement") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Environment_Satisfaction <- renderEcharts4r({
    Environment_Satisfaction <- data %>% 
      group_by(EnvironmentSatisfaction) %>%
      summarise(count= n())%>%
      e_charts(EnvironmentSatisfaction) %>%
      e_bar(count, name = "EnvironmentSatisfaction") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Relationship_Satisfaction <- renderEcharts4r({
    Relationship_Satisfaction <- data %>% 
      group_by(RelationshipSatisfaction) %>%
      summarise(count= n())%>%
      e_charts(RelationshipSatisfaction) %>%
      e_bar(count, name = "RelationshipSatisfaction") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Work_Life_Balance <- renderEcharts4r({
    Work_Life_Balance <- data %>% 
      group_by(WorkLifeBalance) %>%
      summarise(count= n())%>%
      e_charts(WorkLifeBalance) %>%
      e_bar(count, name = "WorkLifeBalance") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$Performance_Rating <- renderEcharts4r({
    Performance_Rating <- data %>% 
      group_by(PerformanceRating) %>%
      summarise(count= n())%>%
      e_charts(PerformanceRating) %>%
      e_bar(count, name = "PerformanceRating") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  ev <- eventReactive(input$btn,{
    input$emp
  }) 
  output$output <- renderText(ev())
  
  # Output berupa tabel
  output$dynamic <- renderDataTable(data, options = list(pageLength = 10))
  
}

shinyApp(ui, server,enableBookmarking = "url")

