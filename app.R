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
                column(
                  width = 4,
                  selectInput(inputId = "x", 
                              label = "Left or Stay :", 
                              #choices = c("left","stay"),
                              left,
                              selected = 'left'),
                ),
                
              ),
              fluidRow(
                box(title = "Employees by Roles", echarts4rOutput("visualisasi1"), width = 12),
              ),
              fluidRow(
                box(
                  title = "Employees by Salary Group", 
                  width = 6,
                  echarts4rOutput("visualisasi2")
                ),
                box(
                  title = "Satisfaction & Evaluation",
                  width = 6,
                  echarts4rOutput("visualisasi3")
                )
              ),
              fluidRow(
                box(
                  title = "Employees by Projects Done",
                  width = 6,
                  echarts4rOutput("visualisasi4")
                ),
                box(
                  title = "Average Monthly Hours Distribution",
                  width = 6,
                  echarts4rOutput("visualisasi5")
                )
              )
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
  # histdata <- rnorm(500)
  # 
  # output$plot1 <- renderPlot({
  #   data <- histdata[seq_len(input$slider)]
  #   hist(data)
  # })
  
  # output$visualisasi1 <- renderEcharts4r({
  #   data %>%
  #     #filter(left == 0) %>%
  #     #gunakan kolom sales untuk mengganti _
  #     count(JobRole) %>%
  #     arrange(n) %>%
  #     e_charts(JobRole) %>%
  #     e_bar(n, name = "Role") %>%
  #     e_tooltip() %>%
  #     e_legend() %>%
  #     e_flip_coords()%>%
  #     e_color(color = "teal", background = "white")  
  # })
  # 
  # output$visualisasi2 <- renderEcharts4r({
  #   data %>%
  #     count(salary) %>%
  #     e_charts(salary) %>%
  #     e_pie(n, name = "salary") %>%
  #     e_tooltip() %>%
  #     e_legend()
  #   
  # })
  
  # output$visualisasi3 <- renderEcharts4r({
  #   data %>%
  #     count(last_evaluation) %>%
  #     arrange(n) %>%
  #     e_charts(last_evaluation) %>%
  #     #e_charts(satisfaction_level) %>%
  #     e_bar(n, name = "") %>%
  #     e_tooltip() %>%
  #     e_flip_coords() %>%
  #     e_legend()
  # })
  
  # output$visualisasi4 <- renderEcharts4r({
  #   data %>%
  #     count(sales) %>%
  #     arrange(n) %>%
  #     e_charts(sales) %>%
  #     e_bar(n, name = "sales") %>%
  #     e_tooltip() %>%
  #     e_legend() %>%
  #     e_flip_coords() %>%
  #     e_color(color = "teal", background = "white")  
  # })
  
  # output$visualisasi5 <- renderEcharts4r({
  #   data %>%
  #     count(sales) %>%
  #     arrange(n) %>%
  #     e_charts(sales) %>%
  #     e_bar(n, name = "sales") %>%
  #     e_tooltip() %>%
  #     e_legend()
  #   
  # })
  
  ev <- eventReactive(input$btn,{
    input$emp
  }) 
  output$output <- renderText(ev())
  
  # Output berupa tabel
  output$dynamic <- renderDataTable(data, options = list(pageLength = 10))
  
}

shinyApp(ui, server,enableBookmarking = "url")

