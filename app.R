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
    title = "SIKAP"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Main Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Attrition Dashboard", tabName = "emp", icon = icon("users")),
      menuItem("Employee Profiler", tabName = "empp", icon = icon("user")),
      menuItem("Employee Databases", tabName = "emppp", icon = icon("users"))
      
    )
  ),
  dashboardBody(
    bookmarkButton(),
    tabItems(
      tabItem(tabName = "dashboard",
              h1("Main Dashboard"),
              fluidRow(
                valueBox(date(),"Date",icon = icon("calendar-days"), color = "aqua",  width = 6),
                valueBox("1233","Existing Employees", icon = icon("users"),color = "aqua",  width = 6),
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
        h1("Attrition Employee Dashboard"),
        fluidRow(
          box(
            width = 4,
            h2("Attrited vs Existing Employee"), echarts4rOutput("fluidrow1_diagram_pie_1")
          ),
          box(
            width = 8,
            h2("Attrition by Department"), echarts4rOutput("fluidrow1_diagram_batang_1")
          )),
        
        fluidRow(box(h2("Attrition by Job Role"), echarts4rOutput("fluidrow2_diagram_batang2"),width = 12
        )),
        
        fluidRow(
          box(
            width = 4,
            h2("Attrition by Gender", echarts4rOutput("fluidrow3_diagram_pie_3")
            )),
          box(
            width = 8, 
            h2("Attrition by Stock Level Option", echarts4rOutput("fluidrow3_diagram_batang_3")
            ))
        )
      ),
      
      #Update
      tabItem(
        tabName = "empp",
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
        tabName = "emppp",
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
  
  
  #====== Server Halaman Menu 2 =======#
  
  #fluidrow_1
  output$fluidrow1_diagram_pie_1 <- renderEcharts4r({
    data %>%
      count(Attrition) %>%
      mutate(ratio = round(n/sum(n)*100, digits = 2)) %>% 
      ## memvisualisasikan hasil count(sales) menggunakan echarts4r
      #selalu diawali e_charts()
      e_charts(Attrition) %>% 
      #menampilkan visualisasi menggunakan diagram lingkaran / pie
      e_pie(ratio, name="Rasio") %>%
      ##untuk menampilan nilai ketika mouse hover
      e_tooltip() %>% 
      ##untuk menampilkan legenda di visualisasi
      e_legend() %>% 
      ##mengatur warna color dan background
      e_color(color = c("#193","#147"), background = "white")
  })
  
  output$fluidrow1_diagram_batang_1  <- renderEcharts4r({
    data %>% 
      count(Attrition, Department, name = "Freq") %>% 
      group_by(Department) %>% 
      mutate(Ratio = round(Freq * 100 / sum(Freq), digits = 2)) %>% 
      ungroup() %>% 
      group_by(Attrition) %>% 
      e_charts(Department) %>% 
      e_bar(Ratio) %>% 
      e_tooltip("axis")
  })
  
  #fluidrow_2
  output$fluidrow2_diagram_batang2 <- renderEcharts4r({
    data %>%
      #1.###### <---- Ubah menggunakan kolom salary ---- >
      count(Attrition,JobRole) %>% #menghitung jumlah masing-masing value pada kolom sales
      arrange(n) %>% #men-sorting berdasarkan value n hasil dari count(sales)
      ## memvisualisasikan hasil count(sales) menggunakan echarts4r
      #selalu diawali e_charts()
      group_by(Attrition) %>% 
      e_charts(JobRole) %>% 
      #menampilkan visualisasi menggunakan bar plot
      e_bar(n) %>%
      ######## <---- ### ---->
      ##untuk menampilan nilai ketika mouse hover
      e_tooltip %>% 
      ##untuk menampilkan legenda di visualisasi
      e_legend() %>% 
      ##mengatur warna color dan background
      e_color(color = c("#193","#147"), background = "white")  
  })
  
  #fluidrow_3
  output$fluidrow3_diagram_pie_3 <- renderEcharts4r({
    data %>%
      #count(Attrition,Gender) %>%
      filter(Attrition == "Yes") %>% 
      count(Gender, name = "Rasio") %>% 
      #arrange(n) %>%
      mutate(ratio = round(Rasio/sum(Rasio)*100, digits = 2)) %>% 
      ## memvisualisasikan hasil count(sales) menggunakan echarts4r
      #selalu diawali e_charts()
      #group_by(Gender) %>% 
      e_charts(Gender) %>% 
      #menampilkan visualisasi menggunakan diagram lingkaran / pie
      e_pie(ratio) %>%
      ##untuk menampilan nilai ketika mouse hover
      e_tooltip() %>% 
      ##untuk menampilkan legenda di visualisasi
      e_legend() %>% 
      ##mengatur warna color dan background
      e_color(color = c("#193","#147"), background = "white")
  })
  
  output$fluidrow3_diagram_batang_3 <- renderEcharts4r({
    data %>%
      #1.###### <---- Ubah menggunakan kolom salary ---- >
      count(Attrition,StockOptionLevel) %>% #menghitung jumlah masing-masing value pada kolom sales
      arrange(n) %>% #men-sorting berdasarkan value n hasil dari count(sales)
      ## memvisualisasikan hasil count(sales) menggunakan echarts4r
      #selalu diawali e_charts()
      group_by(Attrition) %>% 
      e_charts(StockOptionLevel) %>% 
      #menampilkan visualisasi menggunakan bar plot
      e_bar(n) %>%
      ######## <---- ### ---->
      ##untuk menampilan nilai ketika mouse hover
      e_tooltip %>% 
      ##untuk menampilkan legenda di visualisasi
      e_legend() %>% 
      ##mengatur warna color dan background
      e_color(color = c("#193","#147"), background = "white")
  })
  
  #========= Akhir Server Halaman Menu 2 =======#
  
  ev <- eventReactive(input$btn,{
    input$emp
  }) 
  output$output <- renderText(ev())
  
  # Output berupa tabel
  output$dynamic <- renderDataTable(data, options = list(pageLength = 10))
  
}

shinyApp(ui, server,enableBookmarking = "url")

