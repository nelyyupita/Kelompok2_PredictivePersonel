library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(DT)
library(echarts4r) 

data <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
view(data)

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
        textOutput("output") ,
        
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
      
      tabItem(
        tabName = "empp",
        h1("Employee List"),
        dataTableOutput("dynamic"),
      )
    ),
  )
)

server <- function(input, output) {
  
  
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$visualisasi1 <- renderEcharts4r({
    data %>%
      #filter(left == 0) %>%
      #gunakan kolom sales untuk mengganti _
      count(sales) %>%
      arrange(n) %>%
      e_charts(sales) %>%
      e_bar(n, name = "sales") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords()%>%
      e_color(color = "teal", background = "white")  
  })
  
  output$visualisasi2 <- renderEcharts4r({
    data %>%
      count(salary) %>%
      e_charts(salary) %>%
      e_pie(n, name = "salary") %>%
      e_tooltip() %>%
      e_legend()
    
  })
  
  output$visualisasi3 <- renderEcharts4r({
    data %>%
      count(last_evaluation) %>%
      arrange(n) %>%
      e_charts(last_evaluation) %>%
      #e_charts(satisfaction_level) %>%
      e_bar(n, name = "") %>%
      e_tooltip() %>%
      e_flip_coords() %>%
      e_legend()
  })
  
  output$visualisasi4 <- renderEcharts4r({
    data %>%
      count(sales) %>%
      arrange(n) %>%
      e_charts(sales) %>%
      e_bar(n, name = "sales") %>%
      e_tooltip() %>%
      e_legend() %>%
      e_flip_coords() %>%
      e_color(color = "teal", background = "white")  
  })
  
  output$visualisasi5 <- renderEcharts4r({
    data %>%
      count(sales) %>%
      arrange(n) %>%
      e_charts(sales) %>%
      e_bar(n, name = "sales") %>%
      e_tooltip() %>%
      e_legend()
    
  })
  
  ev <- eventReactive(input$btn,{
    input$emp
  }) 
  output$output <- renderText(ev())
  
  # Output berupa tabel
  output$dynamic <- renderDataTable(data, options = list(pageLength = 10))
  
}

shinyApp(ui, server,enableBookmarking = "url")

