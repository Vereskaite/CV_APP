library(shiny)
library(shinydashboard)
library(timevis)
library(excel.link)

# Professional_Experience_tbl <- xl.read.file("C:/CV_APP/Professional Experience.xlsx")


ui <- dashboardPage(
  dashboardHeader( title = "Resume
                   Diana Vereškaitė",
                   titleWidth = 400),
  dashboardSidebar(
    width = 400,
    sidebarMenu(style = "position: fixed; overflow: visible;",
                menuItem("About", tabName = "About", icon = icon("user"),
                         menuSubItem("Summary", tabName = "Summary"),
                         menuSubItem("Contact Info", tabName = "Contact")),
                menuItem("Education", tabName = "Education", icon = icon("user-graduate")),
                menuItem("Professional Experience", tabName = "Professional", icon = icon("briefcase")),
                menuItem("Skills", tabName = "Skills", icon = icon("brain"))
  )
 ),
 dashboardBody(
   tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #0D4869;
                                }

                                # /* logo when hovered */
                                # .skin-blue .main-header .logo:hover {
                                # background-color: #615ab8;
                                # }
# 
#                                 /* navbar (rest of the header) */
#                                 .skin-blue .main-header .navbar {
#                                 background-color: #146c9f;
#                                 }
                                # 
                                # /* main sidebar */
                                # .skin-blue .main-sidebar {
                                # background-color: #f4b943;
                                # }
                                # 
                                # /* active selected tab in the sidebarmenu */
                                # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                # background-color: #ff0000;
                                # }
                                # 
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #00ff00;
                                color: #0D4869;
                                }
                                # 
                                # /* other links in the sidebarmenu when hovered */
                                # .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                # background-color: #ff69b4;
                                # }
                                # /* toggle button when hovered  */
                                # .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                # background-color: #ff69b4;
                                # }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: ##FFFFF0;
                                }

                                '))),
   tabItems(
     tabItem("Education",
             timevisOutput(outputId = "Education_timeline")),
     tabItem("Summary",
             box(title = "Summary", status = "info",
                 textOutput(outputId = "Summary"))),
     tabItem("Contact",
             fluidRow(box( title = "Email", status = "info", textOutput(outputId = "Email"))),
             fluidRow(box( title = "GitHub", status = "info", textOutput(outputId = "GitHub"))),
             fluidRow(box( title = "Phone Number", status = "info", textOutput(outputId = "Phone"))),
             fluidRow(box( title = "Date of Birth", status = "info", textOutput(outputId = "Birth")))),
     tabItem("Professional",
             fluidRow(box(uiOutput(outputId = "Position_list"))),
             fluidRow(box(uiOutput(outputId = "Position_description"))),
             fluidRow(box(uiOutput(outputId = "Position_functions"))))
     )
  )
)

server <- function(input,output) {
  
  Education_data <- data <- data.frame(
    id      = 1:5,
    content = c("Born",
                "Vilnius Emilijos Pliaterytės high school", 
                "Vilnius Žirmūnų gymnasium",
                "BSc Analysis of Economics",
                "MSc Econometrics"),
    start   = c("1993-10-23","2000-09-01", "2008-09-01", "2012-09-01", "2017-09-01"),
    end     = c(NA,"2008-08-31", "2012-08-31", "2016-08-31", "2019-01-31")
  )

  output$Education_timeline <- renderTimevis(
    timevis(
      Education_data
    )
  )
  
  output$Summary <- renderPrint(
    "Currently an Investment Analyst with Master’s degree in Econometrics and 5+ years of experience in Data Analysis. Proficient in Data Processing, Modelling, Forecasting and Visualizing in order to achieve best solutions for business. Experienced in creating, developing, testing and deploying tools to optimize processes."
  )
  
  output$Email <- renderPrint("veresk.diana@gmail.com")
  
  output$GitHub <- renderPrint("github.com/vereskaite")
  
  output$Phone <- renderPrint("+370 665 12 743")
  
  output$Birth <- renderPrint("1993-10-23")
  
  output$Position_list <- renderUI(
    selectInput(inputId = "Positions",
                label = "Positions",
                choices = unique(Professional_Experience_tbl$Position))
  )
  
  output$Position_description <- renderUI({

    textOutput(outputId = "Position")
    textOutput(outputId = "Company")
    
  })
  
  output$Position <- renderPrint(sub( "_"," ",input$Positions))
  
  # output$Company <- renderTable({
  #   Professional_Experience_tbl %>% 
  #     filter(Position %in% input$Position) %>% 
  #     select(Company) %>% 
  #     group_by(Company) %>% 
  #     summarise()
  # })
  
  Dates_reactive <- reactive({
    Dates <- Professional_Experience_tbl %>% 
      filter(Position %in% input$Position) %>% 
      select(Start_Date, End_Date)
    
    unique(Dates)
  })
  
  
}

help(gsub)

shinyApp(ui = ui, server = server)