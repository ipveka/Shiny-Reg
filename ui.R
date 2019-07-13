
##### User Interface

library("RSQLite")
library("shiny")
library("shinydashboard")
library("markdown")
library("lmtest")
library("mctest")
library("lm.beta")
library("visreg")
library("caret")
library("DT")

### Title:

header <- dashboardHeader(title = "ShinyReg")

### SideBar:

sidebar <- dashboardSidebar(width = 230,
                            
                            hr(),
                            
                            sidebarMenu(
                              menuItem("Home", tabName = "home", icon = icon("home", lib = "glyphicon")),
                              menuItem("Data", tabName = "data", icon = icon("cloud-upload", lib = "glyphicon")),
                              menuItem("Summary", tabName = "summary", icon = icon("stats", lib = "glyphicon")),
                              
                              hr(),
                              
                              menuItem("Regression", tabName = "regression", icon = icon("cog", lib = "glyphicon")),
                              menuItem("Diagnostics", tabName = "diagnostics", icon = icon("cog", lib = "glyphicon")),
                              menuItem("Parameters", tabName = "parameters", icon = icon("cog", lib = "glyphicon")),
                              menuItem("Graphics", tabName = "graphics", icon = icon("cog", lib = "glyphicon")),
                              menuItem("Inference", tabName = "inference", icon = icon("cog", lib = "glyphicon")),
                              
                              hr(),
                              
                              menuItem("About", tabName = "about", icon = icon("user", lib = "glyphicon")),
                              
                              hr(),
                              helpText("Developed by ", 
                                       a("Ignasi Pascual", href = "https://github.com/ipveka"), ".",
                                       style = "padding-left:1em; padding-right:1em;position:absolute;")
                            )
)

### Dashboard:

body <- dashboardBody(
  
  # Tabintes:
  tabItems(
    
    ### TAB 0 = Home:
    
    tabItem(tabName = "home",
            fluidPage(
              box(width = 12,
                  shiny::includeMarkdown("Home.md"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
                  tags$head(tags$style(HTML('
                                            .main-header .logo {
                                            font-family: "Georgia", Times, "Times New Roman", serif;
                                            font-weight: bold;
                                            font-size: 20px;
                                            }
                                            ')))))
            
                  ),
    
    ### TAB 1 = Data:
    tabItem(tabName = "data",
            fluidRow(column(4, box(width = 16,
                                   # Input: Select a file ----
                                   fileInput("file1", "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values,text/plain",
                                                        ".csv")),
                                   # Horizontal line ----
                                   tags$hr(),
                                   # Input: Checkbox if file has header ----
                                   checkboxInput("header", "Header", TRUE),
                                   # Input: Select separator ----
                                   radioButtons("sep", "Separator",
                                                choices = c(Comma = ",",
                                                            Semicolon = ";",
                                                            Tab = "\t"),
                                                selected = ","),
                                   # Input: Select quotes ----
                                   radioButtons("quote", "Quote",
                                                choices = c(None = "",
                                                            "Double Quote" = '"',
                                                            "Single Quote" = "'"),
                                                selected = '"')
            )),
            column(8, box(width = 16, DT::dataTableOutput("table", width = 650))
            ))
    ),
    
    ### TAB 2 = Summary:
    tabItem(tabName = "summary",
            fluidRow(
              column(4, box(width = 16,title = "Summary",
                            solidHeader = FALSE,
                            DT::dataTableOutput(outputId = "toc"),
                            tags$hr(),
                            uiOutput('select'),
                            tags$hr(),
                            DT::dataTableOutput("summary"))),
              column(8, box(width = 16,title = "Graphics",
                            solidHeader = FALSE,
                            plotOutput('plot1'),
                            plotOutput('plot2'),
                            plotOutput('plot3'))))
    ),
    
    ### TAB 3 = Regression:
    tabItem(tabName = "regression",
            fluidRow(column(4, box(width = 16,
                                   uiOutput("model_select"),
                                   uiOutput("var1_select"),
                                   uiOutput("rest_var_select"))),
                     column(8, box(width = 16,mainPanel( helpText("Your Selected variables"),
                                                         verbatimTextOutput("other_val_show")))))
    ),
    
    ### TAB 4 = Graphs:
    tabItem(tabName = "diagnostics",
            fluidRow(
              column(12,box(width = 12,title = "Anova",
                            solidHeader = FALSE, 
                            DT::dataTableOutput(outputId = "anova")))),
            fluidRow(
              column(6,box(width = 12,title = "Joint Significance",
                           solidHeader = FALSE, 
                           DT::dataTableOutput(outputId = "model"))),
              column(6,box(width = 12,title = "Determination",
                           solidHeader = FALSE, 
                           DT::dataTableOutput(outputId = "deter")))),
            fluidRow(
              column(6,box(width = 12,
                           title = "Residuals vs Fitted",
                           solidHeader = TRUE, status = "primary",
                           plotOutput(outputId = "reg1"))),
              column(6,box(width = 12,
                           title = "Normal Q-Q",
                           solidHeader = TRUE, status ="primary",
                           plotOutput(outputId = "reg2")))
            ),
            fluidRow(
              column(6,box(width = 12,
                           title = "Cook's distance",
                           solidHeader = TRUE, status = "primary",
                           plotOutput(outputId = "reg4"))),
              column(6,box(width = 12,
                           title = "Residuals vs Leverage",
                           solidHeader = TRUE, status = "primary",
                           plotOutput(outputId = "reg5"))))
    ),
    
    ### TAB  5 = Parameters
    tabItem(tabName = "parameters",
            fluidRow(column(12,
                            box(width = 12,title = "Summary",
                                solidHeader = FALSE, 
                                DT::dataTableOutput(outputId = "reg")))),
            fluidRow(column(6,
                            box(width = 12,title = "Confidence intervals: Parameter Estimate",
                                solidHeader = FALSE,
                                DT::dataTableOutput(outputId = "confint1"))),
                     column(6,
                            box(width = 12,title = "Confidence intervals: Standardized Parameter Estimate",
                                solidHeader = FALSE,
                                DT::dataTableOutput(outputId = "confint2"))))
    ),
    
    ### TAB 6 = Graphics
    tabItem(tabName = "graphics",
            fluidRow(box(width=12,column(12, align="center",
                                         uiOutput("var_plot")))),
            fluidRow(box(width=12,column(12, align="center", 
                                         mainPanel(plotOutput(outputId = "visreg"),
                                                   width = "100%", height = "600px"
                                                   ))))
    ),
    
    ### TAB 7 = Inference:
    tabItem(tabName = "inference",
            fluidRow(column(12, 
                            box(width = 4,title = "Homocedasticity: White",
                                solidHeader = FALSE, 
                                DT::dataTableOutput(outputId = "white")),
                            box(width = 4,title = "Homocedasticity: Breusch-Pagan",
                                solidHeader = FALSE, 
                                DT::dataTableOutput(outputId = "pagan")),
                            box(width = 4,title = "Autocorrelation: Durbin",
                                solidHeader = FALSE, 
                                DT::dataTableOutput(outputId = "durbin")))),
            fluidRow(column(12,
                            box(width = 12,title = "Lineality: Reset",
                                solidHeader = FALSE, 
                                DT::dataTableOutput(outputId = "reset")))),
            fluidRow(column(12, align="center", box(width = 12, title = "Multicollinearity",
                                solidHeader = FALSE,
                                plotOutput(outputId = "vifs", width = "90%", height = "700px")
                                )))
            ),
    
    ### TAB 8 = About
    tabItem(tabName = "about",
            fluidPage(
              box(width = 12,
                  shiny::includeMarkdown("README.md"))
            )
    )
                  )
                  )

ui <- dashboardPage(header, sidebar, body, skin="purple")

