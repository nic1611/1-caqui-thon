library(tidyr)
library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(tidyverse)
source('./src/getData.R')

data <- get()

content <- content(data)

hits <- content$hits$hits

df <- data.frame()


for (v in hits) {
    obj <- v$`_source`
    df <- rbind(df, data.frame(t(sapply(obj,c))))
}

df$createdOn <- as.Date(df$createdOn)

df_client_group <- group_by(df, companyName, createdOn) %>% summarise(acessos = length(companyName))

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                     tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                     textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                     passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                     br(),
                     div(
                         style = "text-align: center;",
                         actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                         shinyjs::hidden(
                             div(id = "nomatch",
                                 tags$p("Oops! Incorrect username or password!",
                                        style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                        class = "text-center")))
                     ))
)

credentials = data.frame(
    username_id = c("bne"),
    passod   = sapply(c("teste"),password_store),
    permission  = c("advanced"), 
    stringsAsFactors = F
)

header <- dashboardHeader( title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui<-dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
    
    login = FALSE
    USER <- reactiveValues(login = login)
    
    observe({ 
        if (USER$login == FALSE) {
            if (!is.null(input$login)) {
                if (input$login > 0) {
                    Username <- isolate(input$userName)
                    Password <- isolate(input$passwd)
                    if(length(which(credentials$username_id==Username))==1) { 
                        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
                        pasverify <- password_verify(pasmatch, Password)
                        if(pasverify) {
                            USER$login <- TRUE
                        } else {
                            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                        }
                    } else {
                        shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
                        shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
                    }
                } 
            }
        }
        
        output$lastAcessPlot <- renderPlot(
            {
                x <- newDf()$createdOn
                
                hist(x, breaks = df_client_group$createdOn, xlab = "Data")
            }
        )
    })
    
    output$logoutbtn <- renderUI({
        req(USER$login)
        tags$li(a(icon("fa fa-sign-out"), "Logout", 
                  href="javascript:window.location.reload(true)"),
                class = "dropdown", 
                style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
    })
    
    output$sidebarpanel <- renderUI({
        if (USER$login == TRUE ){ 
            sidebarMenu(
                menuItem("Main Page", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Second Page", tabName = "second", icon = icon("th")),
                menuItem("Grafico", tabName="grafico"), icon = icon("dashboard")
            )
        }
    })
    
    output$body <- renderUI({
        if (USER$login == TRUE ) {
            tabItems(
                
                # First tab
                tabItem(tabName ="dashboard", class = "active",
                        fluidRow(
                            box(width = 12, dataTableOutput('results'))
                        )),
                # Second tab
                tabItem(tabName = "second",
                        fluidRow(
                            textInput("text", label = h3("Nome empresa"), value = ""),
                            box(width = 12, dataTableOutput('results2')),
                            box(width = 12, plotOutput("lastAcessPlot"))
                        )),
                
                # Third tab
                tabItem(tabName = "grafico")
            )
        }
        else {
            loginpage
        }
    })
    
    newDf <- reactive({df %>% 
            filter(df$companyName == input$text)})
    
    output$results <-  DT::renderDataTable({
        datatable(df_client_group, options = list(autoWidth = TRUE,
                                                  searching = FALSE))
    })
    
    output$results2 <-  DT::renderDataTable({
        datatable(newDf(), options = list(autoWidth = TRUE,
                                        searching = FALSE))
    })
}

runApp(list(ui = ui, server = server))
