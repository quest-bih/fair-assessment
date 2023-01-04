# remotes::install_github("rstudio/shinythemes")
# 
# library(shiny)
# library(shinythemes)
# 
# ui <- navbarPage(title = "Dashboard", id = "navbarID", theme = shinytheme("flatly"), 
#                  tabPanel("Page 1", value = "page_1", "This is Page 1"),
#                  tabPanel("Page 2", value = "page_2", "This is Page 2")
# )
# 
# server <- function(input, output, session){
#   observeEvent(input$navbarID, {
#     # http://127.0.0.1:3252/#page_1
#     # http://127.0.0.1:3252/#page_2
#     
#     newURL <- paste0(
#       session$clientData$url_protocol,
#       "//",
#       session$clientData$url_hostname,
#       ":",
#       session$clientData$url_port,
#       session$clientData$url_pathname,
#       "#",
#       input$navbarID
#     )
#     updateQueryString(newURL, mode = "replace", session)
#   })
#   
#   observe({
#     currentTab <- sub("#", "", session$clientData$url_hash)
#     if(!is.null(currentTab)){
#       updateTabsetPanel(session, "navbarID", selected = currentTab)
#     }
#   })
# }
# 
# shinyApp(ui, server)

########

library(shiny)
library(shinythemes)

ui <- navbarPage(title = "Dashboard", id = "navbarID", theme = shinytheme("flatly"), 
                 tabPanel("Page 1", value = "page_1", "This is Page 1"),
                 tabPanel("Page 2", value = "page_2", "This is Page 2")
)

server <- function(input, output, session){
  observeEvent(input$navbarID, {
    # http://127.0.0.1:3252/#page_1
    # http://127.0.0.1:3252/#page_2
    
    newURL <- paste0("#", input$navbarID)
    updateQueryString(newURL, mode = "push", session)
  })
  
  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabsetPanel(session, "navbarID", selected = currentTab)
    }
  })
}

shinyApp(ui, server)




#########


# 
# library(shiny)
# library(shiny.router)
# 
# 
# home_page <- div(
#   titlePanel("Dashboard"),
#   p("This is a dashboard page")
# )
# 
# settings_page <- div(
#   titlePanel("Settings"),
#   p("This is a settings page")
# )
# 
# contact_page <- div(
#   titlePanel("Contact"),
#   p("This is a contact page")
# )
# 
# 
# router <- make_router(
#   route("/", home_page),
#   route("settings", settings_page),
#   route("contact", contact_page)
# )
# 
# 
# ui <- fluidPage(theme = shinytheme("flatly"),
#                 
#                 
#                 tags$ul(class="navbar navbar-expand-lg navbar-dark bg-primary",
#                         a(class="navbar-brand", href = route_link("/"), "Dashboard"),
#                         a(class="navbar-brand", href = route_link("settings"), "Settings"),
#                         a(class="navbar-brand", href = route_link("contact"), "Contact")
#                 ),
#                 router$ui
# )
# 
# server <- function(input, output, session) {
#   router$server(input, output, session)
# }
# 
# shinyApp(ui, server)