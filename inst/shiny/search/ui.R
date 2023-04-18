library(shiny)

ui <- fluidPage(

  titlePanel("(re)searcher(s)"),

  shinyauthr::loginUI(id = "login"),
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  hr(),
  textInput(
    inputId = "searchterm",
    label = "Enter at least three letters:",
    width = "80%",
    placeholder = "Please enter researcher name search string"
  ),

  selectInput(
    inputId = "rows",
    label = "Number of rows",
    choices = c(10, 50, 100, 150),
    selected = 100
  ),

  actionButton(
    inputId = "start_job",
    label = "Search",
    icon = icon("bolt")),

  DT::dataTableOutput("result_table")
)
