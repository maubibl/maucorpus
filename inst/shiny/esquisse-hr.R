library(shiny)
library(esquisse)
library(lubridate)
library(dplyr)
library(diva)

# https://getpocket.com/explore/item/the-history-of-the-pivot-table-the-spreadsheet-s-most-powerful-tool?utm_source=pocket-newtab

ra <-
  research_areas %>%
  mutate(id = as.character(id)) %>%
  rename(research_area = eng, scb_topic = id) %>%
  select(scb_topic, research_area)

hr <-
  hr_latest() %>%
  left_join(ss_employment_title, by = c("emp_code" = "id")) %>%
  mutate(age = lubridate::year(Sys.Date()) - yob) %>%
  left_join(ra)

ui <- fluidPage(

  titlePanel("Use esquisse as a Shiny module"),

  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "data",
        label = "Data to use:",
        choices = c("iris", "mtcars", "hr"),
        inline = TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "esquisse",
          esquisserUI(
            id = "esquisse",
            header = FALSE, # dont display gadget title
            choose_data = FALSE # dont display button to change data
          )
        ),
        tabPanel(
          title = "output",
          verbatimTextOutput("module_out")
        )
      )
    )
  )
)


server <- function(input, output, session) {

  data_r <- reactiveValues(data = iris, name = "iris")

  observeEvent(input$data, {
    if (input$data == "hr") {
      data_r$data <- hr
      data_r$name <- "hr"
    } else if(input$data == "iris") {
      data_r$data <- iris
      data_r$name <- "iris"
    } else {
      data_r$data <- mtcars
      data_r$name <- "mtcars"
    }
  })

  result <- callModule(
    module = esquisserServer,
    id = "esquisse",
    data = data_r
  )

  output$module_out <- renderPrint({
    str(reactiveValuesToList(result))
  })

}

shinyApp(ui, server)
