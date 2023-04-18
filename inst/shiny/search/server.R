library(shiny)
library(shiny.blueprint)
library(promises)
library(future)
library(bslib)

# Options for asynchronous strategies: multisession,
# multicore (not Windows/RStudio), cluster

plan(multisession)

user_base <- dplyr::tibble(
  user = c("divaapan"),
  password = c("secret"),
  permissions = c("admin"),
  name = c("DiVA-Apan")
)

server <- function(input, output, session) {

  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )

  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  # initiate reactive values
  table_dt <- reactiveVal(NULL)

  # start expensive calculation
  observeEvent(input$start_job, {

    # update actionButton to show we are busy
    updateActionButton(
      inputId = "start_job",
      label = "Search",
      icon = icon("sync", class = "fa-spin")
    )

    # reactive values and reactive expressions cannot be read from
    # within a future, therefore, you need to read any reactive
    # values/expressions in advance of launching the future
    rows <- input$rows
    term <- input$searchterm

    future_promise({
      # long computation
      match_near(term, rows)
    })  %...>%
      table_dt()

  })

  # Display the table data
  output$result_table <- DT::renderDT({

    req(credentials()$user_auth, input$searchterm, input$rows, table_dt())

    rg_col <- which(colnames(table_dt()) == "kthid")
    my_data <- table_dt() |> dplyr::select(-c("term"))

    DT::datatable(
      data = my_data,
      rownames = FALSE,
      escape = FALSE,
      style = "auto",
      extensions = 'RowGroup',
      options = list(
        selection = 'none',
        rowGroup = list(dataSrc = 1),
        pageLength = input$rows,
        # l - length changing input control
        # f - filtering input
        # t - The table!
        # i - Table information summary
        # p - pagination control
        # r - processing display element
        dom = "ifpt"
      )
    )

  })

  observe({

    req(table_dt())#, input$start_job)

    # update actionButton to show data is available and
    # we're ready for another calculation
    updateActionButton(inputId = "start_job",
                       label = "Search",
                       icon = icon("bolt"))

  })

}


