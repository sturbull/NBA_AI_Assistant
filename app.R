library(shiny)
library(bslib)
library(fastmap)
library(duckdb)
library(DBI)
library(fontawesome)
library(reactable)
library(here)
library(plotly)
library(ggplot2)
library(ggridges)
library(dplyr)
library(promises)
library(mirai)

# Allow up to 4 simultaneous chat requests. This number can go higher as it's
# almost all just blocking on the network.
mirai::daemons(4, output = TRUE)

# Open the duckdb database
conn <- dbConnect(duckdb(), dbdir = here("NBA.duckdb"), read_only = TRUE)
# Close the database when the app stops
onStop(\() dbDisconnect(conn))

# Dynamically create the system prompt, based on the real data. For an actually
# large database, you wouldn't want to retrieve all the data like this, but
# instead either hand-write the schema or write your own routine that is more
# efficient than system_prompt().
#
# This value has the shape list(role = "system", content = "<SYSTEM_PROMPT>")
# TODO: update prompt to use SCHEMA not data.

system_prompt_msg <- system_prompt(dbGetQuery(conn, "SELECT * FROM NBA"), "NBA")

# This is the greeting that should initially appear in the sidebar when the app
# loads.
greeting <- paste(readLines(here("greeting.md")), collapse = "\n")

icon_explain <- tags$img(src = "stars.svg")

ui <- page_sidebar(
  theme=bs_theme(preset = 'zephyr'),
#  style = "background-color: rgb(248, 248, 248);",
  title = "🏀 NBA Draft Combine Assistant 🏀",
  includeCSS(here("styles.css")),
  sidebar = sidebar(
    width = 400,
    style = "height: 100%;",
    selectInput("model", NULL, c("GPT-4o" = "gpt-4o", "GPT-4o Mini" = "gpt-4o-mini")),
    chat_ui("chat", height = "100%", fill = TRUE)
  ),
  useBusyIndicators(),

  # 🏷️ Header
  textOutput("show_title", container = h3),
  verbatimTextOutput("show_query") |>
    tagAppendAttributes(style = "max-height: 100px; overflow: auto;"),

  layout_columns(
    style = "min-height: 450px;",
    col_widths = c(18),

    # 🔍 Data table
    card(
      style = "height: 500px;",
      card_header("NBA data"),
      reactableOutput("table", height = "100%")
    ),
  ),
  # WHERE YOUR FOOTER GOES
  hr(),
  tags$footer(
    tags$div(
      tags$span(
        "Contact us at: ",
        tags$a(
          href = "mailto:analytics@synergia.co.nz",
          "analytics@synergia.co.nz",
          alt = "Email Synergia",
          target = "_blank"
        )
      ),
      tags$br(),
      tags$a(
        href = "https://synergia.consulting",
        tags$img(
          src = "logo.jpg",
          alt = "Synergia Logo",
          width = "25px"
        )
      ),
      paste(" ©", format(Sys.Date(), "%Y"), "Synergia Ltd. All rights reserved."),
      tags$span(
        "Example data sourced from: ",
        tags$a(
          href = "https://www.kaggle.com/datasets/marcusfern/nba-draft-combine",
          "Kaggle",
          alt = "Data Source",
          target = "_blank"
        ),
        "under license ",
        tags$a(
          href = "https://creativecommons.org/licenses/by/4.0/",
          "Attribution 4.0 International (CC BY 4.0)",
          alt = "Data Source",
          target = "_blank"
        ),
      ),
    )
  )

)

server <- function(input, output, session) {
  # 🔄 Reactive state/computation --------------------------------------------

  current_title <- reactiveVal(NULL)
  current_query <- reactiveVal("")

  # This object must always be passed as the `.ctx` argument to query(), so that
  # tool functions can access the context they need to do their jobs; in this
  # case, the database connection that query() needs.
  ctx <- list(conn = conn)

  # The reactive data frame. Either returns the entire dataset, or filtered by
  # whatever Sidebot decided.
  NBA_data <- reactive({
    sql <- current_query()
    if (is.null(sql) || sql == "") {
      sql <- "SELECT * FROM NBA;"
    }
    dbGetQuery(conn, sql)
  })



  # 🏷️ Header outputs --------------------------------------------------------

  output$show_title <- renderText({
    current_title()
  })

  output$show_query <- renderText({
    current_query()
  })



  # 🔍 Data table ------------------------------------------------------------

  output$table <- renderReactable({
    reactable(NBA_data(),
      pagination = FALSE, bordered = TRUE
    )
  })




  # ✨ Sidebot ✨ -------------------------------------------------------------

  # The entire chat history up to this point, from the assistant's point of view
  # (not the user). We'll add new messages whenever the user asks a new question
  # or the chat model returns a new response.
  #
  # (We could've just used a list here, but fastqueue has a nicer looking API
  # for adding new elements.)
  messages <- fastqueue()

  # Preload the conversation with the system prompt. These are instructions for
  # the chat model, and must not be shown to the end user.
  messages$add(system_prompt_msg)

  # Prepopulate the chat UI with a welcome message that appears to be from the
  # chat model (but is actually hard-coded). This is just for the user, not for
  # the chat model to see.
  chat_append_message("chat", list(
    role = "assistant",
    content = greeting
  ))

  # Handle user input
  observeEvent(input$chat_user_input, {
    # Add user message to the chat history
    messages$add(
      list(role = "user", content = input$chat_user_input)
    )

    prog <- Progress$new()
    prog$set(value = NULL, message = "Thinking...")

    mirai(
      msgs = messages$as_list(),
      model = input$model,
      {
        library(duckdb)
        library(DBI)
        library(here)
        source(here("R/query.R"), local = TRUE)

        conn <- dbConnect(duckdb(), dbdir = here("NBA.duckdb"), read_only = TRUE)
        on.exit(dbDisconnect(conn))

        result_query <- NULL
        result_title <- NULL

        update_dashboard <- function(query, title) {
          result_query <<- query
          result_title <<- title
        }

        ctx <- list(conn = conn, update_dashboard = update_dashboard)

        c(
          query(msgs, model = model, .ctx = ctx),
          query = result_query,
          title = result_title
        )
      }
    ) |>
      then(\(result) {
        for (imsg in result$intermediate_messages) {
          messages$add(imsg)
        }

        if (!is.null(result$query)) {
          current_query(result$query)
        }
        if (!is.null(result$title)) {
          current_title(result$title)
        }

        completion <- result$completion

        response_msg <- completion$choices[[1]]$message
        # print(response_msg)

        # Add response to the chat history
        messages$add(response_msg)

        chat_append_message("chat", response_msg)
      }) |>
      catch(\(err) {
        print(err)
        err_msg <- list(
          role = "assistant",
          # TODO: Make sure error doesn't contain HTML
          content = paste0("**Error:** ", conditionMessage(err))
        )
        messages$add(err_msg)
        chat_append_message("chat", err_msg)
      }) |>
      finally(\() {
        prog$close()
      })
  })
}

shinyApp(ui, server)
