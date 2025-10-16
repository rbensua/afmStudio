library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Toggle between Zoom and Filter modes"),

  fluidRow(
    column(3,
           uiOutput("filter_button_ui"),
           br(), br(),
           actionButton("reset_data", "Reset data", class = "btn-warning")
    ),
    column(9, plotlyOutput("scatter"))
  )
)

server <- function(input, output, session) {
  # Original dataset
  df_orig <- mtcars %>% mutate(id = row_number())
  df <- reactiveVal(df_orig)

  # Reactive variable to track mode
  filter_mode <- reactiveVal(FALSE)

  # Dynamic button
  output$filter_button_ui <- renderUI({
    if (filter_mode()) {
      actionButton("filter_mode", "Back to Zoom Mode", class = "btn-success")
    } else {
      actionButton("filter_mode", "Enable Filter Mode", class = "btn-primary")
    }
  })

  # Toggle filter mode
  observeEvent(input$filter_mode, {
    filter_mode(!filter_mode())
  })

  # Reset data
  observeEvent(input$reset_data, {
    df(df_orig)
  })

  # Render plot
  output$scatter <- renderPlotly({
    mode <- if (filter_mode()) "select" else "zoom"

    plot_ly(df(), x = ~wt, y = ~mpg, key = ~id,
            type = "scatter", mode = "markers",
            marker = list(size = 10, color = 'steelblue')) %>%
      layout(dragmode = mode)
  })

  # Observe selected points (only in filter mode)
  observeEvent(event_data("plotly_selected"), {
    req(filter_mode())

    sel <- event_data("plotly_selected")
    if (!is.null(sel) && nrow(sel) > 0) {
      n_points <- nrow(sel)

      # Ask for confirmation
      showModal(modalDialog(
        title = "Confirm data filtering",
        paste("You selected", n_points, "points. Do you want to remove them?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_prune", "Yes, remove", class = "btn-danger")
        )
      ))

      # Temporarily store selection in session
      session$userData$selected_ids <- sel$key
    }
  })

  # Confirm pruning
  observeEvent(input$confirm_prune, {
    removeModal()

    ids_to_remove <- session$userData$selected_ids
    if (!is.null(ids_to_remove)) {
      new_df <- df() %>% filter(!id %in% ids_to_remove)
      df(new_df)
      session$userData$selected_ids <- NULL
    }
  })
}

shinyApp(ui, server)
