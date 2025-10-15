#' Server logic for afmStudio
#' @noRd
server <- function(input, output, session) {

  reactable_opts <- list(
    compact = TRUE, sortable = TRUE, filterable = TRUE, striped = TRUE,
    class = 'small', defaultPageSize = 100, rownames = FALSE
  )
  # ---- Set computer volumes for the folder selection ----

  volumes <- c(Home = getwd(), "R Installation" = R.home(), getVolumes()())

  shinyDirChoose(input,id =  "dir_select_btn",
                 roots = volumes,
                 session = session,
                 restrictions = system.file(package = "base"),
                 allowDirCreate = FALSE)


  # Parse path
  folder_path <- reactive({
    req(input$dir_select_btn)
    parseDirPath(volumes, input$dir_select_btn)
  })



  # ---- Set reactive variables ----

  # We initialise with an empty data object
  load_data <- list(file = NULL, cols = NULL, error = NULL)

  afmdata <- reactiveVal() # Initialization of the afmdata reactive value

  reactives <- reactiveValues(
    file = NULL,
    data = load_data,
    filename = NULL
  )

  # Reactive value for filter mode in plots
  filter_mode <- reactiveVal(FALSE)



  # ---- Data ----

  user_file <- file_upload_srv('file_upload')

  observeEvent(user_file(), {
    x <- user_file()
    if (!is.null(x$file) && all(dim(x$file) > 0)) {
      reactives$data <- x
    }
  }, ignoreInit = TRUE)


  data <- reactive({
    # Return the active data object. This can be a data frame sent with the run_pioneer
    # call, a restored data object from a previous session, or a new dataset uploaded
    # by the user.
    reactives$data
  })

  observeEvent(folder_path(),{
    if(length(folder_path())){
      showModal(modalDialog(
        title = "Processing folder...",
        tagList(
          h5("Please wait..."),
          div(style="text-align:center;",
              # Simple CSS spinner
              tags$div(class="lds-dual-ring")
          )
        ),
        footer = NULL,
        easyClose = FALSE
      ))

      res <- afmReadJPKFolder(folder_path())

      afmdata(res) # update reactive value of afmdata
      removeModal()
      showModal(modalDialog(
        title = "Done!",
        "All files processed successfully.",
        easyClose = TRUE
      ))}

  }
  )
  # ---- Show curves list in reactable ----

  output$curves_list_data <- renderReactable({
    if(!is.null(afmdata())){

      files <- names(afmdata())
      df <- data.frame(Curve = files)
      opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
        Curve = colDef(sticky = 'left')),
        onClick = 'select', selection = 'single'
      )
      do.call(reactable, opts)}else{
        NULL
      }
  })
  output$curves_list_prep <- renderReactable({
    if(!is.null(afmdata())){

      files <- names(afmdata())
      df <- data.frame(Curve = files)
      opts <- rlang::list2(!!!reactable_opts, data = df, columns = list(
        Curve = colDef(sticky = 'left')),
        onClick = 'select', selection = 'single'
      )
      do.call(reactable, opts)}else{
        NULL
      }
  })

  # Render plot widgets ----
    ## Preview data Tab plot ----
  output$data_preview_data <- renderUI({
    if(!is.null(afmdata())){
      card(
        layout_column_wrap(
          fill = TRUE,
          selectInput("xaxis_data",label = "X axis",choices = c("Z", "Time", "Indentation")),
          selectInput("yaxis_data", label = "Y axis", choices = c("Force", "ForceCorrected", "Z")),
          uiOutput("filter_button_ui"),
          br(), br(),
          actionButton("reset_data", "Reset data", class = "btn-warning")
          #checkboxInput("showCP", label = "Show Contact Point", value = FALSE)
          #checkboxInput("showDP", label = "Show Detach Point", value = FALSE),
          #checkboxInput("showZ0", label = "Show Z0 Point", value = FALSE)
        ),
        plotlyOutput('plt_data')
      )
    }else {
      NULL
    }
  })

  ## Preprocessing Tab plot ----

  output$data_preview_prep <- renderUI({
    if(!is.null(afmdata())){
      card(
        layout_column_wrap(
          fill = TRUE,
          selectInput("xaxis_prep",label = "X axis",choices = c("Z", "Time", "Indentation")),
          selectInput("yaxis_prep", label = "Y axis", choices = c("Force", "ForceCorrected", "Z")),
          checkboxGroupInput("showPoints",
                             label = NULL,
                             choices = c("Contact point" = "CP",
                                         "Detach point" = "DP",
                                         "Zero-F point" = "Z0")
                            )
          #checkboxInput("showCP", label = "Show Contact Point", value = FALSE),
          #checkboxInput("showDP", label = "Show Detach Point", value = FALSE),
          #checkboxInput("showZ0", label = "Show Z0 Point", value = FALSE)
        ),
        plotlyOutput('plt_prep')
      )
    }else {
      NULL
    }
  })

  # Plot selected curve ----

  ## Data preview tab ----
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

  output$plt_data <- renderPlotly({
    s <- getReactableState("curves_list_data", "selected")
    afmdata <- isolate(afmdata())
    print(filter_mode())
    mode <- if (filter_mode()) "select" else "zoom"
    print(mode)
    fill_colors <- c(
      approach = "#1f77b4",
      pause = "#ff7f0e",
      retract = "#2ca02c"
    )
    if(length(s)){
      df <- afmdata[[s]]$data

      df2 <- data.frame(X = df[,input$xaxis_data],
                        Y = df[,input$yaxis_data],
                        Segment = df[,"Segment"])
      print(mode)

      fig <- plot_ly(df2,
                     x = ~X,
                     y = ~Y,
                     type = "scatter",
                     mode = "lines+markers",
                     marker = list(opacity = 0, size = 8),
                     color = ~Segment,
                     colors = fill_colors
      ) |> layout(dragmode = mode)

      fig
    }else{
      NULL
    }
  })

  # Observe selected points (only in filter mode)
  observeEvent(event_data("plotly_selected"), {
    print(event_data("plotly_selected"))
    req(filter_mode())

    sel <- event_data("plotly_selected")
    print(sel)
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
      print(session$userData$selected_ids)
    }
  })

  # Confirm pruning
  observeEvent(input$confirm_prune, {
    removeModal()
    s <- getReactableState("curves_list_data", "selected")
    afmdata <- isolate(afmdata())
    df <- afmdata[[s]]$data
    ids_to_remove <- session$userData$selected_ids
    print(ids_to_remove)
    if (!is.null(ids_to_remove)) {
      new_df <- df %>% filter(!id %in% ids_to_remove)
      afmdata[[s]]$data <- (new_df)
      session$userData$selected_ids <- NULL
    }
    afmdata(afmdata)
  })



  ## Data preprocessing tab ----
  output$plt_prep <- renderPlotly({
    s <- getReactableState("curves_list_prep", "selected")
    afmdata <- isolate(afmdata())
    # Define fill colors for segments
    fill_colors <- c(
      approach = "#1f77b4",
      pause = "#ff7f0e",
      retract = "#2ca02c"
    )
    if(length(s)){
      df <- afmdata[[s]]$data

      df2 <- data.frame(X = df[,input$xaxis_prep], Y = df[,input$yaxis_prep],
                        Segment = df[,"Segment"])
      fig <- plot_ly(
        df2,
        type = "scatter",
        mode = "markers",
        x = ~X,
        y = ~Y,
        color = ~Segment,
        colors = fill_colors,
        marker = list(
          opacity = 0.6,
          line = list(
            color = "black",
            width = .5
          )
        )
      )

      print(input$showPoints)

      line_specs <- list(
        CP = list(pos = afmdata[[s]]$CP$CP, color = fill_colors["approach"], label = "CP"),
        DP = list(pos = afmdata[[s]]$DP$DP, color = fill_colors["retract"], label = "DP"),
        Z0 = list(pos = afmdata[[s]]$Slopes$Z0Point, color = fill_colors["approach"], label = "Z0")
      )

      shapes = list()

      if(!is.null(input$showPoints)){
        for (line_id in input$showPoints){
          spec <- line_specs[[line_id]]

          shapes <- append(shapes, list(
            list(
              type = "line",
              x0 = spec$pos,
              x1 = spec$pos,
              y0 = 0,
              y1 = 1,
              yref = "paper",
              line = list(color = spec$color, dash = "dash", width =2)
            )
          ))
                  }
      }

      # Add shapes and annotations
      fig <- layout(fig, shapes = shapes)#, annotations = annotations)



      if(input$showCP){
        if ("CP" %in% names(afmdata[[s]])){
          fig <- fig |> layout(shapes = list(vline(afmdata[[s]]$CP$CP)))
        }else{
          showModal(modalDialog(
            title = "No contact point found!",
            "Run Contact Point Estimation First.",
            easyClose = TRUE
          ))
        }
      }


      fig
    }else{
      NULL
    }
  })

  # ---- Preprocessing ----

   ## ---- Contact point estimation ----

  observeEvent(input$CP_go,{
    mul1 <- input$mul1_cp
    mul2 <- input$mul2_cp
    width <- input$width_cp
    LOESS <- !is.null(input$loess_cp)
    Delta <- !is.null(input$delta_cp)
    datatemp <- afmdata()

     showModal(modalDialog(
       title = "Processing...",
       tagList(
         h4("Please wait..."),
         div(style="text-align:center;",
             # Simple CSS spinner
             tags$div(class="lds-dual-ring")
         )
       ),
       footer = NULL,
       easyClose = FALSE
     ))

    CP <- afmContactPoint(datatemp, width = width, mul1 = mul1, mul2 = mul2, Delta = Delta, loessSmooth = LOESS)
    #   print(str(CP))

    removeModal()

    showModal(modalDialog(
      title = "Done!",
      "All files processed successfully.",
      easyClose = TRUE
    ))

    afmdata(CP)

  })

  ## ---- Detach point estimation ----

  observeEvent(input$DP_go,{
    mul1 <- input$mul1_dp
    mul2 <- input$mul2_dp
    width <- input$width_dp
    LOESS <- !is.null(input$loess_dp)
    Delta <- !is.null(input$delta_dp)
    datatemp <- afmdata()

    showModal(modalDialog(
      title = "Processing...",
      tagList(
        h4("Please wait..."),
        div(style="text-align:center;",
            # Simple CSS spinner
            tags$div(class="lds-dual-ring")
        )
      ),
      footer = NULL,
      easyClose = FALSE
    ))

    DP <- afmDetachPoint(datatemp,
                         width = width,
                         mul1 = mul1,
                         mul2 = mul2,
                         Delta = Delta,
                         loessSmooth = LOESS)

    removeModal()

    showModal(modalDialog(
      title = "Done!",
      "All files processed successfully.",
      easyClose = TRUE
    ))

    afmdata(DP)

  })

  ## ---- Baseline correction ----

  observeEvent(input$BC_go,{

    zapp <- NULL
    zret <- NULL

    if(!is.na(input$zapp)){
      zapp <- input$zapp
    }

    if(!is.na(input$zret)){
      zret <- input$zret
    }

    datatemp <- afmdata()
    # showModal(modalDialog(
    #   title = "Processing...",
    #   tagList(
    #     h4("Please wait..."),
    #     div(style="text-align:center;",
    #         # Simple CSS spinner
    #         tags$div(class="lds-dual-ring")
    #     )
    #   ),
    #   footer = NULL,
    #   easyClose = FALSE
    # ))

    BC <- afmBaselineCorrection(datatemp,
                                ZPointApp = zapp,
                                ZPointRet = zret,
                                fitpause = input$bc_fitpause,
                                vsTime = input$vsTime,
                                sinusoidal = input$sinusoidal)

    # removeModal()
    showModal(modalDialog(
      title = "Done!",
      "All files processed successfully.",
      easyClose = TRUE
    ))

    afmdata(BC)


  })

  ## ---- Zero-force point and slopes ----


  observeEvent(input$Z0_go,{
    datatemp <- afmdata()

    # showModal(modalDialog(
    #   title = "Processing...",
    #   tagList(
    #     h4("Please wait..."),
    #     div(style="text-align:center;",
    #         # Simple CSS spinner
    #         tags$div(class="lds-dual-ring")
    #     )
    #   ),
    #   footer = NULL,
    #   easyClose = FALSE
    # ))

    Z0Slope <- afmZeroPointSlope(datatemp,
                                 fstar = input$fstar,
                                 segment = input$segment_z0)

    #removeModal()
    showModal(modalDialog(
      title = "Done!",
      "All files processed successfully.",
      easyClose = TRUE
    ))

    afmdata(Z0Slope)
  })

  ## ---- Indentation ----

  observeEvent(input$Indent_go,{

    datatemp <- afmdata()
    Indentation <- afmIndentation(datatemp)
    afmdata(Indentation)
    showModal(modalDialog(
      title = "Done!",
      "All files processed successfully.",
      easyClose = TRUE
    ))

  })


}



