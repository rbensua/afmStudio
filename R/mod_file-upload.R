#' UI module for file upload function
#' @noRd
file_upload_ui <- function(id, wrap = FALSE, ...) {
  ns <- NS(id)
  if (wrap) {
    div(shinyDirButton(ns('dir_modal'), label = "Select folder", title = 'Select folder containing JPK ascii files'), ...)
  } else {
    shinyDirButton(ns('dir_modal'), label = "Select folder", title = 'Select folder containing JPK ascii files')
  }
}

#' Server module for file upload function
#' @noRd
file_upload_srv <- function(id) {
  moduleServer(
    id, function(input, output, session) {
      ns <- session$ns

      file_params <- reactiveValues(
        ext = NULL,
        sheets = NULL
      )

      cleaned_file <- reactiveValues(
        doc = NULL,
        columns = NULL,
        dim = NULL
      )

      # observeEvent(input$dir_modal, {
      #   showModal(
      #     modalDialog(
      #       easyClose = TRUE, size = 'xl',
      #       title = 'Load JPK file',
      #       footer = tagList(
      #         actionButton(ns('JPKfile_load'), 'Load and close', class = 'btn-sm btn-primary', `data-bs-dismiss` = 'modal'),
      #         bs_modal_button('Cancel', size = 'sm', color = 'secondary')
      #       ),
      #       div(
      #         class = 'input-group mb-3',
      #         tags$input(
      #           class = 'form-control', type = 'file', id = ns('fileInput'),
      #           accept='text/csv,text/plain,text/comma-separated-values,.tsv,.csv,.xls,.xlsx,.dta,.rds,.sav')
      #       )
      #     )
      #   )
      # }, ignoreInit = TRUE)

      observeEvent(input$fileInput, {
        file_params$ext <- tools::file_ext(input$fileInput$name)
        file_params$sheets <- NULL
      })

      jpk_file <- reactive({
        req(input$fileInput)
        if (is.null(file_params$ext)) return()
        jpkfile <- afmReadJPK(input$fileInput)
        return(jpkfile)
      })



      observeEvent(input$JPKfile_load, {
        # When the users click on the load button, we write back the reactive
        # values so that they can be used in the app.
        # clean_doc <- clean_file()
        # cleaned_file$doc <- clean_doc
        # cleaned_file$columns <- names(clean_doc)
        # cleaned_file$dim <- dim(clean_doc)
      })





      return(reactive({
        list(
          file = cleaned_file$doc,
          ext = file_params$ext,
          cols = cleaned_file$columns,
          dim = cleaned_file$dim
        )
      }))
    }
  )
}

