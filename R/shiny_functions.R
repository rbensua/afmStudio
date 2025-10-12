#' @import shiny
#' @import bslib
#' @import ggplot2
#' @import markdown
#' @import reactable
#' @import readxl
#' @import writexl
#' @import afmToolkit
#' @import shinyFiles
#' @import plotly
NULL

#' Function to add JS and CSS dependencies to the app
#' @noRd
afmstudio_scripts <- function() {
  htmltools::htmlDependency(
    name = 'afmstudio-assets',
    version = utils::packageVersion('afmStudio'),
    package = 'afmStudio',
    src = 'www',
    script = 'afmstudio.js',
    style = 'style.css'
  )
}

#' Add a simple CSS spinner
addSpinnerCSS <- "
.lds-dual-ring {
  display: inline-block;
  width: 64px;
  height: 64px;
}
.lds-dual-ring:after {
  content: ' ';
  display: block;
  width: 46px;
  height: 46px;
  margin: 1px;
  border-radius: 50%;
  border: 5px solid #007BFF;
  border-color: #007BFF transparent #007BFF transparent;
  animation: lds-dual-ring 1.2s linear infinite;
}
@keyframes lds-dual-ring {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}
"


#' Run afmStudio
#'
#' Run the afmStudio app on your local machine.
#'
#' @param x A data frame that should be loaded with the app. See details.
#' @param port Integer. The TCP port that the application should listen on.
#' @param ... Other arguments to send to [shiny::runApp()].
#'
#' @details
#' Note that afmStudio must be loaded into the namespace with `library(afmStudio)`
#' before you run the afmStudio app.
#'
#' You can load a data object in your current environment to the app. You can pass
#' a data frame or a character string with the object name of the data frame you
#' want to be loaded when the app launches. Note that you should only use data
#' frame objects. If you have a tibble (from the tidyverse) or a data table, you
#' can convert to an ordinary data.frame using [as.data.frame()]
#'
#' @return None
#' @examples
#' # Only run this example in interactive R sessions
#' if (interactive()) {
#'   df = deaR::Electric_plants
#'   # Load app with data.frame and set port to 8080
#'   run_afmstudio(x = df, port = 8080)
#' }
#' @export
#'

run_afmstudio <- function(x = NULL, port = NULL, ...) {






  if (!is.null(x)) {
    set_local_data(x)
  }

  port <- check_for_unsafe_port(port)

  afmstudio_env <- new.env()
  environment(ui) <- afmstudio_env
  environment(server) <- afmstudio_env

  # Create app object and run app
  app <- shiny::shinyApp(ui = ui,
    server,
    enableBookmarking = 'server')
  shiny::runApp(app, port = port, ...)
}


#' Unset environment variables
#'
#' Unsets the environment variables set by afmStudio
#'
#' @return A logical vector, with elements being `TRUE` if unsetting the variable succeeded
#' @examples
#' unset_env_vars()
#' @export
unset_env_vars <- \() Sys.unsetenv('AFMSTUDIO_DATA')


#' @param level A character string with the level of alert
#' @param message A character string with the message to show the user
#' @param object A reactive object to update
#' @param append Boolean. If the message should be appended to the reactive
#' @noRd
set_message <- function(level, message, object = NULL, append = FALSE) {
  classes <- switch(
    level,
    warning = "alert alert-warning",
    error = "alert alert-danger",
    "alert alert-info"
  )
  icon <- if (level == "info") "info-circle" else "exclamation-circle"
  div <- div(class = classes, role = "alert", list(bsicons::bs_icon(icon, class = "me-2"), message))
  # If we do not have a reactive, return the div
  if (is.null(object)) {
    return(div)
  }
  # If we have a reactive object, set or append our div to the reactive
  if (append) {
    current_tags <- object()
    object(tagList(current_tags, div))
  } else {
    object(div)
  }
  invisible()
}

#' Check if a function issues a warning message and catch it so we can send it to
#' the user
#' @noRd
catch_warnings <- function(expr, handler_expr, reactive, append = FALSE) {
  if (shiny::isRunning() || as.logical(Sys.getenv("AFMSTUDIO_SUPPRESS_WARNINGS", FALSE))) {
    withCallingHandlers(expr, warning = \(w) {
      msg <- if (inherits(w, "rlang_warning")) cli::ansi_strip(w$message) else conditionMessage(w)
      handler_expr("warning", msg, reactive)
      tryInvokeRestart("muffleWarning")
    })
  } else {
    expr
  }
}

#' Check if a function raises an error and catch it so that the app does not stop,
#' but the user is informed
#' @noRd
catch_exceptions <- function(expr, handler_expr, reactive, append = FALSE) {
  # We only want to catch errors if the app is running
  if (shiny::isRunning()) {
    tryCatch(catch_warnings(expr, handler_expr, reactive), error = \(e) {
      msg <- if (inherits(e, "rlang_error")) cli::ansi_strip(e$message) else conditionMessage(e)
      handler_expr("error", msg, reactive)
      NULL
    })
  } else {
    expr
  }
}




