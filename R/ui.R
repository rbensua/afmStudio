#' Define the user interface for pioneeR
#' @noRd
ui <- function(request) {

  ver <- utils::packageVersion('afmStudio')
  bs_ver <- 5

  sidebar_width <- 200

  vals <- list(
    'rts' = c(
      'Variable' = 'vrs', 'Constant' = 'crs', 'Non-increasing RTS' = 'drs',
      'Non-decreasing RTS' = 'irs'),
    'orient' = c('Input oriented' = 'in', 'Output oriented' = 'out')
  )

  if (utils::packageVersion('bslib') > '0.5.1') {
    theme_args = list(version = bs_ver, preset = 'bootstrap')
  } else {
    theme_args = list(version = bs_ver)
  }

  page_navbar(

    title = 'afmStudio',
    id = 'afmstudio',
    theme = do.call(bslib::bs_theme, theme_args),
    fluid = TRUE,
    # Add custom CSS
    header = afmstudio_scripts(),

    tags$style(HTML("
    #dir_select_btn {
      text-align: left !important;
      padding-left: 10px; /* optional: to make the text visually aligned */
    }
  ")),
    tags$style(HTML("
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
")),

    tabPanel(
      'Data', value = 'afmstudio_upload',
      layout_sidebar(
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          shinyDirButton('dir_select_btn',
                         label = "Select folder",
                         title = 'Select folder containing JPK ascii files', class = 'mb-2'),
          #file_upload_ui('file_upload', wrap = TRUE, class = 'mb-2'),
          p(class = 'small', helpText(HTML(
            'Define the folder with the raw JPK ascii data. The folder should only contain JPK files.'
          ))),
          hr(),
          bookmarkButton('Bookmark state'),
          p(class = 'small', helpText(paste(
            'You can save the state of the app, including the data by clicking the bookmark',
            'button. Use the URL to restore the app. Note that the state is saved on the server',
            'and that states might be deleted by the server administrator.'
          )))
        ),
        layout_columns(
          reactableOutput('curves_list_data'),
          uiOutput('data_preview_data')
        )
      )
    ),

    tabPanel(
      'Preprocessing curves', #value = 'afmstudioanalysis',
      layout_sidebar(
        ## --- Preprocessing sidebar ----
        sidebar = sidebar(
          width = sidebar_width, gap = 0,
          accordion(multiple = FALSE,
                    ### Contact point --------------------
                    accordion_panel(
                      title = "Contact Point Estimation",
                      icon = bsicons::bs_icon("menu-app"),
                      sliderInput("width_cp", "Window width", min = 1, max = 100, value = 20),
                      sliderInput("mul1_cp", "Multiplier 1", min = 0, max = 20, value = 1),
                      sliderInput("mul2_cp", "Multiplier 2", min = 0, max = 20, value = 10),
                      checkboxGroupInput("CP_options", "Other options",
                                         c(
                                           "Delta" = "delta_cp",
                                           "LOESS Smoothing" = "loessSmooth_cp"
                                         )),
                      actionButton("CP_go",
                                   label = "Compute CP",
                                   icon = icon("right-to-bracket"),
                                   class = 'btn-sm btn-primary')
                    ),
                    ### Detach point -----------
                    accordion_panel(
                      title = "Detach Point Estimation",
                      icon = bsicons::bs_icon("menu-app"),
                      sliderInput("width_dp", "Window width", min = 1, max = 100, value = 20),
                      sliderInput("mul1_dp", "Multiplier 1", min = 0, max = 20, value = 1),
                      sliderInput("mul2_dp", "Multiplier 2", min = 0, max = 20, value = 10),
                      checkboxGroupInput("DP_options", "Other options",
                                         c(
                                           "Delta" = "delta_cp",
                                           "LOESS Smoothing" = "loessSmooth_cp"
                                         )),
                      actionButton("DP_go", label = "Compute DP",
                                   icon = icon("right-to-bracket"),
                                   class = 'btn-sm btn-primary')
                    ),
                    ### Baseline Correction --------------
                    accordion_panel(
                      title = "Baseline Correction",
                      icon = bsicons::bs_icon("menu-app"),
                      numericInput("zapp", label = "Z pt. Approach", value = NULL),
                      numericInput("zret", label = "Z pr. Retract", value = NULL),
                      selectInput(inputId = "bc_fitpause", label = "Fit pause",
                                  choices = c("approach", "retract", "none"), selected = "approach"),
                      checkboxInput(inputId = "vsTime",
                                    label = "vs Time",
                                    value = FALSE),
                      checkboxInput(inputId = "sinusoidal",
                                    label = "Sinusoidal baseline",
                                    value = FALSE),
                      actionButton("BC_go",
                                   label = "Baseline Correction",
                                   class = 'btn-sm btn-primary',
                                   icon = icon("right-to-bracket"))
                    ),
                    ### Zero-force point and slopes --------------
                    accordion_panel(
                      title = "Zero-force Point",
                      icon = bsicons::bs_icon("menu-app"),
                      numericInput(inputId = "fstar", label = "F* parameter", value = 0),
                      selectInput(inputId = "segment_z0", label = "Segment",
                                  choices = c("approach", "retract"),
                                  selected = "approach"),
                      actionButton("Z0_go", label = "Zero-force point & Slope",
                                   icon = icon("right-to-bracket"),
                                   class = 'btn-sm btn-primary')
                    ),
                    ### Indentation ---------------
                    accordion_panel(
                      title = "Indentation",
                      icon = bsicons::bs_icon("menu-app"),
                      div(
                        actionButton("Indent_go", label = "Indentation",
                                     icon = icon("right-to-bracket"),
                                     class = 'btn-sm btn-primary')
                      )
                    )
          )
        ),
        ## ---- Preprocessing tab main body content ----
        layout_columns(
          reactableOutput('curves_list_prep'),
          uiOutput('data_preview_prep')
        )
      )
    ),



  )

}
