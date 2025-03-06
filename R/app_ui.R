#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    page_sidebar(
      theme = bs_theme(bootswatch = "lux", primary = "#DB4433", bg = "#332B3F", fg = "#C8DEB3"),
      title = "Predicting normative walking biomechanics across the lifespan",
      sidebar = sidebar(
        width = "30%",
        h4("Select Target(s)"),
        uiOutput("model_sel_ui"),
        shiny::tags$hr(),
        h4("Input New Data"),
        fluidRow(
          column(4, numericInput("speed", "Speed (m/s)", value = 1.25, min = 0, step = 0.01)),
          column(4, numericInput("age", "Age (yrs)", value = 41, min = 0, step = 1)),
          column(4, numericInput("cadence", "Cadence (spm)", value = 115, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("height", "Height (m)", value = 1.60, min = 0, step = 0.01)),
          column(6, numericInput("weight", "Weight (kg)", value = 65, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, selectInput("sex", "Sex", choices = c("Male", "Female"), selected = "Female")),
          column(6, selectInput("side", "Side", choices = c("left", "right"), selected = "left"))
        ),
        shiny::tags$hr(),
        h4("Options"),
        uiOutput("gait_cycle_selection"),
        uiOutput("multiplot_option"),
        fluidRow(
          column(6, uiOutput("cond_add_prediction")),
          column(6, uiOutput("cond_clear_table"))
        ),
        fluidRow(
          column(6, uiOutput("cond_download_csv")),
          column(6, uiOutput("cond_download_plot"))
        ),
        shiny::tags$hr(),
        h4("Citation"),
        p("Please cite the following paper when using this application:"),
        p(
          a(
            "LOREM IPSUM",
            href = "tbd",
            target = "_blank"
          )
        ),
        shiny::radioButtons(
          "ci_display",
          "Display CIs?",
          choices = c("Yes (Only use if self-hosted!)", "No"),
          selected = "No"
        ),
        shiny::radioButtons(
          "ci_mode",
          "CI Mode",
          choices = c("95%", "1SD", "2SD"),
          selected = "95%"
        )
      ),

      # h4("Estimated Curve(s)"),
      # shiny::tags$hr(),
      fluidRow(
        column(1),
        column(
          10,
          tabBox(
            id = "tabset", width = 12,
            tabPanel(
              "Estimated Curve(s)", icon = icon("chart-line"),
              br(),
              uiOutput("no_target_infobox"),
              plotOutput("main_plot", height = "600px")
            ),
            tabPanel(
              "Table", icon = icon("table"),
              br(),
              uiOutput("no_data_infobox"),
              tableOutput("predictions_table")
            )
          )
        ),
        column(1)
      ),

      # Paper and GitHub links
      div(
          style = "position: absolute; top: 1px; right: 10px; display: flex; gap: 10px;",
          tags$a(
            href = "https://example.com/paper", target = "_blank",
            bsicons::bs_icon("file-earmark-pdf", style = "color: #DB4433;")
          ),
          tags$a(
            href = "https://github.com/EmanuelSommer/ShinyFOSR", target = "_blank",
            bsicons::bs_icon("github", style = "color: #24292f;")
          )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinyFOSR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
