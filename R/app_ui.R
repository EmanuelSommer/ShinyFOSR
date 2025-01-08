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
      title = "Fancy Functional Dashboard",
      sidebar = sidebar(
        width = "30%",
        h4("Select Target(s)"),
        selectizeInput(
          "model_sel",
          "",
          choices = c(
            "rotationhipflex",
            "rotationhipabad",
            "rotationhiprot",
            "rotationkneeflex",
            "rotationankleflex",
            "rotationanklepron",
            "grf_ap",
            "grf_vert",
            "momenthipflex",
            "momenthipabad",
            "momenthiprot",
            "momentkneeflex",
            "momentankleflex",
            "powerhipflex",
            "powerkneeflex",
            "powerankleflex"
          ),
          multiple = TRUE
        ),
        shiny::tags$hr(),
        h4("Input New Data"),
        fluidRow(
          column(4, numericInput("speed", "Speed", value = 1.25, min = 0, step = 0.01)),
          column(4, numericInput("age", "Age", value = 41, min = 0, step = 1)),
          column(4, numericInput("cadence", "Cadence", value = 115, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("height", "Height", value = 1.60, min = 0, step = 0.01)),
          column(6, numericInput("weight", "Weight", value = 65, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, selectInput("sex", "Sex", choices = c("Male", "Female"), selected = "Female")),
          column(6, selectInput("side", "Side", choices = c("left", "right"), selected = "left"))
        ),
        shiny::tags$hr(),
        uiOutput("multiplot_option")
      ),

      h4("Estimated Curve(s)"),
      shiny::tags$hr(),
      fluidRow(
        column(1),
        column(10, plotOutput("main_plot", height = "600px")),
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
