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
            "hip_flex_deg",
            "hip_abd_deg",
            "hip_rot_deg",
            "knee_flex_deg",
            "ankle_pf_deg",
            "ankle_inv_deg",
            "grf_ap_nm_per_kg",
            "grf_vert_nm_per_kg",
            "hip_flex_moment_nm_per_kg",
            "hip_abd_moment_nm_per_kg",
            "hip_rot_moment_nm_per_kg",
            "knee_flex_moment_nm_per_kg",
            "ankle_pf_moment_nm_per_kg",
            "hip_flex_power_w_per_kg",
            "knee_flex_power_w_per_kg",
            "ankle_pf_power_w_per_kg"
          ),
          multiple = TRUE
        ),
        shiny::tags$hr(),
        h4("Input New Data"),
        fluidRow(
          column(4, numericInput("speed", "Speed", value = 1, min = 0, step = 0.1)),
          column(4, numericInput("age", "Age", value = 2, min = 0, step = 1)),
          column(4, numericInput("cadence", "Cadence", value = 1, min = 0, step = 0.1))
        ),
        fluidRow(
          column(6, numericInput("height", "Height", value = 3, min = 0, step = 0.1)),
          column(6, numericInput("mass", "Mass", value = 0.3, min = 0, step = 0.01))
        ),
        fluidRow(
          column(6, selectInput("sex", "Sex", choices = c("m", "f"), selected = "m")),
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
