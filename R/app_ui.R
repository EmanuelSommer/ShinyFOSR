#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    dashboardPage(
      dashboardHeader(title = "Title TBD"),
      dashboardSidebar(
        sidebarMenu(
          box(title = "Target ", width = 12,
              selectizeInput(
                "model_sel",
                "Select Models",
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
                )
          ),
          box(title = "Input New Data", width = 12, status = "primary",
              numericInput("speed", "Speed", value = 1, min = 0, step = 0.1),
              numericInput("age", "Age", value = 2, min = 0, step = 1),
              numericInput("cadence", "Cadence", value = 1, min = 0, step = 0.1),
              numericInput("height", "Height", value = 3, min = 0, step = 0.1),
              numericInput("mass", "Mass", value = 0.3, min = 0, step = 0.01),
              selectInput("sex", "Sex", choices = c("m", "f"), selected = "m"),
              selectInput("side", "Side", choices = c("left", "right"), selected = "left")
          )
        )
      ),
      dashboardBody(
        fluidRow(
          uiOutput("plot_grid")
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
