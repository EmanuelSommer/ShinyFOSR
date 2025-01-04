#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  load("inst/app/www/models.RData")
  primary_plot_text_col <- "#C8DEB3"

  new_data <- reactive({
    data.frame(
      speed = I(input$speed),
      age = I(input$age),
      cadence = I(input$cadence),
      height = I(input$height),
      mass = I(input$mass),
      sex = I(factor(input$sex, levels = c("m", "f"))),
      side = I(factor(input$side, levels = c("left", "right")))
    )
  })

  output$main_plot <- renderPlot({
    req(input$model_sel)

    selected_models <- lapply(input$model_sel, function(model_name) {
      models[[model_name]]
    })
    preds <- lapply(selected_models, function(m) predict(m, newdata = new_data()))
    names(preds) <- input$model_sel

    # Combine predictions into a single data frame
    plot_data <- do.call(rbind, lapply(input$model_sel, function(model_name) {
      data.frame(
        t = seq_len(ncol(preds[[model_name]])) / ncol(preds[[model_name]]) * 100,
        y = preds[[model_name]][1, ],
        model = model_name
      )
    }))

    if (length(input$model_sel) > 1) {
      ggplot(plot_data, aes(x = t, y = y, color = model)) +
        geom_line(size = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 100, by = 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          x = "Gait Cycle",
          y = "",
          color = "Target"
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E0E0E0", size = 0.1),
          text = element_text(size = 25, color = primary_plot_text_col),
          axis.text = element_text(size = 22, color = primary_plot_text_col)
        )
    } else {
      ggplot(plot_data, aes(x = t, y = y)) +
        geom_line(size = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 100, by = 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          y = paste0(input$model_sel),
          x = "Gait Cycle",
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E0E0E0", size = 0.1),
          text = element_text(size = 25, color = primary_plot_text_col),
          axis.text = element_text(size = 22, color = primary_plot_text_col)
        )
    }
  })
}
