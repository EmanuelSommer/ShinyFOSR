#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  levels_cat <- readRDS(system.file("app/www/levels_cat.RDS", package = "ShinyFOSR"))
  models <- readRDS(system.file("app/www/train_mod_sparse.RDS", package = "ShinyFOSR"))
  ylabels <- readRDS(system.file("app/www/ylabels.RDS", package = "ShinyFOSR"))
  primary_plot_text_col <- "#C8DEB3"
  names(models) <- ylabels
  output$model_sel_ui <- renderUI({
    selectizeInput(
      "model_sel",
      "",
      choices = ylabels,
      multiple = TRUE
    )
  })

  new_data <- reactive({
    data.frame(
      speed = I(input$speed),
      age = I(input$age),
      cadence = I(input$cadence),
      ht = I(input$height),
      wt = I(input$weight),
      sex = I(factor(ifelse(input$sex == "Female", "F", "M"), levels = levels_cat$sex)),
      side = I(factor(ifelse(input$side == "left", "L", "R") , levels = levels_cat$side)),
      id = I(factor("HAC021", levels = c("HAC021", levels_cat$id)))
    )
  })

  multiplot_state <- reactiveVal("Single")
  observe({
    req(input$multiplot)
    multiplot_state(input$multiplot)
  })

  output$multiplot_option <- renderUI({
    req(input$model_sel)
    if (length(input$model_sel) > 1) {
      fluidRow(
        column(12, selectInput(
          "multiplot",
          "Multiplot View",
          c("Single", "Facetted"),
          selected = multiplot_state()
        ))
      )
    }
  })

  output$main_plot <- renderPlot({
    req(input$model_sel)

    selected_models <- lapply(input$model_sel, function(model_name) {
      models[[model_name]]
    })
    preds <- lapply(selected_models, function(m) {
      refund:::predict.pffr(
        m,
        newdata = new_data(),
        type = "response",
        se.fit = FALSE,
        exclude = c("s(id)")
      )
    })
    names(preds) <- input$model_sel

    # Combine predictions into a single data frame
    plot_data <- do.call(rbind, lapply(input$model_sel, function(model_name) {
      data.frame(
        t = seq_len(ncol(preds[[model_name]])) / ncol(preds[[model_name]]) * 100,
        y = preds[[model_name]][1, ],
        model = beautify_plot_label(model_name)
      )
    }))

    if (length(input$model_sel) > 1 && multiplot_state() == "Single") {
      ggplot(plot_data, aes(x = t, y = y, color = model)) +
        geom_line(size = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 100, by = 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          x = "Gait Cycle (0-100%)",
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
    } else if (length(input$model_sel) > 1 && multiplot_state() == "Facetted") {
      ggplot(plot_data, aes(x = t, y = y)) +
        geom_line(size = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 100, by = 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          y = "",
          x = "Gait Cycle (0-100%)",
        ) +
        facet_wrap(
          ~model, scales = "free_y",
          ncol = ifelse(
            length(input$model_sel) <= 12,
            ifelse(length(input$model_sel) > 4, 3, 2),
            4
          )
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E0E0E0", size = 0.1),
          text = element_text(size = 25, color = primary_plot_text_col),
          axis.text = element_text(size = max(22 - 2.5 * length(input$model_sel), 10), color = primary_plot_text_col),
          strip.text = element_text(size = 22, color = primary_plot_text_col)
        )
    } else {
      ggplot(plot_data, aes(x = t, y = y)) +
        geom_line(size = 1.2) +
        scale_x_continuous(
          breaks = seq(0, 100, by = 10),
          labels = function(x) paste0(x, "%")
        ) +
        labs(
          y = beautify_plot_label(input$model_sel),
          x = "Gait Cycle (0-100%)",
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
