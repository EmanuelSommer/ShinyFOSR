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

  output$cond_add_prediction <- renderUI({
    req(input$model_sel)
    actionButton("add_prediction", "Add predictions to table.", icon = icon("plus"))
  })
  output$cond_clear_table <- renderUI({
    req(input$model_sel)
    actionButton("clear_table", "Clear Table", icon = icon("trash"))
  })
  output$cond_download_csv <- renderUI({
    req(input$model_sel)
    downloadButton("download_csv", "Download Table (CSV)")
  })
  output$cond_download_plot <- renderUI({
    req(input$model_sel)
    downloadButton("download_plot", "Download Plot (PNG)")
  })

  plot_data <- reactive({
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

    do.call(rbind, lapply(input$model_sel, function(model_name) {
      data.frame(
        t = seq_len(ncol(preds[[model_name]])) / ncol(preds[[model_name]]) * 100,
        y = preds[[model_name]][1, ],
        model = beautify_plot_label(model_name)
      )
    }))
  })

  output$gait_cycle_selection <- renderUI({
    req(input$model_sel)
    choices <- sort(unique(plot_data()$t))
    sliderInput(
      "gait_cycle_selection",
      "Gait Cycle (%) of Interest:",
      min = min(choices),
      max = max(choices),
      value = 50,
      step = 1
    )
  })
  gait_cycle_selection <- reactiveVal(50)
  observe({
    req(input$gait_cycle_selection)
    gait_cycle_selection(input$gait_cycle_selection)
  })

  reactivity_data <- reactiveVal(data.frame())
  observeEvent(input$add_prediction, {
    req(input$model_sel)
    for (sel_model in input$model_sel) {
      pred <- plot_data()[plot_data()$t == gait_cycle_selection() & plot_data()$model == beautify_plot_label(sel_model), ]
      colnames(pred) <- c("gait_cycle", "prediction", "target")
      new_pred_data <- cbind(
        new_data()[, 1:(ncol(new_data()) - 1)],
        pred
      )
      if (nrow(reactivity_data()) == 0) {
        reactivity_data(new_pred_data)
      } else {
        reactivity_data(rbind(
          reactivity_data(),
          new_pred_data
        ))
      }
    }
  })
  observeEvent(input$clear_table, {
    reactivity_data(data.frame())
  })

  main_plot_object <- reactive({
    req(input$model_sel)
    req(input$gait_cycle_selection)

    if (length(input$model_sel) > 1 && multiplot_state() == "Single") {
      ggplot(plot_data(), aes(x = t, y = y, color = model)) +
        geom_line(size = 1.2) +
        geom_vline(xintercept = gait_cycle_selection(), linetype = "dashed", color = "#DB4433") +
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
      ggplot(plot_data(), aes(x = t, y = y)) +
        geom_line(size = 1.2) +
        geom_vline(xintercept = gait_cycle_selection(), linetype = "dashed", color = "#DB4433") +
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
          ),
          switch = "y"
        ) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = "right",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "#E0E0E0", size = 0.1),
          text = element_text(size = 25, color = primary_plot_text_col),
          axis.text = element_text(size = max(22 - 2.5 * length(input$model_sel), 10), color = primary_plot_text_col),
          strip.text = element_text(size = 22, color = primary_plot_text_col),
          strip.placement = "outside"
        )
    } else {
      ggplot(plot_data(), aes(x = t, y = y)) +
        geom_line(size = 1.2) +
        geom_vline(xintercept = gait_cycle_selection(), linetype = "dashed", color = "#DB4433") +
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

  output$main_plot <- renderPlot({
    main_plot_object()
  })

  output$no_data_infobox <- renderUI({
    req(nrow(reactivity_data()) == 0)
    value_box(
      title = "It's empty around here",
      value = "Click 'Add Predictions to table'",
      showcase = icon("arrow-left"),
      "to add predictions for all selected targets of the current variables at the selected gait cylceof interest to the table."
    )
  })

  # add no_target_infobox
  output$no_target_infobox <- renderUI({
    req(length(input$model_sel) == 0)
    value_box(
      title = "Ready When You Are",
      value = "Select target(s) to get started!",
      showcase = icon("rocket")
    )
  })

  output$predictions_table <- renderTable({
    req(input$model_sel)
    reactivity_data()
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste("predictions-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reactivity_data(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      paste("FOSR-VIZ-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(
        file,
        plot = main_plot_object(),
        device = "png", width = 12, height = 10)
    },
    contentType = "image/png"
  )
}
