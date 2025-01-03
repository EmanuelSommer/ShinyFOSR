#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2
#' @noRd
app_server <- function(input, output, session) {
  load("inst/app/www/models.RData")

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

  output$plot_grid <- renderUI({
    req(input$model_sel)
    plots <- lapply(input$model_sel, function(model_name) {
      plotOutput(paste0("plot_", model_name), height = "300px")
    })
    do.call(fluidRow, plots)
  })

  observe({
    req(input$model_sel)
    # get the selected models for now just dummy models
    selected_models <- models[input$model_sel]
    preds <- lapply(selected_models, function(m) predict(m, newdata = new_data()))

    for (i in seq_along(input$model_sel)) {
      local({
        model_name <- input$model_sel[i]
        pred <- preds[[i]]
        output[[paste0("plot_", model_name)]] <- renderPlot({
          ggplot(data.frame(t = seq_len(ncol(pred)), y = pred[1, ]), aes(x = t, y = y)) +
            geom_line() +
            labs(title = paste0("Predictions for ", model_name),
                 x = "Gait cycle (0-100%)",
                 y = "Estimated curve")
        })
      })
    }
  })
}
