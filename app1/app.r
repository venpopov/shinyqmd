library(shiny)
library(bslib)
library(bmm)
colors <- ggsci::pal_d3()(10)

# UI for application
slider_c <- numericInput(
  inputId = "c", label = "Memory strength (c):",
  min = 0, max = 50, value = 3, step = 0.1
)

slider_kappa <- numericInput(
  inputId = "kappa", label = "Memory precision (kappa):",
  min = 0, max = 50, value = 3.5, step = 0.1,
)

button_save <- actionButton(
  inputId = "save", label = "Add permanently to plot",
  style = "color: #fff; background-color: #007bff; border-color: #007bff"
)

ui <- page_sidebar(
  title = "Signal Discrimination Model",
  sidebar = sidebar(slider_c, slider_kappa, button_save),
  fluidRow(
    plotOutput("sdm_plot", width = "600px", height = "600px")
  )
)

# Server for application
server <- function(input, output) {
  # create a reactive store for the likelihoods that are saved permanently
  store <- reactiveValues(density = list(), c = list(), kappa = list())

  # generate the current sdm likelihood
  x <- seq(-pi, pi, length.out = 360)
  density <- reactive({
    bmm::dsdm(x, c = input$c, kappa = input$kappa)
  })

  # plot the likelihood
  output$sdm_plot <- renderPlot({
    d <- density()

    max_d <- max(c(unlist(store$density), d))

    # base plot
    plot(NULL, xlab = "Error", ylab = "Density", xlim = range(x), ylim = c(0, 1.1 * max_d))

    # add lines
    lines(x, d, lwd = 2, col = "black")
    for (i in seq_along(store$density)) {
      lines(x, store$density[[i]], lwd = 3, col = colors[[i]])
    }
    

    # add legend
    if (length(store$density) > 0) {
      legend("topright", legend = paste0("c = ", store$c, ", kappa = ", store$kappa), fill = colors, bty = "n")
    }
  }, res = 96)

  # if the save button is clicked, save the current likelihood to the store
  observeEvent(input$save, {
    store$density <- c(store$density, list(density()))
    store$c <- c(store$c, input$c)
    store$kappa <- c(store$kappa, input$kappa)
  })
}

shinyApp(ui = ui, server = server)
