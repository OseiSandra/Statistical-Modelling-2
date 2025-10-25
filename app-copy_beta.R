library(shiny)
library(ggplot2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Beta Distribution App"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Beta Distribution Parameters"),
      
      # Shape parameter alpha
      sliderInput(
        inputId = "alpha",
        label = "Shape parameter α (alpha)",
        min = 0.1,
        max = 10,
        value = 2,
        step = 0.1
      ),
      "Must be positive",
      
      hr(),
      
      # Shape parameter beta
      sliderInput(
        inputId = "beta", 
        label = "Shape parameter β (beta)", 
        min = 0.1,
        max = 10,
        value = 2,
        step = 0.1
      ), 
      "Must be positive",
      
      hr(),
      
      h4("Display Options"),
      
      # Radio buttons for PDF/CDF
      radioButtons(
        inputId = "plot_type",
        label = "Plot type:",
        choices = list("Probability Density Function (PDF)" = "pdf",
                       "Cumulative Distribution Function (CDF)" = "cdf"),
        selected = "pdf"
      ),
      
      hr(),
      
      h4("Additional Information"),
      
      # Probability calculation
      sliderInput(
        inputId = "prob_x",
        label = "Calculate P(X ≤ x) for x =",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.01
      ),
      
      # Random sample generator
      numericInput(
        inputId = "n_samples",
        label = "Generate n random samples:",
        value = 10,
        min = 1,
        max = 1000,
        step = 1
      ),
      
      actionButton(
        inputId = "generate_samples",
        label = "Generate New Samples",
        class = "btn-primary"
      )
    ),
    
    mainPanel(
      # Main plot
      plotOutput("distPlot", height = "400px"),
      
      hr(),
      
      # Distribution statistics
      fluidRow(
        column(6,
               h4("Distribution Statistics"),
               verbatimTextOutput("stats")
        ),
        column(6,
               h4("Probability Calculation"),
               verbatimTextOutput("prob_calc")
        )
      ),
      
      hr(),
      
      # Random samples
      h4("Random Samples"),
      verbatimTextOutput("random_samples")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive values for random samples
  samples <- reactiveVal(NULL)
  
  # Generate initial samples
  observe({
    samples(rbeta(input$n_samples, input$alpha, input$beta))
  })
  
  # Generate new samples when button is clicked
  observeEvent(input$generate_samples, {
    samples(rbeta(input$n_samples, input$alpha, input$beta))
  })
  
  # Main distribution plot
  output$distPlot <- renderPlot({
    x_seq <- seq(from = 0.001, to = 0.999, length = 500)
    
    if (input$plot_type == "pdf") {
      # PDF plot
      plot_data <- tibble(
        x = x_seq,
        y = dbeta(x, shape1 = input$alpha, shape2 = input$beta)
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line(color = "blue", size = 1.2) +
        geom_area(alpha = 0.3, fill = "lightblue") +
        labs(
          title = paste0("Beta(α = ", input$alpha, ", β = ", input$beta, ") - PDF"),
          x = "x",
          y = "Density"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        xlim(0, 1)
      
    } else {
      # CDF plot
      plot_data <- tibble(
        x = x_seq,
        y = pbeta(x, shape1 = input$alpha, shape2 = input$beta)
      )
      
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_line(color = "red", size = 1.2) +
        labs(
          title = paste0("Beta(α = ", input$alpha, ", β = ", input$beta, ") - CDF"),
          x = "x",
          y = "P(X ≤ x)"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 14),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10)
        ) +
        xlim(0, 1) +
        ylim(0, 1)
    }
    
    # Add vertical line for probability calculation if showing PDF
    if (input$plot_type == "pdf" && input$prob_x >= 0 && input$prob_x <= 1) {
      p <- p + geom_vline(xintercept = input$prob_x, 
                          color = "red", 
                          linetype = "dashed", 
                          alpha = 0.7)
    }
    
    p
  })
  
  # Distribution statistics
  output$stats <- renderText({
    alpha <- input$alpha
    beta <- input$beta
    
    # Beta distribution statistics
    mean_val <- alpha / (alpha + beta)
    var_val <- (alpha * beta) / ((alpha + beta)^2 * (alpha + beta + 1))
    sd_val <- sqrt(var_val)
    mode_val <- if (alpha > 1 && beta > 1) {
      (alpha - 1) / (alpha + beta - 2)
    } else {
      "Not defined (when α ≤ 1 or β ≤ 1)"
    }
    
    paste0(
      "Mean: ", round(mean_val, 4), "\n",
      "Variance: ", round(var_val, 4), "\n",
      "Standard Deviation: ", round(sd_val, 4), "\n",
      "Mode: ", if(is.numeric(mode_val)) round(mode_val, 4) else mode_val
    )
  })
  
  # Probability calculation
  output$prob_calc <- renderText({
    if (input$prob_x >= 0 && input$prob_x <= 1) {
      prob_val <- pbeta(input$prob_x, shape1 = input$alpha, shape2 = input$beta)
      density_val <- dbeta(input$prob_x, shape1 = input$alpha, shape2 = input$beta)
      
      paste0(
        "P(X ≤ ", input$prob_x, ") = ", round(prob_val, 4), "\n",
        "P(X > ", input$prob_x, ") = ", round(1 - prob_val, 4), "\n",
        "f(", input$prob_x, ") = ", round(density_val, 4)
      )
    } else {
      "Please enter a value between 0 and 1"
    }
  })
  
  # Random samples display
  output$random_samples <- renderText({
    current_samples <- samples()
    if (!is.null(current_samples)) {
      paste0(
        "Sample of ", length(current_samples), " values:\n",
        paste(round(current_samples, 4), collapse = ", "), "\n\n",
        "Sample Statistics:\n",
        "Mean: ", round(mean(current_samples), 4), "\n",
        "Standard Deviation: ", round(sd(current_samples), 4)
      )
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)