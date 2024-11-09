library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Statistical Distributions Teaching Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dist_type", "Select Distribution Type(s):",
                  choices = list(
                    "Discrete" = c("Binomial", "Poisson", "Geometric"),
                    "Continuous" = c("Normal", "Exponential", "Uniform")
                  ),
                  selected = "Normal", multiple = TRUE),
      uiOutput("param_input")
    ),
    mainPanel(
      plotOutput("dist_plot"),
      verbatimTextOutput("dist_info")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive UI for parameters based on distribution type
  output$param_input <- renderUI({
    
    #define the input variable dist_type by the selections from the drop down
    lapply(input$dist_type, function(dist) {
      switch(dist,
             "Binomial" = list(
               numericInput(paste0("size_", dist), paste("Size (n) for", dist, ":"), value = 10, min = 1),
               numericInput(paste0("prob_", dist), paste("Probability (p) for", dist, ":"), value = 0.5, min = 0, max = 1)
             ),
             "Poisson" = list(
               numericInput(paste0("lambda_", dist), paste("Rate (lambda) for", dist, ":"), value = 3, min = 0)
             ),
             "Geometric" = list(
               numericInput(paste0("prob_", dist), paste("Probability (p) for", dist, ":"), value = 0.5, min = 0, max = 1)
             ),
             "Normal" = list(
               numericInput(paste0("mean_", dist), paste("Mean (mu) for", dist, ":"), value = 0),
               numericInput(paste0("sd_", dist), paste("Standard Deviation (sigma) for", dist, ":"), value = 1, min = 0)
             ),
             "Exponential" = list(
               numericInput(paste0("rate_", dist), paste("Rate (lambda) for", dist, ":"), value = 1, min = 0)
             ),
             "Uniform" = list(
               numericInput(paste0("min_", dist), paste("Minimum (a) for", dist, ":"), value = 0),
               numericInput(paste0("max_", dist), paste("Maximum (b) for", dist, ":"), value = 1)
             )
      )
    })
  })
  
  # Plotting the distributions
  output$dist_plot <- renderPlot({
    x_vals <- seq(-10, 10, length.out = 1000)
    plot_data <- data.frame()
    
    #plot a separate distribution for each value in the dropdown list
    for (dist in input$dist_type) {
      dist_data <- switch(dist,
                          "Binomial" = data.frame(x = 0:input[[paste0("size_", dist)]],
                                                  y = dbinom(0:input[[paste0("size_", dist)]],
                                                             input[[paste0("size_", dist)]],
                                                             input[[paste0("prob_", dist)]]),
                                                  dist = dist),
                          "Poisson" = data.frame(x = 0:20,
                                                 y = dpois(0:20, input[[paste0("lambda_", dist)]]),
                                                 dist = dist),
                          "Geometric" = data.frame(x = 0:20,
                                                   y = dgeom(0:20, input[[paste0("prob_", dist)]]),
                                                   dist = dist),
                          "Normal" = data.frame(x = x_vals,
                                                y = dnorm(x_vals,
                                                          input[[paste0("mean_", dist)]],
                                                          input[[paste0("sd_", dist)]]),
                                                dist = dist),
                          "Exponential" = data.frame(x = x_vals[x_vals >= 0],
                                                     y = dexp(x_vals[x_vals >= 0],
                                                              input[[paste0("rate_", dist)]]),
                                                     dist = dist),
                          "Uniform" = data.frame(x = x_vals,
                                                 y = dunif(x_vals,
                                                           input[[paste0("min_", dist)]],
                                                           input[[paste0("max_", dist)]]),
                                                 dist = dist)
      )
      
      #accumulate data for distribution so they can be plotted together
      if (!is.null(dist_data)) {
        plot_data <- rbind(plot_data, dist_data)
      }
    }
    
    ggplot(plot_data, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.5, alpha = 0.7) +
      labs(title = "Comparison of Selected Distributions",
           x = "X Values", 
           y = "Density / \n Probability",
           colour = "Distribution(s)") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20), 
        plot.subtitle = element_text(hjust = 0.5, size = 14), 
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, angle = 0, vjust = 1.1, hjust = 1),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 13), 
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(color = "black", size = 0.5)
      )
  })
  
  # Displaying distribution information
  output$dist_info <- renderPrint({
    dist_info_list <- list(
      "Binomial" = "Distribution: Binomial
      PMF: dbinom(x, n, p)
      Support: x = 0, 1, ..., n 
      Parameters: n (size), p (probability) 
      Mean (First Moment): np
      Variance (Second Moment): np(1-p)
      MGF: (1 - p + p*e^t)^n 
      \n",
      
      "Poisson" = "Distribution: Poisson
      PMF: dpois(x, lambda)
      Support: x = 0, 1, 2, ...
      Parameter: lambda (rate) 
      Mean (First Moment): lambda
      Variance (Second Moment): lambda 
      MGF: exp(lambda * (e^t - 1)) 
      \n",
      
      "Geometric" = "Distribution: Geometric
      PMF: dgeom(x, p)
      Support: x = 0, 1, 2, ...
      Parameter: p (probability) 
      Mean (First Moment): 1/p
      Variance (Second Moment): (1-p)/p^2
      MGF: p*e^t / (1 - (1-p)*e^t) for t < -log(1-p) 
      \n",
      
      "Normal" = "Distribution: Normal
      PDF: dnorm(x, mu, sigma) 
      Support: x in (-∞, ∞) 
      Parameters: mu (mean), sigma (standard deviation)
      Mean (First Moment): mu 
      Variance (Second Moment): sigma^2
      MGF: exp(mu*t + 0.5*sigma^2*t^2) 
      \n",
      
      "Exponential" = "Distribution: Exponential 
      PDF: dexp(x, lambda) 
      Support: x >= 0 
      Parameter: lambda (rate) 
      Mean (First Moment): 1/lambda 
      Variance (Second Moment): 1/lambda^2
      MGF: lambda / (lambda - t) for t < lambda 
      \n",
      
      "Uniform" = "Distribution: Uniform 
      PDF: dunif(x, a, b) 
      Support: x in [a, b]
      Parameters: a (min), b (max)
      Mean (First Moment): (a + b)/2 
      Variance (Second Moment): (b - a)^2 / 12 
      MGF: (e^(bt) - e^(at)) / (t(b - a)) for t != 0
      \n"
    )
    #index into the dist_info_list depending on which distributions are picked
    cat(unlist(dist_info_list[input$dist_type], use.names = FALSE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
