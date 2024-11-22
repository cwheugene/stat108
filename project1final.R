# Load necessary packages
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
      htmlOutput("dist_info"),   # Display distribution info with HTML formatting
      htmlOutput("dist_context") # Context descriptions
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive UI for parameters based on distribution type
  output$param_input <- renderUI({
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
               numericInput(paste0("mean_", dist), paste("Mean (μ) for", dist, ":"), value = 0),
               numericInput(paste0("sd_", dist), paste("Standard Deviation (σ) for", dist, ":"), value = 1, min = 0)
             ),
             "Exponential" = list(
               numericInput(paste0("rate_", dist), paste("Rate (λ) for", dist, ":"), value = 1, min = 0)
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
      
      if (!is.null(dist_data)) {
        plot_data <- rbind(plot_data, dist_data)
      }
    }
    
    # Defining the ggplot settings
    ggplot(plot_data, aes(x = x, y = y, color = dist)) +
      geom_line(linewidth = 1.5, alpha = 0.7) +
      labs(title = "Comparison of Selected Distributions",
           x = "X Values", 
           y = "Density / Probability",
           colour = "Distribution(s)") +
      scale_color_brewer(palette = "Set1") +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 20), 
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, angle = 0, vjust = 1.1, hjust = 1),
        axis.text = element_text(size = 8),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 13), 
        legend.text = element_text(size = 12),
        legend.box.background = element_rect(color = "black", size = 0.5)
      )
  })
  
  # Displaying distribution information using HTML
  output$dist_info <- renderUI({
    dist_info_list <- list(
      "Binomial" = paste0(
        "<b>Distribution: Binomial</b><br>",
        "<b>PMF:</b> P(X = x) = C<sub>n,x</sub> p<sup>x</sup> (1-p)<sup>n-x</sup><br>",
        "<b>Support:</b> x = 0, 1, ..., n<br>",
        "<b>Parameters:</b> n (size), p (probability)<br>",
        "<b>Mean:</b> np<br>",
        "<b>Variance:</b> np(1-p)<br>",
        "<b>MGF:</b> (1 - p + p e<sup>t</sup>)<sup>n</sup>"
      ),
      "Poisson" = paste0(
        "<b>Distribution: Poisson</b><br>",
        "<b>PMF:</b> P(X = x) = (λ<sup>x</sup> e<sup>-λ</sup>) / x!<br>",
        "<b>Support:</b> x = 0, 1, 2, ...<br>",
        "<b>Parameter:</b> λ (rate)<br>",
        "<b>Mean:</b> λ<br>",
        "<b>Variance:</b> λ<br>",
        "<b>MGF:</b> e <sup>(λ (e<sup>t</sup> - 1)) </sup>"
      ),
      "Geometric" = paste0(
        "<b>Distribution: Geometric</b><br>",
        "<b>PMF:</b> P(X = x) = p(1-p)<sup>x</sup><br>",
        "<b>Support:</b> x = 0, 1, 2, ...<br>",
        "<b>Parameter:</b> p (probability)<br>",
        "<b>Mean:</b> 1/p<br>",
        "<b>Variance:</b> (1-p)/p<sup>2</sup><br>",
        "<b>MGF:</b> (p e<sup>t</sup>) / (1 - (1-p)e<sup>t</sup>)"
      ),
      "Normal" = paste0(
        "<b>Distribution: Normal</b><br>",
        "<b>PDF:</b> f(x) = (1 / √(2πσ<sup>2</sup>)) e<sup>-(x-μ)<sup>2</sup> / (2σ<sup>2</sup>)</sup><br>",
        "<b>Support:</b> x ∈ (-∞, ∞)<br>",
        "<b>Parameters:</b> μ (mean), σ (standard deviation)<br>",
        "<b>Mean:</b> μ<br>",
        "<b>Variance:</b> σ<sup>2</sup><br>",
        "<b>MGF:</b> e <sup> (μt + 0.5σ<sup>2</sup>t<sup>2</sup>) </sup>"
      ),
      "Exponential" = paste0(
        "<b>Distribution: Exponential</b><br>",
        "<b>PDF:</b> f(x) = λ e<sup>-λx</sup>, x ≥ 0<br>",
        "<b>Support:</b> x ≥ 0<br>",
        "<b>Parameter:</b> λ (rate)<br>",
        "<b>Mean:</b> 1/λ<br>",
        "<b>Variance:</b> 1/λ<sup>2</sup><br>",
        "<b>MGF:</b> λ / (λ - t)"
      ),
      "Uniform" = paste0(
        "<b>Distribution: Uniform</b><br>",
        "<b>PDF:</b> f(x) = 1 / (b - a), a ≤ x ≤ b<br>",
        "<b>Support:</b> x ∈ [a, b]<br>",
        "<b>Parameters:</b> a (min), b (max)<br>",
        "<b>Mean:</b> (a + b) / 2<br>",
        "<b>Variance:</b> (b - a)<sup>2</sup> / 12<br>",
        "<b>MGF:</b> (e<sup>bt</sup> - e<sup>at</sup>) / (t(b-a))"
      )
    )
    selected_info <- unlist(dist_info_list[input$dist_type], use.names = FALSE)
    HTML(paste0("<b>Distribution Information:</b><br>", 
                paste(selected_info, collapse = "<br><br>")))
  })
  
  # Displaying context descriptions
  output$dist_context <- renderUI({
    context_list <- list(
      "Binomial" = "The Binomial distribution models the number of successes in fixed independent trials, such as quality control scenarios.",
      "Poisson" = "The Poisson distribution is ideal for counting events over fixed intervals, like the number of emails received per hour.",
      "Geometric" = "The Geometric distribution describes the number of trials needed to get the first success, such as modeling attempts to win a game.",
      "Normal" = "The Normal distribution models naturally occurring data like test scores or heights, often used in hypothesis testing.",
      "Exponential" = "The Exponential distribution is used to model the time between events in processes like server request arrivals.",
      "Uniform" = "The Uniform distribution assumes all outcomes in a range are equally likely, often used for random sampling."
    )
    
    selected_context <- unlist(context_list[input$dist_type], use.names = FALSE)
    HTML(paste("<b>Context:</b><br>", paste(selected_context, collapse = "<br><br>")))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)