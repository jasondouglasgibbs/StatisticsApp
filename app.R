library(shiny)
library(bslib)
library(plotly)
library(DT)

# Define a list of popular pre-loaded datasets in R
dataset_choices <- c("mtcars", "iris", "faithful", "airquality", "trees", "quakes", "swiss")

# Define UI
ui <- fluidPage(
  # Apply a dark mode theme using bslib
  theme = bs_theme(bootswatch = "darkly"),
  
  titlePanel("Interactive R Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select the dataset
      selectInput("dataset", "Choose a dataset:", choices = dataset_choices),
      
      # Dynamic dropdown for column selection
      uiOutput("col_select"),
      
      hr(),
      
      # Button to trigger normality testing
      h5("Statistical Tests"),
      actionButton("run_tests", "Run Statistical Tests", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      h4("Dataset Preview"),
      DTOutput("dataTable"),
      
      hr(),
      
      h4("Expanded Descriptive Statistics"),
      verbatimTextOutput("stats"),
      
      hr(),
      
      # Plots Section
      fluidRow(
        column(width = 6,
               h4("Interactive Histogram"),
               plotlyOutput("histogram")
        ),
        column(width = 6,
               h4("Density Curve"),
               plotlyOutput("density")
        )
      ),
      
      hr(),
      
      h4("Interactive Box & Whisker Plot"),
      plotlyOutput("boxplot"),
      
      hr(),
      
      # Normality Test Results Section (Moved below the plots)
      h4("Normality Test Results (Shapiro-Wilk)"),
      verbatimTextOutput("normality_test_out"),
      uiOutput("normality_explanation"), # Dynamic explanation output
      
      br(), br() # Add a little bottom padding
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Reactive expression to fetch the selected dataset
  datasetInput <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  # Dynamically render the column selection input
  output$col_select <- renderUI({
    df <- datasetInput()
    # Filter for only numeric columns
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("column", "Choose a numeric column:", choices = numeric_cols)
  })
  
  # Render the interactive data table
  output$dataTable <- renderDT({
    df <- datasetInput()
    datatable(df, 
              options = list(pageLength = 5, scrollX = TRUE),
              style = "bootstrap4",
              class = "table-dark table-striped")
  })
  
  # Render expanded descriptive statistics
  output$stats <- renderPrint({
    req(input$column)
    df <- datasetInput()
    x <- na.omit(df[[input$column]])
    
    stats <- c(
      "Valid Rows (N)" = length(x),
      "Missing (NA)" = sum(is.na(df[[input$column]])),
      "Minimum" = min(x),
      "1st Quartile" = quantile(x, 0.25, names = FALSE),
      "Median" = median(x),
      "Mean" = mean(x),
      "3rd Quartile" = quantile(x, 0.75, names = FALSE),
      "Maximum" = max(x),
      "Variance" = var(x),
      "Standard Dev" = sd(x),
      "IQR" = IQR(x)
    )
    
    print(round(stats, 4))
  })
  
  # --- Plotting Logic ---
  
  apply_plotly_dark_theme <- function(p) {
    p %>% layout(
      plot_bgcolor = "#222222",
      paper_bgcolor = "#222222",
      font = list(color = "white")
    )
  }
  
  output$histogram <- renderPlotly({
    req(input$column)
    df <- datasetInput()
    
    p <- plot_ly(x = ~df[[input$column]], 
                 type = "histogram", 
                 marker = list(color = "#00bc8c", line = list(color = "white", width = 1)),
                 name = input$column,
                 hovertemplate = paste("Value: %{x}<br>Count: %{y}<extra></extra>")) %>%
      layout(xaxis = list(title = input$column, gridcolor = "#444444"),
             yaxis = list(title = "Frequency", gridcolor = "#444444"))
    apply_plotly_dark_theme(p)
  })
  
  output$density <- renderPlotly({
    req(input$column)
    df <- datasetInput()
    x <- na.omit(df[[input$column]])
    
    dens <- density(x)
    p <- plot_ly(x = ~dens$x, y = ~dens$y, 
                 type = 'scatter', mode = 'lines', fill = 'tozeroy', 
                 fillcolor = "rgba(232, 62, 140, 0.4)",
                 line = list(color = "#e83e8c", width = 2),
                 name = "Density",
                 hovertemplate = paste("Value: %{x:.2f}<br>Density: %{y:.4f}<extra></extra>")) %>%
      layout(xaxis = list(title = input$column, gridcolor = "#444444"),
             yaxis = list(title = "Density", gridcolor = "#444444"))
    apply_plotly_dark_theme(p)
  })
  
  output$boxplot <- renderPlotly({
    req(input$column)
    df <- datasetInput()
    
    p <- plot_ly(y = ~df[[input$column]], 
                 type = "box", name = input$column, 
                 marker = list(color = "#375a7f"), line = list(color = "#375a7f"),
                 fillcolor = "rgba(55, 90, 127, 0.5)", 
                 hovertemplate = paste("Value: %{y}<extra></extra>")) %>%
      layout(yaxis = list(title = input$column, gridcolor = "#444444"),
             xaxis = list(title = ""))
    apply_plotly_dark_theme(p)
  })
  
  # --- Normality Test Logic ---
  
  # Store results in a structured list so we can parse the exact p-value and warnings
  test_results <- reactiveVal(NULL)
  
  observeEvent(c(input$dataset, input$column), {
    test_results(NULL)
  })
  
  observeEvent(input$run_tests, {
    req(input$column)
    df <- datasetInput()
    x <- na.omit(df[[input$column]])
    
    if (length(x) < 3) {
      test_results(list(error = "Sample size is too small (n < 3) to perform the Shapiro-Wilk test."))
    } else if (length(x) > 5000) {
      res <- shapiro.test(sample(x, 5000))
      test_results(list(test = res, warning = "Warning: Sample size > 5000. Data was randomly sampled to 5000 observations."))
    } else {
      res <- shapiro.test(x)
      test_results(list(test = res))
    }
  })
  
  # Output the raw statistical test
  output$normality_test_out <- renderPrint({
    res <- test_results()
    if (is.null(res)) {
      cat("Waiting... Click 'Run Statistical Tests' in the sidebar to execute.")
    } else if (!is.null(res$error)) {
      cat(res$error)
    } else {
      if (!is.null(res$warning)) {
        cat(res$warning, "\n\n")
      }
      print(res$test)
    }
  })
  
  # Output the generated explanation for the test
  output$normality_explanation <- renderUI({
    res <- test_results()
    
    # Don't show anything if test hasn't run or if there's an error
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    p_val <- res$test$p.value
    alpha <- 0.05
    
    if (p_val < alpha) {
      explanation_html <- HTML(paste0(
        "<div style='color: #e74c3c; padding: 15px; border-left: 4px solid #e74c3c; background-color: #333;'>",
        "<b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits = 4), 
        "</b>) is less than the standard significance level of 0.05. ",
        "We reject the null hypothesis, indicating strong evidence that the data for <b>", 
        input$column, "</b> <u>significantly deviates from a normal distribution</u>.",
        "</div>"
      ))
    } else {
      explanation_html <- HTML(paste0(
        "<div style='color: #00bc8c; padding: 15px; border-left: 4px solid #00bc8c; background-color: #333;'>",
        "<b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits = 4), 
        "</b>) is greater than or equal to the significance level of 0.05. ",
        "We fail to reject the null hypothesis, meaning there is <u>insufficient evidence to state that the data deviates from a normal distribution</u>. ",
        "It is reasonable to assume normality for <b>", input$column, "</b>.",
        "</div>"
      ))
    }
    
    return(explanation_html)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)