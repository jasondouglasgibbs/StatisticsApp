library(shiny)
library(bslib)
library(plotly)
library(DT)
library(readxl)
library(rmarkdown)

# Define a list of popular pre-loaded datasets in R
dataset_choices <- c("mtcars", "iris", "faithful", "airquality", "trees", "quakes", "swiss")

# Define UI
ui <- fluidPage(
  # Initialize MathJax to render statistical formulas
  withMathJax(),
  
  # Apply a dark mode theme using bslib
  theme = bs_theme(bootswatch = "darkly"),
  
  # Custom CSS for the modal elements
  tags$head(
    tags$style(HTML("
      /* Force the R Engine Text Area into dark mode */
      #calc_expr { 
        background-color: #222222 !important; 
        color: #ffffff !important; 
        border: 1px solid #444444 !important; 
      }
      /* Dark mode iframe inversion hack for external embedded sites */
      .dark-iframe {
        border: none; 
        border-radius: 5px; 
        filter: invert(0.95) hue-rotate(180deg);
        background-color: #ffffff; /* Requires a white base to invert into dark gray/black */
      }
    "))
  ),
  
  # Flexbox header to place the title on the left and the buttons on the right
  div(
    style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px; margin-bottom: 20px;",
    h2("Interactive R Dataset Explorer", style = "margin: 0;"),
    div(
      actionButton("open_calc_modal", "Scientific Calculator", class = "btn-secondary", style = "margin-right: 10px;"),
      actionButton("open_ref_modal", "Statistical Guide", class = "btn-success")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Data Source Toggle
      radioButtons("data_source", "Data Source:", choices = c("Preloaded Dataset", "Upload Excel")),
      
      # Conditional panel for Preloaded Datasets
      conditionalPanel(
        condition = "input.data_source == 'Preloaded Dataset'",
        selectInput("dataset", "Choose a dataset:", choices = dataset_choices)
      ),
      
      # Conditional panel for Excel Upload
      conditionalPanel(
        condition = "input.data_source == 'Upload Excel'",
        fileInput("file_upload", "Choose Excel File", accept = c(".xlsx", ".xls"))
      ),
      
      # Dynamic dropdown for column selection
      uiOutput("col_select"),
      
      hr(),
      
      # Buttons for Normality Testing
      h5("Analysis & Visuals"),
      actionButton("run_tests", "Run Normality Test", class = "btn-primary", width = "100%"),
      
      br(), br(),
      
      # Group-based statistical testing
      h5("Group-Based Testing"),
      uiOutput("factor_select"),
      
      p(strong("Parametric Tests (Assumes Normality):"), style = "margin-bottom: 5px; color: #f39c12;"),
      fluidRow(
        column(4, actionButton("run_anova", "ANOVA", class = "btn-warning", width = "100%")),
        column(4, actionButton("run_ttest", "T-Test", class = "btn-warning", width = "100%")),
        column(4, actionButton("run_tukey", "Tukey's", class = "btn-warning", width = "100%"))
      ),
      
      br(),
      
      p(strong("Non-Parametric Tests (No Normality Assumed):"), style = "margin-bottom: 5px; color: #00bc8c;"),
      fluidRow(
        column(6, actionButton("run_kruskal", "Kruskal-Wallis", class = "btn-success", width = "100%")),
        column(6, actionButton("run_wilcox", "Wilcoxon", class = "btn-success", width = "100%"))
      ),
      
      br(),
      
      # Categorical testing
      h5("Categorical Testing"),
      uiOutput("chisq_var1_select"),
      uiOutput("chisq_var2_select"),
      actionButton("run_chisq", "Run Chi-Square", class = "btn-info", width = "100%"),
      
      br(), br(),
      
      # Tools
      h5("Tools"),
      actionButton("open_graph_modal", "Custom Graph", class = "btn-info", width = "100%"),
      br(), br(),
      downloadButton("download_report", "Download HTML Report", class = "btn-danger", style = "width: 100%;")
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
      
      # Normality Test Results Section
      wellPanel(
        h4("Normality Test Results (Shapiro-Wilk)"),
        verbatimTextOutput("normality_test_out"),
        uiOutput("normality_explanation")
      ),
      
      hr(),
      
      # Advanced Statistical Tests Results Section
      h3("Group-Based Test Results"),
      
      # Parametric
      wellPanel(
        h4("ANOVA (Analysis of Variance)"),
        verbatimTextOutput("anova_out"),
        uiOutput("anova_exp")
      ),
      
      wellPanel(
        h4("Two-Sample T-Test"),
        verbatimTextOutput("ttest_out"),
        uiOutput("ttest_exp")
      ),
      
      wellPanel(
        h4("Tukey's HSD (Honest Significant Difference)"),
        verbatimTextOutput("tukey_out"),
        uiOutput("tukey_exp")
      ),
      
      # Non-Parametric
      wellPanel(
        h4("Kruskal-Wallis Rank Sum Test"),
        verbatimTextOutput("kruskal_out"),
        uiOutput("kruskal_exp")
      ),
      
      wellPanel(
        h4("Wilcoxon Rank-Sum Test (Mann-Whitney U)"),
        verbatimTextOutput("wilcox_out"),
        uiOutput("wilcox_exp")
      ),
      
      hr(),
      
      h3("Categorical Test Results"),
      wellPanel(
        h4("Chi-Square Test of Independence"),
        plotlyOutput("chisq_plot", height = "350px"),
        br(),
        verbatimTextOutput("chisq_out"),
        uiOutput("chisq_exp")
      ),
      
      br(), br() 
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # Reactive expression to fetch the selected dataset or uploaded file
  datasetInput <- reactive({
    if (input$data_source == "Preloaded Dataset") {
      raw_df <- get(input$dataset, "package:datasets")
      df <- as.data.frame(raw_df)
      
      r_names <- row.names(df)
      default_names <- as.character(seq_len(nrow(df)))
      
      if (!identical(r_names, default_names)) {
        df <- cbind(Title = r_names, df)
        row.names(df) <- NULL 
      }
      
      return(df)
    } else {
      req(input$file_upload)
      df <- as.data.frame(readxl::read_excel(input$file_upload$datapath))
      return(df)
    }
  })
  
  # Dynamically render the numeric column selection input
  output$col_select <- renderUI({
    df <- datasetInput()
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("column", "Choose a numeric column (Dependent Variable):", choices = numeric_cols)
  })
  
  # Dynamically render the factor selection input containing ALL columns
  output$factor_select <- renderUI({
    df <- datasetInput()
    selectInput("factor_column", "Choose a grouping column (Factor):", choices = names(df))
  })
  
  # Dynamically render categorical selection inputs for Chi-Square
  output$chisq_var1_select <- renderUI({
    df <- datasetInput()
    selectInput("chisq_var1", "Variable 1 (Categorical):", choices = names(df))
  })
  
  output$chisq_var2_select <- renderUI({
    df <- datasetInput()
    selectInput("chisq_var2", "Variable 2 (Categorical):", choices = names(df))
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
  
  # --- Plotting Logic (Main Dashboard) ---
  
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
  
  # --- Modal Custom Graph Logic ---
  
  observeEvent(input$open_graph_modal, {
    df <- datasetInput()
    all_cols <- names(df)
    title_text <- if(input$data_source == "Preloaded Dataset") input$dataset else input$file_upload$name
    
    showModal(modalDialog(
      title = paste("Custom Graph Plotter -", title_text),
      size = "xl", 
      fluidRow(
        column(4, selectInput("mod_x", "X-Axis Variable:", choices = all_cols)),
        column(4, selectInput("mod_y", "Y-Axis Variable (Optional):", choices = c("None", all_cols))),
        column(4, selectInput("mod_type", "Plot Type:", choices = c("Scatter", "Line", "Bar", "Box", "Histogram")))
      ),
      hr(),
      plotlyOutput("modal_plot", height = "550px"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$modal_plot <- renderPlotly({
    req(input$mod_x, input$mod_type)
    df <- datasetInput()
    x_col <- input$mod_x
    y_col <- input$mod_y
    type <- input$mod_type
    
    p <- plot_ly()
    
    if (y_col == "None") {
      if (type %in% c("Scatter", "Line")) {
        mode_val <- ifelse(type == "Scatter", "markers", "lines")
        p <- plot_ly(x = ~df[[x_col]], type = "scatter", mode = mode_val, name = x_col, marker = list(color = "#f39c12"))
      } else if (type == "Bar" || type == "Histogram") {
        p <- plot_ly(x = ~df[[x_col]], type = "histogram", name = x_col, marker = list(color = "#f39c12"))
      } else if (type == "Box") {
        p <- plot_ly(y = ~df[[x_col]], type = "box", name = x_col, marker = list(color = "#f39c12"))
      }
    } else {
      if (type %in% c("Scatter", "Line")) {
        mode_val <- ifelse(type == "Scatter", "markers", "lines")
        p <- plot_ly(x = ~df[[x_col]], y = ~df[[y_col]], type = "scatter", mode = mode_val, marker = list(color = "#f39c12"))
      } else if (type == "Bar") {
        p <- plot_ly(x = ~df[[x_col]], y = ~df[[y_col]], type = "bar", marker = list(color = "#f39c12"))
      } else if (type == "Box") {
        p <- plot_ly(x = ~df[[x_col]], y = ~df[[y_col]], type = "box", marker = list(color = "#f39c12"))
      } else if (type == "Histogram") {
        p <- plot_ly(x = ~df[[x_col]], y = ~df[[y_col]], type = "histogram2d")
      }
    }
    
    p <- p %>% layout(
      xaxis = list(title = x_col, gridcolor = "#444444"),
      yaxis = list(title = ifelse(y_col == "None", "Value / Frequency", y_col), gridcolor = "#444444")
    )
    
    apply_plotly_dark_theme(p)
  })
  
  # --- Scientific Calculator Modal Logic ---
  observeEvent(input$open_calc_modal, {
    showModal(modalDialog(
      title = "Scientific & Statistical Calculator",
      size = "xl", 
      tabsetPanel(
        tabPanel("Scientific Calculator",
                 br(),
                 tags$iframe(src = "https://www.desmos.com/scientific", width = "100%", height = "500px", class = "dark-iframe")
        ),
        tabPanel("Graphing Calculator",
                 br(),
                 tags$iframe(src = "https://www.desmos.com/calculator", width = "100%", height = "500px", class = "dark-iframe")
        ),
        tabPanel("R Statistical Engine",
                 br(),
                 p("Evaluate advanced R mathematical and statistical expressions in real-time."),
                 textAreaInput("calc_expr", "Expression:", width = "100%", height = "100px",
                               placeholder = "e.g., \n# Trigonometry\nround(sin(pi/4), 3) \n\n# Probability (Normal Dist)\npnorm(1.96)"),
                 wellPanel(
                   style = "background-color: #222; border-color: #444;",
                   tags$style("#calc_out { background-color: transparent; border: none; color: #00bc8c; font-size: 1.1em; font-weight: bold; }"),
                   verbatimTextOutput("calc_out")
                 ),
                 hr(),
                 fluidRow(
                   column(4,
                          h5(style="color: #f39c12;", "Basic Math"),
                          tags$ul(
                            tags$li(code("sqrt(x)"), ", ", code("abs(x)")),
                            tags$li(code("exp(x)"), ", ", code("log(x)")),
                            tags$li(code("factorial(x)")),
                            tags$li(code("round(x, digits)")),
                            tags$li(code("choose(n, k)"))
                          )
                   ),
                   column(4,
                          h5(style="color: #f39c12;", "Trigonometry"),
                          tags$ul(
                            tags$li(code("sin(x)"), ", ", code("asin(x)")),
                            tags$li(code("cos(x)"), ", ", code("acos(x)")),
                            tags$li(code("tan(x)"), ", ", code("atan(x)")),
                            tags$li(code("pi"))
                          )
                   ),
                   column(4,
                          h5(style="color: #f39c12;", "Statistical"),
                          tags$ul(
                            tags$li(code("pnorm(q)"), " (Normal Prob)"),
                            tags$li(code("qnorm(p)"), " (Normal Quant)"),
                            tags$li(code("pt(q, df)"), " (T Prob)"),
                            tags$li(code("qt(p, df)"), " (T Quant)"),
                            tags$li(code("pchisq(q, df)"), " (Chi-Sq Prob)")
                          )
                   )
                 )
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Real-time evaluation logic for the R Engine Calculator
  output$calc_out <- renderPrint({
    req(input$calc_expr)
    tryCatch({
      expr <- parse(text = input$calc_expr)
      if (length(expr) > 0) {
        res <- eval(expr)
        print(res)
      } else {
        cat("")
      }
    }, error = function(e) {
      cat("Error: ", e$message)
    })
  })
  
  # --- Statistical Reference Modal Logic ---
  
  observeEvent(input$open_ref_modal, {
    showModal(modalDialog(
      title = "Statistical Guide & Reference",
      size = "l", 
      easyClose = TRUE,
      footer = modalButton("Close"),
      withMathJax(
        tabsetPanel(
          tabPanel("Glossary", 
                   br(),
                   HTML("
              <ul style='font-size: 1.1em; line-height: 1.8;'>
                <li><b>p-value:</b> The probability of observing results as extreme as those in the data, assuming the null hypothesis is true.</li>
                <li><b>Alpha (\\(\\alpha\\)):</b> The significance level, typically set to 0.05.</li>
                <li><b>Null Hypothesis (\\(H_0\\)):</b> The default assumption that there is no significant effect or difference.</li>
                <li><b>Interquartile Range (IQR):</b> A measure of statistical dispersion representing the middle 50% of the data.</li>
                <li><b>Parametric Tests:</b> Statistical tests (like ANOVA, T-Test) that assume data follows a normal distribution.</li>
                <li><b>Non-Parametric Tests:</b> Statistical tests (like Kruskal-Wallis, Wilcoxon) used when data is skewed or ordinal.</li>
                <li><b>Chi-Square Test:</b> Evaluates whether there is a significant association between two categorical variables.</li>
              </ul>
            ")
          ),
          tabPanel("Assumptions Checklist",
                   br(),
                   HTML("
              <div style='padding: 10px; background-color: #333; border-radius: 5px; margin-bottom: 15px;'>
                <h5 style='color: #f39c12;'>Parametric Tests (T-Test & ANOVA)</h5>
                <ul>
                  <li><b>Independence:</b> Observations in one group are independent of observations in another.</li>
                  <li><b>Normality:</b> The data in each group should be approximately normally distributed.</li>
                  <li><b>Homogeneity of Variance:</b> The variances of the groups should be roughly equal.</li>
                </ul>
              </div>
              <div style='padding: 10px; background-color: #333; border-radius: 5px; margin-bottom: 15px;'>
                <h5 style='color: #00bc8c;'>Non-Parametric Tests (Wilcoxon & Kruskal-Wallis)</h5>
                <ul>
                  <li><b>Use When:</b> Your data fails the Shapiro-Wilk test (p < 0.05).</li>
                  <li><b>Independence:</b> The samples are independent of each other.</li>
                </ul>
              </div>
              <div style='padding: 10px; background-color: #333; border-radius: 5px;'>
                <h5 style='color: #3498db;'>Categorical Tests (Chi-Square)</h5>
                <ul>
                  <li><b>Independence:</b> The observations are independent.</li>
                  <li><b>Expected Cell Frequencies:</b> Usually assumes at least 5 expected observations per group/cell.</li>
                </ul>
              </div>
            ")
          ),
          tabPanel("Formulae",
                   br(),
                   HTML("
              <div style='text-align: center; font-size: 1.2em; padding: 15px; background-color: #222; border-radius: 5px;'>
                <p style='color: #00bc8c; font-weight: bold;'>Sample Variance:</p>
                <p>$$s^2 = \\frac{\\sum_{i=1}^{n} (x_i - \\bar{x})^2}{n-1}$$</p>
                <hr style='border-color: #444;'>
                <p style='color: #f39c12; font-weight: bold;'>T-Statistic (Welch's Two-Sample):</p>
                <p>$$t = \\frac{\\bar{x}_1 - \\bar{x}_2}{\\sqrt{ \\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2} }}$$</p>
                <hr style='border-color: #444;'>
                <p style='color: #3498db; font-weight: bold;'>Chi-Square Statistic:</p>
                <p>$$\\chi^2 = \\sum \\frac{(O_i - E_i)^2}{E_i}$$</p>
              </div>
            ")
          )
        )
      ) 
    ))
  })
  
  # --- Automated HTML Report Generation Logic ---
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("Statistical_Report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      showNotification("Generating report, this may take a moment...", type = "message", id = "report_notif", duration = NULL)
      on.exit(removeNotification("report_notif"), add = TRUE)
      
      tempReport <- file.path(tempdir(), "automated_report.Rmd")
      
      rmd_content <- c(
        "---",
        "title: 'Exploratory Data Analysis Report'",
        "date: '`r Sys.Date()`'",
        "output:",
        "  html_document:",
        "    theme: darkly",
        "    toc: true",
        "    toc_float: true",
        "params:",
        "  df: NA",
        "  num_col: NA",
        "  factor_col: NA",
        "  ds_name: NA",
        "---",
        "",
        "```{r setup, include=FALSE}",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
        "library(plotly)",
        "```",
        "",
        "## Analysis Configuration",
        "**Dataset Source:** `r params$ds_name`  ",
        "**Numeric Variable (Dependent):** `r params$num_col`  ",
        "**Grouping Variable (Factor):** `r params$factor_col`  ",
        "",
        "---",
        "",
        "## Descriptive Statistics",
        "```{r}",
        "x <- na.omit(params$df[[params$num_col]])",
        "stats_df <- data.frame(",
        "  Metric = c('Valid Rows', 'Missing (NA)', 'Minimum', '1st Quartile', 'Median', 'Mean', '3rd Quartile', 'Maximum', 'Variance', 'Standard Dev', 'IQR'),",
        "  Value = c(length(x), sum(is.na(params$df[[params$num_col]])), min(x), quantile(x, 0.25, names = FALSE), median(x), mean(x), quantile(x, 0.75, names = FALSE), max(x), var(x), sd(x), IQR(x))",
        ")",
        "knitr::kable(stats_df, digits = 4, col.names = c('Metric', 'Value'), align = 'l')",
        "```",
        "",
        "---",
        "",
        "## Visualizations",
        "```{r fig.width=8, fig.height=4}",
        "plot_ly(x = ~params$df[[params$num_col]], type = 'histogram', name = params$num_col, marker = list(color = '#00bc8c', line = list(color = 'white', width = 1))) %>%",
        "  layout(title = paste('Histogram of', params$num_col), xaxis = list(title = params$num_col), yaxis = list(title = 'Frequency'), plot_bgcolor = '#222222', paper_bgcolor = '#222222', font = list(color = 'white'))",
        "```",
        "",
        "```{r fig.width=8, fig.height=4}",
        "plot_ly(y = ~params$df[[params$num_col]], type = 'box', name = params$num_col, marker = list(color = '#375a7f'), line = list(color = '#375a7f')) %>%",
        "  layout(title = paste('Boxplot of', params$num_col), yaxis = list(title = params$num_col), plot_bgcolor = '#222222', paper_bgcolor = '#222222', font = list(color = 'white'))",
        "```",
        "",
        "---",
        "",
        "## Normality Assumption (Shapiro-Wilk)",
        "```{r}",
        "if(length(x) >= 3 && length(x) <= 5000) {",
        "  res <- shapiro.test(x)",
        "  cat('**W-Statistic:**', round(res$statistic, 4), '  \\n')",
        "  cat('**p-value:**', format.pval(res$p.value, digits = 4), '  \\n\\n')",
        "  if (res$p.value < 0.05) {",
        "    cat('**Conclusion:** The data significantly deviates from a normal distribution. **Non-parametric tests are recommended.**')",
        "  } else {",
        "    cat('**Conclusion:** There is insufficient evidence to state the data deviates from normal distribution. **Parametric tests are appropriate.**')",
        "  }",
        "} else {",
        "  cat('Sample size must be between 3 and 5000 observations to run the Shapiro-Wilk test.')",
        "}",
        "```"
      )
      
      writeLines(rmd_content, tempReport)
      
      ds_name_val <- if (input$data_source == "Preloaded Dataset") input$dataset else input$file_upload$name
      
      rmarkdown::render(tempReport, output_file = file,
                        params = list(
                          df = datasetInput(),
                          num_col = input$column,
                          factor_col = input$factor_column,
                          ds_name = ds_name_val
                        ),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  # --- Statistical Tests Data Store ---
  
  normality_results <- reactiveVal(NULL)
  test_store <- reactiveValues(anova = NULL, ttest = NULL, tukey = NULL, wilcox = NULL, kruskal = NULL, chisq = NULL)
  
  observeEvent(c(input$data_source, input$dataset, input$file_upload, input$column, input$factor_column, input$chisq_var1, input$chisq_var2), {
    normality_results(NULL)
    test_store$anova <- NULL
    test_store$ttest <- NULL
    test_store$tukey <- NULL
    test_store$wilcox <- NULL
    test_store$kruskal <- NULL
    test_store$chisq <- NULL
  })
  
  # --- Normality Logic & Popup ---
  observeEvent(input$run_tests, {
    req(input$column)
    df <- datasetInput()
    x <- na.omit(df[[input$column]])
    
    if (length(x) < 3) {
      normality_results(list(error = "Sample size is too small (n < 3) to perform the Shapiro-Wilk test."))
    } else {
      if (length(x) > 5000) {
        res <- shapiro.test(sample(x, 5000))
        normality_results(list(test = res, warning = "Warning: Sample size > 5000. Data was randomly sampled to 5000 observations."))
      } else {
        res <- shapiro.test(x)
        normality_results(list(test = res))
      }
      
      if (res$p.value < 0.05) {
        showModal(modalDialog(
          title = "Normality Assumption Violated",
          HTML(paste0(
            "<div style='color: #e74c3c; margin-bottom: 15px;'>",
            "The Shapiro-Wilk test indicates that the data for <b>", input$column, 
            "</b> significantly deviates from a normal distribution (p < 0.05).",
            "</div>",
            "<div style='background-color: #333; padding: 15px; border-radius: 5px;'>",
            "<h5 style='color: #00bc8c;'>Recommendation:</h5>",
            "<p>It is highly recommended to use the <b>Non-Parametric alternatives</b> for your group-based testing instead of ANOVA or the T-Test:</p>",
            "<ul>",
            "<li>Use <b>Wilcoxon</b> for 2 groups.</li>",
            "<li>Use <b>Kruskal-Wallis</b> for 3 or more groups.</li>",
            "</ul>",
            "</div>"
          )),
          easyClose = TRUE,
          footer = modalButton("Understood")
        ))
      }
    }
  })
  
  output$normality_test_out <- renderPrint({
    res <- normality_results()
    if (is.null(res)) cat("Waiting... Click 'Run Normality Test' in the sidebar.")
    else if (!is.null(res$error)) cat(res$error)
    else {
      if (!is.null(res$warning)) cat(res$warning, "\n\n")
      print(res$test)
    }
  })
  
  output$normality_explanation <- renderUI({
    res <- normality_results()
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$test$p.value
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> p-value (<b>", format.pval(p_val, digits = 4), "</b>) < 0.05. Strong evidence that the data <u>significantly deviates from a normal distribution</u>.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> p-value (<b>", format.pval(p_val, digits = 4), "</b>) >= 0.05. <u>Insufficient evidence to state the data deviates from a normal distribution</u>. Normal distribution is assumed.</div>"))
    }
  })
  
  # --- Group-Based Testing Core Execution Logic ---
  
  prepare_group_data <- function(df, factor_col_name) {
    original_col <- df[[factor_col_name]]
    df[[factor_col_name]] <- as.factor(df[[factor_col_name]])
    levels_count <- length(levels(na.omit(df[[factor_col_name]])))
    return(list(df = df, original_col = original_col, levels_count = levels_count))
  }
  
  revert_group_data <- function(df, factor_col_name, original_col) {
    df[[factor_col_name]] <- original_col
    return(df)
  }
  
  # Validator to catch errors before test runs
  validate_factor <- function(df, dep_col, factor_col) {
    if (dep_col == factor_col) {
      return("The dependent variable and the grouping column cannot be the same. This mistakenly converts your target variable into a categorical factor, collapsing the model matrix.")
    }
    col_data <- df[[factor_col]]
    unique_vals <- length(unique(na.omit(col_data)))
    
    # Check if a numeric column was selected that has too many distinct values
    if (is.numeric(col_data) && unique_vals > 20) {
      return(paste0("The selected grouping column <b>", factor_col, "</b> has too many unique numeric values (", unique_vals, "). It is likely a continuous variable, not a factorable category."))
    }
    if (unique_vals == nrow(df) && nrow(df) > 2) {
      return(paste0("The selected grouping column <b>", factor_col, "</b> contains completely unique rows. It is likely an ID column, not a factorable category."))
    }
    return(NULL)
  }
  
  # ANOVA
  observeEvent(input$run_anova, {
    req(input$column, input$factor_column)
    df <- datasetInput()
    
    # Run the pre-flight checks
    err_msg <- validate_factor(df, input$column, input$factor_column)
    if (!is.null(err_msg)) {
      showModal(modalDialog(
        title = "Invalid Grouping Factor",
        HTML(paste0("<div style='color: #e74c3c; font-size: 1.1em;'>", err_msg, "</div>")),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    prep <- prepare_group_data(df, input$factor_column)
    fmla <- as.formula(paste0("`", input$column, "` ~ `", input$factor_column, "`"))
    
    if (prep$levels_count < 2) {
      test_store$anova <- list(error = paste("ANOVA requires at least 2 distinct groups. Found:", prep$levels_count))
    } else {
      aov_res <- aov(fmla, data = prep$df)
      test_store$anova <- list(res = summary(aov_res), factor = input$factor_column, dep = input$column)
    }
    df <- revert_group_data(prep$df, input$factor_column, prep$original_col)
  })
  
  output$anova_out <- renderPrint({
    if (is.null(test_store$anova)) cat("Waiting... Click 'ANOVA' in the sidebar.")
    else if (!is.null(test_store$anova$error)) cat("ERROR:", test_store$anova$error)
    else print(test_store$anova$res)
  })
  
  output$anova_exp <- renderUI({
    res <- test_store$anova
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$res[[1]][["Pr(>F)"]][1]
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is < 0.05. There is a statistically significant difference in the mean of <b>", res$dep, "</b> between at least two groups of <b>", res$factor, "</b>.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is >= 0.05. There is no statistically significant difference in the mean of <b>", res$dep, "</b> across the groups of <b>", res$factor, "</b>.</div>"))
    }
  })
  
  # T-Test
  observeEvent(input$run_ttest, {
    req(input$column, input$factor_column)
    df <- datasetInput()
    
    err_msg <- validate_factor(df, input$column, input$factor_column)
    if (!is.null(err_msg)) {
      showModal(modalDialog(
        title = "Invalid Grouping Factor",
        HTML(paste0("<div style='color: #e74c3c; font-size: 1.1em;'>", err_msg, "</div>")),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    prep <- prepare_group_data(df, input$factor_column)
    fmla <- as.formula(paste0("`", input$column, "` ~ `", input$factor_column, "`"))
    
    if (prep$levels_count != 2) {
      test_store$ttest <- list(error = paste("A Two-Sample T-Test requires exactly 2 distinct groups. Found:", prep$levels_count))
    } else {
      t_res <- t.test(fmla, data = prep$df)
      test_store$ttest <- list(res = t_res, factor = input$factor_column, dep = input$column)
    }
    df <- revert_group_data(prep$df, input$factor_column, prep$original_col)
  })
  
  output$ttest_out <- renderPrint({
    if (is.null(test_store$ttest)) cat("Waiting... Click 'T-Test' in the sidebar.")
    else if (!is.null(test_store$ttest$error)) cat("ERROR:", test_store$ttest$error)
    else print(test_store$ttest$res)
  })
  
  output$ttest_exp <- renderUI({
    res <- test_store$ttest
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$res$p.value
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is < 0.05. The means of the two groups are <u>significantly different</u> from each other.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is >= 0.05. The means of the two groups are <u>not significantly different</u>.</div>"))
    }
  })
  
  # Tukey's
  observeEvent(input$run_tukey, {
    req(input$column, input$factor_column)
    df <- datasetInput()
    
    err_msg <- validate_factor(df, input$column, input$factor_column)
    if (!is.null(err_msg)) {
      showModal(modalDialog(
        title = "Invalid Grouping Factor",
        HTML(paste0("<div style='color: #e74c3c; font-size: 1.1em;'>", err_msg, "</div>")),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    prep <- prepare_group_data(df, input$factor_column)
    fmla <- as.formula(paste0("`", input$column, "` ~ `", input$factor_column, "`"))
    
    if (prep$levels_count < 2) {
      test_store$tukey <- list(error = paste("Tukey's HSD requires at least 2 distinct groups. Found:", prep$levels_count))
    } else {
      aov_res <- aov(fmla, data = prep$df)
      tukey_res <- TukeyHSD(aov_res)
      test_store$tukey <- list(res = tukey_res, factor = input$factor_column, dep = input$column)
    }
    df <- revert_group_data(prep$df, input$factor_column, prep$original_col)
  })
  
  output$tukey_out <- renderPrint({
    if (is.null(test_store$tukey)) cat("Waiting... Click 'Tukey's' in the sidebar.")
    else if (!is.null(test_store$tukey$error)) cat("ERROR:", test_store$tukey$error)
    else print(test_store$tukey$res)
  })
  
  output$tukey_exp <- renderUI({
    res <- test_store$tukey
    if (is.null(res) || !is.null(res$error)) return(NULL)
    
    tukey_mat <- res$res[[1]]
    sig_pairs <- rownames(tukey_mat)[which(tukey_mat[, "p adj"] < 0.05)]
    nonsig_pairs <- rownames(tukey_mat)[which(tukey_mat[, "p adj"] >= 0.05)]
    
    sig_text <- if (length(sig_pairs) > 0) paste0("<ul style='color: #f39c12; margin-bottom: 10px;'>", paste("<li>", sig_pairs, "</li>", collapse = ""), "</ul>") else "<p style='color: #f39c12; margin-bottom: 10px;'><em>None</em></p>"
    nonsig_text <- if (length(nonsig_pairs) > 0) paste0("<ul style='color: #00bc8c;'>", paste("<li>", nonsig_pairs, "</li>", collapse = ""), "</ul>") else "<p style='color: #00bc8c;'><em>None</em></p>"
    
    HTML(paste0("<div style='padding: 15px; border-left: 4px solid #3498db; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> Based on the adjusted p-values:<br><br><span style='color: #f39c12; font-weight: bold;'>Significantly different means (p < 0.05):</span>", sig_text, "<span style='color: #00bc8c; font-weight: bold;'>Not significantly different means (p >= 0.05):</span>", nonsig_text, "</div>"))
  })
  
  # Kruskal-Wallis
  observeEvent(input$run_kruskal, {
    req(input$column, input$factor_column)
    df <- datasetInput()
    
    err_msg <- validate_factor(df, input$column, input$factor_column)
    if (!is.null(err_msg)) {
      showModal(modalDialog(
        title = "Invalid Grouping Factor",
        HTML(paste0("<div style='color: #e74c3c; font-size: 1.1em;'>", err_msg, "</div>")),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    prep <- prepare_group_data(df, input$factor_column)
    fmla <- as.formula(paste0("`", input$column, "` ~ `", input$factor_column, "`"))
    
    if (prep$levels_count < 2) {
      test_store$kruskal <- list(error = paste("The Kruskal-Wallis Test requires at least 2 distinct groups. Found:", prep$levels_count))
    } else {
      res <- kruskal.test(fmla, data = prep$df)
      test_store$kruskal <- list(res = res, factor = input$factor_column, dep = input$column)
    }
    df <- revert_group_data(prep$df, input$factor_column, prep$original_col)
  })
  
  output$kruskal_out <- renderPrint({
    if (is.null(test_store$kruskal)) cat("Waiting... Click 'Kruskal-Wallis' in the sidebar.")
    else if (!is.null(test_store$kruskal$error)) cat("ERROR:", test_store$kruskal$error)
    else print(test_store$kruskal$res)
  })
  
  output$kruskal_exp <- renderUI({
    res <- test_store$kruskal
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$res$p.value
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is < 0.05. There is a statistically significant difference in the distributions (or medians) of <b>", res$dep, "</b> between at least two groups of <b>", res$factor, "</b>.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is >= 0.05. There is no statistically significant difference in the distributions of <b>", res$dep, "</b> across the groups of <b>", res$factor, "</b>.</div>"))
    }
  })
  
  # Wilcoxon
  observeEvent(input$run_wilcox, {
    req(input$column, input$factor_column)
    df <- datasetInput()
    
    err_msg <- validate_factor(df, input$column, input$factor_column)
    if (!is.null(err_msg)) {
      showModal(modalDialog(
        title = "Invalid Grouping Factor",
        HTML(paste0("<div style='color: #e74c3c; font-size: 1.1em;'>", err_msg, "</div>")),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    prep <- prepare_group_data(df, input$factor_column)
    fmla <- as.formula(paste0("`", input$column, "` ~ `", input$factor_column, "`"))
    
    if (prep$levels_count != 2) {
      test_store$wilcox <- list(error = paste("The Wilcoxon Rank-Sum Test requires exactly 2 distinct groups. Found:", prep$levels_count))
    } else {
      res <- wilcox.test(fmla, data = prep$df)
      test_store$wilcox <- list(res = res, factor = input$factor_column, dep = input$column)
    }
    df <- revert_group_data(prep$df, input$factor_column, prep$original_col)
  })
  
  output$wilcox_out <- renderPrint({
    if (is.null(test_store$wilcox)) cat("Waiting... Click 'Wilcoxon' in the sidebar.")
    else if (!is.null(test_store$wilcox$error)) cat("ERROR:", test_store$wilcox$error)
    else print(test_store$wilcox$res)
  })
  
  output$wilcox_exp <- renderUI({
    res <- test_store$wilcox
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$res$p.value
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is < 0.05. The distributions (or medians) of the two groups are <u>significantly different</u> from each other.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is >= 0.05. The distributions of the two groups are <u>not significantly different</u>.</div>"))
    }
  })
  
  # --- Chi-Square Logic ---
  observeEvent(input$run_chisq, {
    req(input$chisq_var1, input$chisq_var2)
    df <- datasetInput()
    
    if (input$chisq_var1 == input$chisq_var2) {
      showModal(modalDialog(
        title = "Invalid Selection",
        HTML("<div style='color: #e74c3c; font-size: 1.1em;'>The two categorical variables cannot be the same. Please select two distinct columns.</div>"),
        easyClose = TRUE, footer = modalButton("Close")
      ))
      return()
    }
    
    v1 <- as.factor(df[[input$chisq_var1]])
    v2 <- as.factor(df[[input$chisq_var2]])
    
    if (length(levels(v1)) < 2 || length(levels(v2)) < 2) {
      test_store$chisq <- list(error = "Both variables must have at least 2 distinct levels to run a Chi-Square test.")
    } else {
      tbl <- table(v1, v2)
      res <- suppressWarnings(chisq.test(tbl)) 
      test_store$chisq <- list(res = res, tbl = tbl, v1_name = input$chisq_var1, v2_name = input$chisq_var2)
    }
  })
  
  output$chisq_out <- renderPrint({
    if (is.null(test_store$chisq)) cat("Waiting... Click 'Run Chi-Square' in the sidebar.")
    else if (!is.null(test_store$chisq$error)) cat("ERROR:", test_store$chisq$error)
    else print(test_store$chisq$res)
  })
  
  output$chisq_exp <- renderUI({
    res <- test_store$chisq
    if (is.null(res) || !is.null(res$error)) return(NULL)
    p_val <- res$res$p.value
    
    if (p_val < 0.05) {
      HTML(paste0("<div style='color: #e74c3c; padding: 10px; border-left: 4px solid #e74c3c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is < 0.05. There is a statistically significant association between <b>", res$v1_name, "</b> and <b>", res$v2_name, "</b>. The variables are likely dependent.</div>"))
    } else {
      HTML(paste0("<div style='color: #00bc8c; padding: 10px; border-left: 4px solid #00bc8c; background-color: #333; margin-top: 10px;'><b>Interpretation:</b> The p-value (<b>", format.pval(p_val, digits=4), "</b>) is >= 0.05. There is no statistically significant association between <b>", res$v1_name, "</b> and <b>", res$v2_name, "</b>. The variables appear independent.</div>"))
    }
  })
  
  output$chisq_plot <- renderPlotly({
    res <- test_store$chisq
    if (is.null(res) || !is.null(res$error)) {
      p <- plot_ly() %>% layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      return(apply_plotly_dark_theme(p))
    }
    
    df_tbl <- as.data.frame(res$tbl)
    colnames(df_tbl) <- c("Var1", "Var2", "Freq")
    
    p <- plot_ly(df_tbl, x = ~Var1, y = ~Freq, color = ~Var2, type = "bar") %>%
      layout(barmode = "stack",
             title = paste("Proportion Stack:", res$v1_name, "by", res$v2_name),
             xaxis = list(title = res$v1_name, gridcolor = "#444444"),
             yaxis = list(title = "Count", gridcolor = "#444444"),
             legend = list(title = list(text = res$v2_name)))
    
    apply_plotly_dark_theme(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)