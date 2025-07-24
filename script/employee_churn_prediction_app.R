# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(pROC)
library(rpart.plot)
library(ROCR)
library(psych)
library(gridExtra)
library(rlang)

# Load pre-trained models and prediction results
forward_model        <- readRDS("models/forward_model.rds")
forward_pred         <- readRDS("models/forward_pred.rds")
ridge_pred           <- readRDS("models/ridge_pred.rds")
lasso_pred           <- readRDS("models/lasso_pred.rds")
elastic_pred         <- readRDS("models/elastic_pred.rds")
opt_tree_model       <- readRDS("models/opt_tree_model.rds")
baseline_tree_model  <- readRDS("models/baseline_tree_model.rds")
baseline_tree_pred   <- readRDS("models/baseline_tree_pred.rds")
opt_tree_pred        <- readRDS("models/opt_tree_pred.rds")
test_data            <- readRDS("models/test_data.rds")

# Final processed dataset
data                 <- readRDS("data/data.rds")


# Define User Interface
ui <- navbarPage("Employee Churn - Predictive Modeling",

# Introduction tab
                 
               tabPanel("Introduction",
                          tags$head(
                          tags$style(HTML("
        body {
          background-color: #f7f7f7;
        }
        h2, h4 {
          font-weight: bold;
          margin-top: 20px;
          background-color: #2c3e50;
          color: white;
          padding: 15px;
          border-radius: 5px;
        }
        h2 {
          text-align: center;
        }
        p, ul {
          font-size: 16px;
          color: #2c3e50;
        }
        .well, .shadow-box {
          background-color: white;
          padding: 20px;
          border-radius: 10px;
          box-shadow: 0 0 10px #ccc;
          margin-top: 20px;
          border: 2px solid #2c3e50;
        }
        .navbar-nav > li.active > a {
          background-color: #4D4D5B !important;
          color: white !important;
        }
        .conf-box {
          background-color: #f9f9f9;
          border: 1.5px solid #2c3e50;
          border-radius: 10px;
          padding: 10px 15px;
          margin-bottom: 15px;
          box-shadow: 2px 2px 6px rgba(0,0,0,0.1);
        }
        .conf-title {
          font-weight: bold;
          text-align: center;
          background-color: #2c3e50;
          color: white;
          padding: 5px;
          border-radius: 6px;
          margin-bottom: 8px;
        }
        .conf-matrix {
          font-family: monospace;
          text-align: center;
          white-space: pre-wrap;
          margin-top: 5px;
        }
      "))
                          ),
                          fluidPage(
                            h2("Employee Churn Prediction"),
                            div(class = "well",
                                div(class = "well",
                                    p("This project presents an exploratory analysis of employee churn in a simulated tech company environment."),
                                    p("Using a dataset of over 20,000 synthetic records, I applied logistic regression, regularization techniques, and decision tree models to understand the main drivers behind employee resignation."),
                                    p("The goal was to evaluate model performance, interpret results, and summarize findings in a clear and visual way.")
                                )
                            ),
                            h4("What does this app include?"),
                            div(class = "shadow-box",
                                tags$ul(
                                  tags$li("Exploratory analysis of employee data with visual summaries"),
                                  tags$li("Application of logistic regression and decision tree models"),
                                  tags$li("Performance evaluation using ROC curves, AUC, and key metrics"),
                                  tags$li("Visualization of prediction results and classification outcomes"),
                                  tags$li("Interactive dashboard built with Shiny and ggplot2")
                                )
                            )
                          )
                 ),

# Descriptive analysis tab              
                 tabPanel("Descriptive Analysis",
                          fluidPage(
                            h2("Exploratory Data Analysis"),
                            
                            # 🔹 Distribución de variables numéricas
                            div(class = "shadow-box",
                                h4("Univariate distribution of numeric variable", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px;"),
                                selectInput("var_descriptiva", "Select a variable:",
                                            choices = names(select_if(test_data, is.numeric)),
                                            selected = names(select_if(test_data, is.numeric))[1]),
                                plotOutput("hist_descriptivo"),
                                h4("Summary", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px; margin-top:20px;"),
                                tableOutput("summary_stat")
                            ),
                            
                            # 🔹 Gráfico de churn general
                            div(class = "shadow-box",
                                h4("Overall employee churn distribution", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px;"),
                                plotOutput("churn_plot")
                            ),
                            
                            # 🔹 Gráfico de churn por categoría
                            div(class = "shadow-box",
                                h4("Employee churn by categorical variable", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px;"),
                                selectInput("var_cat", "Select a variable:", 
                                            choices = setdiff(names(Filter(function(x) is.factor(x) || is.character(x), data)), "left"),
                                            selected = setdiff(names(Filter(function(x) is.factor(x) || is.character(x), data)), "left")[1]),
                                plotOutput("churn_categoria_plot")
                            )
                          )
                 ),
                 
                          
# Model evaluation tab        
                 tabPanel("Model Evaluation",
                          fluidPage(
                            h2("Model Performance Comparison"),
                            div(class = "shadow-box",
                                fluidRow(
                                  column(6, plotOutput("roc_plot")),
                                  column(6,
                                         h4("Confusion Matrices", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px;"),
                                         fluidRow(
                                           column(6, uiOutput("conf_matrix_forward")),
                                           column(6, uiOutput("conf_matrix_ridge"))
                                         ),
                                         fluidRow(
                                           column(6, uiOutput("conf_matrix_lasso")),
                                           column(6, uiOutput("conf_matrix_elastic"))
                                         ),
                                         fluidRow(
                                           column(6, uiOutput("conf_matrix_arbol")),
                                           column(6, uiOutput("conf_matrix_arbol_modif"))
                                         )
                                  )
                                ),
                                h4("Metrics Summary", style = "background-color:#2c3e50; color:white; padding:8px; border-radius:5px; margin-top:20px;"),
                                tableOutput("metrics_table")
                            )
                          )
                 )
)

server <- function(input, output) {
  
  output$hist_descriptivo <- renderPlot({
    var <- test_data[[input$var_descriptiva]]
    if (length(unique(var)) <= 10) {
      ggplot(test_data, aes(x = factor(var))) +
        geom_bar(fill = "#180662") +
        theme_minimal() +
        labs(title = paste("Distribution of:", input$var_descriptiva), x = input$var_descriptiva, y = "Count") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#2c3e50"),
              plot.background = element_rect(fill = "white", colour = "#2c3e50", size = 1))
    } else {
      ggplot(test_data, aes(x = var)) +
        geom_histogram(fill = "#180662", bins = 30, color = "white") +
        theme_minimal() +
        labs(title = paste("Distribution of:", input$var_descriptiva), x = input$var_descriptiva, y = "Frequency") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "#2c3e50"),
              plot.background = element_rect(fill = "white", colour = "#2c3e50", size = 1))
    }
  })
  
  output$summary_stat <- renderTable({
    var <- test_data[[input$var_descriptiva]]
    if (length(unique(var)) <= 10) {
      freqs <- table(var, useNA = "ifany")
      props <- round(100 * prop.table(freqs), 2)
      df <- data.frame(Category = names(freqs), Frequency = as.vector(freqs), Proportion = paste0(props, "%"))
      colnames(df) <- c("Category", "Freq", "Prop (%)")
      df
    } else {
      stats <- summary(var)
      sd_val <- round(sd(var, na.rm = TRUE), 2)
      df <- data.frame(Statistic = names(stats), Value = as.numeric(stats), row.names = NULL)
      df <- rbind(df, data.frame(Statistic = "Standard deviation", Value = sd_val))
      df
    }
  }, striped = TRUE, bordered = TRUE, spacing = "s", digits = 6)
  
  output$coef_forward <- renderTable({
    coefs <- coef(forward_model)
    coef_df <- data.frame(Variable = names(coefs), Coefficient = round(as.numeric(coefs), 4))
    coef_df <- coef_df[order(-abs(coef_df$Coefficient)), ]
    coef_df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$imp_arbol <- renderTable({
    imp <- baseline_tree_model_modif$variable.importance
    imp_df <- data.frame(Variable = names(imp), Importance = round(as.numeric(imp), 4))
    imp_df <- imp_df[order(-imp_df$Importance), ]
    imp_df
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  output$roc_plot <- renderPlot({
    real <- test_data$left
    plot(roc(real, forward_pred), col = "#429ed6", main = "ROC Curve", lwd = 2)
    lines(roc(real, ridge_pred), col = "#5f58ee", lwd = 2)
    lines(roc(real, lasso_pred), col = "#251eb6", lwd = 2)
    lines(roc(real, elastic_pred), col = "#646199", lwd = 2)
    lines.roc(real, baseline_tree_pred, col = "#c1bfec", lwd = 2)
    lines.roc(real, opt_tree_pred, col = "#aeadc1", lwd = 2)
    legend("bottomright", legend = c("Forward", "Ridge", "Lasso", "Elastic Net", "Árbol", "Árbol Optimizado"),
           col = c("#429ed6", "#5f58ee", "#251eb6", "#646199", "#c1bfec", "#aeadc1"), lwd = 2, cex = 0.8)
  })
  
  render_conf_matrix <- function(pred, label) {
    pred_class <- ifelse(pred > 0.5, 1, 0)
    mat <- table(Predicted = pred_class, Actual = test_data$left)
    TP <- mat["1", "1"]
    FP <- mat["1", "0"]
    FN <- mat["0", "1"]
    TN <- mat["0", "0"]
    
    HTML(paste0('
    <div class="conf-box">
      <div class="conf-title">', label, '</div>
      <table style="margin:auto; border-collapse: collapse; font-family: monospace; width: 100%; text-align: center; table-layout: fixed;">
        <thead>
          <tr>
            <th></th>
            <th><b>Actual 1</b></th>
            <th><b>Actual 0</b></th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <th><b>Pred 1</b></th>
            <td>', TP, '</td>
            <td>', FP, '</td>
          </tr>
          <tr>
            <th><b>Pred 0</b></th>
            <td>', FN, '</td>
            <td>', TN, '</td>
          </tr>
        </tbody>
      </table>
    </div>
  '))
  }
  
  output$conf_matrix_forward <- renderUI({ render_conf_matrix(forward_pred, "Logistic Regression (Forward Stepwise)") })
  output$conf_matrix_ridge <- renderUI({ render_conf_matrix(ridge_pred, "Logistic Regression + Ridge") })
  output$conf_matrix_lasso <- renderUI({ render_conf_matrix(lasso_pred, "Logistic Regression + Lasso") })
  output$conf_matrix_elastic <- renderUI({ render_conf_matrix(elastic_pred, "Logistic Regression + Elastic Net") })
  output$conf_matrix_arbol <- renderUI({ render_conf_matrix(baseline_tree_pred, "Baseline Decision Tree") })
  output$conf_matrix_arbol_modif <- renderUI({ render_conf_matrix(opt_tree_pred, "Optimized Tree Model") })
  output$churn_plot <- renderPlot({
    # Asegurar que left esté en formato factor con niveles correctos
    data$left <- as.character(data$left)
    data <- data %>% filter(left %in% c("0", "1", 0, 1, "yes", "no", "Yes", "No", 1L, 0L))  
    # Reasignar valores claros
    data$Categoria <- ifelse(data$left %in% c(1, "1", "yes", "Yes"), "yes", "no")
    data$Categoria <- factor(data$Categoria, levels = c("no", "yes"))
    
    churn_summary <- data %>%
      count(Categoria) %>%
      mutate(percentage = round(n / sum(n) * 100, 2))
    
    colores <- c("no" = "lightblue", "yes" = "darkblue")
    
    p1 <- ggplot(churn_summary, aes(x = Categoria, y = n, fill = Categoria)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold", size = 5, color = "black") +
      scale_fill_manual(values = colores) +
      labs(title = "Amount", x = "left", y = "Count", fill = "Category") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    p2 <- ggplot(churn_summary, aes(x = Categoria, y = percentage, fill = Categoria)) +
      geom_bar(stat = "identity", width = 0.6) +
      geom_text(aes(label = paste0(percentage, "%")), vjust = -0.5, fontface = "bold", size = 5, color = "black") +
      scale_fill_manual(values = colores) +
      labs(title = "Percentage", x = "left", y = "Percentage", fill = "Category") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5))
    
    gridExtra::grid.arrange(p1, p2, ncol = 2)
  })
  
  output$churn_categoria_plot <- renderPlot({
    req(input$var_cat)
    var <- input$var_cat
    df <- data
    df$left <- as.character(df$left)
    df <- df %>% filter(left %in% c("0", "1", 0, 1, "yes", "no", "Yes", "No", 1L, 0L))  
    df$Categoria <- ifelse(df$left %in% c(1, "1", "yes", "Yes"), "yes", "no")
    df$Categoria <- factor(df$Categoria, levels = c("no", "yes"))
    df[[var]] <- as.factor(df[[var]])
    
    table <- df %>%
      group_by(.data[[var]], Categoria) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[var]]) %>%
      mutate(pct = round(n / sum(n) * 100, 1))
    
    ggplot(table, aes(x = .data[[var]], y = pct, fill = Categoria)) +
      labs(fill = "Category") +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = paste0(pct, "%")), position = position_dodge(0.9), vjust = -0.3, size = 4) +
      scale_fill_manual(values = c("no" = "lightblue", "yes" = "darkblue")) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Table: compute and summarize performance metrics for all models
  output$metrics_table <- renderTable({
    calc_metrics <- function(real, pred_prob) {
      pred_class <- ifelse(pred_prob > 0.5, 1, 0)
      acc <- mean(pred_class == real)
      spec <- sum(pred_class == 1 & real == 1) / sum(real == 1)
      sens <- sum(pred_class == 0 & real == 0) / sum(real == 0)
      tab <- table(factor(pred_class, levels = c(0,1)), factor(real, levels = c(0,1)))
      kappa_val <- psych::cohen.kappa(tab)$kappa
      auc_val <- pROC::auc(pROC::roc(real, pred_prob))
      pred_obj <- ROCR::prediction(pred_prob, real)
      perf <- ROCR::performance(pred_obj, "tpr", "fpr")
      ks_val <- max(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]]) * 100
      data.frame(
        Accuracy    = formatC(acc, format = "f", digits = 6),
        Sensitivity = formatC(sens, format = "f", digits = 6),
        Specificity = formatC(spec, format = "f", digits = 6),
        Kappa       = formatC(kappa_val, format = "f", digits = 6),
        AUC         = formatC(auc_val, format = "f", digits = 6),
        KS          = formatC(ks_val, format = "f", digits = 6)
      )
    }
    
    real <- test_data$left
    rbind(
      "Logistic Regression (Forward Stepwise)" = calc_metrics(real, forward_pred),
      "Logistic Regression + Ridge"            = calc_metrics(real, ridge_pred),
      "Logistic Regression + Lasso"            = calc_metrics(real, lasso_pred),
      "Logistic Regression + Elastic Net"      = calc_metrics(real, elastic_pred),
      "Baseline Decision Tree"                 = calc_metrics(real, baseline_tree_pred),
      "Optimized Tree Model"                   = calc_metrics(real, opt_tree_pred)
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = TRUE)
}

# Launch the application
shinyApp(ui, server)



