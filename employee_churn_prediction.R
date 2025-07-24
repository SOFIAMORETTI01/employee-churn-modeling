# Load required libraries
library(readxl)
library(moments)
library(corrplot)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(caret)
library(MASS)
library(car)
library(polycor)
library(DescTools)
library(reshape2)
library(pROC)
library(glmnet)
library(knitr)
library(tidyr)
library(randomForest)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(MLmetrics)
library(writexl)
library(lmtest)
library(nortest)
library(rattle)
library(RColorBrewer)

# Set working directory
setwd("C:/Users/SMoretti/Downloads/Portfolio/05. Churn")

# Load dataset
data <- read_excel("employee_churn_database.xlsx", sheet = "Sheet1")

head(data)
summary(data)
dim(data)


###########################################################################################################
##############################################    DATA TREATMENT    #######################################
###########################################################################################################

#######################################  Numeric Variable Processing  #####################################

numeric_columns <- data[, sapply(data, is.numeric), drop = FALSE]

# Function to generate descriptive statistics table
create_descriptive_table <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data.frame.")
  data_numeric <- data[, sapply(data, is.numeric), drop = FALSE]
  
  is_binary <- function(x) {
    unique_values <- unique(x[!is.na(x)]) 
    length(unique_values) == 2 && all(unique_values %in% c(0, 1)) 
  }
  binaries <- sapply(data_numeric, is_binary)
  
  descriptive_table <- data.frame(
    Field = colnames(data_numeric),
    Type = ifelse(binaries, "Binary", "Range"),
    Min = round(sapply(data_numeric, min, na.rm = TRUE), 1),
    Max = round(sapply(data_numeric, max, na.rm = TRUE), 1),
    Mean = round(sapply(data_numeric, mean, na.rm = TRUE), 1),
    SD = round(sapply(data_numeric, sd, na.rm = TRUE), 1),
    Median = round(sapply(data_numeric, median, na.rm = TRUE), 1),
    Outliers = sapply(seq_along(data_numeric), function(i) {
      col_name <- colnames(data_numeric)[i]
      if (binaries[i]) {
        sum(!data_numeric[[i]] %in% c(0, 1), na.rm = TRUE)
      } else if (col_name == "age") {
        sum(data_numeric[[i]] < 18 | data_numeric[[i]] > 65, na.rm = TRUE)
      } else if (col_name == "review") {
        sum(data_numeric[[i]] < 0 | data_numeric[[i]] > 1, na.rm = TRUE)
      } else if (col_name == "tenure") {
        sum(data_numeric[[i]] < 0 | data_numeric[[i]] > 40, na.rm = TRUE)
      } else if (col_name == "satisfaction") {
        sum(data_numeric[[i]] < 0 | data_numeric[[i]] > 1, na.rm = TRUE)
      } else if (col_name == "avg_hrs_month") {
        sum(data_numeric[[i]] < 80 | data_numeric[[i]] > 200, na.rm = TRUE)
      } else if (col_name == "workloadscore") {
        sum(data_numeric[[i]] < 0 | data_numeric[[i]] > 1, na.rm = TRUE)
      } else {
        iqr <- IQR(data_numeric[[i]], na.rm = TRUE)
        lower <- quantile(data_numeric[[i]], 0.25, na.rm = TRUE) - 1.5 * iqr
        upper <- quantile(data_numeric[[i]], 0.75, na.rm = TRUE) + 1.5 * iqr
        sum(data_numeric[[i]] < lower | data_numeric[[i]] > upper, na.rm = TRUE)
      }
    }),
    Extremes = sapply(data_numeric, function(x) {
      sum(is.na(x) | x == Inf | x == -Inf)
    }),
    Valid = sapply(data_numeric, function(x) sum(!is.na(x)))
  )
  
  return(descriptive_table)
}
descriptive_table <- create_descriptive_table(data)
write_xlsx(descriptive_table, "descriptive_table.xlsx")

# Boxplot for each numeric variable

numeric_columns2 <- setdiff(names(data)[sapply(data, is.numeric)], c("ID", "promoted", "bonus", "work_life_balance"))

par(mfrow = c(3, 3)) 

for (col in numeric_columns2) {
  col_data <- data[[col]]
  boxplot(
    col_data,
    main = paste("Boxplot of", col), 
    ylab = col, 
    col = "lightblue", 
    border = "darkblue" 
  )
}

# Outlier treatment

outliers_review <- data$review[data$review < 0 | data$review > 1]
outliers_review

data$review[data$review > 1] <- 1
data$review[data$review < 0] <- 0

outliers_satisfaction <- data$satisfaction[data$satisfaction < 0 | data$satisfaction > 1]
outliers_satisfaction

data$satisfaction[data$satisfaction > 1] <- 1
data$satisfaction[data$satisfaction < 0] <- 0

outliers_wls <- data$workloadscore[data$workloadscore < 0 | data$workloadscore > 1]
outliers_wls

data$workloadscore[data$workloadscore > 1] <- 1
data$workloadscore[data$workloadscore < 0] <- 0

age_review <- data$age[data$age < 18 | data$age > 65]
data <- subset(data, age >= 18 & age <= 65)

descriptive_table <- create_descriptive_table(data)

# Missing value analysis

missing_analysis <- numeric_columns %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  arrange(desc(Missing_Count))

# Plot of missing values
ggplot(missing_analysis, aes(x = Missing_Count, y = reorder(Variable, Missing_Count))) +
  geom_point(size = 3) +  
  geom_segment(aes(x = 0, xend = Missing_Count, y = Variable, yend = Variable), color = "darkblue") +
  labs(title = "MISSING VALUES - Numeric Variables",
       x = "Number of missing values",
       y = "Variable") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(size = 16, face = "bold", hjust = 0, color = "darkblue"))

missing_analysis2 <- numeric_columns %>%
  summarise(across(everything(), ~sum(is.na(.)) / n() * 100)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Percentage") %>%
  arrange(desc(Missing_Percentage))

print(missing_analysis2)

# Impute missing values
data$commute_time[is.na(data$commute_time)] <- median(data$commute_time, na.rm = TRUE)
data$workloadscore[is.na(data$workloadscore)] <- median(data$workloadscore, na.rm = TRUE)
data$satisfaction[is.na(data$satisfaction)] <- median(data$satisfaction, na.rm = TRUE)
data$projects[is.na(data$projects)] <- median(data$projects, na.rm = TRUE)
data$age[is.na(data$age)] <- mean(data$age, na.rm = TRUE)

sum(is.na(data)) # Check for remaining missing values



######################################  Categorical Variable Processing  ######################################

# Missing value analysis

categorical_variables <- data[, sapply(data, function(x) is.factor(x) || is.character(x)), drop = FALSE]

missing_analysis_cat <- categorical_variables %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  arrange(desc(Missing_Count))

print(missing_analysis_cat)

# Plot of missing values
ggplot(missing_analysis_cat, aes(x = Missing_Count, y = reorder(Variable, Missing_Count))) +
  geom_point(size = 3, color = "darkblue") +
  geom_segment(aes(x = 0, xend = Missing_Count, y = Variable, yend = Variable), color = "darkblue") +
  theme_minimal() +
  labs(
    title = "MISSING VALUES - Categorical Variables",
    x = "Number of Missing Values",
    y = "Variables"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0, color = "darkblue"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    axis.text.y = element_text(size = 12)
  )
########################################  Correlation Analysis  ###################################################

data_clean <- data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.integer), as.numeric))

categorical_vars <- names(data_clean)[sapply(data_clean, is.factor)]
continuous_vars <- setdiff(names(data_clean)[sapply(data_clean, is.numeric)], "ID")

# Assumption checks for Pearson correlation between continuous variables
for (i in 1:length(continuous_vars)) {
  for (j in (i+1):length(continuous_vars)) {
    var1 <- continuous_vars[i]
    var2 <- continuous_vars[j]
    
    cat("\n### Assumption Check:", var1, "vs", var2, "###\n")
    
    # 1. Linearity: scatter plot with regression line
    print(ggplot(data, aes_string(x = var1, y = var2)) +
            geom_point(color = "blue") +
            geom_smooth(method = "lm", col = "red") +
            ggtitle(paste("Scatterplot:", var1, "vs", var2)) +
            theme_minimal())
    
    # 2. Normality test (Kolmogorov-Smirnov & Anderson-Darling if n > 5000, otherwise Shapiro-Wilk)
    if (length(data[[var1]]) > 5000) {
      ks_test_var1 <- ks.test(data[[var1]], "pnorm", mean(data[[var1]], na.rm = TRUE), sd(data[[var1]], na.rm = TRUE))
      ad_test_var1 <- ad.test(data[[var1]])
      cat("Kolmogorov-Smirnov for", var1, "- p-value:", ks_test_var1$p.value, "\n")
      cat("Anderson-Darling for", var1, "- p-value:", ad_test_var1$p.value, "\n")
    } else {
      shapiro_var1 <- shapiro.test(data[[var1]])
      cat("Shapiro-Wilk for", var1, "- p-value:", shapiro_var1$p.value, "\n")
    }
    
    if (length(data[[var2]]) > 5000) {
      ks_test_var2 <- ks.test(data[[var2]], "pnorm", mean(data[[var2]], na.rm = TRUE), sd(data[[var2]], na.rm = TRUE))
      ad_test_var2 <- ad.test(data[[var2]])
      cat("Kolmogorov-Smirnov for", var2, "- p-value:", ks_test_var2$p.value, "\n")
      cat("Anderson-Darling for", var2, "- p-value:", ad_test_var2$p.value, "\n")
    } else {
      shapiro_var2 <- shapiro.test(data[[var2]])
      cat("Shapiro-Wilk for", var2, "- p-value:", shapiro_var2$p.value, "\n")
    }
    
    # 3. Outlier detection using boxplots
    print(ggplot(data, aes_string(y = var1)) + 
            geom_boxplot(fill = "lightblue", outlier.color = "red") + 
            ggtitle(paste("Boxplot:", var1)))
    
    print(ggplot(data, aes_string(y = var2)) + 
            geom_boxplot(fill = "lightblue", outlier.color = "red") + 
            ggtitle(paste("Boxplot:", var2)))
    
    # 4. Homoscedasticity test (Breusch-Pagan)
    model <- lm(as.formula(paste(var2, "~", var1)), data = data)
    bp_test <- bptest(model)
    cat("Breusch-Pagan Test - p-value:", bp_test$p.value, "\n")
    
    # Decision: Pearson vs Spearman
    normal1 <- ifelse(length(data[[var1]]) > 5000, ks_test_var1$p.value >= 0.05, shapiro_var1$p.value >= 0.05)
    normal2 <- ifelse(length(data[[var2]]) > 5000, ks_test_var2$p.value >= 0.05, shapiro_var2$p.value >= 0.05)
    
    if (!normal1 | !normal2) {
      cat("Spearman is recommended due to lack of normality.\n")
    } else {
      cat("If linearity and homoscedasticity hold, Pearson is appropriate.\n")
    }
    
    readline(prompt = "Press Enter to continue to next pair...")
  }
}

# Pearson not applicable → use Spearman

correlation_results <- list()

# Continuous vs continuous (Spearman)
for (i in 1:length(continuous_vars)) {
  for (j in i:length(continuous_vars)) {
    cont_data1 <- data_clean[[continuous_vars[i]]]
    cont_data2 <- data_clean[[continuous_vars[j]]]
    cor_value <- cor(cont_data1, cont_data2, method = "spearman", use = "complete.obs")
    correlation_results[[paste(continuous_vars[i], continuous_vars[j], sep = "_vs_")]] <- cor_value
  }
}

# Categorical vs categorical (Cramér's V)
for (i in 1:length(categorical_vars)) {
  for (j in i:length(categorical_vars)) {
    cat_data1 <- data_clean[[categorical_vars[i]]]
    cat_data2 <- data_clean[[categorical_vars[j]]]
    cramers_v_value <- CramerV(table(cat_data1, cat_data2))
    correlation_results[[paste(categorical_vars[i], categorical_vars[j], sep = "_vs_")]] <- cramers_v_value
  }
}

# Categorical vs continuous (Spearman)
for (cat_var in categorical_vars) {
  for (cont_var in continuous_vars) {
    cat_data <- as.numeric(as.factor(data_clean[[cat_var]]))
    cont_data <- data_clean[[cont_var]]
    cor_value <- cor(cont_data, cat_data, method = "spearman")
    correlation_results[[paste(cat_var, cont_var, sep = "_vs_")]] <- cor_value
  }
}

correlation_df <- data.frame(variable_pair = names(correlation_results),
                             correlation = unlist(correlation_results))

print(correlation_df)

# Correlation matrix
correlation_matrix <- reshape2::dcast(correlation_df, variable_pair ~ correlation, value.var = "correlation")

correlation_long <- correlation_df %>%
  separate(variable_pair, into = c("Var1", "Var2"), sep = "_vs_") %>%
  mutate(correlation = round(correlation, 2))

correlation_long <- complete(correlation_long, Var1, Var2, fill = list(correlation = 0))

ggplot(correlation_long, aes(x = Var1, y = Var2, fill = correlation)) +
  geom_tile(color = "light blue", size = 0.6) +  
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1), name = "Correlation") +
  geom_text(aes(label = ifelse(abs(correlation) < 0.001, "0", round(correlation, 2))), 
            color = "black", size = 3.5, fontface = "bold") +  
  theme_minimal() +
  labs(title = "Correlation Matrix",
       x = "Variable 1",
       y = "Variable 2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "right",
        legend.key.height = unit(1, "cm"))

###########################################################################################################
##########################################    DESCRIPTIVE ANALYSIS    #####################################
###########################################################################################################

# Charts based on the target variable (LEFT)
plot_left <- function(data, target_var = "left") {
  table_data <- as.data.frame(table(data[[target_var]]))
  colnames(table_data) <- c("Category", "Frequency")
  table_data$Percentage <- (table_data$Frequency / sum(table_data$Frequency)) * 100
  
  p1 <- ggplot(table_data, aes(x = Category, y = Frequency, fill = Category)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    geom_text(aes(label = Frequency), vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("lightblue", "darkblue")) +
    labs(title = "Employee Churn (Count)", x = target_var, y = "Count") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  p2 <- ggplot(table_data, aes(x = Category, y = Percentage, fill = Category)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    geom_text(aes(label = sprintf("%.2f%%", Percentage)), vjust = -0.5, size = 5) +
    scale_fill_manual(values = c("lightblue", "darkblue")) +
    labs(title = "Employee Churn (%)", x = target_var, y = "Percentage") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

plot_left(data, target_var = "left")

# Bar plots for categorical variables
plot_categorical_frequencies <- function(data) {
  categorical_columns <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
  num_vars <- length(categorical_columns)
  rows <- ceiling(sqrt(num_vars))
  cols <- ceiling(num_vars / rows)
  par(mfrow = c(rows, cols), mar = c(4, 4, 4, 2))  
  
  for (col in categorical_columns) {
    tab <- table(data[[col]], useNA = "ifany")
    colors <- colorRampPalette(c("lightblue", "blue"))(length(tab))
    barplot(tab,
            main = paste("Frequency of", col),
            xlab = col,
            ylab = "Frequency",
            col = colors,
            las = 2)
  }
}
plot_categorical_frequencies(data)
# Bar plots of categorical variables by target (LEFT)

plot_by_left <- function(data, target_var = "left") {
  categorical_columns <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
  categorical_columns <- setdiff(categorical_columns, target_var)
  
  plot_list <- list()
  
  for (col in categorical_columns) {
    tab <- as.data.frame(table(data[[col]], data[[target_var]]))
    colnames(tab) <- c("Category", "Target", "Frequency")
    
    tab <- tab %>%
      group_by(Category) %>%
      mutate(Percentage = Frequency / sum(Frequency) * 100)
    
    p <- ggplot(tab, aes(x = Category, y = Percentage, fill = Target)) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = sprintf("%.1f%%", Percentage)),
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
      scale_fill_manual(values = c("darkblue", "lightblue")) +
      labs(title = paste("Distribution of", col, "by", target_var),
           x = col,
           y = "Percentage",
           fill = target_var) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom",
            plot.title = element_text(hjust = 0.5))
    
    plot_list[[col]] <- p
  }
  
  grid.arrange(grobs = plot_list, ncol = 3)
}

plot_by_left(data, target_var = "left")

# Density plots for numeric variables by target (LEFT)

plot_numerics_by_left <- function(data, target_var = "left") {
  numeric_columns <- setdiff(names(data)[sapply(data, is.numeric)], c("ID", "promoted", "bonus", "work_life_balance"))
  
  plot_list <- list()
  
  for (col in numeric_columns) {
    p2 <- ggplot(data, aes_string(x = col, fill = target_var)) +
      geom_density(alpha = 0.5, adjust = 1.2) +
      scale_fill_manual(values = c("darkblue", "lightblue")) +
      labs(title = paste("Density of", col, "by", target_var),
           x = col, y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    plot_list <- append(plot_list, list(p2))
  }
  if (length(plot_list) > 0) {
    grid.arrange(grobs = plot_list, ncol = 2)
  } else {
    print("There are no numeric variables in the dataset.")
  }
}

plot_numerics_by_left(data, target_var = "left")


###########################################################################################################
######################################    PREDICTIVE MODELING SECTION    ###################################
###########################################################################################################

# Need to balance the dataset?
prop.table(table(data$left)) * 100

# The target variable 'left' shows a distribution of 58.8% for the 'no' class and 41.2% for the 'yes' class,
# which indicates a relatively balanced dataset. Therefore, no resampling techniques such as oversampling,
# undersampling, or SMOTE were necessary.

#### FORWARD REGRESSION 

set.seed(123)

# Split into train (70%) and test (30%)
indices <- createDataPartition(data$left, p = 0.7, list = FALSE)

data$age_group <- cut(data$age,
                      breaks = c(18, 30, 40, 60, Inf),
                      labels = c("18-30", "31-40", "41-50", "60+"),
                      include.lowest = TRUE)

data$age_group <- factor(data$age_group, levels = c("18-30", "31-40", "41-50", "60+"))

train_data <- data[indices, ]
test_data <- data[-indices, ]

train_data <- train_data[, !names(train_data) %in% c("age", "ID")]
test_data <- test_data[, !names(test_data) %in% c("age", "ID")]

# Convert target variable to binary
train_data$left <- ifelse(train_data$left == "yes", 1, 0)
test_data$left <- ifelse(test_data$left == "yes", 1, 0)

train_data$left <- factor(train_data$left, levels = c(0, 1))
test_data$left <- factor(test_data$left, levels = c(0, 1))

# Fit basic forward regression
empty_model <- glm(left ~ 1, family = binomial(link = 'logit'), data = train_data)
full_model <- glm(left ~ ., family = binomial(link = 'logit'), data = train_data)

forward_model <- step(empty_model, 
                      scope = list(lower = empty_model, upper = full_model), 
                      direction = "forward")

options(scipen = 999)  
summary(forward_model)  
anova(forward_model, test = "Chisq")

# Predictions
predictions <- predict(forward_model, newdata = test_data, type = "response")
predicted_class <- ifelse(predictions > 0.5, 1, 0)
predicted_class <- factor(predicted_class, levels = c(0, 1))

# Metrics on train and test
train_scores <- predict(forward_model, newdata = train_data, type = "response")
test_scores <- predict(forward_model, newdata = test_data, type = "response")

train_labels <- factor(train_data$left, levels = c(0, 1))
test_labels <- factor(test_data$left, levels = c(0, 1))

ks_stat <- function(scores, labels) {
  pred <- prediction(scores, labels)
  perf <- performance(pred, "tpr", "fpr")
  ks <- max(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]])
  return(ks)
}

ks_train <- ks_stat(train_scores, train_labels)
ks_test <- ks_stat(test_scores, test_labels)

print(paste("KS Train:", round(ks_train * 100, 2), "%"))
print(paste("KS Test:", round(ks_test * 100, 2), "%"))

# Confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_class), as.factor(test_data$left))
print(conf_matrix)

conf_table <- conf_matrix$table
total_rows <- rowSums(conf_table)
total_cols <- colSums(conf_table)
total_total <- sum(conf_table)
conf_percent <- round(conf_table / total_total * 100, 1)

result_table <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(total_cols[2] / total_total * 100, 1), "%"),
             paste0(round(total_cols[1] / total_total * 100, 1), "%"),
             "100%")
)

print(result_table, row.names = FALSE)

# Accuracy
accuracy <- mean(predicted_class == test_data$left)
print(paste("Model Accuracy - Test:", round(accuracy * 100, 2), "%"))

# ROC Curve
par(mfrow = c(1, 1)) 
roc_curve <- roc(test_data$left, predictions)
plot(roc_curve, col = "blue", main = "ROC Curve - Test")
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))

# Precision, Recall, Specificity
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
specificity <- conf_matrix$byClass["Specificity"]

print(paste("Precision:", round(precision, 4)))
print(paste("Recall:", round(recall, 4)))
print(paste("Specificity:", round(specificity, 4)))

# Multicollinearity
vif(forward_model)

# Variable importance
importance <- varImp(forward_model, scale = FALSE)
print(importance)

# Kappa
kappa <- conf_matrix$overall["Kappa"]
print(paste("Kappa:", round(kappa, 4)))

# Predictions on training set
train_preds <- predict(forward_model, newdata = train_data, type = "response")
predicted_train_class <- ifelse(train_preds > 0.5, 1, 0)
predicted_train_class <- factor(predicted_train_class, levels = c(0, 1))

# Confusion matrix - Train
conf_matrix_train <- confusionMatrix(as.factor(predicted_train_class), as.factor(train_data$left))
conf_table <- conf_matrix_train$table
total_rows <- rowSums(conf_table)
total_cols <- colSums(conf_table)
total_total <- sum(conf_table)
conf_percent <- round(conf_table / total_total * 100, 1)

result_table <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(total_cols[2] / total_total * 100, 1), "%"),
             paste0(round(total_cols[1] / total_total * 100, 1), "%"),
             "100%")
)

print(result_table, row.names = FALSE)

# Accuracy - Train
accuracy_train <- mean(predicted_train_class == train_data$left)
print(paste("Model Accuracy - Train:", round(accuracy_train * 100, 2), "%"))

# KS - Train
ks_train <- ks_stat(train_preds, train_data$left)
print(paste("KS Train:", round(ks_train * 100, 2), "%"))

# AUC - Train
roc_curve_train <- roc(train_data$left, train_preds)
plot(roc_curve_train, col = "blue", main = "ROC Curve - Train")
auc_train <- auc(roc_curve_train)
print(paste("AUC - Train:", round(auc_train, 4)))

# Precision, Recall, Specificity - Train
precision_train <- conf_matrix_train$byClass["Precision"]
recall_train <- conf_matrix_train$byClass["Recall"]
specificity_train <- conf_matrix_train$byClass["Specificity"]

print(paste("Precision - Train:", round(precision_train, 4)))
print(paste("Recall - Train:", round(recall_train, 4)))
print(paste("Specificity - Train:", round(specificity_train, 4)))

# RMSE, MAE, R² - Train
train_left_numeric <- as.numeric(as.character(train_data$left))

rmse_train <- sqrt(mean((train_preds - train_left_numeric)^2, na.rm = TRUE))
mae_train <- mean(abs(train_preds - train_left_numeric))
r2_train <- 1 - (sum((train_left_numeric - train_preds)^2) / 
                   sum((train_left_numeric - mean(train_left_numeric))^2))

print(paste("RMSE - Train:", round(rmse_train, 4)))
print(paste("MAE - Train:", round(mae_train, 4)))
print(paste("R² - Train:", round(r2_train, 4)))

# Kappa - Train
kappa_train <- conf_matrix_train$overall["Kappa"]
print(paste("Kappa - Train:", round(kappa_train, 4)))



#### REGULARIZATION

X_train <- model.matrix(left ~ ., data = train_data)[, -1]  
y_train <- as.numeric(as.character(train_data$left))

X_test <- model.matrix(left ~ ., data = test_data)[, -1]
y_test <- as.numeric(as.character(test_data$left))
y_test <- factor(y_test, levels = c(0, 1))

# Ridge
ridge_model <- glmnet(X_train, y_train, family = "binomial", alpha = 0)
cv_ridge <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0)
best_lambda_ridge <- cv_ridge$lambda.min
ridge_final <- glmnet(X_train, y_train, family = "binomial", alpha = 0, lambda = best_lambda_ridge)
summary(ridge_final)

# LASSO
lasso_model <- glmnet(X_train, y_train, family = "binomial", alpha = 1)
cv_lasso <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 1)
best_lambda_lasso <- cv_lasso$lambda.min
lasso_final <- glmnet(X_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda_lasso)

# Elastic Net (alpha = 0.5)
elastic_model <- glmnet(X_train, y_train, family = "binomial", alpha = 0.5)
cv_elastic <- cv.glmnet(X_train, y_train, family = "binomial", alpha = 0.5)
best_lambda_elastic <- cv_elastic$lambda.min
elastic_final <- glmnet(X_train, y_train, family = "binomial", alpha = 0.5, lambda = best_lambda_elastic)

# Confusion matrices
ridge_pred <- predict(ridge_final, newx = X_test, type = "response")
ridge_pred_class <- ifelse(ridge_pred > 0.5, 1, 0)
ridge_pred_class <- factor(ridge_pred_class, levels = c(0, 1))
conf_matrix_ridge <- confusionMatrix(ridge_pred_class, y_test)

lasso_pred <- predict(lasso_final, newx = X_test, type = "response")
lasso_pred_class <- ifelse(lasso_pred > 0.5, 1, 0)
lasso_pred_class <- factor(lasso_pred_class, levels = c(0, 1))
conf_matrix_lasso <- confusionMatrix(lasso_pred_class, y_test)

elastic_pred <- predict(elastic_final, newx = X_test, type = "response")
elastic_pred_class <- ifelse(elastic_pred > 0.5, 1, 0)
elastic_pred_class <- factor(elastic_pred_class, levels = c(0, 1))
conf_matrix_elastic <- confusionMatrix(elastic_pred_class, y_test)

# Metric calculation function
calculate_metrics <- function(model, X_train, y_train, X_test, y_test) {
  train_pred <- predict(model, newx = X_train, type = "response")
  test_pred <- predict(model, newx = X_test, type = "response")
  
  train_pred_class <- ifelse(train_pred > 0.5, 1, 0)
  test_pred_class <- ifelse(test_pred > 0.5, 1, 0)
  
  conf_train <- confusionMatrix(factor(train_pred_class, levels = c(0, 1)), factor(y_train, levels = c(0, 1)))
  conf_test <- confusionMatrix(factor(test_pred_class, levels = c(0, 1)), factor(y_test, levels = c(0, 1)))
  
  accuracy_train <- conf_train$overall["Accuracy"]
  kappa_train <- conf_train$overall["Kappa"]
  accuracy_test <- conf_test$overall["Accuracy"]
  kappa_test <- conf_test$overall["Kappa"]
  
  sensitivity_train <- conf_train$byClass["Sensitivity"]
  specificity_train <- conf_train$byClass["Specificity"]
  precision_train <- conf_train$byClass["Precision"]
  
  sensitivity_test <- conf_test$byClass["Sensitivity"]
  specificity_test <- conf_test$byClass["Specificity"]
  precision_test <- conf_test$byClass["Precision"]
  
  auc_train <- auc(roc(y_train, train_pred))
  auc_test <- auc(roc(y_test, test_pred))
  
  ks_stat <- function(scores, labels) {
    pred <- prediction(scores, labels)
    perf <- performance(pred, "tpr", "fpr")
    max(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]])
  }
  
  ks_train <- ks_stat(train_pred, y_train)
  ks_test <- ks_stat(test_pred, y_test)
  
  train_numeric <- as.numeric(as.character(y_train))
  test_numeric <- as.numeric(as.character(y_test))
  
  rmse_train <- sqrt(mean((train_pred - train_numeric)^2))
  mae_train <- mean(abs(train_pred - train_numeric))
  r2_train <- 1 - (sum((train_numeric - train_pred)^2) / sum((train_numeric - mean(train_numeric))^2))
  
  rmse_test <- sqrt(mean((test_pred - test_numeric)^2))
  mae_test <- mean(abs(test_pred - test_numeric))
  r2_test <- 1 - (sum((test_numeric - test_pred)^2) / sum((test_numeric - mean(test_numeric))^2))
  
  return(list(
    accuracy_train = accuracy_train, kappa_train = kappa_train,
    accuracy_test = accuracy_test, kappa_test = kappa_test,
    sensitivity_train = sensitivity_train, specificity_train = specificity_train, precision_train = precision_train,
    sensitivity_test = sensitivity_test, specificity_test = specificity_test, precision_test = precision_test,
    auc_train = auc_train, auc_test = auc_test,
    ks_train = ks_train * 100, ks_test = ks_test * 100,
    rmse_train = rmse_train, mae_train = mae_train, r2_train = r2_train,
    rmse_test = rmse_test, mae_test = mae_test, r2_test = r2_test
  ))
}

# Apply to each model
metrics_ridge <- calculate_metrics(ridge_final, X_train, y_train, X_test, y_test)
metrics_lasso <- calculate_metrics(lasso_final, X_train, y_train, X_test, y_test)
metrics_elastic <- calculate_metrics(elastic_final, X_train, y_train, X_test, y_test)

print("Ridge Metrics")
print(metrics_ridge)

print("LASSO Metrics")
print(metrics_lasso)

print("Elastic Net Metrics")
print(metrics_elastic)

# ROC Curve Comparison (Test)
plot(roc(y_test, ridge_pred), col = "blue", main = "ROC Curve - Model Comparison (Test)", lwd = 2)
plot(roc(y_test, lasso_pred), col = "darkblue", add = TRUE, lwd = 2)
plot(roc(y_test, elastic_pred), col = "purple", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Ridge", "LASSO", "Elastic Net"), 
       col = c("blue", "darkblue", "purple"), lwd = 3)

# ROC Curve Comparison (Train)
plot(roc(y_train, predict(ridge_final, newx = X_train, type = "response")), col = "blue", main = "ROC Curve - Model Comparison (Train)", lwd = 2)
plot(roc(y_train, predict(lasso_final, newx = X_train, type = "response")), col = "darkblue", add = TRUE, lwd = 2)
plot(roc(y_train, predict(elastic_final, newx = X_train, type = "response")), col = "purple", add = TRUE, lwd = 2)
legend("bottomright", legend = c("Ridge", "LASSO", "Elastic Net"), 
       col = c("blue", "darkblue", "purple"), lwd = 3)

# Coefficients Table
coef_ridge <- as.matrix(coef(ridge_final, s = best_lambda_ridge))
coef_lasso <- as.matrix(coef(lasso_final, s = best_lambda_lasso))
coef_elastic <- as.matrix(coef(elastic_final, s = best_lambda_elastic))

coef_ridge_df <- data.frame(Variable = rownames(coef_ridge), Coef_Ridge = coef_ridge[,1])
coef_lasso_df <- data.frame(Variable = rownames(coef_lasso), Coef_LASSO = coef_lasso[,1])
coef_elastic_df <- data.frame(Variable = rownames(coef_elastic), Coef_Elastic = coef_elastic[,1])

coef_combined <- merge(coef_ridge_df, coef_lasso_df, by = "Variable", all = TRUE)
coef_combined <- merge(coef_combined, coef_elastic_df, by = "Variable", all = TRUE)

kable(coef_combined, digits = 4, caption = "Coefficients in Ridge, LASSO and Elastic Net (including removed variables)")

# Model Comparison Table
model_comparison <- data.frame(
  Model = c("Logistic Regression", "Ridge", "LASSO", "Elastic Net"),
  Accuracy = c(
    round(conf_matrix_test$overall["Accuracy"], 4),
    round(conf_matrix_ridge$overall["Accuracy"], 4),
    round(conf_matrix_lasso$overall["Accuracy"], 4),
    round(conf_matrix_elastic$overall["Accuracy"], 4)
  ),
  Sensitivity = c(
    round(conf_matrix_test$byClass["Sensitivity"], 4),
    round(conf_matrix_ridge$byClass["Sensitivity"], 4),
    round(conf_matrix_lasso$byClass["Sensitivity"], 4),
    round(conf_matrix_elastic$byClass["Sensitivity"], 4)
  ),
  Specificity = c(
    round(conf_matrix_test$byClass["Specificity"], 4),
    round(conf_matrix_ridge$byClass["Specificity"], 4),
    round(conf_matrix_lasso$byClass["Specificity"], 4),
    round(conf_matrix_elastic$byClass["Specificity"], 4)
  ),
  Precision = c(
    round(conf_matrix_test$byClass["Pos Pred Value"], 4),
    round(conf_matrix_ridge$byClass["Pos Pred Value"], 4),
    round(conf_matrix_lasso$byClass["Pos Pred Value"], 4),
    round(conf_matrix_elastic$byClass["Pos Pred Value"], 4)
  ),
  Kappa = c(
    round(conf_matrix_test$overall["Kappa"], 4),
    round(conf_matrix_ridge$overall["Kappa"], 4),
    round(conf_matrix_lasso$overall["Kappa"], 4),
    round(conf_matrix_elastic$overall["Kappa"], 4)
  ),
  AUC = c(
    round(auc(y_test, predictions), 4),
    round(auc(y_test, ridge_pred), 4),
    round(auc(y_test, lasso_pred), 4),
    round(auc(y_test, elastic_pred), 4)
  ),
  KS = c(
    round(ks_stat(predictions, as.numeric(as.character(test_data$left))) * 100, 2),
    round(ks_stat(ridge_pred, y_test) * 100, 2),
    round(ks_stat(lasso_pred, y_test) * 100, 2),
    round(ks_stat(elastic_pred, y_test) * 100, 2)
  )
)

kable(model_comparison, caption = "Model Performance Comparison")

# Overfitting was assessed by comparing performance metrics between the training and test sets.
# Since AUC, accuracy, sensitivity, specificity, and KS values are similar across both sets,
# there is no significant evidence of overfitting in the final model.


#### BASELINE DECISION TREE 

data$age_group <- cut(data$age, 
                      breaks = c(18, 30, 40, 50, 60, Inf), 
                      labels = c("18-30", "31-40", "41-50", "51-60", "60+"))

data$age_group <- as.factor(data$age_group)

train_data2 <- data[indices, ]
test_data2 <- data[-indices, ]

train_data2 <- train_data2[, !names(train_data2) %in% c("age", "ID")]
test_data2 <- test_data2[, !names(test_data2) %in% c("age", "ID")]

# Build decision tree
train_data2$left <- factor(train_data2$left, levels = c("no", "yes"), labels = c(0, 1))
test_data2$left <- factor(test_data2$left, levels = c("no", "yes"), labels = c(0, 1))

tree_model <- rpart(left ~ ., data = train_data2, method = "class", control = rpart.control(cp = 0.01))

rpart.plot(tree_model, 
           type = 2,
           extra = 104,
           fallen.leaves = TRUE,
           box.palette = "Blues",
           shadow.col = "gray",
           branch.lty = 3,
           tweak = 1.2)

pred_test <- predict(tree_model, test_data2, type = "class")
pred_test <- factor(pred_test, levels = levels(test_data2$left))

pred_train <- predict(tree_model, train_data2, type = "class")
pred_train <- factor(pred_train, levels = levels(train_data2$left))

# Confusion matrices
conf_matrix_test <- confusionMatrix(pred_test, test_data2$left)
conf_matrix_train <- confusionMatrix(pred_train, train_data2$left)

print(conf_matrix_test)
print(conf_matrix_train)

# Summary table (Test)
conf_table <- conf_matrix_test$table
conf_percent <- round(conf_table / sum(conf_table) * 100, 1)
summary_test <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(sum(conf_table[,2]) / sum(conf_table) * 100, 1), "%"),
             paste0(round(sum(conf_table[,1]) / sum(conf_table) * 100, 1), "%"),
             "100%")
)
print(summary_test, row.names = FALSE)

# Summary table (Train)
conf_table <- conf_matrix_train$table
conf_percent <- round(conf_table / sum(conf_table) * 100, 1)
summary_train <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(sum(conf_table[,2]) / sum(conf_table) * 100, 1), "%"),
             paste0(round(sum(conf_table[,1]) / sum(conf_table) * 100, 1), "%"),
             "100%")
)
print(summary_train, row.names = FALSE)

# Metrics (Test)
accuracy <- conf_matrix_test$overall["Accuracy"]
kappa <- conf_matrix_test$overall["Kappa"]
sensitivity <- conf_matrix_test$byClass["Sensitivity"]
specificity <- conf_matrix_test$byClass["Specificity"]
precision <- conf_matrix_test$byClass["Precision"]

print(paste("Accuracy:", round(accuracy, 4)))
print(paste("Kappa:", round(kappa, 4)))
print(paste("Recall (Sensitivity):", round(sensitivity, 4)))
print(paste("Specificity:", round(specificity, 4)))
print(paste("Precision:", round(precision, 4)))

# Metrics (Train)
accuracy_train <- conf_matrix_train$overall["Accuracy"]
kappa_train <- conf_matrix_train$overall["Kappa"]
sensitivity_train <- conf_matrix_train$byClass["Sensitivity"]
specificity_train <- conf_matrix_train$byClass["Specificity"]
precision_train <- conf_matrix_train$byClass["Precision"]

print(paste("Accuracy (Train):", round(accuracy_train, 4)))
print(paste("Kappa (Train):", round(kappa_train, 4)))
print(paste("Recall (Train):", round(sensitivity_train, 4)))
print(paste("Specificity (Train):", round(specificity_train, 4)))
print(paste("Precision (Train):", round(precision_train, 4)))

# AUC - ROC curves
par(mfrow = c(1, 1))
prob_test <- predict(tree_model, test_data2, type = "prob")[, 2]
roc_test <- roc(as.numeric(as.character(test_data2$left)), prob_test)
auc_test <- auc(roc_test)
print(paste("AUC - Test:", round(auc_test, 4)))

prob_train <- predict(tree_model, train_data2, type = "prob")[, 2]
roc_train <- roc(as.numeric(as.character(train_data2$left)), prob_train)
auc_train <- auc(roc_train)
print(paste("AUC - Train:", round(auc_train, 4)))

# Plot ROC curves
plot(roc_test, col = "blue", main = "ROC Curve - Test", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_test, 4)), col = "blue", lwd = 2)

plot(roc_train, col = "blue", main = "ROC Curve - Train", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_train, 4)), col = "blue", lwd = 2)

# KS statistic
ks_stat <- function(scores, labels) {
  pred <- prediction(scores, labels)
  perf <- performance(pred, "tpr", "fpr")
  max(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]])
}
ks <- ks_stat(prob_test, as.numeric(as.character(test_data2$left)))
print(paste("KS Statistic:", round(ks * 100, 2), "%"))

ks_train <- ks_stat(prob_train, as.numeric(as.character(train_data2$left)))
print(paste("KS (Train):", round(ks_train * 100, 2), "%"))

# Comparative metrics table
metrics_summary <- data.frame(
  Set = c("Train", "Test"),
  Accuracy = c(accuracy_train, accuracy),
  Kappa = c(kappa_train, kappa),
  Sensitivity = c(sensitivity_train, sensitivity),
  Specificity = c(specificity_train, specificity),
  Precision = c(precision_train, precision),
  AUC_ROC = c(auc_train, auc_test),
  KS = c(ks_train * 100, ks * 100)
)

kable(metrics_summary, caption = "Metric Comparison between Train and Test")

#### OPTIMIZED TREE MODEL

# Define maxdepth values to test
maxdepth_values <- 2:20  

# Store results
results <- list()
best_metrics <- data.frame(cp = numeric(), maxdepth = integer(), Accuracy = numeric())

# Cross-validation setup
control_cv <- trainControl(method = "cv", number = 10)

# Grid search over maxdepth values
for (d in maxdepth_values) {
  temp_model <- train(left ~ ., 
                      data = train_data2, 
                      method = "rpart",
                      trControl = control_cv,
                      tuneGrid = expand.grid(cp = seq(0.0001, 0.005, by = 0.005)),
                      control = rpart.control(maxdepth = d))  
  
  results[[paste0("maxdepth_", d)]] <- temp_model
  
  best_cp <- temp_model$bestTune$cp
  best_acc <- max(temp_model$results$Accuracy)
  best_metrics <- rbind(best_metrics, data.frame(cp = best_cp, maxdepth = d, Accuracy = best_acc))
}

# Identify best hyperparameter combination
best_combo <- best_metrics[which.max(best_metrics$Accuracy), ]
print(best_combo)

# Best parameters found manually or from search
best_cp <- 0.001  
best_maxdepth <- 19 

# Train model with optimal hyperparameters
tree_model_opt <- train(left ~ ., 
                        data = train_data2, 
                        method = "rpart", 
                        tuneGrid = expand.grid(cp = best_cp),  
                        trControl = trainControl(method = "cv", number = 10),  
                        control = rpart.control(maxdepth = best_maxdepth, 
                                                minsplit = 20,
                                                minbucket = 10))

# Export fancy tree plot
png("arbol_fancy.png", width = 6000, height = 4000, res = 300)

fancyRpartPlot(tree_model_opt$finalModel, 
               sub = "",  
               main = "Decision Tree",
               palettes = "Blues")  

dev.off()

# ---- PREDICTIONS ----
pred_test <- predict(tree_model_opt, newdata = test_data2)
pred_test_prob <- predict(tree_model_opt, newdata = test_data2, type = "prob")[, 2] 

pred_train <- predict(tree_model_opt, newdata = train_data2)
pred_train_prob <- predict(tree_model_opt, newdata = train_data2, type = "prob")[, 2] 

# ---- CONFUSION MATRICES ----
conf_matrix_test <- confusionMatrix(pred_test, test_data2$left)
conf_matrix_train <- confusionMatrix(pred_train, train_data2$left)

# Test set results
conf_table <- conf_matrix_test$table
conf_percent <- round(conf_table / sum(conf_table) * 100, 1)
summary_test <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(sum(conf_table[,2]) / sum(conf_table) * 100, 1), "%"),
             paste0(round(sum(conf_table[,1]) / sum(conf_table) * 100, 1), "%"),
             "100%")
)
print(summary_test, row.names = FALSE)

# Train set results
conf_table <- conf_matrix_train$table
conf_percent <- round(conf_table / sum(conf_table) * 100, 1)
summary_train <- data.frame(
  REAL = c("Positives", "Negatives", "Total"),
  Positives = c(paste0(conf_percent[2,2], "%"), paste0(conf_percent[2,1], "%"), paste0(sum(conf_percent[2, ]), "%")),
  Negatives = c(paste0(conf_percent[1,2], "%"), paste0(conf_percent[1,1], "%"), paste0(sum(conf_percent[1, ]), "%")),
  Totals = c(paste0(round(sum(conf_table[,2]) / sum(conf_table) * 100, 1), "%"),
             paste0(round(sum(conf_table[,1]) / sum(conf_table) * 100, 1), "%"),
             "100%")
)
print(summary_train, row.names = FALSE)

# ---- METRICS ----
compute_metrics <- function(conf_matrix) {
  list(
    Accuracy = round(conf_matrix$overall["Accuracy"], 4),
    Kappa = round(conf_matrix$overall["Kappa"], 4),
    Sensitivity = round(conf_matrix$byClass["Sensitivity"], 4),
    Specificity = round(conf_matrix$byClass["Specificity"], 4),
    Precision = round(conf_matrix$byClass["Precision"], 4)
  )
}

# Test metrics
metrics_test <- compute_metrics(conf_matrix_test)
print(paste("Accuracy (Test):", metrics_test$Accuracy))
print(paste("Kappa (Test):", metrics_test$Kappa))
print(paste("Recall (Test):", metrics_test$Sensitivity))
print(paste("Specificity (Test):", metrics_test$Specificity))
print(paste("Precision (Test):", metrics_test$Precision))

# Train metrics
metrics_train <- compute_metrics(conf_matrix_train)
print(paste("Accuracy (Train):", metrics_train$Accuracy))
print(paste("Kappa (Train):", metrics_train$Kappa))
print(paste("Recall (Train):", metrics_train$Sensitivity))
print(paste("Specificity (Train):", metrics_train$Specificity))
print(paste("Precision (Train):", metrics_train$Precision))

# ---- AUC-ROC ----
roc_test <- roc(as.numeric(as.character(test_data2$left)), pred_test_prob)
auc_test <- auc(roc_test)

roc_train <- roc(as.numeric(as.character(train_data2$left)), pred_train_prob)
auc_train <- auc(roc_train)

print(paste("AUC-ROC (Test):", round(auc_test, 4)))
print(paste("AUC-ROC (Train):", round(auc_train, 4)))

# Plot ROC
plot(roc_test, col = "blue", main = "ROC Curve - Test", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_test, 4)), col = "blue", lwd = 2)

plot(roc_train, col = "blue", main = "ROC Curve - Train", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_train, 4)), col = "blue", lwd = 2)

# ---- KS ----
ks_stat <- function(scores, labels) {
  pred <- prediction(scores, labels)
  perf <- performance(pred, "tpr", "fpr")
  max(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]]) * 100
}

ks_test <- ks_stat(pred_test_prob, as.numeric(as.character(test_data2$left)))
ks_train <- ks_stat(pred_train_prob, as.numeric(as.character(train_data2$left)))

print(paste("KS (Test):", round(ks_test, 2), "%"))
print(paste("KS (Train):", round(ks_train, 2), "%"))


# Create a data frame with metrics as columns and sets (Train/Test) as rows
metrics_comparison2 <- data.frame(
  Set = c("Train", "Test"),
  Accuracy = c(metrics_train$Accuracy, metrics_test$Accuracy),
  Kappa = c(metrics_train$Kappa, metrics_test$Kappa),
  Sensitivity = c(metrics_train$Sensitivity, metrics_test$Sensitivity),
  Specificity = c(metrics_train$Specificity, metrics_test$Specificity),
  Precision = c(metrics_train$Precision, metrics_test$Precision),
  AUC_ROC = c(auc_train, auc_test),
  KS = c(ks_train, ks_test)
)

library(knitr)
kable(metrics_comparison2, caption = "Comparison of Metrics between Train and Test")

# Overfitting was assessed by comparing performance metrics between the training and test sets.
# Since AUC, accuracy, sensitivity, specificity, and KS values are similar across both sets,
# there is no significant evidence of overfitting in the final model.



# Save Forward Selection model and its predictions
saveRDS(forward_model, "forward_model.rds")
predictions <- predict(forward_model, newdata = test_data, type = "response")
predictions <- predictions[1:length(test_data$left)]  
saveRDS(predictions, "forward_pred.rds")

# Save regularized model predictions
saveRDS(ridge_pred, "ridge_pred.rds")
saveRDS(lasso_pred, "lasso_pred.rds")
saveRDS(elastic_pred, "elastic_pred.rds")

# Save decision tree models
saveRDS(tree_model, "baseline_tree_model.rds")
saveRDS(tree_model_opt, "opt_tree_model.rds")

# Save processed test dataset
saveRDS(test_data, "test_data.rds")

# Save predicted probabilities for tree models
pred_prob_tree <- predict(tree_model, test_data, type = "prob")[, 2]
pred_prob_tree_opt <- predict(tree_model_opt, test_data, type = "prob")[, 2]
saveRDS(pred_prob_tree, "baseline_tree_pred.rds")
saveRDS(pred_prob_tree_opt, "opt_tree_pred.rds")

# Save final dataset with all modifications
saveRDS(data, "data.rds")
