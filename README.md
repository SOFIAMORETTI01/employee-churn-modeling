# 👥 Employee churn modeling

This project addresses the problem of employee attrition through the implementation of supervised machine learning techniques. 
The objective is to predict churn behavior and analyze key drivers using interpretable and replicable models. 

The final output is a shiny dashboard that allows dynamic comparison across different modeling strategies.

---

### 🔗 Access the dashboard online

<a href="https://sofia-moretti.shinyapps.io/05_churn/" target="_blank">
  <img src="https://img.shields.io/badge/Launch-Dashboard-blue?style=for-the-badge&logo=R" alt="Launch Dashboard">
</a>

---

### 🧰 Tech Stack

- **Language**: R  
- **Modeling**: `glmnet`, `rpart`, `pROC`, `ROCR`  
- **Data Manipulation & Visualization**: `dplyr`, `ggplot2`, `gridExtra`  
- **Deployment**: `shiny`  
- **Reproducibility**: `readRDS`, estructura modular con carpetas `script/`, `models/`, `data/`

---

### 📊 Models Explored

Several supervised learning models were developed to predict employee churn:

- **Stepwise Logistic Regression**  
  A baseline model using a forward selection strategy to retain only the most relevant features based on statistical criteria.

- **Regularized Logistic Regression**  
  Different regularization techniques were tested to improve generalization and control multicollinearity:
  - *Ridge Regression*: Shrinks all coefficients to reduce variance.
  - *Lasso Regression*: Performs variable selection by reducing some coefficients to zero.
  - *Elastic Net*: Combines both penalties for balanced performance.

- **Decision Trees**  
  Simple interpretable models were first fitted, followed by an optimized version using cross-validation to tune complexity and prevent overfitting.

All models were evaluated on unseen data using multiple performance metrics (e.g., Accuracy, AUC, KS, Precision, Recall), and their results were visualized through ROC curves and confusion matrices for comparative analysis.

