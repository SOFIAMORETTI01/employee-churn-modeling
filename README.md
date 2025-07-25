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
- **Data manipulation & visualization**: `dplyr`, `ggplot2`, `gridExtra`  
- **Deployment**: `shiny`
  
---

### 📊 Models explored

- **Stepwise Logistic Regression**  
  A classic baseline model that uses forward feature selection to include only statistically significant variables. It’s interpretable and useful for understanding which features most influence churn.

- **Regularized Logistic Regression**  
  Penalized models were used to improve generalization and control multicollinearity:
  - **Ridge**: Shrinks all coefficients to reduce overfitting without removing features.
  - **Lasso**: Forces some coefficients to zero, effectively performing feature selection.
  - **Elastic Net**: Blends both penalties to balance feature selection and stability.

- **Decision Trees**  
  Captures non-linear patterns and interactions between features. A simple tree was compared to a fine-tuned version with cross-validation and hyperparameter optimization for better predictive power and structure clarity.

Each model was evaluated based on performance metrics, robustness, and ease of interpretation, balancing predictive accuracy with business understanding.

---

### 📈 Model evaluation

Models were evaluated on both training and test sets using metrics such as Accuracy, ROC Curve, Precision, and Recall. Confusion matrices and visualizations helped compare their real-world performance and interpretability.

---

### 🚀 Future improvements

- Try ensemble models (e.g., Random Forest) to improve accuracy.
- Test different data splits and cross-validation strategies to ensure model stability.
- Explore ensemble methods like Random Forest or Gradient Boosting.
- Test non-linear models like Support Vector Machines (SVM) or k-Nearest Neighbors (k-NN).
  
---

## 🛠️ How to run locally

1. **Clone this repository:**
```bash
git clone https://github.com/SOFIAMORETTI01/employee-churn-modeling.git
cd employee-churn-modeling

2. **Open RStudio**
   
Make sure you're in the root directory of the project.

4. **Install required packages**
Install all required libraries using:
```bash
packages <- c("readxl", "moments", "corrplot", "ggplot2", "dplyr", "gridExtra",
              "caret", "MASS", "car", "polycor", "DescTools", "reshape2", "pROC",
              "glmnet", "knitr", "tidyr", "randomForest", "ROCR", "rpart",
              "rpart.plot", "MLmetrics", "writexl", "lmtest", "nortest", "rattle",
              "RColorBrewer")
install.packages(setdiff(packages, installed.packages()[,"Package"]))

5. **Run the scripts**
```bash
python script/employee-churn-prediction.py
python script/employee-churn-prediction_app.py


