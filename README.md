# Fatalities Regression Analysis

## Project Objective
Using the Fatalities dataset in R, this project demonstrates a rigorous, step-by-step process for the diagnostic analysis and validation of a multiple linear regression model. The primary focus is not on predictive accuracy, but on the application of statistical tests to validate model assumptions and refine the model based on that evidence.

## Key Findings & Conclusion
- Model Validation: The initial linear regression model was found to violate key statistical assumptions. A diagnostic check of the residuals revealed they were not normally distributed and exhibited heteroscedasticity.

- Data Transformation: Applying a log transformation to the response variable successfully corrected these issues, resulting in a statistically valid model with normally distributed and homoscedastic residuals.

- Model Refinement: The analysis identified several high-leverage and influential data points. As a demonstration of a robust modeling process, these points were removed, which improved the final model's performance and reduced the residual standard error.

- Conclusion: This project is a case study in the proper diagnostic and validation workflow for statistical modeling. It proves the ability to not just build a model, but to rigorously test its assumptions and refine it based on statistical evidence.

## Technology Stack & Methods
* **Languages/Libraries:** R, RStudio, faraway, sur, smallstuff, broom, AER
* **Model Diagnostics:**
  * Residual vs. Fitted Analysis (for homoscedasticity)
  * Q-Q Plots and the Shapiro-Wilk Test (for normality)
  * Log Transformation of the response variable
* **Outlier & Influence Analysis:**
   * Leverage calculation (Hat Values)
   * Outlier detection (Studentized Residuals)
   * Influential point detection (Cook's Distance)
* **Collinearity Testing:**
  * Correlation Matrix analysis
  * Variance Inflation Factor (VIF) calculation

## Project Structure
* fatalities_analysis.R file: The main file containing the entire analysis, from data loading to model diagnostics and conclusion.

* smallstuff2.R file: A file containing set of pre-made internal functions by Dr. Small.

## How to Run
1. Clone the repository to your local machine.
2. The main analysis script is fatalities_analysis.R. The helper script smallstuff2.R should be included in the repository and is sourced automatically.
3. Ensure to have the required libraries installed in your R environment.
4. Run the fatalities_analysis.R script.

## Data Sources
* The analysis uses the Fatalities dataset, which is included in the **AER package** in R.