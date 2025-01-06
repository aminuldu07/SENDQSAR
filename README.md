SENDQSAR
================

# SENDQSAR: QSAR Modeling with SEND Database

## About

This package facilitates developing Quantitative Structure-Activity
Relationship (QSAR) models using the SEND database. It streamlines data
acquisition, preprocessing, descriptor calculation, and model
evaluation, enabling researchers to efficiently explore molecular
descriptors and create robust predictive models.

## Features

- **Automated Data Processing**: Simplifies data acquisition and
  preprocessing steps.
- **Comprehensive Analysis**: Provides z-score calculations for various
  parameters such as body weight, liver-to-body weight ratio, and
  laboratory tests.
- **Machine Learning Integration**: Supports classification modeling,
  hyperparameter tuning, and performance evaluation.
- **Visualization Tools**: Includes histograms, bar plots, and AUC
  curves for better data interpretation.

## Functions Overview

### Data Acquisition and Processing

- `get_compile_data` - Fetches data from the database specified by the
  database path into a structured data frame for analysis.
- `get_bw_score` - Calculates body weight (BW) z-scores for each animal.
- `get_livertobw_zscore` - Computes liver-to-body weight z-scores.
- `get_lb_score` - Calculates z-scores for laboratory test (LB) results.
- `get_mi_score` - Computes z-scores for microscopic findings (MI).
- **`get_liver_om_lb_mi_tox_score_list`**
  - Purpose: Combines z-scores of LB, MI, and liver-to-BW ratio into a
    single data frame.
  - Workflow: Internally calls the following functions:
    `get_compile_data`, `get_bw_score`,
    `get_livertobw_zscore`,`get_lb_score`, `get_mi_score`.
  - Output: A data frame with the first column as STUDYID and the
    remaining columns containing calculated scores for “Liver to BW
    ratio”, “LB” , and “MI” domains.  
- **`get_col_harmonized_scores_df`**
  - Harmonizes column names across the data frame created by the
    get_liver_om_lb_mi_tox_score_list function.
  - The harmonization is required to get the consistent columns across
    the STUDYIDs.

### Machine Learning Preparation and Modeling

- `get_ml_data_and_tuned_hyperparameters` - Prepares data and tunes
  hyperparameters for machine learning.
- `get_rf_model_with_cv` - Builds a random forest model with
  cross-validation and outputs performance metrics.
- `get_zone_exclusioned_rf_model_with_cv` - Introduces an indeterminate
  zone for improved classification accuracy.
- `get_imp_features_from_rf_model_with_cv` - Computes feature importance
  for model interpretation.
- `get_auc_curve_with_rf_model` - Generates AUC curves to evaluate model
  performance.

### Visualization and Reporting

- `get_histogram_barplot` - Creates bar plots for target variable
  classes.
- `get_reprtree_from_rf_model` - Builds representative decision trees
  for interpretability.
- `get_prediction_plot` - Visualizes prediction probabilities with
  histograms.

### Automated Pipelines

- `get_Data_formatted_for_ml_and_best.m` - Formats data for machine
  learning pipelines.
- `get_rf_input_param_list_output_cv_imp` - Automates preprocessing,
  modeling, and evaluation in one step.
- `get_zone_exclusioned_rf_model_cv_imp` - Similar to the above
  function, but excludes uncertain predictions based on thresholds.

## Workflow

1.  **Input Database Path**: Provide the database path containing
    nonclinical study results for each STUDYID.
2.  **Preprocessing**: Use functions 1-8 to clean, harmonize, and
    prepare data.
3.  **Model Building**: Employ machine learning functions (9-18) for
    training, validation, and evaluation.
4.  **Visualization**: Generate plots and performance metrics for better
    interpretation.

## Dependencies

- `randomForest`
- `ROCR`
- `ggplot2`
- `reprtree`

## Installation

``` r
# Install from GitHub
devtools::install_github("aminuldu07/SENDQSAR")
```

## Examples

### Example 1: Basic Data Compilation

``` r
library(SENDQSAR)
data <- get_compile_data("/path/to/database")
```

### Example 2: Z-Score Calculation

``` r
bw_scores <- get_bw_score(data)
liver_scores <- get_livertobw_zscore(data)
```

### Example 3: Machine Learning Model

``` r
model <- get_rf_model_with_cv(data, n_repeats=10)
print(model$confusion_matrix)
```

### Example 4: Visualization

``` r
get_histogram_barplot(data, target_col="target_variable")
```

## Contribution

Contributions are welcome! Feel free to submit issues or pull requests
via GitHub.

## License

This project is licensed under the MIT License - see the LICENSE file
for details.

## Contact

For more information, visit the project GitHub Page or contact
<email@example.com>.
