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

## Modular Functions Overview

- **Core Functions**:  
  Functions `f1` to `f12` are essential components, each designed to
  fulfill a specific role in the overall process.

- **Function Dependencies**:

  - **Function `f6`**: Aggregates outputs from `f1`, `f2`, `f3`, `f4`,
    and `f5` to produce a data frame.  
  - **Function `f7`**: Utilizes the output from `f6` to generate a new
    data frame.  
  - **Function `f8`**: Processes the output from `f7` to create another
    data frame.  
  - **Function `f9`**: Leverages the output from `f8` to generate the
    `random forest` performance matrix.

- **Random Forest Model Building**:  
  To build a `random forest model` using the function
  `get_rf_model_with_cv` (referred to as `f9`), the following functions
  must be executed sequentially to prepare the `Data` argument:  
  `f1`, `f2`, `f3`, `f4`, `f5`, `f6`, `f7`, and `f8`.  
  Alternatively, the **composite function** `f18` can be used to
  directly generate the `Data` argument.

- Functions `f10`, `f11`, and `f12` require an input named `Data`, which
  is a data frame. To create `Data`, functions `f1`, `f2`, `f3`, `f4`,
  `f5`, `f6`, `f7`, and `f8` must be called sequentially, or
  alternatively, the **Composite** function `f18` can be used to
  generate `Data` argument.

- Detailed descriptions of each function are available in the “Articles”
  section of the [GitHub-hosted
  website](https://aminuldu07.github.io/SENDQSAR/).

## Composite Functions Overview

- Composite functions are created by combining several `Modular`
  functions to form more complex operations.
- Functions `f13`, `f14`, and `f15` are examples of composite functions.
- Function `f18` is designed to create a `machine learning-ready` data
  frame by calling the `f1` to `f8` modular functions (a total of 8
  functions). `f18` has a optional argument to do hyper parameter
  tuning.

### Visualization functions

\-`f13`, `f14`, and `f15` for visualization.

### Data Acquisition and Processing

- `f1` -`get_compile_data` - Fetches data from the database specified by
  the database path into a structured data frame for analysis.

- `f2` -`get_bw_score` - Calculates body weight (BW) z-scores for each
  animal.

- `f3` -`get_livertobw_zscore` - Computes liver-to-body weight z-scores.

- `f4` -`get_lb_score` - Calculates z-scores for laboratory test (LB)
  results.

- `f5` -`get_mi_score` - Computes z-scores for microscopic findings
  (MI).

- `f6` -**`get_liver_om_lb_mi_tox_score_list`**

  - Purpose: Combines z-scores of LB, MI, and liver-to-BW ratio into a
    single data frame.
  - Workflow: Internally calls the following functions:
    `get_compile_data`, `get_bw_score`,
    `get_livertobw_zscore`,`get_lb_score`, `get_mi_score`.
  - Output: A data frame with the first column as STUDYID and the
    remaining columns containing calculated scores for “Liver to BW
    ratio”, “LB” , and “MI” domains.

- `f7` -**`get_col_harmonized_scores_df`**

  - Harmonizes column names across the data frame created by the
    `get_liver_om_lb_mi_tox_score_list` function.
  - The harmonization is required to get consistent columns across the
    STUDYIDs.

### Machine Learning Preparation and Modeling

- `f8` -**`get_ml_data_and_tuned_hyperparameters`**
  - Prepares data and tunes hyper parameters for machine learning.
  - The input is output data frame from `get_col_harmonized_scores_df`.
- `f9` -**`get_rf_model_with_cv`**
  - Builds a random forest model with cross-validation and outputs
    performance metrics.
  - The input is output data frame from
    `get_ml_data_and_tuned_hyperparameters`.
- \``f10` -`get_zone_exclusioned_rf_model_with_cv` - Introduces an
  indeterminate zone for improved classification accuracy.
- `f11` -`get_imp_features_from_rf_model_with_cv` - Computes feature
  importance for model interpretation.
- `f12` -`get_auc_curve_with_rf_model` - Generates AUC curves to
  evaluate model performance.

### Helper functions

- `h1` -`get_treatment_group_&_dose` - Retrieve treatment groups from
  the `tx` domain
- `h2` -**`get_repeat_dose_parallel_studyids`**
  - Retrieves `STUDYID`s from the SEND database that correspond to both
    repeat dose and parallel study designs.
  - There is an optional argument, `rat_studies`, that allows further
    filtering to retrieve only “rat” species studies.

### Visualization and Reporting

- `f13` -`get_histogram_barplot` - Creates bar plots for target variable
  classes.
- `f14` -`get_reprtree_from_rf_model` - Builds representative decision
  trees for interpretability.
- `f15` -`get_prediction_plot` - Visualizes prediction probabilities
  with histograms.

### Automated Pipelines

- `f16` -`get_Data_formatted_for_ml_and_best.m` - Formats data for
  machine learning pipelines.
- `f17` -`get_rf_input_param_list_output_cv_imp` - Automates
  pre-processing, modeling, and evaluation in one step.
- `f18` -`get_zone_exclusioned_rf_model_cv_imp` - Similar to the above
  function, but excludes uncertain predictions based on thresholds.

### Functions in Development

- `fx1` - `get_all_LB_TESTCD_score` - Based on `get_lb_score`. This
  funciton calculates the score for each of the LBTESTCD
- `fx2` - `get_indiv_score_om_lb_mi_domain_df` - Calculate and returns
  `Liver To BW ratio`, all TESTCD `LB` score, and all `MI` score

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
