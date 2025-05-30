SENDQSAR
================

- [SENDQSAR: A `R Package` for `QSAR Modeling` with
  `SEND Database`](#sendqsar-a-r-package-for-qsar-modeling-with-send-database)
  - [About](#about)
  - [Features](#features)
  - [Workflow](#workflow)
  - [Modular Functions Overview](#modular-functions-overview)
  - [Helper Functions](#helper-functions)
  - [Functions in Development](#functions-in-development)
  - [Dependencies](#dependencies)
  - [Installation](#installation)
  - [Examples](#examples)
  - [Contribution](#contribution)
  - [License](#license)
  - [Contact](#contact)

# SENDQSAR: A `R Package` for `QSAR Modeling` with `SEND Database`

## About

- This package facilitates developing Quantitative Structure-Activity
  Relationship (QSAR) models using the SEND database. It streamlines
  data acquisition, pre-processing, organ wise toxicity score
  calculation, descriptor calculation, and model evaluation, enabling
  researchers to efficiently explore molecular descriptors and create
  robust predictive models.
- Detailed descriptions of each function are available in the “Articles”
  section of the [GitHub-hosted
  website](https://aminuldu07.github.io/SENDQSAR/).

## Features

- **Automated Data Processing**: Simplifies data acquisition and
  pre-processing steps.
- **Comprehensive Analysis**: Provides z-score calculations for various
  parameters such as body weight, liver-to-body weight ratio, and
  laboratory tests.
- **Machine Learning Integration**: Supports classification modeling,
  hyperparameter tuning, and performance evaluation.
- **Visualization Tools**: Includes but not limited to histograms, bar
  plots, and AUC curves for better data interpretation.

## Workflow

- **Input Database Path**: Provide the path for database or `.xpt` files
  containing nonclinical study data in `SEND` format.
- **Data Pre processing**: Use functions `f1` to `f8` to clean,
  harmonize, and prepare data for Machine Learning (ML).
- **Model Building**: Employ ML functions (`f9` to `f18`) for ML model
  training and evaluation.
- **Visualization**: Generate plots and performance metrics for better
  interpretation (`f12` to `f15`).
- **Automated Pipelines**: Use functions `f15` to `f18` to perform the
  above workflows in A single step by providing the database path and a
  `.csv` file containing the label (TOXIC/NON-TOXIC) of the `STUDYID`.

## Modular Functions Overview

- **Liver Toxicity Score Calculation for Individual `STUDYID`** :

  - `f1`: **`get_compile_data`**
    - Fetches structured data from the specified database path.
  - `f2`: **`get_bw_score`**
    - Calculates body weight z-scores for each animal (depends on `f1`).
  - `f3`: **`get_livertobw_zscore`**
    - Computes liver-to-body weight z-scores(depends on `f1`).
  - `f4`: **`get_lb_score`**
    - Calculates z-scores for laboratory test results(depends on `f1`).
  - `f5`: **`get_mi_score`**
    - Computes z-scores for microscopic findings(depends on `f1`).

- **Liver Toxicity Score Calculation and Aggregation for Multiple
  `STUDYID`**:

  - `f6`: **`get_liver_om_lb_mi_tox_score_list`**
    - Combines z-scores for LB, MI, and liver-to-BW ratio into a single
      data frame.
    - Internally calls `f1` to `f5`.

- **Machine Learning Data Preparation**:

  - `f7`: **`get_col_harmonized_scores_df`** - Harmonizes column names
    across columns for consistency from the data frame (depends on
    `f6`).

  - `f8`: **`get_ml_data_and_tuned_hyperparameters`** - Prepares data
    and tunes hyper parameters for machine learning (depends on `f7`).

- **Machine Learning Model Building and Performance Evaluation**:

  - **Model Training**

    - `f9`: **`get_rf_model_with_cv`**
      - Builds a random forest model with cross-validation (depends on
        `f8`).

  - **Improved Classification Accuracy**

    - `f10`: **`get_zone_exclusioned_rf_model_with_cv*`**
      - Enhances classification accuracy by excluding uncertain
        predictions (depends on `f8`).

  - **Feature Importance & Visualization**

    - `f11`: **`get_imp_features_from_rf_model_with_cv`**
      - Computes feature importance for model interpretation (depends on
        f8).

  - **Model Performance Visualization**

    - `f12`: **`get_histogram_barplot_modular`**
      - Generates histogram of the two levels of data (depends on f8).
    - `f13`: **`get_auc_curve_with_rf_model_modular`**
      - Generates AUC curves to evaluate model performance(depends on
        f8).
    - `f14`: **`get_reprtree_from_rf_model_modular`**
      - Generates representative tree of a decision tree form rf
        model(depends on f8).

#### Notes for Modular Functions

- **Data Preparation**
  - Functions `f1` to `f8` must be executed sequentially to prepare the
    `Data` argument required by these functions.  
  - Alternatively, the **composite function** `f18` can be used to
    directly generate the `Data` argument, combining the functionality
    of `f1` to `f8`.
  - For `f9`, `f10`, `f11`, and `f12`, Functions `f1`, `f2`, `f3`, `f4`,
    `f5`, `f6`, `f7`, and `f8`must be executed sequentially to prepare
    the `Data` argument. Alternatively, the **composite function** `f18`
    can be used to directly generate the `Data` argument.

------------------------------------------------------------------------

### Composite Functions Overview

- Combine multiple modular functions for complex operations.

- **Visualization and Reporting** :

  - `f15`: **`get_histogram_barplot`** - Creates bar plots for target
    variable classes (depends on functions `f1` to `f8`).

  - `f16`: **`get_auc_curve_with_rf_model`** - Generates AUC curves to
    evaluate model performance (depends on functions `f1` to `f8`).

  - `f17`: **`get_reprtree_from_rf_model`** - Builds representative
    decision trees (depends on functions `f1` to `f8`).

  - `f18`: **`get_prediction_plot`** - Visualizes prediction
    probabilities with histograms(depends on functions `f1` to `f8`)..

### Automated Pipelines

- `f19`: **`get_Data_formatted_for_ml_and_best.m`**
  - Creates machine learning-ready data by executing `f1` to `f8`
    -Formats data for ML pipelines.
  - Provide the same result as `f8` by merging functionality of
    functions from `f1` to `f7`
- `f20`: **`get_rf_input_param_list_output_cv_imp`**
  - Automates pre-processing, modeling, and evaluation.
  - Provide the same result as `f9` by merging functionality of
    functions from `f1` to `f8`
- `f21`: **`get_zone_exclusioned_rf_model_cv_imp`**
  - Similar to `f17` but excludes uncertain predictions.
  - Provide the same result as `f10` by merging functionality of
    functions from `f1` to `f8`
  - Optional argument for hyperparameter tuning.

------------------------------------------------------------------------

## Helper Functions

- `h1`: **`get_treatment_group_&_dose`**
  - Retrieve treatment groups from the `tx` domain.
- `h2`: **`get_repeat_dose_parallel_studyids`**
  - Retrieves `STUDYID`s for repeat dose and parallel study designs.
  - Optional filtering for “rat” species studies.
- `h3`: **`get_studyid_metadata`**
  - Crate a data frame for each STUYIDY labeling it either Liver or
    not_Liver

------------------------------------------------------------------------

## Functions in Development

- `fid1`: `get_all_LB_TESTCD_score` - Calculates scores for each
  `LBTESTCD` based on `get_lb_score`.
- `fid2`: `get_indiv_score_om_lb_mi_domain_df` - Returns domain-specific
  scores including liver-to-BW ratio, LB, and MI scores.

------------------------------------------------------------------------

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
