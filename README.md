# BIXI Departures Modelling

Statistical and machine-learning models in R for predicting the number of departures from [BIXI](https://bixi.com/) bike-share stations in Montréal, using weather, calendar and station-surroundings data.

The project has two parts:

1. **Station-day models (non-spatial)** – predict daily departures per station from weather, calendar effects, and neighbourhood characteristics, comparing regularised regression, trees, boosting and mixed models.
2. **Spatial models** – predict a station's *average* departures from its surroundings and location, comparing ordinary regression and boosting against **kriging** and **regression kriging** (a mean model plus kriging of its residuals).

## Repository contents

| File | Purpose |
|---|---|
| `main.R` | Entry point. Loads packages and data, builds model equations, splits the data, trains and compares models. |
| `functions.R` | All helpers: preprocessing, station aggregation, train/validation/test splitting, error metrics, formula builders, and the fitting code for every model type. |
| `func_part2.R` | Earlier version of the helper functions. Not used by `main.R`. |
| `func_part1_deprecated.R` | Oldest version of the helper functions (part 1 only). Deprecated and not used. |
| `BIXI.RData` | The dataset, containing two objects: `Bixi_data` and `Spatial_positions`. |
| `BIXI_project.Rproj` | RStudio project file. |
| `LICENSE` | MIT license. |

## Data

`BIXI.RData` provides two data frames:

- **`Bixi_data`** – one row per station and date, with the response `nb_departure` (departures, stored on a rescaled value that the preprocessing converts back to whole counts) plus the predictors:
  - *Weather and calendar:* `time`, `holiday`, `humidity`, `mean_temp_c`, `max_temp_f`, `total_precip_mm`
  - *Station surroundings:* `area_park`, `len_cycle_path`, `len_major_road`, `len_minor_road`, `num_metro_stations`, `num_university`, `num_other_commercial`, `num_restaurants`, `num_pop`, `num_bus_stations`, `num_bus_routes`, `walkscore`
  - *Station:* `location` (station name) and `capacity`
- **`Spatial_positions`** – `latitude` and `longitude` for each station `location`.

### Preprocessing (`preprocessing()`)

- Projects station coordinates from WGS84 (EPSG:4326) to UTM zone 18N (EPSG:32618) so that distances are in metres, then shifts them so the minimum is 10,000.
- Shortens station names to their numeric id and keeps a dictionary back to the full names.
- Drops rows with missing `nb_departure` (treated as station malfunctions).
- Converts `nb_departure` back to whole-number counts by dividing by its smallest non-missing value.
- Derives `days` (weekday, reference Monday) and `months` (reference April) from `time`.
- Adds `latitude_dist` and `longitude_dist`: normalised distance from the mean station position, a rough "distance from the centre" measure.
- Casts `holiday` and `num_university` to factors with `0` as reference level.

### Station aggregation (`aggregate_by_station()`)

For the spatial part, the data is collapsed to one row per station: mean `nb_departure`, the (constant) station-environment variables, and the coordinates. `walkscore` is squared in this aggregated table.

## Models

All models are specified as `c(name, type, equation, options, train set, test set, validation set)` rows in `main.R`, and run through `train_models()`.

### Part 1 – station-day models

| Name | Method |
|---|---|
| `baseline` | Predicts the training mean |
| `poisson` | Poisson GLM |
| `linear` | Linear regression |
| `elasticnet1se`, `elasticnetmin` | Elastic net (alpha searched over 0–1) using the 1-SE and minimum-CV lambda |
| `relaxlasso` | Relaxed lasso (`glmnet`) |
| `regressiontree`, `regressiontree1se` | `rpart` regression tree (anova), minimum-error or 1-SE pruning |
| `poissontree`, `poissontree1se` | `rpart` Poisson tree, minimum-error or 1-SE pruning |
| `conditional_tree` | Conditional inference tree (`party`) |
| `boosttree` | Gradient boosting (`gbm`), tuned on trees, depth, and shrinkage |
| `forward_regr` | Forward stepwise linear regression, selected on validation RMSE |
| `randintercept` | Linear mixed model with a random intercept per station (`nlme`) |

The linear-type models use a formula built from the base predictors plus all pairwise interactions (`get_inteq()`) and polynomial terms (`get_polymeq()`, powers 0.5 and 2). The tree and boosting models use the base predictors only. Random forests (`randomForest` and `ranger`) are implemented in `functions.R` but are not enabled in `main.R`.

### Part 2 – spatial models (station-level)

| Name | Method |
|---|---|
| `baseline`, `baselinespat` | Training-mean baselines (station-day and station-level) |
| `kriging` | Universal kriging (`gstat`) with the station predictors as covariates |
| `boostkriging` | Boosted-tree mean model plus kriging of its residuals |
| `linearkriging` | Linear mean model plus kriging of its residuals |
| `boosttreespat` | Boosted trees without a spatial term |
| `linearspat` | Linear regression without a spatial term |

For the kriging models, the variogram family is chosen by 5-fold cross-validation among `Nug`, `Exp` and `Sph`.

### Evaluation

Each model reports **RMSE** and **MAE** on the validation set and the test set, together with the selected hyperparameters. Predictions are clipped at zero before errors are computed. The kriging-based models report cross-validated errors in the validation columns.

## Results
### Non-spatial models
<img width="900" height="604" alt="image" src="https://github.com/user-attachments/assets/dcf3a77a-7c9a-4d61-a5e9-209e623e8a45" />

### Spatial models
<img width="963" height="452" alt="image" src="https://github.com/user-attachments/assets/f83c48bf-df4b-4cd4-b85e-268fc85532cc" />



## Getting started

### Requirements

- R (with the ability to install packages)
- The following packages, which `main.R` installs automatically if missing:

`here`, `MASS`, `glmnet`, `rpart`, `party`, `randomForest`, `gbm`, `car`, `mboost`, `nlme`, `dplyr`, `ranger`, `gstat`, `sp`, `sf`, `spdep`

### Running

1. Clone the repository and set the working directory to its root, for example by opening `BIXI_project.Rproj` in RStudio.
2. Choose what to run with the flags at the top of `main.R`:

   ```r
   data_explo <- FALSE   # summary statistics, correlation heatmaps, VIF check
   part1      <- FALSE   # non-spatial station-day models
   part2      <- TRUE    # spatial station-level models
   ```

3. Run the script:

   ```r
   source("main.R")
   ```

   or from a terminal:

   ```sh
   Rscript main.R
   ```

The script prints a results table with one row per model (validation and test RMSE/MAE, plus the selected parameters).

### Notes

- A fixed seed is set (`set.seed(2112026)`), so the data splits are reproducible.
- **Splits:** the station-day data is split 80% train, 10% validation, 10% test (when `data_explo` is `TRUE`, a much smaller subset is used to save time). The station-level data is split 90% train and 10% test.
- `part1` is computationally heavy: it fits many models with hyperparameter searches and a large interaction/polynomial design matrix.
- Diagnostic PNGs (residual plots and variograms) are only written when the script is run from the author's own local path; elsewhere that step is skipped. `*.png` files are git-ignored.

## License

Released under the [MIT License](LICENSE).
