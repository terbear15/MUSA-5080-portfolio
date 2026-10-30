from pathlib import Path

root = Path(__file__).resolve().parents[2]
prepare = (root / 'data/assignment5/prepare_features.R').read_text()
fit = (root / 'data/assignment5/fit_models.R').read_text()

header = '''---
title: "Assignment 5: House Price Prediction, Chicago"
subtitle: "Single-family sales, 2023–2025"
author: "Oscar Luo"
date: "2026-10-22"
format:
  html:
    theme: flatly
    toc: true
    code-fold: true
    code-summary: "Show code"
    code-tools: true
    embed-resources: true
execute:
  warning: false
  message: false
freeze: false
---

## Part 0: Setup

```{r}
#| label: setup
library(tidyverse)
library(tidycensus)
library(sf)
library(caret)
library(spdep)
library(nngeo)
library(gt)
options(scipen = 999, timeout = 300, tigris_use_cache = TRUE)
theme_set(theme_minimal(base_size = 12))
knitr::opts_chunk$set(fig.width = 8, fig.height = 5, dpi = 160)
dollars <- scales::label_dollar(accuracy = 1)
dir.create("data/assignment5/figures", recursive = TRUE, showWarnings = FALSE)
save_plot <- function(plot, name, width = 8, height = 5) {
  ggsave(file.path("data/assignment5/figures", paste0(name, ".png")),
         plot, width = width, height = height, dpi = 160, bg = "white")
  plot
}
```

## Part 1: Clean and explore

### Market-sale sample

I exclude transfers at or below $10,000, multiple-property sales, and records without a valid building area or location. A multiple-property transaction does not identify the price of one home, following the [Cook County Assessor's sales criteria](https://github.com/ccao-data/model-res-avm#types-of-sales-excluded). There is no upper-price cutoff. Missing predictors are filled using training-fold medians.

```{r}
#| label: prepare-data
#| results: hide
'''

part1 = '''
```

```{r}
#| label: cleaning-table
cleaning_log %>%
  gt() %>% cols_label(rule = "Rule", reason = "Reason", removed = "Removed",
                     remaining = "Remaining") %>%
  fmt_number(columns = c(removed, remaining), decimals = 0)
```

The analysis retains **`r scales::comma(nrow(train_raw))` training sales**. All **`r scales::comma(nrow(test_raw))` test rows** remain. Of the retained sales, `r sum(train_raw$sale_price < 50000)` are below $50,000 and `r sum(train_raw$sale_price > 2000000)` exceed $2 million.

### Exploration

```{r}
#| label: price-distribution
#| fig-alt: "Histogram of sale prices with a logarithmic dollar axis, showing the long upper tail."
p_price <- ggplot(train_raw, aes(sale_price)) +
  geom_histogram(bins = 45, fill = "#2c7fb8", color = "white") +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(title = "Sale prices have a long upper tail", x = "Sale price (log scale)",
       y = "Sales")
save_plot(p_price, "price_distribution")
```

```{r}
#| label: price-map
#| fig-height: 7
#| fig-alt: "Map of Chicago sales colored by sale price. Higher prices cluster north of downtown and near the lake."
p_map <- ggplot() +
  geom_sf(data = wards, fill = "#f7f7f7", color = "#cccccc", linewidth = 0.15) +
  geom_sf(data = train_raw, aes(color = sale_price), size = 0.35, alpha = 0.7) +
  scale_color_viridis_c(trans = "log10", labels = scales::label_dollar(),
                        name = "Sale price", option = "C") +
  coord_sf(datum = NA) + labs(title = "Price varies sharply across Chicago") +
  theme(panel.grid = element_blank())
save_plot(p_map, "price_map", height = 7)
```

```{r}
#| label: size-price
#| fig-alt: "Scatterplot of building area and sale price with a quadratic fitted curve."
p_size <- ggplot(train_raw, aes(building_000sf, sale_price)) +
  geom_point(alpha = 0.18, size = 0.7, color = "#2c7fb8") +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = FALSE,
              color = "#b35806") +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "m")) +
  labs(title = "Larger houses have a wider range of prices",
       x = "Building area (1,000 square feet)", y = "Sale price")
save_plot(p_size, "size_price")
```

```{r}
#| label: age-price
#| fig-alt: "Median sale price by decade of building age, showing a nonlinear age pattern."
age_summary <- train_raw %>% st_drop_geometry() %>%
  mutate(age_bin = floor(building_age / 10) * 10) %>%
  group_by(age_bin) %>% summarize(n = n(), median_price = median(sale_price),
                                 .groups = "drop")
p_age <- ggplot(age_summary, aes(age_bin, median_price)) +
  geom_line(color = "#2c7fb8") + geom_point(aes(size = n), color = "#2c7fb8") +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = "Price does not decline uniformly with age",
       x = "Building age (start of 10-year band)", y = "Median sale price", size = "Sales")
save_plot(p_age, "age_price")
```

The price map motivates location effects. The size plot suggests a nonlinear area term and different area slopes across locations. The age pattern supports a quadratic term. I keep the expensive houses because they are part of the prediction task; their large dollar errors will have a strong influence on RMSE.

## Part 2: Build the model

### Features

| Feature | Construction |
|---|---|
| Structure | Building area, age, lot size, bedrooms, bathrooms, fireplaces, garage, air conditioning, stories, basement, exterior, and furnace |
| Transformations | Log lot area; squared building area and age |
| Time | Year and calendar-quarter indicators |
| Broad location | Distance to the downtown reference point, its square, and east/north coordinates in miles |
| Local location | Spatial join to 2023 wards; building area × ward interactions |
| Robbery count | Log of one plus incidents within 660 feet during the previous complete quarter |
| Robbery distance | Mean distance to the five nearest incidents in that quarter, also tested as `log1p(distance)` |
| Final interaction | Building area × building age |

The [2023 ward boundaries](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Wards-2023-/p293-wvbd) define fixed geographic zones. Wards with fewer than 80 retained training sales are merged with the nearest adequately represented ward: 34 → 27, 42 → 2, and 46 → 44. The `r sum(all_sales$ward_gap_ft > 0)` points outside a ward polygon use the nearest ward; the largest gap is `r round(max(all_sales$ward_gap_ft))` feet.

The [Chicago robbery records](https://data.cityofchicago.org/Public-Safety/Crimes-2001-to-Present/ijzp-q8t2) cover January 2023 through September 2025. Each house uses a complete preceding quarter ending at least seven days before its sale date. Early-2023 houses have no permitted earlier window, so both crime features are filled from the training fold and flagged as unavailable. Of `r scales::comma(nrow(crime_raw))` downloaded events, `r sum(is.na(crime_raw$latitude))` lack coordinates. The portal supplies a current extract of historical events, not archived releases.

### Cross-validation

I use ten fixed folds, with seed 5080. Sales at identical coordinates stay in the same fold because the files do not contain parcel IDs. Every candidate uses the same retained rows and folds. Medians are learned inside each training fold, then applied to its held-out rows. The log reports mean fold RMSE and Moran's I of out-of-fold residuals using five nearest sales and row-standardized weights.

```{r}
#| label: fit-models
#| results: hide
'''

part3 = '''
```

```{r}
#| label: model-log
model_log %>% gt() %>%
  cols_label(model = "Model", added = "Change", cv_rmse = "CV RMSE",
             moran_i = "Moran's I", notes = "Reason") %>%
  fmt_currency(columns = cv_rmse, decimals = 0) %>%
  fmt_number(columns = moran_i, decimals = 3) %>%
  tab_options(table.font.size = px(12), data_row.padding = px(6))
```

## Part 3: Final model

The final specification is **`r final_name`**, with the lowest mean fold RMSE: **`r dollars(min(model_log$cv_rmse))`**. This is a **`r sprintf('%.1f', 100 * (1 - min(model_log$cv_rmse) / model_log$cv_rmse[1]))`%** improvement over the structure-only baseline. The outcome remains in dollars.

```{r}
#| label: final-formula
f_final
```

```{r}
#| label: selected-coefficients
coef_table <- broom::tidy(m_final) %>%
  filter(term %in% c("num_full_baths", "num_half_baths", "central_air",
                     "log_lot", "I(building_000sf^2)", "I(age_decade^2)",
                     "log_robbery", "log_knn5", "building_000sf:age_decade")) %>%
  select(term, estimate, std.error)
coef_table %>% gt() %>%
  cols_label(term = "Term", estimate = "Coefficient", std.error = "Standard error") %>%
  fmt_number(columns = c(estimate, std.error), decimals = 1) %>%
  tab_header(title = "Selected coefficients", subtitle = "Full retained training sample")
write_csv(broom::tidy(m_final), "data/assignment5/final_coefficients.csv")
```

Ward effects produced the largest single reduction, from **`r dollars(model_log$cv_rmse[4])` to `r dollars(model_log$cv_rmse[5])`**. Allowing the area slope to vary by ward reduced RMSE by another **`r dollars(model_log$cv_rmse[5] - model_log$cv_rmse[6])`**. Crime features were disappointing: the count and distance terms together improved m6 by only **`r dollars(model_log$cv_rmse[6] - model_log$cv_rmse[9])`**. The area–age interaction added a further **`r dollars(model_log$cv_rmse[9] - model_log$cv_rmse[10])`** improvement.

```{r}
#| label: residual-maps
#| fig-height: 7
#| fig-alt: "Side-by-side maps of out-of-fold residuals from the baseline and final models on the same color scale."
residual_maps <- diagnostics %>%
  select(id, residual_baseline, residual_final) %>%
  pivot_longer(starts_with("residual_"), names_to = "model", values_to = "residual") %>%
  mutate(model = factor(model, levels = c("residual_baseline", "residual_final"),
                         labels = c("Baseline", "Final")))
p_residual <- ggplot() +
  geom_sf(data = wards, fill = "#f7f7f7", color = "#d8d8d8", linewidth = 0.1) +
  geom_sf(data = residual_maps, aes(color = residual), size = 0.3, alpha = 0.65) +
  scale_color_gradient2(low = "#b2182b", mid = "#f7f7f7", high = "#2166ac",
                         limits = c(-500000, 500000), breaks = c(-500000, 0, 500000),
                         oob = scales::squish,
                         labels = scales::label_dollar(scale = 1e-3, suffix = "k"),
                         name = "Actual − predicted",
                         guide = guide_colorbar(barwidth = grid::unit(9, "cm"),
                                                title.position = "top")) +
  facet_wrap(~model) + coord_sf(datum = NA) +
  labs(title = "Location features reduce clustered errors",
       caption = "Blue: underprediction. Red: overprediction. Colors capped at ±$500,000; all sales shown.") +
  theme(legend.position = "bottom", panel.grid = element_blank())
save_plot(p_residual, "residual_maps", height = 7)
```

Moran's I falls from **`r sprintf('%.3f', model_log$moran_i[1])` to `r sprintf('%.3f', tail(model_log$moran_i, 1))`**. The broad north-side underprediction and south-side overprediction weaken. Local clusters remain, and m10 has slightly more residual clustering than m9 despite its lower RMSE.

## Part 4: Where it fails, and who pays

### 4A. Largest errors

```{r}
#| label: error-tables
error_by_ward <- diagnostics %>% st_drop_geometry() %>% group_by(ward) %>%
  summarize(sales = n(), rmse = sqrt(mean(residual_final^2)),
             mean_residual = mean(residual_final),
             median_price = median(sale_price), .groups = "drop")
error_by_ward %>% filter(sales >= 50) %>% arrange(desc(rmse)) %>% slice_head(n = 6) %>%
  gt() %>% cols_label(ward = "Ward", sales = "Sales", rmse = "OOF RMSE",
                      mean_residual = "Mean actual − predicted", median_price = "Median price") %>%
  fmt_currency(columns = c(rmse, mean_residual, median_price), decimals = 0) %>%
  tab_header(title = "Largest ward-level errors", subtitle = "Wards with at least 50 retained sales")

largest_errors <- diagnostics %>% st_drop_geometry() %>%
  arrange(desc(abs(residual_final))) %>% slice_head(n = 5) %>%
  select(id, ward, sale_price, pred_final, residual_final)
largest_errors %>% gt() %>%
  cols_label(id = "ID", ward = "Ward", sale_price = "Sale price",
             pred_final = "OOF prediction", residual_final = "Actual − predicted") %>%
  fmt_currency(columns = c(sale_price, pred_final, residual_final), decimals = 0)
```

Ward 43 has the largest RMSE among wards with at least 50 sales: **`r dollars(error_by_ward$rmse[error_by_ward$ward == 43])`**. Its mean residual is only **`r dollars(error_by_ward$mean_residual[error_by_ward$ward == 43])`**, so errors in both directions cancel in the average. The most extreme misses are underpredictions: the $14.25 million sale is predicted at **`r dollars(diagnostics$pred_final[diagnostics$id == 19188])`**. Interior quality, renovation, and unusual luxury properties are plausible missing factors.

Ward 42 has only `r error_by_ward$sales[error_by_ward$ward == 42]` sales and is merged with Ward 2 for fitting. Two of the largest misses occur there. Pooling keeps the category estimable but loses some local detail.

```{r}
#| label: observed-predicted
#| fig-alt: "Observed versus out-of-fold predicted prices with a 45-degree reference line. Several luxury homes are substantially underpredicted."
p_fit <- ggplot(diagnostics, aes(sale_price, pred_final)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, color = "#555555") +
  geom_point(alpha = 0.25, size = 0.8, color = "#2c7fb8") +
  scale_x_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "m")) +
  scale_y_continuous(labels = scales::label_dollar(scale = 1e-6, suffix = "m")) +
  coord_equal() + labs(title = "The largest misses are expensive houses",
                       x = "Observed sale price", y = "Out-of-fold prediction")
save_plot(p_fit, "observed_predicted")
```

### 4B. Neighborhood income and valuation ratios

Neighborhood income is tract median household income from **2023 ACS five-year table B19013**, covering 2019–2023. These estimates were [released in December 2024](https://www.census.gov/programs-surveys/acs/news/data-releases/2023.html), so they are used for this retrospective equity check, not as predictors of earlier sales. The `r sum(is.na(diagnostics$income))` sales without a valid tract income remain in the model and are excluded only from the income comparison.

```{r}
#| label: income-ratios
income_summary <- diagnostics %>% st_drop_geometry() %>% filter(!is.na(income_group)) %>%
  group_by(income_group) %>% summarize(
    sales = n(), baseline_ratio = median(pred_baseline / sale_price),
    median_ratio = median(ratio), mean_ratio = mean(ratio),
    share_overvalued = mean(ratio > 1), .groups = "drop")
income_summary %>% gt() %>%
  cols_label(income_group = "Tract median income", sales = "Sales",
             baseline_ratio = "Baseline median ratio", median_ratio = "Final median ratio",
             mean_ratio = "Final mean ratio", share_overvalued = "Above 1") %>%
  fmt_number(columns = c(baseline_ratio, median_ratio, mean_ratio), decimals = 3) %>%
  fmt_percent(columns = share_overvalued, decimals = 1)

p_income <- income_summary %>%
  select(income_group, baseline_ratio, median_ratio) %>%
  pivot_longer(-income_group, names_to = "model", values_to = "ratio") %>%
  mutate(model = factor(model, levels = c("baseline_ratio", "median_ratio"),
                         labels = c("Baseline", "Final"))) %>%
  ggplot(aes(income_group, ratio, group = model, color = model)) +
  geom_hline(yintercept = 1, linetype = 2, color = "#666666") +
  geom_line(linewidth = 0.8) + geom_point(aes(shape = model), size = 3) +
  scale_color_manual(values = c("#888888", "#2166ac")) +
  labs(title = "Lower-income neighborhoods remain overvalued",
       x = "Tract median household income", y = "Median predicted / sale price",
       color = NULL, shape = NULL) + theme(legend.position = "bottom")
save_plot(p_income, "income_ratios")
```

The final median ratio is **`r sprintf('%.3f', income_summary$median_ratio[1])`** in tracts below $50,000 and **`r sprintf('%.3f', income_summary$median_ratio[4])`** in tracts above $100,000. In the lowest-income group, **`r scales::percent(income_summary$share_overvalued[1], accuracy = 0.1)`** of homes are overvalued. The mean ratio is higher than the median because some low-price sales have very large proportional errors.

```{r}
#| label: cleaning-by-income
cleaning_by_income <- all_sales %>% st_drop_geometry() %>% filter(split == "train") %>%
  mutate(income_group = cut(income, c(-Inf, 50000, 75000, 100000, Inf),
                             labels = levels(diagnostics$income_group))) %>%
  filter(!is.na(income_group)) %>% group_by(income_group) %>%
  summarize(original_sales = n(), excluded = sum(exclusion != "Retained"),
             excluded_share = mean(exclusion != "Retained"),
             retained_under_50k = sum(exclusion == "Retained" & sale_price < 50000),
             .groups = "drop")
cleaning_by_income %>% gt() %>%
  cols_label(income_group = "Tract median income", original_sales = "Original sales",
             excluded = "Excluded", excluded_share = "Excluded share",
             retained_under_50k = "Retained sales below $50k") %>%
  fmt_percent(columns = excluded_share, decimals = 1)
```

Cleaning removes multiple-property transactions while retaining inexpensive single-property sales. A higher price cutoff would remove some of the homes most affected by overvaluation. Exclusion rates also differ across income groups, so these ratios describe the retained single-property sample. [The Tax Divide](https://www.propublica.org/series/the-tax-divide) documented disproportionately high assessments on lower-value homes. The final model reduces the income gap but does not eliminate that pattern. Holding tax rates and exemptions constant, overvaluation increases an owner's share of the tax burden.

## Part 5: Predict the test houses

```{r}
#| label: predict-test
# Refit on all retained training sales, then apply the same preprocessing to test.
fill_final <- learn_fill(train_raw)
train <- prepare(train_raw, fill_final)
test <- prepare(test_raw, fill_final)
m_final <- lm(f_final, data = train, na.action = na.fail)
predictor_vars <- all.vars(delete.response(terms(f_final)))
stopifnot(all(complete.cases(test[predictor_vars])))
preds <- tibble(id = test$id, pred = as.numeric(predict(m_final, newdata = test)))
stopifnot(identical(preds$id, raw_test$id), nrow(preds) == nrow(raw_test),
          !anyDuplicated(preds$id), all(is.finite(preds$pred)))
write_csv(preds, "assignment5_predictions.csv")
tibble(test_rows = nrow(preds), finite_predictions = sum(is.finite(preds$pred)),
       negative_predictions = sum(preds$pred < 0)) %>% knitr::kable()
```

**Final CV RMSE: `r dollars(min(model_log$cv_rmse))`.** Pooled out-of-fold RMSE is **`r dollars(sqrt(mean(diagnostics$residual_final^2)))`**. The test file contains `r scales::comma(nrow(preds))` predictions in the original ID order. Test RMSE requires the instructor's hidden sale prices. The linear model produces `r sum(preds$pred < 0)` negative test predictions; the CSV keeps the same unmodified prediction rule evaluated in CV.

## Part 6: Reflection

Location made the largest difference. Adding ward indicators reduced mean fold RMSE from $235,864 to $196,399, and allowing building-area slopes to vary across wards reduced it to $182,389. A square foot does not carry the same predicted value everywhere. The crime measures added little after these terms, suggesting that broad location already captures much of the variation they describe. The final area–age interaction brought RMSE to $181,295, a 35.6% reduction from the baseline.

**Coefficient reading:** The full-bathroom coefficient is **`r dollars(coef(m_final)['num_full_baths'])`**. Holding the model's other characteristics constant, homes with one additional full bathroom have an estimated price that is higher by this amount. This is a conditional association: bathrooms can also proxy for unobserved interior quality. **Prediction reading:** For two otherwise identical feature records, changing full bathrooms from one to two increases the fitted price by **`r dollars(coef(m_final)['num_full_baths'])`**. It does not establish the return from adding a bathroom.

I would use this model as a starting point for assessment review. I would not use its output directly for tax bills. The final median valuation ratio remains about 1.13 in the lowest-income tracts, compared with 0.99 in the highest-income group. Optimizing dollar RMSE has not removed unequal proportional errors. The most expensive homes also produce the largest misses, and the model generates **`r sum(diagnostics$pred_final < 0)` negative held-out predictions**. Those failures need review before values are assigned.

The next improvements would focus on renovation, property condition, and finer location information for sparsely represented areas. I would evaluate them on the same retained sample and inspect both RMSE and income-group ratios. The house-grouped folds estimate performance on other houses in this three-year market. Predicting a future assessment year would need a time-based validation. Finally, the instructor's held-out test will show how well the model-selection result carries over to unseen houses.
'''

(root / 'assign5.qmd').write_text(header + prepare + part1 + fit + part3)
print(root / 'assign5.qmd')
