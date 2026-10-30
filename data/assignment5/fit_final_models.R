# Run from the repository root. Outputs are specific to the NS report.
suppressPackageStartupMessages({library(tidyverse); library(sf); library(spdep)})
out <- "data/assignment5"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
features <- readRDS("data/assignment5/features.rds")
reference <- readRDS("references/original_m10/model_results.rds")
labels_all <- read_csv("data/assignment5/model_specifications.csv", show_col_types = FALSE)
train_raw <- features$train_raw; test_raw <- features$test_raw
fold_id <- reference$fold_id; weights <- reference$weights
stopifnot(identical(train_raw$id, reference$train$id), identical(test_raw$id, reference$test$id),
  all(vapply(split(fold_id, train_raw$location_id), function(x) length(unique(x)) == 1L, logical(1))))
impute_vars <- c("lotsize_acre", "num_half_baths", "num_fireplaces", "garage_size",
  "full_basement", "warmairfurnace", "robbery_660ft", "robbery_knn5_mi")
learn_recipe <- function(d) list(fill = vapply(st_drop_geometry(d)[impute_vars], median, numeric(1), na.rm = TRUE))
prepare <- function(d, recipe) {
  d <- st_drop_geometry(d)
  for (v in names(recipe$fill)) d[[v]][is.na(d[[v]])] <- recipe$fill[[v]]
  d %>% mutate(age_decade = building_age / 10, log_lot = log(lotsize_acre),
    log_robbery = log1p(robbery_660ft), log_knn5 = log1p(robbery_knn5_mi),
    lot_zone = factor(if_else(north_mi >= 0, "North", "South"), levels = c("North", "South")))
}
fold_data <- lapply(1:10, function(k) {
  recipe <- learn_recipe(train_raw[fold_id != k, ])
  list(analysis = prepare(train_raw[fold_id != k, ], recipe),
    assessment = prepare(train_raw[fold_id == k, ], recipe), rows = which(fold_id == k))
})
recipe_final <- learn_recipe(train_raw)
train <- prepare(train_raw, recipe_final); test <- prepare(test_raw, recipe_final)
moran_i <- function(e) unname(spdep::moran(e, weights, length(e), Szero(weights))$I)
f1 <- sale_price ~ building_000sf + age_decade + lotsize_acre + num_bedrooms +
  num_full_baths + num_half_baths + num_fireplaces + garage_size + central_air +
  one_story + full_basement + finished_basement + brick_stone_ext + warmairfurnace
f2 <- update(f1, . ~ . - lotsize_acre + log_lot + I(building_000sf^2) + I(age_decade^2))
f3 <- update(f2, . ~ . + year_f + quarter_f)
f4 <- update(f3, . ~ . + downtown_mi + I(downtown_mi^2) + east_mi + north_mi)
f5 <- update(f4, . ~ . + model_ward)
f6 <- update(f5, . ~ . + building_000sf:model_ward)
f7 <- update(f6, . ~ . + log_robbery + crime_unavailable)
f8 <- update(f7, . ~ . + robbery_knn5_mi)
f9 <- update(f8, . ~ . - robbery_knn5_mi + log_knn5)
f10 <- update(f9, . ~ . + building_000sf:age_decade)
f_ns <- update(f10, . ~ . + lot_zone + log_lot:lot_zone)
formulas <- list(m1=f1, m2=f2, m3=f3, m4=f4, m5=f5, m6=f6, m7=f7, m8=f8, m9=f9, m10=f10,
  E1=update(f10, . ~ . - log_lot + lotsize_acre),
  E2=update(f10, . ~ . - log_lot + log1p(lotsize_acre)),
  E3=update(f10, . ~ . + log_lot:model_ward),
  E4=update(f10, . ~ . - I(building_000sf^2)), NS=f_ns)
labels <- labels_all[match(names(formulas), labels_all$model), ]
stopifnot(identical(labels$model, names(formulas)))
added <- labels$added; notes <- labels$notes
model_log <- tibble(); fold_scores <- tibble(); income_metrics <- tibble()
oof <- list(); test_predictions <- list(); all_coefficients <- list()
income_group <- cut(train$income, c(-Inf, 50000, 75000, 100000, Inf),
  labels = c("Under $50k", "$50k-75k", "$75k-100k", "$100k+"))
writeLines("Refit m1-m10, E1-E4 and NS on the original ten folds.", file.path(out, "formal_fit.log"))
for (j in seq_along(formulas)) {
  name <- names(formulas)[j]; formula <- formulas[[j]]
  stopifnot(identical(formula[[2]], as.name("sale_price")))
  p <- rep(NA_real_, nrow(train)); scores <- numeric(10)
  for (k in 1:10) {
    d <- fold_data[[k]]
    fit <- lm(formula, d$analysis, na.action = na.fail)
    stopifnot(fit$rank == length(coef(fit)))
    p[d$rows] <- as.numeric(predict(fit, d$assessment))
    scores[k] <- sqrt(mean((d$assessment$sale_price - p[d$rows])^2))
  }
  full_fit <- lm(formula, train, na.action = na.fail)
  stopifnot(full_fit$rank == length(coef(full_fit)), all(is.finite(p)))
  tp <- as.numeric(predict(full_fit, test)); stopifnot(all(is.finite(tp)))
  if (name == "NS") m_final <- full_fit
  e <- train$sale_price - p
  inc <- tibble(income_group, pred = p, ratio = p / train$sale_price, residual = e) %>%
    filter(!is.na(income_group)) %>% group_by(income_group) %>% summarize(
      n = n(), median_ratio = median(ratio), mean_ratio = mean(ratio),
      share_overvalued = mean(ratio > 1), rmse = sqrt(mean(residual^2)),
      mean_residual = mean(residual), negative = sum(pred < 0), .groups = "drop") %>%
    mutate(model = name, .before = 1)
  low <- inc %>% filter(income_group == "Under $50k")
  high <- inc %>% filter(income_group == "$100k+")
  model_log <- bind_rows(model_log, tibble(model = name, added = added[j],
    cv_rmse = mean(scores), pooled_oof_rmse = sqrt(mean(e^2)), moran_i = moran_i(e),
    notes = notes[j], oof_negative = sum(p < 0), test_negative = sum(tp < 0),
    test_min = min(tp), low_income_median_ratio = low$median_ratio,
    low_income_mean_ratio = low$mean_ratio, low_income_overvalued = low$share_overvalued,
    high_income_median_ratio = high$median_ratio,
    income_ratio_gap = low$median_ratio - high$median_ratio,
    parameters = length(coef(full_fit)), selected = name == "NS"))
  income_metrics <- bind_rows(income_metrics, inc)
  fold_scores <- bind_rows(fold_scores, tibble(model = name, fold = 1:10, rmse = scores))
  oof[[name]] <- p; test_predictions[[name]] <- tp
  all_coefficients[[name]] <- enframe(coef(full_fit), "term", "estimate")
  progress <- sprintf("%s: CV RMSE %.2f; Moran I %.6f; negatives %d/%d; low-income ratio %.4f",
    name, mean(scores), tail(model_log$moran_i, 1), sum(p < 0), sum(tp < 0), low$median_ratio)
  cat(progress, "\n"); cat(progress, "\n", file = file.path(out, "formal_fit.log"), append = TRUE)
}
final_name <- "NS"; f_final <- f_ns
stopifnot(max(abs(oof$m10 - reference$diagnostics$pred_final)) < 1e-5,
  max(abs(oof$NS - (read_csv("experiments/north_south_lot/results/out_of_fold_comparison.csv",
    show_col_types=FALSE) %>% filter(model=="north_south_lot"))$pred)) < 1e-5,
  max(abs(test_predictions$NS - read_csv("experiments/north_south_lot/results/candidate_predictions.csv",
    show_col_types=FALSE)$pred)) < 1e-5)
preds <- tibble(id = test$id, pred = test_predictions$NS)
stopifnot(identical(preds$id, read_csv("chicago_test.csv", show_col_types = FALSE)$id),
  nrow(preds) == 5789L, !anyDuplicated(preds$id), all(is.finite(preds$pred)))
diagnostics <- train_raw %>% mutate(fold = fold_id, pred_baseline = oof$m1,
  pred_original = oof$m10, pred_final = oof$NS,
  residual_baseline = sale_price - pred_baseline, residual_original = sale_price - pred_original,
  residual_final = sale_price - pred_final, ratio = pred_final / sale_price, income_group = income_group)
comparison <- model_log %>% filter(model %in% c("m1", "m10", "NS"))
negative_test <- test %>% select(id, ward, lotsize_acre, building_000sf, building_age, is_multisale) %>%
  mutate(pred = preds$pred) %>% filter(pred < 0)
write_csv(model_log, file.path(out, "model_log.csv"))
write_csv(fold_scores, file.path(out, "fold_scores.csv"))
write_csv(income_metrics, file.path(out, "income_group_metrics.csv"))
write_csv(st_drop_geometry(diagnostics), file.path(out, "out_of_fold_predictions.csv"))
write_csv(negative_test, file.path(out, "negative_test_predictions.csv"))
write_csv(preds, "team_Mahjong_predictions.csv")
write_csv(preds, "assignment5_predictions.csv")
write_csv(comparison, file.path(out, "model_comparison.csv"))
write_csv(all_coefficients[[final_name]], file.path(out, "final_coefficients.csv"))
saveRDS(list(final_name=final_name, f_final=f_final, m_final=m_final, model_log=model_log,
  formulas=formulas, fold_scores=fold_scores, income_metrics=income_metrics, diagnostics=diagnostics,
  preds=preds, oof=oof, test_predictions=test_predictions, recipe_final=recipe_final, coefficients=all_coefficients,
  train=train, test=test, fold_id=fold_id, weights=weights), file.path(out,"model_results.rds"))
