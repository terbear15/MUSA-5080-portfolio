# Run from the repository root after rendering, before check_submission.py.
suppressPackageStartupMessages({library(tidyverse); library(sf)})
x <- readRDS("data/assignment5/model_results.rds")
p <- read_csv("team_Mahjong_predictions.csv", show_col_types = FALSE)
a <- read_csv("assignment5_predictions.csv", show_col_types = FALSE)
t <- read_csv("chicago_test.csv", show_col_types = FALSE)
vars <- all.vars(delete.response(terms(x$m_final)))
stopifnot(x$final_name == "NS", identical(x$f_final[[2]], as.name("sale_price")),
  !any(c("income", "income_group", "sale_price", "ratio", "pred", "residual") %in% vars),
  x$m_final$rank == length(coef(x$m_final)), length(coef(x$m_final)) == 124,
  !any(grepl("grid|size_band|age_band|lot_band", vars)),
  identical(p$id, t$id), identical(p$id, a$id), identical(p$pred, a$pred), nrow(p) == 5789,
  max(abs(predict(x$m_final, x$test) - p$pred)) < .00001,
  max(abs(x$preds$pred - p$pred)) < .00001,
  all(vapply(split(x$fold_id, x$diagnostics$location_id),
             function(v) length(unique(v)) == 1L, logical(1))))
b <- read_csv("experiments/north_south_lot/results/candidate_predictions.csv",
               show_col_types = FALSE)
stopifnot(identical(p$id, b$id), max(abs(p$pred - b$pred)) < .00001)
# Verify the visible annotated formula as well as the fitted model.
report <- readLines("assign5.qmd")
start <- grep("label: final-formula", report)
end <- which(seq_along(report) > start & report == "```")[[1]]
display <- new.env()
eval(parse(text = paste(report[(start + 1):(end - 1)], collapse = "\n")), display)
stopifnot(identical(attr(terms(display$f_final), "term.labels"),
                    attr(terms(x$f_final), "term.labels")))
final <- x$model_log %>% filter(model == "NS")
error <- x$train$sale_price - x$oof$NS
stopifnot(abs(unname(spdep::moran(error, x$weights, length(error),
  spdep::Szero(x$weights))$I) - final$moran_i) < 1e-10)
h <- tools::md5sum(c("data/assignment5/model_results.rds", "team_Mahjong_predictions.csv",
                     "assignment5_predictions.csv"))
verification <- tibble(check = c("final_model", "response", "full_rank", "coefficients",
   "finite_test_predictions", "negative_test_predictions",
  "predict_max_difference", "trial_max_difference", "model_file_md5", "team_file_md5", "alias_file_md5"),
  value = c(x$final_name, "sale_price", TRUE, length(coef(x$m_final)), sum(is.finite(p$pred)), sum(p$pred < 0),
    max(abs(predict(x$m_final, x$test) - p$pred)), max(abs(p$pred - b$pred)), unname(h)))
write_csv(verification, "data/assignment5/model_verification.csv")
print(verification)
