# Shared feature recipes. Only analysis-fold predictors determine the recipe.
learn_grid <- function(data, width, min_n = 80L) {
  cells <- tibble(gx = floor(data$east_mi / width), gy = floor(data$north_mi / width)) %>%
    count(gx, gy) %>% filter(n >= min_n) %>% arrange(gx, gy) %>%
    mutate(key = paste(gx, gy, sep = "/"), east = (gx + .5) * width,
           north = (gy + .5) * width)
  stopifnot(nrow(cells) > 0)
  list(width = width, min_n = min_n, cells = cells)
}
apply_grid <- function(data, recipe) {
  centers <- recipe$cells
  key <- paste(floor(data$east_mi / recipe$width), floor(data$north_mi / recipe$width), sep = "/")
  index <- match(key, centers$key)
  missing <- which(is.na(index))
  if (length(missing)) {
    distance_squared <- outer(data$east_mi[missing], centers$east, "-")^2 +
      outer(data$north_mi[missing], centers$north, "-")^2
    index[missing] <- max.col(-distance_squared, ties.method = "first")
  }
  factor(centers$key[index], levels = centers$key)
}
impute_vars <- c("lotsize_acre", "num_half_baths", "num_fireplaces", "garage_size",
                 "full_basement", "warmairfurnace", "robbery_660ft", "robbery_knn5_mi")
learn_recipe <- function(data) {
  data <- st_drop_geometry(data)
  list(fill = vapply(data[impute_vars], median, numeric(1), na.rm = TRUE),
       grid_3mi = learn_grid(data, 3))
}
prepare <- function(data, recipe) {
  data <- st_drop_geometry(data)
  for (v in names(recipe$fill)) data[[v]][is.na(data[[v]])] <- recipe$fill[[v]]
  data %>% mutate(age_decade = building_age / 10, log_lot = log(lotsize_acre),
    log_robbery = log1p(robbery_660ft), log_knn5 = log1p(robbery_knn5_mi),
    lot_zone = factor(if_else(north_mi >= 0, "North", "South"), levels = c("North", "South")),
    size_band = cut(building_000sf, c(0, 1, 1.5, 2.5, Inf), include.lowest = TRUE,
      labels = c("Up to 1000", "1000-1500", "1500-2500", "Over 2500")),
    age_band = cut(building_age, c(-Inf, 30, 60, 90, 120, Inf),
      labels = c("Up to 30", "31-60", "61-90", "91-120", "Over 120")),
    lot_band = cut(lotsize_acre, c(0, .025, .05, .1, .2, Inf), include.lowest = TRUE,
      labels = c("Up to .025", ".025-.05", ".05-.10", ".10-.20", "Over .20")),
    grid_3mi = apply_grid(data, recipe$grid_3mi))
}
