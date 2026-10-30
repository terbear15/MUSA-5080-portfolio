suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(tidycensus)
  library(nngeo)
})
options(scipen = 999, timeout = 300, tigris_use_cache = TRUE)
data_dir <- "data/assignment5"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

raw_train <- read_csv("chicago_train.csv", show_col_types = FALSE)
raw_test <- read_csv("chicago_test.csv", show_col_types = FALSE)
all_sales <- bind_rows(mutate(raw_train, split = "train"),
                      mutate(raw_test, split = "test")) %>%
  mutate(sale_date = mdy(sale_date),
         location_id = paste(lat, lon, sep = "/"),
         exclusion = case_when(
           split == "test" ~ "Test",
           !is.finite(sale_price) | sale_price <= 10000 ~ "Price at or below $10,000",
           is_multisale == 1 ~ "Multiple-property sale",
           !is.finite(building_000sf) | building_000sf <= 0 ~ "Invalid building area",
           !is.finite(lat) | !is.finite(lon) ~ "Missing coordinates",
           TRUE ~ "Retained"
         )) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3435)

cleaning_log <- tibble(
  rule = c("Price at or below $10,000", "Multiple-property sale",
           "Invalid building area", "Missing coordinates"),
  reason = c("Remove nominal transfers", "Price covers more than one property",
             "Require a positive residential building area", "Require a location")
) %>%
  left_join(st_drop_geometry(all_sales) %>% count(exclusion),
            by = c("rule" = "exclusion")) %>%
  mutate(removed = replace_na(n, 0L),
         remaining = nrow(raw_train) - cumsum(removed)) %>% select(-n)

# Save fixed external responses so subsequent renders use the same inputs.
ward_file <- file.path(data_dir, "wards_2023.rds")
if (!file.exists(ward_file)) {
  wards <- st_read(paste0("https://data.cityofchicago.org/resource/",
                          "p293-wvbd.geojson?$limit=100"), quiet = TRUE)
  saveRDS(wards, ward_file)
}
wards <- readRDS(ward_file) %>% st_transform(3435) %>%
  select(ward) %>% mutate(ward = as.integer(ward)) %>% arrange(ward)

income_file <- file.path(data_dir, "acs_2023_tracts.rds")
if (!file.exists(income_file)) {
  tracts <- get_acs(geography = "tract", state = "IL", county = "Cook",
                    variables = c(income = "B19013_001"), year = 2023,
                    survey = "acs5", geometry = TRUE, output = "wide")
  saveRDS(tracts, income_file)
}
tracts <- readRDS(income_file) %>% st_transform(3435) %>%
  mutate(incomeE = if_else(incomeE > 0, incomeE, NA_real_))

# One polygon per sale, including small coordinate/boundary discrepancies.
ward_matches <- st_intersects(all_sales, wards)
ward_index <- vapply(ward_matches, function(x) if (length(x)) x[1] else NA_integer_, 1L)
missing_ward <- is.na(ward_index)
ward_index[missing_ward] <- st_nearest_feature(all_sales[missing_ward, ], wards)
all_sales$ward <- wards$ward[ward_index]
all_sales$ward_gap_ft <- 0
all_sales$ward_gap_ft[missing_ward] <- as.numeric(st_distance(
  all_sales[missing_ward, ], wards[ward_index[missing_ward], ], by_element = TRUE))

tract_matches <- st_intersects(all_sales, tracts)
tract_index <- vapply(tract_matches, function(x) if (length(x)) x[1] else NA_integer_, 1L)
all_sales$GEOID <- tracts$GEOID[tract_index]
all_sales$income <- tracts$incomeE[tract_index]
all_sales$income_moe <- tracts$incomeM[tract_index]

# Merge sparse wards using geography and training counts, without sale prices.
ward_counts <- all_sales %>% filter(exclusion == "Retained") %>%
  st_drop_geometry() %>% count(ward)
common_wards <- ward_counts %>% filter(n >= 80) %>% pull(ward)
centers <- st_point_on_surface(wards)
ward_map <- tibble(ward = wards$ward, model_ward = wards$ward)
rare <- !ward_map$ward %in% common_wards
ward_map$model_ward[rare] <- common_wards[st_nearest_feature(
  centers[rare, ], centers[match(common_wards, wards$ward), ])]
all_sales$model_ward <- factor(ward_map$model_ward[match(all_sales$ward, ward_map$ward)])

# Distances are measured in miles from a fixed downtown reference point.
downtown <- st_sfc(st_point(c(-87.6319, 41.8838)), crs = 4326) %>% st_transform(3435)
xy <- st_coordinates(all_sales)
origin <- st_coordinates(downtown)[1, ]
all_sales <- all_sales %>% mutate(
  east_mi = (xy[, 1] - origin[1]) / 5280,
  north_mi = (xy[, 2] - origin[2]) / 5280,
  downtown_mi = as.numeric(st_distance(all_sales, downtown)) / 5280,
  year_f = factor(year), quarter_f = factor(sale_qrt),
  crime_window_end = as.Date(floor_date(sale_date - days(7), "quarter")),
  crime_window_start = crime_window_end %m-% months(3),
  crime_unavailable = as.integer(crime_window_start < as.Date("2023-01-01")),
  robbery_660ft = NA_real_, robbery_knn5_mi = NA_real_
)

crime_file <- file.path(data_dir, "robbery_2023_2025.csv")
if (!file.exists(crime_file)) {
  query <- list(
    "$where" = paste("date >= '2023-01-01T00:00:00'",
                     "AND date < '2025-10-01T00:00:00' AND primary_type = 'ROBBERY'"),
    "$select" = "id,date,latitude,longitude,updated_on",
    "$order" = "date,id", "$limit" = "100000")
  response <- httr::GET("https://data.cityofchicago.org/resource/ijzp-q8t2.csv",
                         query = query, httr::timeout(300))
  httr::stop_for_status(response)
  writeBin(httr::content(response, as = "raw"), crime_file)
  writeLines(response$url, file.path(data_dir, "robbery_query.txt"))
}
crime_raw <- read_csv(crime_file, show_col_types = FALSE)
robbery <- crime_raw %>% filter(is.finite(latitude), is.finite(longitude)) %>%
  mutate(event_date = as.Date(date)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% st_transform(3435)
crime_coverage <- tibble()
for (end_text in as.character(sort(unique(all_sales$crime_window_end[all_sales$crime_unavailable == 0])))) {
  end <- as.Date(end_text)
  start <- end %m-% months(3)
  rows <- which(all_sales$crime_window_end == end & all_sales$crime_unavailable == 0)
  incidents <- robbery %>% filter(event_date >= start, event_date < end)
  all_sales$robbery_660ft[rows] <- lengths(st_intersects(
    st_buffer(all_sales[rows, ], 660), incidents))
  nearest <- nngeo::st_nn(all_sales[rows, ], incidents, k = 5,
                          returnDist = TRUE, progress = FALSE)
  all_sales$robbery_knn5_mi[rows] <- vapply(nearest$dist, mean, numeric(1)) / 5280
  crime_coverage <- bind_rows(crime_coverage, tibble(
    start = start, end = end, incidents = nrow(incidents), houses = length(rows)))
}

train_raw <- all_sales %>% filter(exclusion == "Retained")
test_raw <- all_sales %>% filter(split == "test")
stopifnot(nrow(all_sales) == nrow(raw_train) + nrow(raw_test),
          nrow(test_raw) == nrow(raw_test), !anyDuplicated(all_sales$id),
          all(all_sales$crime_window_end <= all_sales$sale_date - 7))
saveRDS(list(all_sales = all_sales, train_raw = train_raw, test_raw = test_raw,
             wards = wards, tracts = tracts, cleaning_log = cleaning_log,
             ward_map = ward_map, ward_counts = ward_counts,
             crime_coverage = crime_coverage,
             crime_missing_coordinates = sum(is.na(crime_raw$latitude)),
             crime_records = nrow(crime_raw)), file.path(data_dir, "features.rds"))
write_csv(cleaning_log, file.path(data_dir, "cleaning_log.csv"))
write_csv(crime_coverage, file.path(data_dir, "crime_coverage.csv"))
cat("Feature rows:", nrow(all_sales), "Training:", nrow(train_raw), "Test:", nrow(test_raw), "\n")
cat("Ward matches repaired:", sum(missing_ward), "Max gap (ft):", max(all_sales$ward_gap_ft), "\n")
cat("Missing income:", sum(is.na(train_raw$income)), "\n")
