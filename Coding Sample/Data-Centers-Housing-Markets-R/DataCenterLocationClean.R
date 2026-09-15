# ------------------------------------------------------------
# DataCenterLocationClean.R
# Clean Tennessee data center location file
# ------------------------------------------------------------

rm(list = ls())

library(data.table)

# ------------------------------------------------------------
# 1. Folder paths
# ------------------------------------------------------------

source_dir <- "D:/Data_Center_Project/Source/IHS"
output_dir <- "D:/Data_Center_Project/DataCentersOneDrive/output"

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

facilities_file <- file.path(source_dir, "cmx_facilities_20260623.csv")

# ------------------------------------------------------------
# 2. Reading data center facilities file
# ------------------------------------------------------------

facilities <- fread(facilities_file)

# Cleaning column names: lowercase and simple names
old_names <- names(facilities)
new_names <- tolower(gsub("[^A-Za-z0-9]+", "_", old_names))
new_names <- gsub("_+$", "", new_names)
setnames(facilities, old_names, new_names)

print(names(facilities))

# ------------------------------------------------------------
# 3. Making sure important columns exist
# ------------------------------------------------------------

if (!"state" %in% names(facilities)) {
  stop("The facilities file does not have a state column.")
}

if (!"lat" %in% names(facilities)) {
  if ("latitude" %in% names(facilities)) {
    setnames(facilities, "latitude", "lat")
  } else {
    stop("The facilities file does not have lat or latitude column.")
  }
}

if (!"lon" %in% names(facilities)) {
  if ("longitude" %in% names(facilities)) {
    setnames(facilities, "longitude", "lon")
  } else {
    stop("The facilities file does not have lon or longitude column.")
  }
}

if (!"name" %in% names(facilities)) {
  facilities[, name := paste0("data_center_", .I)]
}

if (!"operator" %in% names(facilities)) {
  facilities[, operator := NA_character_]
}

if (!"city" %in% names(facilities)) {
  facilities[, city := NA_character_]
}

if (!"county" %in% names(facilities)) {
  facilities[, county := NA_character_]
}

if (!"status" %in% names(facilities)) {
  facilities[, status := NA_character_]
}

if (!"capacity_mw" %in% names(facilities)) {
  facilities[, capacity_mw := NA_real_]
}

if (!"year" %in% names(facilities)) {
  facilities[, year := NA_integer_]
}

# ------------------------------------------------------------
# 4. Converting important variables
# ------------------------------------------------------------

facilities[, state := toupper(trimws(state))]
facilities[, lat := as.numeric(lat)]
facilities[, lon := as.numeric(lon)]
facilities[, capacity_mw := as.numeric(capacity_mw)]
facilities[, year := as.integer(year)]

# ------------------------------------------------------------
# 5. Keeping Tennessee data centers
# ------------------------------------------------------------

tn_data_centers_all <- facilities[state == "TN"]

# Adding a clean data center ID
tn_data_centers_all[, data_center_id := paste0("TNDC_", sprintf("%04d", .I))]

# ------------------------------------------------------------
# 6. Keeping rows with usable Tennessee coordinates
# ------------------------------------------------------------

tn_data_centers_with_coords <- tn_data_centers_all[
  !is.na(lat) &
    !is.na(lon) &
    lat >= 34 & lat <= 37 &
    lon >= -91 & lon <= -81
]

tn_data_centers_missing_coords <- tn_data_centers_all[
  is.na(lat) | is.na(lon)
]

# ------------------------------------------------------------
# 7. Summary files
# ------------------------------------------------------------

data_center_location_summary <- data.table(
  total_tn_data_center_rows = nrow(tn_data_centers_all),
  rows_with_coordinates = nrow(tn_data_centers_with_coords),
  rows_missing_coordinates = nrow(tn_data_centers_missing_coords),
  unique_operators_with_coordinates = uniqueN(tn_data_centers_with_coords$operator),
  unique_cities_with_coordinates = uniqueN(tn_data_centers_with_coords$city)
)

operator_summary <- tn_data_centers_with_coords[
  ,
  .(
    rows = .N,
    known_capacity_rows = sum(!is.na(capacity_mw)),
    total_known_capacity_mw = sum(capacity_mw, na.rm = TRUE)
  ),
  by = operator
][order(-rows)]

city_summary <- tn_data_centers_with_coords[
  ,
  .(
    rows = .N,
    known_capacity_rows = sum(!is.na(capacity_mw)),
    total_known_capacity_mw = sum(capacity_mw, na.rm = TRUE)
  ),
  by = city
][order(-rows)]

# ------------------------------------------------------------
# 8. Saving outputs
# ------------------------------------------------------------

fwrite(
  tn_data_centers_all,
  file.path(output_dir, "tn_data_centers_all.csv")
)

fwrite(
  tn_data_centers_with_coords,
  file.path(output_dir, "tn_data_centers_with_coords.csv")
)

fwrite(
  tn_data_centers_missing_coords,
  file.path(output_dir, "tn_data_centers_missing_coords.csv")
)

fwrite(
  data_center_location_summary,
  file.path(output_dir, "data_center_location_summary.csv")
)

fwrite(
  operator_summary,
  file.path(output_dir, "data_center_operator_summary.csv")
)

fwrite(
  city_summary,
  file.path(output_dir, "data_center_city_summary.csv")
)

# ------------------------------------------------------------
# 9. Print summary and open output folder
# ------------------------------------------------------------

print(data_center_location_summary)

shell.exec(output_dir)
