# ------------------------------------------------------------
# DataCenterMissingGeocoding.R
# Fill missing data center latitude/longitude
# ------------------------------------------------------------
# Purpose:
# 1. Read TN data centers missing lat/lon.
# 2. Geocode missing rows using facility name + city.
# 3. Save improved TN data center file with original + geocoded coordinates.
# ------------------------------------------------------------

rm(list = ls())

# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidygeocoder")
# install.packages("readxl")
# install.packages("stringr")

library(data.table)
library(dplyr)
library(tidygeocoder)
library(readxl)
library(stringr)

# ------------------------------------------------------------
# 1. Folder paths
# ------------------------------------------------------------

base_dir <- "D:/Data_Center_Project"
output_dir <- file.path(base_dir, "DataCentersOneDrive", "output")

source_other_dir <- file.path(base_dir, "Source", "Other")

missing_file <- file.path(output_dir, "tn_data_centers_missing_coords.csv")
all_file <- file.path(output_dir, "tn_data_centers_all.csv")



# ------------------------------------------------------------
# 2. Reading the 26 CMX rows missing lat/lon
# ------------------------------------------------------------

tn_missing <- fread(missing_file)

# Creating geocoding search text.
# Since these rows do not have full street address, we use name + city + TN.

tn_missing <- tn_missing %>%
  mutate(
    name = trimws(name),
    operator = trimws(operator),
    city = trimws(city),
    state = trimws(state),
    
    geocode_query = paste(
      name,
      city,
      state,
      "USA",
      sep = ", "
    )
  )

# ------------------------------------------------------------
# 3. Geocoding missing data centers using ArcGIS first
# ------------------------------------------------------------

arcgis_missing_file <- file.path(output_dir, "tn_data_centers_missing_arcgis_result.csv")

if (file.exists(arcgis_missing_file)) {
  
  message("Reading existing ArcGIS missing-data-center result...")
  dc_arcgis_result <- fread(arcgis_missing_file)
  
} else {
  
  message("Running ArcGIS geocoding for missing data centers...")
  
  dc_arcgis_result <- tn_missing %>%
    geocode(
      address = geocode_query,
      method = "arcgis",
      lat = arcgis_lat,
      long = arcgis_lon
    )
  
  fwrite(dc_arcgis_result, arcgis_missing_file)
}

# ------------------------------------------------------------
# 4. Finding ArcGIS failures
# ------------------------------------------------------------

dc_arcgis_failures <- dc_arcgis_result %>%
  filter(is.na(arcgis_lat) | is.na(arcgis_lon))

fwrite(
  dc_arcgis_failures,
  file.path(output_dir, "tn_data_centers_missing_arcgis_failures.csv")
)

# ------------------------------------------------------------
# 5. Trying OSM only for ArcGIS failures
# ------------------------------------------------------------

osm_missing_file <- file.path(output_dir, "tn_data_centers_missing_osm_fallback_result.csv")

if (file.exists(osm_missing_file)) {
  
  message("Reading existing OSM fallback result...")
  dc_osm_result <- fread(osm_missing_file)
  
} else {
  
  if (nrow(dc_arcgis_failures) > 0) {
    
    message("Running OSM fallback geocoding for ArcGIS failures...")
    
    dc_osm_result <- dc_arcgis_failures %>%
      geocode(
        address = geocode_query,
        method = "osm",
        lat = osm_lat,
        long = osm_lon,
        mode = "single",
        min_time = 1.2
      )
    
  } else {
    
    dc_osm_result <- data.table(
      data_center_id = character(),
      osm_lat = numeric(),
      osm_lon = numeric()
    )
  }
  
  fwrite(dc_osm_result, osm_missing_file)
}

# ------------------------------------------------------------
# 6. Combining ArcGIS + OSM results for missing CMX data centers
# ------------------------------------------------------------

dc_arcgis_result <- dc_arcgis_result %>%
  mutate(data_center_id = as.character(data_center_id))

dc_osm_result <- dc_osm_result %>%
  mutate(data_center_id = as.character(data_center_id))

dc_osm_clean <- dc_osm_result %>%
  select(data_center_id, osm_lat, osm_lon) %>%
  distinct(data_center_id, .keep_all = TRUE)

tn_missing_geocoded <- dc_arcgis_result %>%
  left_join(dc_osm_clean, by = "data_center_id") %>%
  mutate(
    geocoded_lat = coalesce(arcgis_lat, osm_lat),
    geocoded_lon = coalesce(arcgis_lon, osm_lon),
    
    geocode_source = case_when(
      !is.na(arcgis_lat) & !is.na(arcgis_lon) ~ "arcgis_name_city",
      !is.na(osm_lat) & !is.na(osm_lon) ~ "osm_name_city",
      TRUE ~ "failed"
    ),
    
    valid_tn_coordinate = ifelse(
      !is.na(geocoded_lat) &
        !is.na(geocoded_lon) &
        geocoded_lat >= 34 &
        geocoded_lat <= 37 &
        geocoded_lon >= -91 &
        geocoded_lon <= -81,
      TRUE,
      FALSE
    )
  )

fwrite(
  tn_missing_geocoded,
  file.path(output_dir, "tn_data_centers_missing_coords_geocoded.csv")
)
# ------------------------------------------------------------
# 7. Adding geocoded coordinates back to all TN data centers
# ------------------------------------------------------------

tn_all <- fread(all_file)

tn_missing_geocoded_clean <- tn_missing_geocoded %>%
  select(
    data_center_id,
    geocoded_lat,
    geocoded_lon,
    geocode_source,
    valid_tn_coordinate
  )

tn_data_centers_all_with_filled_coords <- tn_all %>%
  left_join(tn_missing_geocoded_clean, by = "data_center_id") %>%
  mutate(
    final_dc_lat = case_when(
      !is.na(lat) ~ lat,
      is.na(lat) & valid_tn_coordinate == TRUE ~ geocoded_lat,
      TRUE ~ NA_real_
    ),
    
    final_dc_lon = case_when(
      !is.na(lon) ~ lon,
      is.na(lon) & valid_tn_coordinate == TRUE ~ geocoded_lon,
      TRUE ~ NA_real_
    ),
    
    dc_coordinate_source = case_when(
      !is.na(lat) & !is.na(lon) ~ "original_cmx",
      is.na(lat) & is.na(lon) & valid_tn_coordinate == TRUE ~ geocode_source,
      TRUE ~ "missing"
    )
  )

tn_data_centers_with_coords_improved <- tn_data_centers_all_with_filled_coords %>%
  filter(!is.na(final_dc_lat) & !is.na(final_dc_lon))

tn_data_centers_still_missing_coords <- tn_data_centers_all_with_filled_coords %>%
  filter(is.na(final_dc_lat) | is.na(final_dc_lon))

# ------------------------------------------------------------
# 8. Saving improved data center files
# ------------------------------------------------------------

fwrite(
  tn_data_centers_all_with_filled_coords,
  file.path(output_dir, "tn_data_centers_all_with_filled_coords.csv")
)

fwrite(
  tn_data_centers_with_coords_improved,
  file.path(output_dir, "tn_data_centers_with_coords_improved.csv")
)

fwrite(
  tn_data_centers_still_missing_coords,
  file.path(output_dir, "tn_data_centers_still_missing_coords.csv")
)

dc_geocoding_summary <- tn_data_centers_all_with_filled_coords %>%
  summarise(
    total_tn_data_center_rows = n(),
    original_coordinate_rows = sum(dc_coordinate_source == "original_cmx", na.rm = TRUE),
    arcgis_name_city_rows = sum(dc_coordinate_source == "arcgis_name_city", na.rm = TRUE),
    osm_name_city_rows = sum(dc_coordinate_source == "osm_name_city", na.rm = TRUE),
    final_rows_with_coordinates = sum(!is.na(final_dc_lat) & !is.na(final_dc_lon)),
    final_rows_still_missing_coordinates = sum(is.na(final_dc_lat) | is.na(final_dc_lon))
  )

fwrite(
  dc_geocoding_summary,
  file.path(output_dir, "data_center_geocoding_summary.csv")
)

print(dc_geocoding_summary)


# ------------------------------------------------------------
# 10. Open output folder
# ------------------------------------------------------------

shell.exec(output_dir)