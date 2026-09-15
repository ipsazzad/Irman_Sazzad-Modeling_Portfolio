# ------------------------------------------------------------
# HousingGeocoding1.R
# Geocode housing rows with missing latitude/longitude
# ------------------------------------------------------------
# Purpose:
# 1. Read rows that have full address but missing latitude/longitude.
# 2. Create one full address string for geocoding.
# 3. Geocode unique addresses using Census first.
# 4. Use OSM/Nominatim for addresses Census could not geocode.
# 5. Use ArcGIS for remaining failed addresses.
# 6. Combine all geocoding results and attach them back to all missing-coordinate rows.
# ------------------------------------------------------------

rm(list = ls())

# Install packages if needed
# install.packages("data.table")
# install.packages("dplyr")
# install.packages("tidygeocoder")

library(data.table)
library(dplyr)
library(tidygeocoder)

# ------------------------------------------------------------
# 1. Folder paths
# ------------------------------------------------------------

base_dir <- "D:/Data_Center_Project/DataCentersOneDrive"
output_dir <- file.path(base_dir, "output")
input_file <- file.path(output_dir, "housing_missing_latlon_with_full_address.csv")

# ------------------------------------------------------------
# 2. Reading missing-coordinate rows
# ------------------------------------------------------------
# This file comes from HousingCode1.R.
# It contains properties that are missing latitude/longitude but have street address, city, state, and ZIP.

missing_latlon <- fread(input_file)

# ------------------------------------------------------------
# 3. Creating full address and address key
# ------------------------------------------------------------
# full_address_for_geocoding: Used by geocoders that accept one full address string.
# address_key: Used to match geocoded coordinates back to the original rows.

missing_latlon <- missing_latlon %>%
  mutate(
    property_address = trimws(property_address),
    property_city = trimws(property_city),
    property_state = trimws(property_state),
    property_zip = trimws(property_zip),
    
    full_address_for_geocoding = paste(
      property_address,
      property_city,
      property_state,
      property_zip,
      sep = ", "
    ),
    
    address_key = paste(
      property_address,
      property_city,
      property_state,
      property_zip,
      sep = " | "
    )
  )

# ------------------------------------------------------------
# 4. Keeping unique addresses only
# ------------------------------------------------------------
# Some addresses appear more than once because the same property can have multiple sales records.
# We geocode each unique address once, then attach the result back to all matching rows.

unique_addresses <- missing_latlon %>%
  select(
    address_key,
    property_address,
    property_city,
    property_state,
    property_zip,
    full_address_for_geocoding
  ) %>%
  distinct(address_key, .keep_all = TRUE)

fwrite(
  unique_addresses,
  file.path(output_dir, "geocoding_unique_addresses_to_process.csv")
)

cat("Rows needing geocoding:", nrow(missing_latlon), "\n")
cat("Unique addresses to geocode:", nrow(unique_addresses), "\n")

# ------------------------------------------------------------
# 5. Geocodeing with Census first
# ------------------------------------------------------------
# This uses separate street, city, state, and ZIP fields.

census_file <- file.path(output_dir, "geocode_census_result.csv")

if (file.exists(census_file)) {
  
  message("Reading existing Census geocoding result...")
  census_result <- fread(census_file)
  
} else {
  
  message("Running Census geocoding...")
  
  census_result <- unique_addresses %>%
    geocode(
      street = property_address,
      city = property_city,
      state = property_state,
      postalcode = property_zip,
      method = "census",
      lat = census_latitude,
      long = census_longitude,
      mode = "batch"
    )
  
  fwrite(census_result, census_file)
}

# ------------------------------------------------------------
# 6. Finding Census failures
# ------------------------------------------------------------
# These are addresses where Census did not return latitude/longitude.

census_failures <- census_result %>%
  filter(is.na(census_latitude) | is.na(census_longitude)) %>%
  select(
    address_key,
    property_address,
    property_city,
    property_state,
    property_zip,
    full_address_for_geocoding
  ) %>%
  distinct(address_key, .keep_all = TRUE)

fwrite(
  census_failures,
  file.path(output_dir, "geocode_census_failures.csv")
)

cat("Census failures:", nrow(census_failures), "\n")

# ------------------------------------------------------------
# 7. Geocodeing Census failures using OSM/Nominatim in chunks
# ------------------------------------------------------------

options(timeout = 1200)

osm_chunk_dir <- file.path(output_dir, "osm_chunks")
dir.create(osm_chunk_dir, showWarnings = FALSE)

chunk_size <- 100

census_failures <- census_failures %>%
  mutate(row_id = row_number())

n_chunks <- ceiling(nrow(census_failures) / chunk_size)

if (nrow(census_failures) > 0) {
  
  for (i in 1:n_chunks) {
    
    chunk_file <- file.path(
      osm_chunk_dir,
      paste0("osm_chunk_", sprintf("%03d", i), ".csv")
    )
    
    # Skip chunk if it already exists
    if (file.exists(chunk_file)) {
      message("Skipping completed OSM chunk ", i, " of ", n_chunks)
      next
    }
    
    message("Starting OSM chunk ", i, " of ", n_chunks)
    
    chunk_data <- census_failures %>%
      filter(
        row_id > (i - 1) * chunk_size,
        row_id <= i * chunk_size
      )
    
    chunk_result <- tryCatch(
      {
        chunk_data %>%
          geocode(
            address = full_address_for_geocoding,
            method = "osm",
            lat = osm_latitude,
            long = osm_longitude,
            mode = "single",
            min_time = 1.2
          )
      },
      error = function(e) {
        
        message("Chunk ", i, " failed: ", e$message)
        
        chunk_data %>%
          mutate(
            osm_latitude = NA_real_,
            osm_longitude = NA_real_
          )
      }
    )
    
    fwrite(chunk_result, chunk_file)
    
    message("Saved OSM chunk ", i, " of ", n_chunks)
  }
}

# ------------------------------------------------------------
# 8. Combining all OSM chunks
# ------------------------------------------------------------

osm_files <- list.files(
  osm_chunk_dir,
  pattern = "osm_chunk_.*\\.csv$",
  full.names = TRUE
)

if (length(osm_files) > 0) {
  
  osm_result <- rbindlist(
    lapply(osm_files, fread),
    fill = TRUE
  )
  
} else {
  
  osm_result <- data.table(
    address_key = character(),
    osm_latitude = numeric(),
    osm_longitude = numeric()
  )
}

fwrite(
  osm_result,
  file.path(output_dir, "geocode_osm_fallback_result.csv")
)

# ------------------------------------------------------------
# 9. Combining Census and OSM results
# ------------------------------------------------------------

osm_result_clean <- osm_result %>%
  select(address_key, osm_latitude, osm_longitude) %>%
  distinct(address_key, .keep_all = TRUE)

final_geocode_unique <- census_result %>%
  left_join(osm_result_clean, by = "address_key") %>%
  mutate(
    final_geocoded_latitude = coalesce(census_latitude, osm_latitude),
    final_geocoded_longitude = coalesce(census_longitude, osm_longitude),
    
    geocode_source = case_when(
      !is.na(census_latitude) & !is.na(census_longitude) ~ "census",
      !is.na(osm_latitude) & !is.na(osm_longitude) ~ "osm_fallback",
      TRUE ~ "failed"
    )
  )

fwrite(
  final_geocode_unique,
  file.path(output_dir, "geocode_final_unique_addresses.csv")
)

# ------------------------------------------------------------
# 10. Attaching Census/OSM result back to all missing-coordinate rows
# ------------------------------------------------------------

missing_latlon_geocoded_all <- missing_latlon %>%
  left_join(
    final_geocode_unique %>%
      select(
        address_key,
        final_geocoded_latitude,
        final_geocoded_longitude,
        geocode_source
      ),
    by = "address_key"
  )

fwrite(
  missing_latlon_geocoded_all,
  file.path(output_dir, "housing_missing_latlon_geocoded_all.csv")
)

# ------------------------------------------------------------
# 11. Saving remaining failures after Census + OSM
# ------------------------------------------------------------

geocode_remaining_failures <- missing_latlon_geocoded_all %>%
  filter(
    is.na(final_geocoded_latitude) |
      is.na(final_geocoded_longitude)
  )

fwrite(
  geocode_remaining_failures,
  file.path(output_dir, "housing_missing_latlon_geocoding_failures.csv")
)

# ------------------------------------------------------------
# 12. Summary after Census + OSM
# ------------------------------------------------------------

geocode_summary <- missing_latlon_geocoded_all %>%
  summarise(
    total_missing_latlon_full_address_rows = n(),
    unique_addresses = n_distinct(address_key),
    geocoded_success_rows = sum(!is.na(final_geocoded_latitude) & !is.na(final_geocoded_longitude)),
    geocoded_failed_rows = sum(is.na(final_geocoded_latitude) | is.na(final_geocoded_longitude)),
    census_success_rows = sum(geocode_source == "census", na.rm = TRUE),
    osm_fallback_success_rows = sum(geocode_source == "osm_fallback", na.rm = TRUE),
    failed_rows = sum(geocode_source == "failed", na.rm = TRUE)
  )

fwrite(
  geocode_summary,
  file.path(output_dir, "geocode_final_summary.csv")
)

print(geocode_summary)

# ------------------------------------------------------------
# 13. Trying ArcGIS on remaining failed unique addresses
# ------------------------------------------------------------
# ArcGIS is used only for addresses that still failed after Census + OSM.

failed_unique <- geocode_remaining_failures %>%
  select(
    address_key,
    property_address,
    property_city,
    property_state,
    property_zip,
    full_address_for_geocoding
  ) %>%
  distinct(address_key, .keep_all = TRUE)

arcgis_file <- file.path(output_dir, "geocode_arcgis_failed_addresses_result.csv")

if (file.exists(arcgis_file)) {
  
  message("Reading existing ArcGIS result...")
  arcgis_result <- fread(arcgis_file)
  
} else {
  
  message("Running ArcGIS fallback geocoding...")
  
  if (nrow(failed_unique) > 0) {
    
    arcgis_result <- failed_unique %>%
      geocode(
        address = full_address_for_geocoding,
        method = "arcgis",
        lat = arcgis_latitude,
        long = arcgis_longitude
      )
    
  } else {
    
    arcgis_result <- data.table(
      address_key = character(),
      arcgis_latitude = numeric(),
      arcgis_longitude = numeric()
    )
  }
  
  fwrite(arcgis_result, arcgis_file)
}

# ------------------------------------------------------------
# 14. ArcGIS summary
# ------------------------------------------------------------

arcgis_summary <- arcgis_result %>%
  summarise(
    unique_failed_addresses_tried = n(),
    arcgis_success = sum(!is.na(arcgis_latitude) & !is.na(arcgis_longitude)),
    arcgis_failed = sum(is.na(arcgis_latitude) | is.na(arcgis_longitude))
  )

fwrite(
  arcgis_summary,
  file.path(output_dir, "geocode_arcgis_failed_addresses_summary.csv")
)

print(arcgis_summary)

# ------------------------------------------------------------
# 15. Combining Census + OSM + ArcGIS results
# ------------------------------------------------------------

arcgis_clean <- arcgis_result %>%
  select(address_key, arcgis_latitude, arcgis_longitude) %>%
  distinct(address_key, .keep_all = TRUE)

final_geocode_unique_all_methods <- final_geocode_unique %>%
  left_join(arcgis_clean, by = "address_key") %>%
  mutate(
    final_latitude_all_methods = coalesce(
      final_geocoded_latitude,
      arcgis_latitude
    ),
    
    final_longitude_all_methods = coalesce(
      final_geocoded_longitude,
      arcgis_longitude
    ),
    
    final_geocode_source_all_methods = case_when(
      geocode_source == "census" ~ "census",
      geocode_source == "osm_fallback" ~ "osm_fallback",
      !is.na(arcgis_latitude) & !is.na(arcgis_longitude) ~ "arcgis_fallback",
      TRUE ~ "failed"
    )
  )

fwrite(
  final_geocode_unique_all_methods,
  file.path(output_dir, "geocode_final_unique_addresses_all_methods.csv")
)

# ------------------------------------------------------------
# 16. Attaching final all-method coordinates back to all missing-coordinate rows
# ------------------------------------------------------------

missing_latlon_geocoded_all_methods <- missing_latlon %>%
  left_join(
    final_geocode_unique_all_methods %>%
      select(
        address_key,
        final_latitude_all_methods,
        final_longitude_all_methods,
        final_geocode_source_all_methods
      ),
    by = "address_key"
  )

fwrite(
  missing_latlon_geocoded_all_methods,
  file.path(output_dir, "housing_missing_latlon_geocoded_all_methods.csv")
)

# ------------------------------------------------------------
# 17. Final all-method summary
# ------------------------------------------------------------

geocode_all_methods_summary <- missing_latlon_geocoded_all_methods %>%
  summarise(
    total_missing_latlon_full_address_rows = n(),
    unique_addresses = n_distinct(address_key),
    geocoded_success_rows = sum(!is.na(final_latitude_all_methods) & !is.na(final_longitude_all_methods)),
    geocoded_failed_rows = sum(is.na(final_latitude_all_methods) | is.na(final_longitude_all_methods)),
    census_success_rows = sum(final_geocode_source_all_methods == "census", na.rm = TRUE),
    osm_fallback_success_rows = sum(final_geocode_source_all_methods == "osm_fallback", na.rm = TRUE),
    arcgis_fallback_success_rows = sum(final_geocode_source_all_methods == "arcgis_fallback", na.rm = TRUE),
    failed_rows = sum(final_geocode_source_all_methods == "failed", na.rm = TRUE)
  )

fwrite(
  geocode_all_methods_summary,
  file.path(output_dir, "geocode_all_methods_summary.csv")
)

print(geocode_all_methods_summary)

# ------------------------------------------------------------
# 18. Open output folder
# ------------------------------------------------------------

shell.exec(output_dir)