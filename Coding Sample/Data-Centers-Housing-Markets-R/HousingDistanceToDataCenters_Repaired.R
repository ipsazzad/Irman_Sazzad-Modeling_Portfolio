# ------------------------------------------------------------
# HousingDistanceToDataCenters_Repaired.R
# Calculating distance from each housing property to nearest data center
# ------------------------------------------------------------
# Purpose:
# 1. Read repaired final housing file with final_latitude/final_longitude.
# 2. Read improved TN data center file with final_dc_lat/final_dc_lon.
# 3. Calculate distance from each property to every data center.
# 4. Keep the nearest data center for each property.
# 5. Save repaired distance summaries, sample file, and parquet result.
# ------------------------------------------------------------

rm(list = ls())

library(data.table)
library(duckdb)
library(DBI)

# ------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------

# I have already tested for sample of 10000 data,now we can run full.
# Still if we want to test first, change TRUE to FALSE.
# Running full takes hours to complete
run_full_distance <- TRUE

test_row_limit <- 10000
chunk_size <- 100000

# ------------------------------------------------------------
# 2. Folder paths
# ------------------------------------------------------------

output_dir <- "D:/Data_Center_Project/DataCentersOneDrive/output"

# Using repaired housing coordinate file, not the old one.
housing_file <- file.path(output_dir, "housing_final_coordinates_repaired.parquet")

data_center_file <- file.path(output_dir, "tn_data_centers_with_coords_improved.csv")

housing_path <- normalizePath(housing_file, winslash = "/", mustWork = TRUE)
data_center_path <- normalizePath(data_center_file, winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------
# 3. Output suffix
# ------------------------------------------------------------

if (run_full_distance) {
  suffix <- "full_repaired"
} else {
  suffix <- "test_10000_repaired"
}

# ------------------------------------------------------------
# 4. Connecting to DuckDB
# ------------------------------------------------------------

con <- dbConnect(
  duckdb(),
  dbdir = file.path(output_dir, "housing_distance_to_datacenters_repaired.duckdb")
)

# ------------------------------------------------------------
# 5. Reading housing and data center files
# ------------------------------------------------------------

dbExecute(con, "DROP VIEW IF EXISTS housing_raw;")
dbExecute(con, "DROP VIEW IF EXISTS data_centers_raw;")
dbExecute(con, "DROP VIEW IF EXISTS housing_for_distance;")
dbExecute(con, "DROP VIEW IF EXISTS data_centers_for_distance;")
dbExecute(con, "DROP TABLE IF EXISTS housing_nearest_data_center;")

dbExecute(con, paste0(
  "CREATE OR REPLACE VIEW housing_raw AS
   SELECT *
   FROM read_parquet('", housing_path, "');"
))

dbExecute(con, paste0(
  "CREATE OR REPLACE VIEW data_centers_raw AS
   SELECT *
   FROM read_csv_auto('", data_center_path, "',
      strict_mode = false,
      all_varchar = true,
      null_padding = true
   );"
))

# ------------------------------------------------------------
# 6. Preparing housing rows
# ------------------------------------------------------------
# We used final_latitude/final_longitude because these include:
# original ATTOM coordinates + geocoded/repaired coordinates.
# We also kept only valid Tennessee coordinates.
# This removed the 188 rows with missing final coordinates.

if (run_full_distance) {
  
  dbExecute(con, "
  CREATE OR REPLACE VIEW housing_for_distance AS
  SELECT
    row_number() OVER () AS housing_row_id,
    *
  FROM housing_raw
  WHERE final_latitude IS NOT NULL
    AND final_longitude IS NOT NULL
    AND final_latitude BETWEEN 34 AND 37
    AND final_longitude BETWEEN -91 AND -81;
  ")
  
} else {
  
  dbExecute(con, paste0(
    "CREATE OR REPLACE VIEW housing_for_distance AS
     SELECT
       row_number() OVER () AS housing_row_id,
       *
     FROM housing_raw
     WHERE final_latitude IS NOT NULL
       AND final_longitude IS NOT NULL
       AND final_latitude BETWEEN 34 AND 37
       AND final_longitude BETWEEN -91 AND -81
     LIMIT ", test_row_limit, ";"
  ))
  
}

# ------------------------------------------------------------
# 7. Preparing data center rows
# ------------------------------------------------------------
# We used final_dc_lat/final_dc_lon because these include:
# original CMX coordinates + geocoded missing data center coordinates.
# We also kept only valid Tennessee data center coordinates.

dbExecute(con, "
CREATE OR REPLACE VIEW data_centers_for_distance AS
SELECT
  data_center_id,
  name AS data_center_name,
  operator AS data_center_operator,
  city AS data_center_city,
  county AS data_center_county,
  state AS data_center_state,
  status AS data_center_status,
  TRY_CAST(capacity_mw AS DOUBLE) AS data_center_capacity_mw,
  TRY_CAST(year AS INTEGER) AS data_center_year,
  dc_coordinate_source,

  TRY_CAST(final_dc_lat AS DOUBLE) AS data_center_latitude,
  TRY_CAST(final_dc_lon AS DOUBLE) AS data_center_longitude

FROM data_centers_raw
WHERE TRY_CAST(final_dc_lat AS DOUBLE) IS NOT NULL
  AND TRY_CAST(final_dc_lon AS DOUBLE) IS NOT NULL
  AND TRY_CAST(final_dc_lat AS DOUBLE) BETWEEN 34 AND 37
  AND TRY_CAST(final_dc_lon AS DOUBLE) BETWEEN -91 AND -81;
")

# ------------------------------------------------------------
# 8. Input summary
# ------------------------------------------------------------

distance_input_summary <- dbGetQuery(con, "
SELECT
  (SELECT COUNT(*) FROM housing_for_distance) AS housing_rows_used,
  (SELECT COUNT(*) FROM data_centers_for_distance) AS data_center_rows_used;
")

print(distance_input_summary)

fwrite(
  distance_input_summary,
  file.path(output_dir, paste0("distance_input_summary_", suffix, ".csv"))
)

# ------------------------------------------------------------
# 9. Distance calculation by chunks
# ------------------------------------------------------------
# This uses the Haversine formula.
# Distance unit = miles.
# Earth radius used = 3958.7613 miles.
# For each housing row:
# 1. Compare it to all data centers.
# 2. Rank distances from nearest to farthest.
# 3. Keep only the nearest data center.

total_housing_rows <- dbGetQuery(con, "
SELECT COUNT(*) AS n
FROM housing_for_distance;
")$n

start_rows <- seq(1, total_housing_rows, by = chunk_size)

for (start_row in start_rows) {
  
  end_row <- min(start_row + chunk_size - 1, total_housing_rows)
  
  message("Processing housing rows ", start_row, " to ", end_row, " of ", total_housing_rows)
  
  chunk_query <- paste0(
    "
    WITH chunk_housing AS (
      SELECT *
      FROM housing_for_distance
      WHERE housing_row_id BETWEEN ", start_row, " AND ", end_row, "
    ),

    distance_candidates AS (
      SELECT
        h.*,

        dc.data_center_id AS nearest_data_center_id,
        dc.data_center_name AS nearest_data_center_name,
        dc.data_center_operator AS nearest_data_center_operator,
        dc.data_center_city AS nearest_data_center_city,
        dc.data_center_county AS nearest_data_center_county,
        dc.data_center_state AS nearest_data_center_state,
        dc.data_center_status AS nearest_data_center_status,
        dc.data_center_capacity_mw AS nearest_data_center_capacity_mw,
        dc.data_center_year AS nearest_data_center_year,
        dc.dc_coordinate_source AS nearest_data_center_coordinate_source,
        dc.data_center_latitude AS nearest_data_center_latitude,
        dc.data_center_longitude AS nearest_data_center_longitude,

        3958.7613 * 2 * ASIN(
          LEAST(
            1,
            SQRT(
              POWER(SIN(RADIANS(dc.data_center_latitude - h.final_latitude) / 2), 2)
              +
              COS(RADIANS(h.final_latitude))
              * COS(RADIANS(dc.data_center_latitude))
              * POWER(SIN(RADIANS(dc.data_center_longitude - h.final_longitude) / 2), 2)
            )
          )
        ) AS distance_to_nearest_data_center_miles

      FROM chunk_housing h
      CROSS JOIN data_centers_for_distance dc
    ),

    ranked AS (
      SELECT
        *,
        ROW_NUMBER() OVER (
          PARTITION BY housing_row_id
          ORDER BY distance_to_nearest_data_center_miles ASC
        ) AS distance_rank
      FROM distance_candidates
    )

    SELECT
      * EXCLUDE(distance_rank)
    FROM ranked
    WHERE distance_rank = 1
    "
  )
  
  if (start_row == 1) {
    
    dbExecute(con, paste0(
      "CREATE TABLE housing_nearest_data_center AS ",
      chunk_query
    ))
    
  } else {
    
    dbExecute(con, paste0(
      "INSERT INTO housing_nearest_data_center ",
      chunk_query
    ))
    
  }
}

# ------------------------------------------------------------
# 10. Distance summaries
# ------------------------------------------------------------

distance_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS rows_with_nearest_data_center,
  MIN(distance_to_nearest_data_center_miles) AS min_distance_miles,
  quantile_cont(distance_to_nearest_data_center_miles, 0.25) AS p25_distance_miles,
  quantile_cont(distance_to_nearest_data_center_miles, 0.50) AS median_distance_miles,
  AVG(distance_to_nearest_data_center_miles) AS average_distance_miles,
  quantile_cont(distance_to_nearest_data_center_miles, 0.75) AS p75_distance_miles,
  MAX(distance_to_nearest_data_center_miles) AS max_distance_miles
FROM housing_nearest_data_center;
")

distance_band_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,

  COUNT(CASE WHEN distance_to_nearest_data_center_miles <= 1 THEN 1 END) AS within_1_mile,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles <= 5 THEN 1 END) AS within_5_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles <= 10 THEN 1 END) AS within_10_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles <= 25 THEN 1 END) AS within_25_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles <= 50 THEN 1 END) AS within_50_miles,

  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 50 THEN 1 END) AS over_50_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 100 THEN 1 END) AS over_100_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 500 THEN 1 END) AS over_500_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 1000 THEN 1 END) AS over_1000_miles

FROM housing_nearest_data_center;
")

nearest_data_center_summary <- dbGetQuery(con, "
SELECT
  nearest_data_center_id,
  nearest_data_center_name,
  nearest_data_center_operator,
  nearest_data_center_city,
  nearest_data_center_county,
  nearest_data_center_status,
  nearest_data_center_coordinate_source,
  COUNT(*) AS housing_rows_nearest_to_this_data_center,
  AVG(distance_to_nearest_data_center_miles) AS average_distance_miles
FROM housing_nearest_data_center
GROUP BY
  nearest_data_center_id,
  nearest_data_center_name,
  nearest_data_center_operator,
  nearest_data_center_city,
  nearest_data_center_county,
  nearest_data_center_status,
  nearest_data_center_coordinate_source
ORDER BY housing_rows_nearest_to_this_data_center DESC;
")

data_center_coordinate_source_distance_summary <- dbGetQuery(con, "
SELECT
  nearest_data_center_coordinate_source,
  COUNT(*) AS housing_rows,
  AVG(distance_to_nearest_data_center_miles) AS average_distance_miles
FROM housing_nearest_data_center
GROUP BY nearest_data_center_coordinate_source
ORDER BY housing_rows DESC;
")

housing_coordinate_source_distance_summary <- dbGetQuery(con, "
SELECT
  coordinate_source AS housing_coordinate_source,
  COUNT(*) AS housing_rows,
  AVG(distance_to_nearest_data_center_miles) AS average_distance_miles,
  MIN(distance_to_nearest_data_center_miles) AS min_distance_miles,
  MAX(distance_to_nearest_data_center_miles) AS max_distance_miles
FROM housing_nearest_data_center
GROUP BY coordinate_source
ORDER BY housing_rows DESC;
")

# ------------------------------------------------------------
# 11. Save summaries and sample
# ------------------------------------------------------------

fwrite(
  distance_summary,
  file.path(output_dir, paste0("distance_summary_", suffix, ".csv"))
)

fwrite(
  distance_band_summary,
  file.path(output_dir, paste0("distance_band_summary_", suffix, ".csv"))
)

fwrite(
  nearest_data_center_summary,
  file.path(output_dir, paste0("nearest_data_center_summary_", suffix, ".csv"))
)

fwrite(
  data_center_coordinate_source_distance_summary,
  file.path(output_dir, paste0("data_center_coordinate_source_distance_summary_", suffix, ".csv"))
)

fwrite(
  housing_coordinate_source_distance_summary,
  file.path(output_dir, paste0("housing_coordinate_source_distance_summary_", suffix, ".csv"))
)

distance_sample <- dbGetQuery(con, "
SELECT *
FROM housing_nearest_data_center
LIMIT 1000;
")

fwrite(
  distance_sample,
  file.path(output_dir, paste0("housing_nearest_data_center_sample_1000_", suffix, ".csv"))
)

# ------------------------------------------------------------
# 12. Saving distance result as Parquet
# ------------------------------------------------------------

parquet_path <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

if (run_full_distance) {
  distance_parquet_name <- "housing_nearest_data_center_full_repaired.parquet"
} else {
  distance_parquet_name <- "housing_nearest_data_center_test_10000_repaired.parquet"
}

distance_parquet_file <- file.path(output_dir, distance_parquet_name)

if (file.exists(distance_parquet_file)) {
  file.remove(distance_parquet_file)
}

dbExecute(con, paste0(
  "COPY (
     SELECT *
     FROM housing_nearest_data_center
   )
   TO '", parquet_path, "/", distance_parquet_name, "'
   (FORMAT PARQUET, COMPRESSION ZSTD);"
))

# ------------------------------------------------------------
# 13. Final quality check
# ------------------------------------------------------------

final_distance_quality_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles IS NULL THEN 1 END) AS missing_distance,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles < 0 THEN 1 END) AS negative_distance,

  COUNT(CASE
    WHEN final_latitude < 34 OR final_latitude > 37
      OR final_longitude < -91 OR final_longitude > -81
    THEN 1 END) AS housing_coordinates_outside_tn_bounds,

  COUNT(CASE
    WHEN nearest_data_center_latitude < 34 OR nearest_data_center_latitude > 37
      OR nearest_data_center_longitude < -91 OR nearest_data_center_longitude > -81
    THEN 1 END) AS data_center_coordinates_outside_tn_bounds,

  MIN(distance_to_nearest_data_center_miles) AS min_distance_miles,
  MAX(distance_to_nearest_data_center_miles) AS max_distance_miles,

  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 50 THEN 1 END) AS rows_over_50_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 100 THEN 1 END) AS rows_over_100_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 500 THEN 1 END) AS rows_over_500_miles,
  COUNT(CASE WHEN distance_to_nearest_data_center_miles > 1000 THEN 1 END) AS rows_over_1000_miles

FROM housing_nearest_data_center;
")

fwrite(
  final_distance_quality_check,
  file.path(output_dir, paste0("final_distance_quality_check_", suffix, ".csv"))
)

# ------------------------------------------------------------
# 14. Print summaries
# ------------------------------------------------------------

print(distance_input_summary)
print(distance_summary)
print(distance_band_summary)
print(data_center_coordinate_source_distance_summary)
print(housing_coordinate_source_distance_summary)
print(final_distance_quality_check)

# ------------------------------------------------------------
# 15. Close connection and open output folder
# ------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)

shell.exec(output_dir)