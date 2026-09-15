# ------------------------------------------------------------
# ApplyBadCoordinateRepairs_184.R
# Apply repaired coordinates to main housing file
# ------------------------------------------------------------
# This creates a NEW parquet file:
# housing_final_coordinates_repaired.parquet
#
# It does NOT overwrite the original housing_final_coordinates.parquet
# ------------------------------------------------------------

rm(list = ls())

library(data.table)
library(duckdb)
library(DBI)

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

output_dir <- "D:/Data_Center_Project/DataCentersOneDrive/output"

original_housing_file <- file.path(output_dir, "housing_final_coordinates.parquet")
repaired_housing_file <- file.path(output_dir, "housing_final_coordinates_repaired.parquet")

repair_file <- file.path(output_dir, "bad_housing_coordinate_repair_attempts.csv")
exclude_file <- file.path(output_dir, "sanity_repaired_rows_over_50_miles.csv")

original_housing_path <- normalizePath(original_housing_file, winslash = "/", mustWork = TRUE)

repaired_housing_path <- file.path(
  normalizePath(output_dir, winslash = "/", mustWork = TRUE),
  "housing_final_coordinates_repaired.parquet"
)

# Remove old repaired output if it already exists
if (file.exists(repaired_housing_file)) {
  file.remove(repaired_housing_file)
}

# ------------------------------------------------------------
# 2. Reading repair files
# ------------------------------------------------------------

repair_attempts <- fread(repair_file)

repair_attempts[, ATTOMID_key := as.numeric(ATTOMID)]
repair_attempts[, TRANSACTIONID_key := as.numeric(TRANSACTIONID)]
repair_attempts[, repaired_latitude := as.numeric(repaired_latitude)]
repair_attempts[, repaired_longitude := as.numeric(repaired_longitude)]

# Read the 1 weak row to exclude
if (file.exists(exclude_file)) {
  exclude_rows <- fread(exclude_file)
} else {
  exclude_rows <- repair_attempts[
    ATTOMID == "1004649700" & TRANSACTIONID == "1034341692"
  ]
}

exclude_rows[, ATTOMID_key := as.numeric(ATTOMID)]
exclude_rows[, TRANSACTIONID_key := as.numeric(TRANSACTIONID)]

exclude_keys <- unique(
  exclude_rows[, .(ATTOMID_key, TRANSACTIONID_key)]
)

# ------------------------------------------------------------
# 3. Keeping only valid repairs, excluding the 1 weak row
# ------------------------------------------------------------

repairs_to_apply <- repair_attempts[
  !is.na(repaired_latitude) &
    !is.na(repaired_longitude) &
    repaired_latitude >= 34 &
    repaired_latitude <= 37 &
    repaired_longitude >= -91 &
    repaired_longitude <= -81
]

repairs_to_apply <- repairs_to_apply[
  !exclude_keys,
  on = .(ATTOMID_key, TRANSACTIONID_key)
]

repairs_to_apply[, repaired_coordinate_source := paste0("repaired_", repair_source)]

repairs_to_apply <- repairs_to_apply[
  ,
  .(
    ATTOMID_key,
    TRANSACTIONID_key,
    repaired_latitude,
    repaired_longitude,
    repaired_coordinate_source
  )
]

repairs_to_exclude <- repair_attempts[
  exclude_keys,
  on = .(ATTOMID_key, TRANSACTIONID_key)
]

repairs_to_exclude <- repairs_to_exclude[
  ,
  .(
    ATTOMID_key,
    TRANSACTIONID_key
  )
]

cat("Repair attempts:", nrow(repair_attempts), "\n")
cat("Repairs to apply:", nrow(repairs_to_apply), "\n")
cat("Rows to exclude / leave unresolved:", nrow(repairs_to_exclude), "\n")

# ------------------------------------------------------------
# 4. Connecting to DuckDB
# ------------------------------------------------------------

con <- dbConnect(
  duckdb(),
  dbdir = file.path(output_dir, "apply_bad_coordinate_repairs.duckdb")
)

# Register small repair tables
dbWriteTable(con, "repairs_to_apply", repairs_to_apply, overwrite = TRUE)
dbWriteTable(con, "repairs_to_exclude", repairs_to_exclude, overwrite = TRUE)

# ------------------------------------------------------------
# 5. Copying original housing parquet into DuckDB table
# ------------------------------------------------------------

dbExecute(con, paste0(
  "CREATE OR REPLACE TABLE housing_repaired AS
   SELECT *
   FROM read_parquet('", original_housing_path, "');"
))

# ------------------------------------------------------------
# 6. Applying 184 repaired coordinates
# ------------------------------------------------------------

dbExecute(con, "
UPDATE housing_repaired AS h
SET
  final_latitude = r.repaired_latitude,
  final_longitude = r.repaired_longitude,
  coordinate_source = r.repaired_coordinate_source
FROM repairs_to_apply AS r
WHERE TRY_CAST(h.ATTOMID AS DOUBLE) = r.ATTOMID_key
  AND TRY_CAST(h.TRANSACTIONID AS DOUBLE) = r.TRANSACTIONID_key;
")

# ------------------------------------------------------------
# 7. Exclude the 1 weak no-address row from distance use
# ------------------------------------------------------------
# We keep the row, but final_latitude/final_longitude become NULL.
# Original latitude/longitude columns remain unchanged.

dbExecute(con, "
UPDATE housing_repaired AS h
SET
  final_latitude = NULL,
  final_longitude = NULL,
  coordinate_source = 'unresolved_bad_coordinate_excluded'
FROM repairs_to_exclude AS e
WHERE TRY_CAST(h.ATTOMID AS DOUBLE) = e.ATTOMID_key
  AND TRY_CAST(h.TRANSACTIONID AS DOUBLE) = e.TRANSACTIONID_key;
")

# ------------------------------------------------------------
# 8. Final check before saving
# ------------------------------------------------------------

repaired_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,

  COUNT(CASE
    WHEN final_latitude IS NULL OR final_longitude IS NULL
    THEN 1 END) AS missing_final_coordinates,

  COUNT(CASE
    WHEN final_latitude < 34 OR final_latitude > 37
      OR final_longitude < -91 OR final_longitude > -81
    THEN 1 END) AS final_coordinates_outside_tn_bounds,

  COUNT(CASE
    WHEN final_latitude = -1 OR final_longitude = -1
    THEN 1 END) AS final_coordinates_equal_negative_one,

  COUNT(CASE
    WHEN coordinate_source = 'repaired_arcgis_full_address'
    THEN 1 END) AS repaired_arcgis_full_address_rows,

  COUNT(CASE
    WHEN coordinate_source = 'repaired_arcgis_partial_address'
    THEN 1 END) AS repaired_arcgis_partial_address_rows,

  COUNT(CASE
    WHEN coordinate_source = 'unresolved_bad_coordinate_excluded'
    THEN 1 END) AS unresolved_bad_coordinate_excluded_rows,

  MIN(final_latitude) AS min_final_latitude,
  MAX(final_latitude) AS max_final_latitude,
  MIN(final_longitude) AS min_final_longitude,
  MAX(final_longitude) AS max_final_longitude

FROM housing_repaired;
")

print(repaired_check)

fwrite(
  repaired_check,
  file.path(output_dir, "housing_final_coordinates_repaired_check.csv")
)

# ------------------------------------------------------------
# 9. Saving new repaired parquet
# ------------------------------------------------------------

dbExecute(con, paste0(
  "COPY housing_repaired
   TO '", repaired_housing_path, "'
   (FORMAT PARQUET);"
))

# ------------------------------------------------------------
# 10. Saving coordinate source summary
# ------------------------------------------------------------

coordinate_source_summary <- dbGetQuery(con, "
SELECT
  coordinate_source,
  COUNT(*) AS rows
FROM housing_repaired
GROUP BY coordinate_source
ORDER BY rows DESC;
")

print(coordinate_source_summary)

fwrite(
  coordinate_source_summary,
  file.path(output_dir, "housing_final_coordinates_repaired_source_summary.csv")
)

# ------------------------------------------------------------
# 11. Close
# ------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)

shell.exec(output_dir)
