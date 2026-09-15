# ------------------------------------------------------------
# HousingCode1.R
# TN Housing Data Cleaning and Summary Checks
# ------------------------------------------------------------
# Purpose:
# 1. Read the compiled TN sales and tax/assessor files.
# 2. Clean residential, likely arms-length sales.
# 3. Merge sales with tax/assessor property characteristics using ATTOMID.
# 4. Create a coordinate-ready file for future distance analysis.
# 5. Create summary/check files to answer data-quality questions.
# ------------------------------------------------------------

rm(list = ls())

# install.packages("data.table")
# install.packages("duckdb")
# install.packages("DBI")

library(data.table)
library(duckdb)
library(DBI)

# ------------------------------------------------------------
# 1. Main folders and file paths
# ------------------------------------------------------------

base_dir <- "D:/Data_Center_Project/DataCentersOneDrive"
compiled_dir <- file.path(base_dir, "data", "housingDataCompiled")
output_dir <- file.path(base_dir, "output")

dir.create(output_dir, showWarnings = FALSE)

sales_file <- file.path(compiled_dir, "TN_sales.csv")
tax_file   <- file.path(compiled_dir, "TN_tax.csv")

sales_path <- normalizePath(sales_file, winslash = "/", mustWork = TRUE)
tax_path   <- normalizePath(tax_file, winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------
# 2. Connecting to DuckDB
# ------------------------------------------------------------
# DuckDB lets us work with the large CSV files without loading everything directly into R memory.

con <- dbConnect(
  duckdb(),
  dbdir = file.path(output_dir, "housing_project.duckdb")
)

# ------------------------------------------------------------
# 3. Reading compiled sales and tax files safely
# ------------------------------------------------------------

dbExecute(con, "DROP VIEW IF EXISTS sales;")
dbExecute(con, "DROP VIEW IF EXISTS tax;")

dbExecute(con, paste0(
  "CREATE OR REPLACE VIEW sales AS
   SELECT *
   FROM read_csv_auto('", sales_path, "',
      strict_mode = false,
      all_varchar = true,
      null_padding = true
   );"
))

dbExecute(con, paste0(
  "CREATE OR REPLACE VIEW tax AS
   SELECT *
   FROM read_csv_auto('", tax_path, "',
      strict_mode = false,
      all_varchar = true,
      null_padding = true
   );"
))

# ------------------------------------------------------------
# 4. Cleaning sales data
# ------------------------------------------------------------
# Cleaning choices:
# - Kept Residential only, because the project focuses on housing prices.
# - Kept ARMSLENGTHFLAG = 1, using the existing ATTOM variable for likely
#   normal market transactions.
# - Kept sale amounts from $1,000 to $2,210,000 to remove very small/zero
#   values and extreme high outliers.
# - Kept sales from 1988 onward because earlier records are sparse.

dbExecute(con, "
CREATE OR REPLACE VIEW clean_sales AS
SELECT
  ATTOMID,
  TRANSACTIONID,
  DOCUMENTTYPECODE,
  ARMSLENGTHFLAG,
  DOCUMENTRECORDINGCOUNTYNAME AS county,
  PROPERTYADDRESSFULL AS property_address,
  PROPERTYADDRESSCITY AS property_city,
  PROPERTYADDRESSSTATE AS property_state,
  PROPERTYADDRESSZIP AS property_zip,
  PROPERTYUSEGROUP,
  PROPERTYUSESTANDARDIZED,
  TRY_CAST(TRANSFERAMOUNT AS DOUBLE) AS sale_amount,
  TRY_CAST(RECORDINGDATE AS DATE) AS recording_date,
  year(TRY_CAST(RECORDINGDATE AS DATE)) AS sale_year,
  source_file
FROM sales
WHERE PROPERTYUSEGROUP = 'Residential'
  AND ARMSLENGTHFLAG = '1'
  AND TRY_CAST(TRANSFERAMOUNT AS DOUBLE) BETWEEN 1000 AND 2210000
  AND TRY_CAST(RECORDINGDATE AS DATE) >= DATE '1988-01-01'
  AND TRY_CAST(RECORDINGDATE AS DATE) <= DATE '2026-12-31';
")

# ------------------------------------------------------------
# 5. Cleaning tax/assessor property data
# ------------------------------------------------------------
# Kept only variables needed for location, property characteristics, and future housing-price analysis.

dbExecute(con, "
CREATE OR REPLACE VIEW clean_tax AS
SELECT
  ATTOMID,
  TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
  TRY_CAST(LONGITUDE AS DOUBLE) AS longitude,
  PROPERTYADDRESSFULL AS tax_property_address,
  PROPERTYADDRESSCITY AS tax_property_city,
  PROPERTYADDRESSSTATE AS tax_property_state,
  PROPERTYADDRESSZIP AS tax_property_zip,
  SITUSCOUNTY AS situs_county,
  PROPERTYUSEGROUP AS tax_property_use_group,
  PROPERTYUSESTANDARDIZED AS tax_property_use_standardized,
  TRY_CAST(AREABUILDING AS DOUBLE) AS building_area,
  TRY_CAST(AREALOTSF AS DOUBLE) AS lot_size_sqft,
  TRY_CAST(AREALOTACRES AS DOUBLE) AS lot_size_acres,
  TRY_CAST(YEARBUILT AS INTEGER) AS year_built,
  TRY_CAST(BEDROOMSCOUNT AS DOUBLE) AS bedrooms,
  TRY_CAST(BATHCOUNT AS DOUBLE) AS bathrooms,
  TRY_CAST(TAXMARKETVALUETOTAL AS DOUBLE) AS tax_market_value_total,
  TRY_CAST(TAXMARKETVALUELAND AS DOUBLE) AS tax_market_value_land,
  TRY_CAST(TAXMARKETVALUEIMPROVEMENTS AS DOUBLE) AS tax_market_value_improvements,
  TRY_CAST(TAXASSESSEDVALUETOTAL AS DOUBLE) AS tax_assessed_value_total
FROM tax
WHERE ATTOMID IS NOT NULL;
")

# ------------------------------------------------------------
# 6. Merge cleaned sales with tax/assessor data
# ------------------------------------------------------------
# Merge key: ATTOMID.

dbExecute(con, "
CREATE OR REPLACE VIEW housing_clean_merged AS
SELECT
  s.ATTOMID,
  s.TRANSACTIONID,
  s.DOCUMENTTYPECODE,
  s.ARMSLENGTHFLAG,
  s.county,
  s.property_address,
  s.property_city,
  s.property_state,
  s.property_zip,
  s.PROPERTYUSEGROUP,
  s.PROPERTYUSESTANDARDIZED,
  s.sale_amount,
  s.recording_date,
  s.sale_year,
  t.latitude,
  t.longitude,
  t.situs_county,
  t.building_area,
  t.lot_size_sqft,
  t.lot_size_acres,
  t.year_built,
  t.bedrooms,
  t.bathrooms,
  t.tax_market_value_total,
  t.tax_market_value_land,
  t.tax_market_value_improvements,
  t.tax_assessed_value_total
FROM clean_sales s
LEFT JOIN clean_tax t
ON s.ATTOMID = t.ATTOMID;
")

# ------------------------------------------------------------
# 7. Creating coordinate-ready version
# ------------------------------------------------------------

dbExecute(con, "
CREATE OR REPLACE VIEW housing_analysis_ready AS
SELECT *
FROM housing_clean_merged
WHERE latitude IS NOT NULL
  AND longitude IS NOT NULL;
")

# ------------------------------------------------------------
# 8. Basic merged-data check
# ------------------------------------------------------------
# This checks row counts, coordinate availability, sale values, and date range before excluding rows with missing coordinates.

housing_merged_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS clean_merged_rows,
  COUNT(latitude) AS rows_with_latitude,
  COUNT(longitude) AS rows_with_longitude,
  COUNT(*) - COUNT(latitude) AS rows_missing_latitude,
  COUNT(*) - COUNT(longitude) AS rows_missing_longitude,
  MIN(sale_amount) AS min_sale_amount,
  quantile_cont(sale_amount, 0.50) AS median_sale_amount,
  AVG(sale_amount) AS average_sale_amount,
  MAX(sale_amount) AS max_sale_amount,
  MIN(recording_date) AS earliest_date,
  MAX(recording_date) AS latest_date
FROM housing_clean_merged;
")

# ------------------------------------------------------------
# 9. Analysis-ready file check
# ------------------------------------------------------------
# This checks the coordinate-ready dataset

analysis_ready_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS analysis_ready_rows,
  MIN(sale_amount) AS min_sale_amount,
  quantile_cont(sale_amount, 0.25) AS p25_sale_amount,
  quantile_cont(sale_amount, 0.50) AS median_sale_amount,
  AVG(sale_amount) AS average_sale_amount,
  quantile_cont(sale_amount, 0.75) AS p75_sale_amount,
  MAX(sale_amount) AS max_sale_amount,
  MIN(recording_date) AS earliest_date,
  MAX(recording_date) AS latest_date
FROM housing_analysis_ready;
")

# ------------------------------------------------------------
# 10. Missing-value check
# ------------------------------------------------------------
# This checks common missing values in the coordinate-ready file.

missing_value_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,
  COUNT(CASE WHEN property_address IS NULL OR TRIM(property_address) = '' THEN 1 END) AS missing_property_address,
  COUNT(CASE WHEN property_city IS NULL OR TRIM(property_city) = '' THEN 1 END) AS missing_property_city,
  COUNT(CASE WHEN property_zip IS NULL OR TRIM(property_zip) = '' THEN 1 END) AS missing_property_zip,
  COUNT(CASE WHEN year_built IS NULL THEN 1 END) AS missing_year_built
FROM housing_analysis_ready;
")

# ------------------------------------------------------------
# 11. Suspicious-value check
# ------------------------------------------------------------
# This gave a quick count of values that may need treatment later.
# We didn't drop these rows here.

suspicious_value_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,
  COUNT(CASE WHEN building_area <= 0 THEN 1 END) AS zero_or_negative_building_area,
  COUNT(CASE WHEN lot_size_sqft <= 0 THEN 1 END) AS zero_or_negative_lot_size,
  COUNT(CASE WHEN bedrooms = 0 THEN 1 END) AS zero_bedrooms,
  COUNT(CASE WHEN bathrooms = 0 THEN 1 END) AS zero_bathrooms,
  COUNT(CASE WHEN tax_market_value_total <= 0 THEN 1 END) AS zero_or_negative_market_value,
  COUNT(CASE WHEN year_built IS NOT NULL AND year_built < 1800 THEN 1 END) AS year_built_before_1800,
  COUNT(CASE WHEN year_built IS NOT NULL AND year_built > sale_year THEN 1 END) AS year_built_after_sale_year
FROM housing_analysis_ready;
")

# ------------------------------------------------------------
# 12. Detailed zero-value check
# ------------------------------------------------------------
# This table does NOT assume zeros are missing.
# It simply separates missing, zero, negative, and positive values for building area, lot size, bedrooms, and bathrooms.

zero_value_detail_check <- dbGetQuery(con, "
WITH z AS (

  SELECT
    'building_area' AS variable,
    COUNT(*) AS total_rows,
    COUNT(CASE WHEN building_area IS NULL THEN 1 END) AS missing_count,
    COUNT(CASE WHEN building_area = 0 THEN 1 END) AS zero_count,
    COUNT(CASE WHEN building_area < 0 THEN 1 END) AS negative_count,
    COUNT(CASE WHEN building_area > 0 THEN 1 END) AS positive_count,
    MIN(CASE WHEN building_area > 0 THEN building_area END) AS min_positive,
    quantile_cont(CASE WHEN building_area > 0 THEN building_area END, 0.50) AS median_positive,
    AVG(CASE WHEN building_area > 0 THEN building_area END) AS average_positive,
    MAX(CASE WHEN building_area > 0 THEN building_area END) AS max_positive
  FROM housing_clean_merged

  UNION ALL

  SELECT
    'lot_size_sqft' AS variable,
    COUNT(*) AS total_rows,
    COUNT(CASE WHEN lot_size_sqft IS NULL THEN 1 END) AS missing_count,
    COUNT(CASE WHEN lot_size_sqft = 0 THEN 1 END) AS zero_count,
    COUNT(CASE WHEN lot_size_sqft < 0 THEN 1 END) AS negative_count,
    COUNT(CASE WHEN lot_size_sqft > 0 THEN 1 END) AS positive_count,
    MIN(CASE WHEN lot_size_sqft > 0 THEN lot_size_sqft END) AS min_positive,
    quantile_cont(CASE WHEN lot_size_sqft > 0 THEN lot_size_sqft END, 0.50) AS median_positive,
    AVG(CASE WHEN lot_size_sqft > 0 THEN lot_size_sqft END) AS average_positive,
    MAX(CASE WHEN lot_size_sqft > 0 THEN lot_size_sqft END) AS max_positive
  FROM housing_clean_merged

  UNION ALL

  SELECT
    'bedrooms' AS variable,
    COUNT(*) AS total_rows,
    COUNT(CASE WHEN bedrooms IS NULL THEN 1 END) AS missing_count,
    COUNT(CASE WHEN bedrooms = 0 THEN 1 END) AS zero_count,
    COUNT(CASE WHEN bedrooms < 0 THEN 1 END) AS negative_count,
    COUNT(CASE WHEN bedrooms > 0 THEN 1 END) AS positive_count,
    MIN(CASE WHEN bedrooms > 0 THEN bedrooms END) AS min_positive,
    quantile_cont(CASE WHEN bedrooms > 0 THEN bedrooms END, 0.50) AS median_positive,
    AVG(CASE WHEN bedrooms > 0 THEN bedrooms END) AS average_positive,
    MAX(CASE WHEN bedrooms > 0 THEN bedrooms END) AS max_positive
  FROM housing_clean_merged

  UNION ALL

  SELECT
    'bathrooms' AS variable,
    COUNT(*) AS total_rows,
    COUNT(CASE WHEN bathrooms IS NULL THEN 1 END) AS missing_count,
    COUNT(CASE WHEN bathrooms = 0 THEN 1 END) AS zero_count,
    COUNT(CASE WHEN bathrooms < 0 THEN 1 END) AS negative_count,
    COUNT(CASE WHEN bathrooms > 0 THEN 1 END) AS positive_count,
    MIN(CASE WHEN bathrooms > 0 THEN bathrooms END) AS min_positive,
    quantile_cont(CASE WHEN bathrooms > 0 THEN bathrooms END, 0.50) AS median_positive,
    AVG(CASE WHEN bathrooms > 0 THEN bathrooms END) AS average_positive,
    MAX(CASE WHEN bathrooms > 0 THEN bathrooms END) AS max_positive
  FROM housing_clean_merged
)

SELECT
  *,
  ROUND(100.0 * missing_count / total_rows, 3) AS missing_percent,
  ROUND(100.0 * zero_count / total_rows, 3) AS zero_percent,
  ROUND(100.0 * positive_count / total_rows, 3) AS positive_percent
FROM z;
")

# ------------------------------------------------------------
# 13. Bedroom and bathroom distributions
# ------------------------------------------------------------
# These shows whether zero bedroom/bathroom values are isolated cases or a larger pattern.

bedroom_distribution <- dbGetQuery(con, "
SELECT
  bedrooms,
  COUNT(*) AS rows
FROM housing_clean_merged
GROUP BY bedrooms
ORDER BY bedrooms;
")

bathroom_distribution <- dbGetQuery(con, "
SELECT
  bathrooms,
  COUNT(*) AS rows
FROM housing_clean_merged
GROUP BY bathrooms
ORDER BY bathrooms;
")

# ------------------------------------------------------------
# 14. Missing latitude/longitude check
# ------------------------------------------------------------
# This counts:
# - total cleaned merged rows
# - rows missing latitude or longitude
# - missing-coordinate rows that still have street address
# - missing-coordinate rows that have full address: street, city, state, ZIP

coordinate_missing_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_clean_merged_rows,

  COUNT(CASE
    WHEN latitude IS NULL OR longitude IS NULL
    THEN 1 END) AS rows_missing_lat_or_lon,

  COUNT(CASE
    WHEN (latitude IS NULL OR longitude IS NULL)
      AND TRIM(COALESCE(property_address, '')) <> ''
    THEN 1 END) AS missing_latlon_with_street_address,

  COUNT(CASE
    WHEN (latitude IS NULL OR longitude IS NULL)
      AND TRIM(COALESCE(property_address, '')) <> ''
      AND TRIM(COALESCE(property_city, '')) <> ''
      AND TRIM(COALESCE(property_state, '')) <> ''
      AND TRIM(COALESCE(property_zip, '')) <> ''
    THEN 1 END) AS missing_latlon_with_full_address

FROM housing_clean_merged;
")

# Save all rows that are missing latitude or longitude.
# These are kept separately for possible geocoding later.

missing_latlon_all <- dbGetQuery(con, "
SELECT *
FROM housing_clean_merged
WHERE latitude IS NULL
   OR longitude IS NULL;
")

# Save rows missing latitude/longitude but having full address information.
# These are the best candidates for future geocoding.

missing_latlon_with_full_address <- dbGetQuery(con, "
SELECT *
FROM housing_clean_merged
WHERE (latitude IS NULL OR longitude IS NULL)
  AND TRIM(COALESCE(property_address, '')) <> ''
  AND TRIM(COALESCE(property_city, '')) <> ''
  AND TRIM(COALESCE(property_state, '')) <> ''
  AND TRIM(COALESCE(property_zip, '')) <> '';
")

# ------------------------------------------------------------
# 15. Coordinate precision check
# ------------------------------------------------------------
# This checks the number of decimal digits in the original ATTOM latitude and longitude text values.

coordinate_precision_check <- dbGetQuery(con, "
WITH coord_digits AS (
  SELECT
    LENGTH(regexp_replace(split_part(TRIM(t.LATITUDE), '.', 2), '[^0-9]', '', 'g')) AS latitude_decimal_digits,
    LENGTH(regexp_replace(split_part(TRIM(t.LONGITUDE), '.', 2), '[^0-9]', '', 'g')) AS longitude_decimal_digits
  FROM housing_clean_merged h
  LEFT JOIN tax t
    ON h.ATTOMID = t.ATTOMID
  WHERE h.latitude IS NOT NULL
    AND h.longitude IS NOT NULL
    AND t.LATITUDE IS NOT NULL
    AND t.LONGITUDE IS NOT NULL
    AND TRIM(t.LATITUDE) <> ''
    AND TRIM(t.LONGITUDE) <> ''
)

SELECT
  latitude_decimal_digits,
  longitude_decimal_digits,
  COUNT(*) AS rows
FROM coord_digits
GROUP BY
  latitude_decimal_digits,
  longitude_decimal_digits
ORDER BY rows DESC;
")

# ------------------------------------------------------------
# 16. Year and county summaries
# ------------------------------------------------------------

sales_year_summary <- dbGetQuery(con, "
SELECT
  sale_year,
  COUNT(*) AS rows,
  quantile_cont(sale_amount, 0.50) AS median_sale_amount,
  AVG(sale_amount) AS average_sale_amount
FROM housing_analysis_ready
GROUP BY sale_year
ORDER BY sale_year;
")

sales_county_summary <- dbGetQuery(con, "
SELECT
  county,
  COUNT(*) AS rows,
  quantile_cont(sale_amount, 0.50) AS median_sale_amount,
  AVG(sale_amount) AS average_sale_amount
FROM housing_analysis_ready
GROUP BY county
ORDER BY rows DESC;
")


# ------------------------------------------------------------
# Full row counts for each main dataset version
# ------------------------------------------------------------

main_row_count_check <- dbGetQuery(con, "
SELECT
  (SELECT COUNT(*) FROM clean_sales) AS clean_sales_rows,

  (SELECT COUNT(*) FROM clean_tax) AS clean_tax_rows,

  (SELECT COUNT(*) FROM housing_clean_merged) AS housing_clean_merged_rows,

  (SELECT COUNT(*) FROM housing_analysis_ready) AS housing_analysis_ready_rows,

  (SELECT COUNT(*)
   FROM housing_clean_merged
   WHERE latitude IS NULL OR longitude IS NULL) AS missing_latlon_rows,

  (SELECT COUNT(*)
   FROM housing_clean_merged
   WHERE (latitude IS NULL OR longitude IS NULL)
     AND TRIM(COALESCE(property_address, '')) <> ''
     AND TRIM(COALESCE(property_city, '')) <> ''
     AND TRIM(COALESCE(property_state, '')) <> ''
     AND TRIM(COALESCE(property_zip, '')) <> ''
  ) AS missing_latlon_with_full_address_rows;
")


# ------------------------------------------------------------
# 17. Small CSV samples for visual inspection
# ------------------------------------------------------------
# sample of 1000 rows
# housing_analysis_ready_sample_1000: rows with latitude and longitude only
# housing_clean_merged_sample_1000: all cleaned merged rows, including missing-coordinate rows
# housing_missing_latlon_sample_1000:only rows missing latitude or longitude

housing_analysis_ready_sample_1000 <- dbGetQuery(con, "
SELECT *
FROM housing_analysis_ready
LIMIT 1000;
")

housing_clean_merged_sample_1000 <- dbGetQuery(con, "
SELECT *
FROM housing_clean_merged
LIMIT 1000;
")

housing_missing_latlon_sample_1000 <- dbGetQuery(con, "
SELECT *
FROM housing_clean_merged
WHERE latitude IS NULL
   OR longitude IS NULL
LIMIT 1000;
")

# ------------------------------------------------------------
# 18. Save all output CSV files
# ------------------------------------------------------------

fwrite(housing_merged_check,
       file.path(output_dir, "housing_merged_check.csv"))

fwrite(analysis_ready_check,
       file.path(output_dir, "housing_analysis_ready_check.csv"))

fwrite(missing_value_check,
       file.path(output_dir, "housing_analysis_ready_missing_value_check.csv"))

fwrite(suspicious_value_check,
       file.path(output_dir, "housing_analysis_ready_suspicious_value_check.csv"))

fwrite(zero_value_detail_check,
       file.path(output_dir, "zero_value_detail_check.csv"))

fwrite(bedroom_distribution,
       file.path(output_dir, "bedroom_distribution.csv"))

fwrite(bathroom_distribution,
       file.path(output_dir, "bathroom_distribution.csv"))

fwrite(coordinate_missing_summary,
       file.path(output_dir, "coordinate_missing_summary.csv"))

fwrite(missing_latlon_all,
       file.path(output_dir, "housing_missing_latlon_all.csv"))

fwrite(missing_latlon_with_full_address,
       file.path(output_dir, "housing_missing_latlon_with_full_address.csv"))

fwrite(coordinate_precision_check,
       file.path(output_dir, "coordinate_precision_check.csv"))

fwrite(main_row_count_check,
       file.path(output_dir, "main_row_count_check.csv"))

fwrite(sales_year_summary,
       file.path(output_dir, "sales_year_summary.csv"))

fwrite(sales_county_summary,
       file.path(output_dir, "sales_county_summary.csv"))

fwrite(housing_analysis_ready_sample_1000,
       file.path(output_dir, "housing_analysis_ready_sample_1000.csv"))

fwrite(housing_clean_merged_sample_1000,
       file.path(output_dir, "housing_clean_merged_sample_1000.csv"))

fwrite(housing_missing_latlon_sample_1000,
       file.path(output_dir, "housing_missing_latlon_sample_1000.csv"))

# ------------------------------------------------------------
# 19. Creating Parquet file
# ------------------------------------------------------------

save_parquet <- FALSE

if (save_parquet) {
  
  parquet_path <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  parquet_file <- file.path(output_dir, "housing_analysis_ready.parquet")
  
  if (file.exists(parquet_file)) {
    file.remove(parquet_file)
  }
  
  dbExecute(con, paste0(
    "COPY (
       SELECT *
       FROM housing_analysis_ready
     )
     TO '", parquet_path, "/housing_analysis_ready.parquet'
     (FORMAT PARQUET, COMPRESSION ZSTD);"
  ))
}

# ------------------------------------------------------------
# 20. Cleaning notes
# ------------------------------------------------------------
# Summary of the cleaning choices and limitations.

cleaning_notes <- data.table(
  step = c(
    "Clean sales",
    "Merge tax file",
    "Coordinate-ready file",
    "Missing values",
    "Zero-value check",
    "Missing coordinates",
    "Coordinate precision",
    "Important limitation"
  ),
  note = c(
    "Kept residential, likely arms-length sales from 1988 onward with sale amount between 1000 and 2210000.",
    "Merged clean sales with selected tax/assessor variables using ATTOMID.",
    "Created housing_analysis_ready with rows that have non-missing latitude and longitude for future distance analysis.",
    "Checked missing address, city, ZIP, and year_built values.",
    "Checked missing, zero, negative, and positive values for building_area, lot_size_sqft, bedrooms, and bathrooms. Zero values were counted separately and not automatically treated as missing.",
    "Saved rows with missing latitude/longitude separately, including rows with full address information for possible future geocoding.",
    "Checked decimal digits in original latitude/longitude values to see whether coordinate precision is uniform.",
    "Tax/assessor characteristics may reflect latest/current property records, not necessarily property condition at time of sale."
  )
)

fwrite(cleaning_notes,
       file.path(output_dir, "housing_cleaning_notes.csv"))

# ------------------------------------------------------------
# 21. Close connection and open output folder
# ------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)

shell.exec(output_dir)



























