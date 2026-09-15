# ------------------------------------------------------------
# HousingFinalCoordinates.R
# Adding geocoded coordinates back to cleaned housing data
# ------------------------------------------------------------
# Purpose:
# 1. Recreate cleaned housing sales + tax/assessor merged data.
# 2. Add geocoded coordinates for rows that were missing ATTOM lat/lon.
# 3. Create final latitude/longitude variables.
# 4. Track coordinate source: ATTOM_original, census, osm_fallback, arcgis_fallback, or missing.
# 5. Save final check files, sample file, and full final Parquet file.
# ------------------------------------------------------------

rm(list = ls())

library(data.table)
library(duckdb)
library(DBI)

# ------------------------------------------------------------
# 1. Folder paths
# ------------------------------------------------------------

base_dir <- "D:/Data_Center_Project/DataCentersOneDrive"
compiled_dir <- file.path(base_dir, "data", "housingDataCompiled")
output_dir <- file.path(base_dir, "output")

sales_file <- file.path(compiled_dir, "TN_sales.csv")
tax_file   <- file.path(compiled_dir, "TN_tax.csv")

geocoded_file <- file.path(output_dir, "housing_missing_latlon_geocoded_all_methods.csv")

sales_path <- normalizePath(sales_file, winslash = "/", mustWork = TRUE)
tax_path <- normalizePath(tax_file, winslash = "/", mustWork = TRUE)
geocoded_path <- normalizePath(geocoded_file, winslash = "/", mustWork = TRUE)

# ------------------------------------------------------------
# 2. Connecting to DuckDB
# ------------------------------------------------------------

con <- dbConnect(
  duckdb(),
  dbdir = file.path(output_dir, "housing_final_coordinates.duckdb")
)

# ------------------------------------------------------------
# 3. Reading files as DuckDB views
# ------------------------------------------------------------

dbExecute(con, "DROP VIEW IF EXISTS sales;")
dbExecute(con, "DROP VIEW IF EXISTS tax;")
dbExecute(con, "DROP VIEW IF EXISTS geocoded_missing;")

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

dbExecute(con, paste0(
  "CREATE OR REPLACE VIEW geocoded_missing AS
   SELECT *
   FROM read_csv_auto('", geocoded_path, "',
      strict_mode = false,
      all_varchar = true,
      null_padding = true
   );"
))

# ------------------------------------------------------------
# 4. Recreating cleaned sales data
# ------------------------------------------------------------
# Same cleaning rule as HousingCode1:
# residential only, likely arms-length sales, sale amount 1000-2210000, and sale dates from 1988 onward.

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
  year(TRY_CAST(RECORDINGDATE AS DATE)) AS sale_year
FROM sales
WHERE PROPERTYUSEGROUP = 'Residential'
  AND ARMSLENGTHFLAG = '1'
  AND TRY_CAST(TRANSFERAMOUNT AS DOUBLE) BETWEEN 1000 AND 2210000
  AND TRY_CAST(RECORDINGDATE AS DATE) >= DATE '1988-01-01'
  AND TRY_CAST(RECORDINGDATE AS DATE) <= DATE '2026-12-31';
")

# ------------------------------------------------------------
# 5. Recreating cleaned tax/property data
# ------------------------------------------------------------

dbExecute(con, "
CREATE OR REPLACE VIEW clean_tax AS
SELECT
  ATTOMID,
  TRY_CAST(LATITUDE AS DOUBLE) AS latitude,
  TRY_CAST(LONGITUDE AS DOUBLE) AS longitude,
  SITUSCOUNTY AS situs_county,
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
# 6. Merging sales + tax using ATTOMID
# ------------------------------------------------------------

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
# 7. Adding geocoded coordinates
# ------------------------------------------------------------
# If original ATTOM latitude/longitude exists, keep it.
# If original coordinates are missing, use geocoded coordinates.

dbExecute(con, "
CREATE OR REPLACE VIEW housing_final_coordinates AS
SELECT
  h.*,

  COALESCE(
    h.latitude,
    TRY_CAST(g.final_latitude_all_methods AS DOUBLE)
  ) AS final_latitude,

  COALESCE(
    h.longitude,
    TRY_CAST(g.final_longitude_all_methods AS DOUBLE)
  ) AS final_longitude,

  CASE
    WHEN h.latitude IS NOT NULL AND h.longitude IS NOT NULL
      THEN 'ATTOM_original'
    WHEN TRY_CAST(g.final_latitude_all_methods AS DOUBLE) IS NOT NULL
      AND TRY_CAST(g.final_longitude_all_methods AS DOUBLE) IS NOT NULL
      THEN g.final_geocode_source_all_methods
    ELSE 'missing'
  END AS coordinate_source

FROM housing_clean_merged h
LEFT JOIN geocoded_missing g
ON h.ATTOMID = g.ATTOMID
AND h.TRANSACTIONID = g.TRANSACTIONID;
")

# ------------------------------------------------------------
# 8. Final coordinate summary
# ------------------------------------------------------------

final_coordinate_summary <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,
  COUNT(CASE WHEN latitude IS NOT NULL AND longitude IS NOT NULL THEN 1 END) AS original_attom_coordinate_rows,
  COUNT(CASE WHEN latitude IS NULL OR longitude IS NULL THEN 1 END) AS originally_missing_coordinate_rows,
  COUNT(CASE WHEN final_latitude IS NOT NULL AND final_longitude IS NOT NULL THEN 1 END) AS final_rows_with_coordinates,
  COUNT(CASE WHEN final_latitude IS NULL OR final_longitude IS NULL THEN 1 END) AS final_rows_still_missing_coordinates
FROM housing_final_coordinates;
")

coordinate_source_summary <- dbGetQuery(con, "
SELECT
  coordinate_source,
  COUNT(*) AS rows
FROM housing_final_coordinates
GROUP BY coordinate_source
ORDER BY rows DESC;
")

# ------------------------------------------------------------
# 9. County match check
# ------------------------------------------------------------
# county comes from the sales/recording file.
# situs_county comes from the tax/assessor/property file.
# This checks whether recording county and property situs county match.

county_match_check <- dbGetQuery(con, "
SELECT
  COUNT(*) AS total_rows,

  COUNT(CASE
    WHEN county IS NOT NULL
      AND situs_county IS NOT NULL
      AND LOWER(TRIM(county)) = LOWER(TRIM(situs_county))
    THEN 1 END) AS same_county_rows,

  COUNT(CASE
    WHEN county IS NOT NULL
      AND situs_county IS NOT NULL
      AND LOWER(TRIM(county)) <> LOWER(TRIM(situs_county))
    THEN 1 END) AS different_county_rows,

  COUNT(CASE
    WHEN situs_county IS NULL OR TRIM(situs_county) = ''
    THEN 1 END) AS missing_situs_county_rows

FROM housing_final_coordinates;
")

county_mismatch_sample <- dbGetQuery(con, "
SELECT
  ATTOMID,
  TRANSACTIONID,
  county,
  situs_county,
  property_address,
  property_city,
  property_state,
  property_zip,
  final_latitude,
  final_longitude,
  coordinate_source
FROM housing_final_coordinates
WHERE county IS NOT NULL
  AND situs_county IS NOT NULL
  AND LOWER(TRIM(county)) <> LOWER(TRIM(situs_county))
LIMIT 1000;
")

# ------------------------------------------------------------
# 10. Save check files
# ------------------------------------------------------------

fwrite(
  final_coordinate_summary,
  file.path(output_dir, "final_coordinate_summary.csv")
)

fwrite(
  coordinate_source_summary,
  file.path(output_dir, "coordinate_source_summary.csv")
)

fwrite(
  county_match_check,
  file.path(output_dir, "county_match_check.csv")
)

fwrite(
  county_mismatch_sample,
  file.path(output_dir, "county_mismatch_sample.csv")
)

# ------------------------------------------------------------
# 11. Save sample file
# ------------------------------------------------------------

final_sample <- dbGetQuery(con, "
SELECT *
FROM housing_final_coordinates
LIMIT 1000;
")

fwrite(
  final_sample,
  file.path(output_dir, "housing_final_coordinates_sample_1000.csv")
)

# ------------------------------------------------------------
# 12. Saving full final coordinate-ready dataset as Parquet
# ------------------------------------------------------------

parquet_path <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
parquet_file <- file.path(output_dir, "housing_final_coordinates.parquet")

if (file.exists(parquet_file)) {
  file.remove(parquet_file)
}

dbExecute(con, paste0(
  "COPY (
     SELECT *
     FROM housing_final_coordinates
   )
   TO '", parquet_path, "/housing_final_coordinates.parquet'
   (FORMAT PARQUET, COMPRESSION ZSTD);"
))

# ------------------------------------------------------------
# 13. Print summaries in console
# ------------------------------------------------------------

print(final_coordinate_summary)
print(coordinate_source_summary)
print(county_match_check)

# ------------------------------------------------------------
# 14. Close connection and open output folder
# ------------------------------------------------------------

dbDisconnect(con, shutdown = TRUE)

shell.exec(output_dir)
