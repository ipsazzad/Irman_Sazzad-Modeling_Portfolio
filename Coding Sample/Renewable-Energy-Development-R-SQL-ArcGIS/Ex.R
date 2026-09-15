require(sf) # spatial vector data
require(openxlsx2) # read/write xlsx
require(RSQLite) # Driver to speak with SQLite database
require(DBI) # read and write tables to a db
require(tidyverse) # our main data.frame toolset

## Script to make datbase of small mammals/herps from SREL
setwd("E:/Study/Spring 24/GIS/Assignments/Assignment 5")
#### 1) Read data Section ####
# read in spreadsheet of measurements
dat<- wb_read("./SQL_Example/SmallMammalData.xlsx", sheet = "FieldData") %>% 
  rename(SampDate = Date,
         DirOfMvmt = `Dir Of Mvmt`,
         HF_mm = `HF (mm)`,
         Tail_mm = `Tail (mm)`,
         Body_mm = `Body (mm)`,
         Wt_g = `Wt (g)`,
         Temp_H_F = `Temp (H)`,
         Temp_L_F = `Temp (L)`,
         Humidity_H_pct = `Humidity (H)`,
         Humidity_L_pct = `Humidity (L)`,
         BasalArea_m2_ha = `Basal Area (m^2/ha)`) %>% 
  mutate(Plot = as.character(Plot))

# get all names
str(dat)

# get the plots
plots<- st_read(dsn = "./SQL_Example", layer = "Plots") %>% 
  rename(Plot = PLOTID)

# get the traps
traps<- st_read(dsn = "./SQL_Example", layer = "TrapArrays") %>% 
  mutate(Plot = as.character(Plot))

# Get species table
spec<- wb_read("./SQL_Example/SmallMammalData.xlsx", sheet = "Species")

#### 2) Deal with Weather Data ####
wet<- dat %>% 
  select(SampDate, Temp_H_F:Moon) %>% 
  distinct() 

# # test for duplicate
# test<- wet %>% 
#   group_by(SampDate) %>% 
#   summarise(Count = n()) %>% 
#   filter(Count > 1)
# 
# tmp<- wet %>% 
#   filter(SampDate %in% test$SampDate)

#### 3) Basal Area ####
ba<- dat %>% 
  select(Plot, BasalArea_m2_ha) %>% 
  group_by(Plot) %>% 
  summarise(BasalArea_m2_ha = mean(BasalArea_m2_ha),
            Count = n())

plots<- plots %>% 
  left_join(ba %>% select(-Count), by = "Plot")

#### 4) Quality Check dat data.frame ####

# Do plots match?
unique(dat$Plot) %in% unique(plots$Plot)

# check all tests at once
all(unique(dat$Plot) %in% unique(plots$Plot))
any(unique(dat$Plot) %in% unique(plots$Plot))

# do all plot arrays match (Fixed - to CENTER)
unique(dat$PlotArray) %in% unique(traps$PlotArray)

dat %>% 
  group_by(PlotArray) %>% 
  summarise(Count = n())

# check species codes
unique(dat$SpecCode) %in% unique(spec$SpecCode)
all(unique(dat$SpecCode) %in% unique(spec$SpecCode))

#### 5) Prepare measurement data ####
tmp<- dat %>% 
  group_by(SampDate, Plot, PlotArray, SpecCode) %>% 
  summarise(Count = n()) 

# OK, we need pseudo pit tags, create a new column
dat$Replicate<- NA_integer_

outDF<- data.frame()
# loop through unique combos
for(i in 1:nrow(tmp)){
  # first, subset the dat df
  df<- dat %>% 
    filter(SampDate == tmp$SampDate[i] & Plot == tmp$Plot[i] & PlotArray == tmp$PlotArray[i] & SpecCode == tmp$SpecCode[i])
  
  # now loop through each subset row and assign replicate
  for(j in 1:nrow(df)){
    df$Replicate[j]<- j
  }
  
  # write output
  outDF<- bind_rows(outDF, df)
}

outDF %>% 
  group_by(SampDate, Plot, PlotArray, SpecCode, Replicate) %>% 
  summarise(Count = n()) %>% 
  filter(Count > 1)

# list which columns should be numeric
num<- c("SVL", "HF_mm", "Tail_mm", "Body_mm", "Wt_g")

# test coercing SVL
as.numeric(outDF$SVL)
unique(outDF$SVL)

tmp<- outDF %>% 
  group_by(SVL) %>% 
  summarise(Count = n())

# Update SVL
outDF<- outDF %>% 
  mutate(SVL = case_when(SVL %in% c("0", "-", ">150") ~ NA_character_,
                         TRUE ~ SVL)) %>% 
  mutate(SVL = as.numeric(SVL))

# HF stuff
unique(outDF$HF_mm)

outDF<- outDF %>% 
  mutate(HF_mm = case_when(HF_mm %in% c("-") ~ NA_character_,
                           TRUE ~ HF_mm)) %>% 
  mutate(HF_mm = as.numeric(HF_mm))

# Tail stuff
unique(outDF$Tail_mm)

outDF<- outDF %>% 
  mutate(Tail_mm = case_when(Tail_mm %in% c("-") ~ NA_character_,
                             TRUE ~ Tail_mm)) %>% 
  mutate(Tail_mm = as.numeric(Tail_mm))

# Body stuff
unique(outDF$Body_mm)

outDF<- outDF %>% 
  mutate(Body_mm = case_when(Body_mm %in% c("-") ~ NA_character_,
                             TRUE ~ Body_mm)) %>% 
  mutate(Body_mm = as.numeric(Body_mm))

# Wt stuff
unique(outDF$Wt_g)

tmp<- outDF %>% 
  group_by(Wt_g) %>% 
  summarise(Count = n())


outDF<- outDF %>% 
  mutate(Wt_g = case_when(Wt_g %in% c("-", ">60") ~ NA_character_,
                          TRUE ~ Wt_g)) %>% 
  mutate(Wt_g = as.numeric(Wt_g))

# Sex stuff
unique(outDF$Sex)

tmp<- outDF %>% 
  group_by(Sex) %>% 
  summarise(Count = n())

outDF<- outDF %>% 
  mutate(Sex = case_when(Sex %in% c("-") ~ "UNK",
                         Sex %in% c("M?") ~ "M",
                         Sex %in% c("E") ~ "F",
                         TRUE ~ Sex))

# Age stuff
unique(outDF$Age)

tmp<- outDF %>% 
  group_by(Age) %>% 
  summarise(Count = n())

outDF<- outDF %>% 
  mutate(Age = case_when(Age %in% c("-") ~ "UNK",
                         is.na(Age) ~ "UNK",
                         TRUE ~ Age))

# DirMvnt stuff
unique(outDF$DirOfMvmt)

tmp<- outDF %>% 
  group_by(DirOfMvmt) %>% 
  summarise(Count = n())

outDF<- outDF %>% 
  mutate(DirOfMvmt = case_when(DirOfMvmt %in% c("-") ~ "UNK",
                               is.na(DirOfMvmt) ~ "UNK",
                               TRUE ~ DirOfMvmt))

# Voucher stuff ---- Not touching this with 10ft pole
unique(outDF$Voucher)

# Breeding stuff
unique(outDF$Breeding)

outDF<- outDF %>% 
  mutate(Breeding = case_when(Breeding %in% c("-") ~ "UNK",
                              is.na(Breeding) ~ "UNK",
                              TRUE ~ Breeding))


#### 6) Prepare to store in db ####
datVert<- outDF %>% 
  select(Plot, PlotArray, SampDate, SpecCode, Replicate, DirOfMvmt, Sex:Breeding) %>% 
  pivot_longer(cols = DirOfMvmt:Breeding, names_to = "Param", values_to = "Value", values_drop_na = TRUE, values_transform = as.character)

# Make two vertical tables for numeric and character
numVert<- outDF %>% 
  select(Plot, PlotArray, SampDate, SpecCode, Replicate, SVL:Wt_g) %>% 
  pivot_longer(cols = SVL:Wt_g, names_to = "Param", values_to = "Value", values_drop_na = TRUE, values_transform = as.character)

chrVert<- outDF %>% 
  select(Plot, PlotArray, SampDate, SpecCode, Replicate, DirOfMvmt, Sex, Age, Voucher, Breeding) %>% 
  pivot_longer(cols = DirOfMvmt:Breeding, names_to = "Param", values_to = "Value", values_drop_na = TRUE)

# Make a vertical with all data
allVert<- outDF %>% 
  select(Plot, PlotArray, SampDate, SpecCode, Replicate, DirOfMvmt, Sex:Breeding) %>% 
  mutate(across(SVL:Wt_g, as.character)) %>% 
  pivot_longer(cols = DirOfMvmt:Breeding, names_to = "Param", values_to = "Value", values_drop_na = TRUE)

#### 7) Write to db ####
# Create a connection to our database, for SQLite if the file doesn't exist, you create the blank db here
db<- "./SQL_Example/SmallMammalDB.gpkg"


# write plots
st_write(plots, dsn = db, layer = "fc_plots", driver = "GPKG", delete_dsn = FALSE)

# write traps
st_write(traps, dsn = db, layer = "fc_traps", driver = "GPKG", delete_dsn = FALSE)

# what exists inside the db
st_layers(db)

# Connect to db via ODBC
dbConn<- dbConnect(SQLite(), db)

# what exists indide db, using odbc
dbListTables(dbConn)

# species
dbWriteTable(dbConn, name = "lu_species", value = spec)

# weather
# Recreate the table, using SQL
dbExecute(dbConn, 'CREATE TABLE "tbl_weather" (
  "SampDate"	TEXT NOT NULL,
  "Temp_H_F"	REAL,
  "Temp_L_F"	REAL,
  "Humidity_H_pct"	REAL,
  "Humidity_L_pct"	REAL,
  "Precip_current_day"	REAL,
  "Precip_prev_24_hours"	REAL,
  "Moon"	TEXT,
  PRIMARY KEY("SampDate")
)')
;
dbWriteTable(dbConn, name = "tbl_weather", value = wet %>%  mutate(SampDate = as.character(SampDate)), append = TRUE)

# write observation
dbExecute(dbConn, 'CREATE TABLE "tbl_observations" (
"Plot"	TEXT NOT NULL,
"PlotArray"	TEXT NOT NULL,
"SampDate"	TEXT NOT NULL,
"SpecCode"	TEXT NOT NULL,
"Replicate"	INTEGER NOT NULL,
"Param"	TEXT NOT NULL,
"Value"	TEXT,
FOREIGN KEY("SpecCode") REFERENCES "lu_species"("SpecCode") ON UPDATE CASCADE ON DELETE CASCADE,
PRIMARY KEY("Plot","PlotArray","SampDate","Replicate","SpecCode","Param")
)')

dbWriteTable(dbConn, name = "tbl_observations", value = allVert %>% mutate(SampDate = as.character(SampDate)), append = TRUE)


# Create metadata of value types
colTypes<- data.frame(Param = unique(allVert$Param),
                      DataType = c("chr", "chr", "chr", "chr", "chr", "num", "num", "num", "num", "num"))

# save to db
dbWriteTable(dbConn, name = "lu_params", value = colTypes)

dbDisconnect(dbConn)


#### 8) Demo retreiving data from DB ####

# Connect to db via ODBC
dbConn<- dbConnect(SQLite(), db)

# Question: How many captures of each species
dbListTables(dbConn)
# First get observations via brute force
obs<- dbReadTable(dbConn, "tbl_observations") %>% 
  distinct(Plot, PlotArray, SampDate, SpecCode, Replicate) %>% 
  group_by(SpecCode) %>% 
  summarise(Count = n()) %>% 
  left_join(dbReadTable(dbConn, "lu_species"), by = "SpecCode") %>% 
  select(CommonName, Count) %>% 
  arrange(CommonName)

# OK, now let's do some data coercion and summarization
# Get our params table
param<- dbReadTable(dbConn, "lu_params")

# Get data and summarize numeric measurements
meas<- dbReadTable(dbConn, "tbl_observations") %>% 
  filter(Param %in% param$Param[param$DataType %in% c("num")]) %>% 
  mutate(Value = as.numeric(Value)) %>% 
  group_by(SpecCode, Param) %>% 
  summarise(MeanVal = round(mean(Value, na.rm = TRUE), 1),
            SDVal = round(sd(Value, na.rm = TRUE), 1)) %>% 
  mutate(SDVal = replace_na(SDVal, replace = 0)) %>% 
  mutate(Combined = paste0(MeanVal, " (", SDVal, ")")) %>% 
  select(-c(MeanVal, SDVal)) %>% 
  pivot_wider(id_cols = SpecCode, names_from = Param, values_from = Combined, values_fill = "-")


#### 9) Do some work using SQL ####
# pretend 100's of millions of rows

pretend<- dbSendQuery(dbConn, "SELECT * FROM tbl_observations WHERE SpecCode IN('SOLO', 'AGCO')") %>% 
  dbFetch()

