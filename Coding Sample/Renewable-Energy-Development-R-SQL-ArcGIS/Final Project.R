## Libraries

require(WDI) # World Development Indicator
require(openxlsx2) # rwork with xlsx
require(lubridate) # Date type mutation
require(ggplot2) # visualization
require(corrplot) # Correlation
require(ggthemes) # Specialized themes
require(gganimate) # Animations
require(gapminder) # Further visualization
require(RSQLite) # Driver to speak with SQLite database
require(DBI) # read and write tables to a db
require(tidyverse) # Data manipulation
require(summarytools) # Summary stats
require(stargazer) # Regression tables
require(plotly) # Interactive graphs
require(rgl) # 3D Scatter Plot
require(shiny) # Interactive web application on local machine
require(DT) # To tackle depricated Shiny data table output
require(gridExtra) # Arrange plots
require(heatmaply) # heatmap
require(caret) # Machine Learning
require(randomForest) # Regression using ML
require(cluster) # Clustering using ML
require(maps) # Get World map

## Cleaning

# Setting the working directory and importing data
setwd("E:/Study/Spring 24/GIS/Assignments/Project")
wdi <- read.csv("WDI.csv")
owid <- read.csv("OWID.csv")

# Fetch data from the World Bank's API for new indicators from 2016 to 2020 and presenting them in a tibble (more consistent table)
?WDI                                                                            # Help

wdi_new <- WDI(country = "all", 
               indicator = c("EG.FEC.RNEW.ZS", "NE.EXP.GNFS.CD"), 
               start = 2016, 
               end = 2020, 
               extra = FALSE, 
               cache = NULL, 
               latest = NULL, 
               language = "en") %>%
  as_tibble()                                                     

# Get unique country names from wdi
unique_countries <- distinct(wdi, Country.Name)
unique_countries

# Filter wdi_new to include only countries present in wdi
wdi_new_filtered <- wdi_new %>%
  filter(country %in% unique_countries$Country.Name) %>%
  distinct(country, .keep_all = TRUE)

# Merge wdi_new_filtered with wdi based on common countries using left join
merged_wdi <- wdi %>%
  left_join(wdi_new_filtered, by = c("Country.Name" = "country"))

# Filtering the OWID data from 2016 to 2020
owid <- owid %>%
  filter(year %in% c(2016, 2017, 2018, 2019, 2020))

# Renaming the OWID columns to match the WDI columns
owid <- rename(owid, Country.Name = country, Country.Code = iso_code, Time = year)

# Merging the modified WDI and OWID data based on the common columns
Development_raw <- merge(merged_wdi, owid[, c("Country.Name", "Country.Code", "Time", "electricity_demand", "energy_per_capita", "renewables_energy_per_capita")], 
                         by = c("Country.Name", "Country.Code", "Time"), all.x = TRUE)

# Replacing the '..' inputs with NA
Development_raw[Development_raw == ".."] <- NA

# Checking data 1
head(Development_raw, n = 10)

# Using separate & unite
Development_raw_1 <- separate(Development_raw, Time.Code, into = c("Format", "Year"), sep = 2)
Development_raw_2 <- unite(Development_raw_1, Time.Code, Format, Year, sep = "")

# Dropping the Time Code and Country Code columns from the data
Development_clean <- Development_raw[, !(names(Development_raw) %in% c("Time.Code", "Country.Code"))]

# Checking data 2
tail(Development_clean)

# printing the column names
colnames(Development_clean)

# Renaming the relevant variables
Development_clean <- Development_clean %>%
  rename(Country = Country.Name,
         Year = Time,
         GFCF = Gross.fixed.capital.formation..current.US....NE.GDI.FTOT.CD.,
         AE = Access.to.electricity....of.population...EG.ELC.ACCS.ZS.,
         TRD = Trade....of.GDP...NE.TRD.GNFS.ZS.,
         BM = Broad.money....of.GDP...FM.LBL.BMNY.GD.ZS.,
         FDI = Foreign.direct.investment..net.inflows....of.GDP...BX.KLT.DINV.WD.GD.ZS.,
         GDP_pc = GDP.per.capita..current.US....NY.GDP.PCAP.CD.,
         POP = Population..total..SP.POP.TOTL.,
         ED = electricity_demand,
         E_pc = energy_per_capita,
         RE_pc = renewables_energy_per_capita)

# Checking the type of the variables using select and summarize_all
Development_clean %>%
  select(Country, Year, GFCF, AE, TRD, BM, FDI, GDP_pc, POP, ED, E_pc, RE_pc) %>%
  summarise_all(class)

# Converting selected variables to date/numeric type and adding 7 mutated variables
Development_clean <- Development_clean %>%
  mutate(Year = as.Date(paste(Year, "-01-01", sep = ""), format = "%Y-%m-%d")) %>%
  mutate(across(c(GFCF, AE, TRD, BM, FDI, GDP_pc, POP, ED, E_pc, RE_pc), as.numeric)) %>%
  mutate(GFCF_pc = GFCF / POP,
         TRD_pc = TRD * GDP_pc,
         BM_pc = BM * GDP_pc,
         FDI_pc = FDI * GDP_pc,
         ED_pc = ED / POP,
         AE_pc = AE / 100,
         NRE_pc = E_pc - RE_pc)

# Reordering variables
Development_clean <- Development_clean %>%
  select(Country, Year, RE_pc, GFCF_pc, NRE_pc, AE_pc, ED_pc, E_pc, TRD_pc, BM_pc, 
         FDI_pc, GDP_pc, GFCF, ED, AE, TRD, BM, FDI, POP)


# Converting Electricity Demand per Capita to KWh from TWh in 'Development_clean'
Development_clean$ED_pc <- Development_clean$ED_pc * 1e6

# Using slice & bind_rows
Sliced_Development_clean_1 <- slice(Development_clean, 1:100)
Sliced_Development_clean_2 <- slice(Development_clean, 101:200)
Combined_Slices <- bind_rows(Sliced_Development_clean_1, Sliced_Development_clean_2)

# Using recursive to count NA values in each column using function
NA_recusrsive <- function(data, col_index = 1, counts = NULL) {
  if (col_index > ncol(data)) {
    return(counts)
  } else {
    na_count <- sum(is.na(data[[col_index]]))
    counts <- c(counts, na_count)
    return(NA_recusrsive(data, col_index + 1, counts))
  }
}
NA_counts <- NA_recusrsive(Combined_Slices)
print(NA_counts)

# Selecting and displaying specific variables from the Development_clean data
Primary_variables <- Development_clean[, c("Country", "Year", "RE_pc", "GFCF_pc", "NRE_pc", "AE_pc", "ED_pc")]

# Checking unique values using unique and lapply
unique(Primary_variables$Country)
lapply(Primary_variables[, c("Year" ,"RE_pc", "GFCF_pc", "NRE_pc", "AE_pc", "ED_pc")], unique)

# Dropping irrelevant country names using vectors, !, and %in% (element inclusion binary operator)
irrelevant_country_names <- c("Data from database: World Development Indicators", "Last Updated: 10/26/2023")
Primary_variables <- Primary_variables[!(Primary_variables$Country %in% irrelevant_country_names), ]                

# Dropping blank rows & removing row names in case any was previously assigned to the df
Primary_variables <- Primary_variables[-(1:3), ]              
row.names(Primary_variables) <- NULL                           

# Reading in a new xlsx file from Github which includes country with their respective regions
Regions <- wb_read("all.xlsx")

# Filtering country and region only
Regions <- Regions[, c('Country', 'region')]

# Merging previous data with Regions
Primary_variables <- merge(Primary_variables, Regions, by = 'Country', all.x = TRUE)

# Renaming and merging
Primary_variables <- Primary_variables %>%
  rename(Region = region)

# Counting how many countries are left
country_count <- length(unique(Primary_variables$Country))

# Checking how many countries are falling into which regions
Primary_variables %>%
  group_by(Region) %>%
  summarise(unique_countries = n_distinct(Country))

# Applying log for later representation
variables_with_log <- c('RE_pc', 'GFCF_pc', 'NRE_pc', 'AE_pc', 'ED_pc')
logged_variables <- lapply(Primary_variables[variables_with_log], log)
Primary_variables[paste0("l", variables_with_log)] <- logged_variables

# Replacing NA Regions with the correct Regions
country_to_region <- list(
  Asia = c("Hong Kong SAR, China", "Korea, Dem. People's Rep.", "Macao SAR, China", "West Bank and Gaza", "Yemen, Rep."),
  Europe = c("Channel Islands", "Kosovo", "Turkiye"),
  Americas = c("Bahamas, The", "Bolivia", "British Virgin Islands", "Curacao", "St. Kitts and Nevis", "St. Lucia", "St. Martin (French part)", "St. Vincent and the Grenadines", "Venezuela, RB"),
  Africa = c("Egypt, Arab Rep."),
  Oceania = c("Micronesia, Fed. Sts."))

# Incorporating it in the Original data using coalesce
Primary_variables <- Primary_variables %>%
  mutate(Region = coalesce(Region,
                           case_when(
                             Country %in% country_to_region$Asia ~ "Asia",
                             Country %in% country_to_region$Europe ~ "Europe",
                             Country %in% country_to_region$Americas ~ "Americas",
                             Country %in% country_to_region$Africa ~ "Africa",
                             Country %in% country_to_region$Oceania ~ "Oceania",
                             TRUE ~ NA_character_
                           )))

# Checking again how many countries are falling into which regions
Primary_variables %>%
  group_by(Region) %>%
  summarise(unique_countries = n_distinct(Country))

# Rearranging the data
Primary_variables <- Primary_variables %>%
  arrange(Region, Country, Year) %>% 
  select(Region, Country, Year, RE_pc, lRE_pc, NRE_pc, lNRE_pc, GFCF_pc, lGFCF_pc, AE_pc, lAE_pc, ED_pc, lED_pc)

# Trimming the day and month from Year and thereby converting it into character
Primary_variables <- Primary_variables %>% 
  mutate(Year = format(Year, "%Y"))

# Checking data types  
sapply(Primary_variables, class)                    

# Writing and saving the Primary_variables into csv
write.csv(Primary_variables, file = "Primary_variables.csv")

# Checking data 3
glimpse(Primary_variables)
str(Primary_variables)

# Locating the outliers for the logged variables
lvariables <- c("lRE_pc", "lNRE_pc", "lGFCF_pc", "lAE_pc", "lED_pc") # making a sub-dataframe for the logged variables
for (col in lvariables) {                                            # for loop iterating over each element
  if (is.numeric(Primary_variables[[col]])) {                        # Check if the column contains numeric values
    q1 <- quantile(Primary_variables[[col]], 0.25, na.rm = TRUE)     # Defining the quartiles
    q3 <- quantile(Primary_variables[[col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1                                                   # Defining Interquartile range
    non_na_values <- !is.na(Primary_variables[[col]])               # logical vector
    non_na_data <- Primary_variables[[col]][non_na_values]          # for non-NA values only
    lower_bound <- q1 - 2 * iqr
    upper_bound <- q3 + 2 * iqr
    outliers <- non_na_data[non_na_data < lower_bound | non_na_data > upper_bound] # lower bound < or > upper bound
    cat("Outliers for", col, ":", "\n")                              # printing names of columns with outliers
    print(outliers)                                                 # printing outlier values
    cat("\n")}}                                                     # printing empty line to separate outlier for each variable

# Checking if the variables have any missing values using if_esle
variables <- c("RE_pc", "NRE_pc", "GFCF_pc", "AE_pc", "ED_pc")

for (variable in variables) {
  if (any(is.na(Primary_variables[[variable]]))) {
    cat(paste(variable, "contains missing values.\n"))
  } else {
    cat(paste("There are no missing values in", variable, "\n"))
  }
}

# Checking which country & year combo in Oceania have NA values using boolian for RE_pc
oceania_data <- Primary_variables[Primary_variables$Region == "Oceania", ]  
result <- ifelse(oceania_data$RE_pc > 0, TRUE, FALSE)
result[is.na(result)] <- FALSE                     
for (i in 1:nrow(oceania_data)) {
  if (result[i]) {
    cat(oceania_data$Country[i], " - ", oceania_data$Year[i], ": TRUE\n")
  } else {
    cat(oceania_data$Country[i], " - ", oceania_data$Year[i], ": FALSE\n")
  }
}

# Check if the values for the variables are > 0 for each country and year combination using boolian
results_df <- data.frame(Region = character(),                     # Initialize an empty dataframe to store results
                         Variable = character(),
                         Country = character(),
                         Year = character(),
                         Result = logical(),
                         stringsAsFactors = FALSE)

regions <- c("Africa", "Asia", "Europe", "Americas", "Oceania")

for (region in regions) {
  region_data <- Primary_variables[Primary_variables$Region == region, ]
  
  cat("Region:", region, "\n")
  
  for (variable in variables) {
    result <- ifelse(region_data[[variable]] > 0, TRUE, FALSE)
    result[is.na(result)] <- FALSE  # Replace NA values with FALSE
    
    cat("Variable:", variable, "\n")
    
    # Create a dataframe to store the results for this variable
    variable_results <- data.frame(
      Region = region,
      Variable = variable,
      Country = region_data$Country,
      Year = region_data$Year,
      Result = result
    )
    
    # Append the df to results_df
    results_df <- rbind(results_df, variable_results)
  }
}

# Pivot results_df from long to wide format
results_df_wide <- pivot_wider(results_df, 
                               names_from = Variable, 
                               values_from = Result, 
                               values_fill = list(Result = FALSE))

# Creating Results Folder
if (!file.exists("results")) {dir.create("results")}

## Analysis and Graphing

# Deriving Descriptive Stats
output_file_sink <- "results/descriptive_stats.txt"
sink(output_file_sink)
descr(Primary_variables)
sink()

# Summary Stats (Mean and 95% CI) for Country
summary_stats_general <- Primary_variables %>%
  group_by(Country) %>%
  summarise(
    Mean_RE_pc = mean(RE_pc, na.rm = TRUE),
    CI95_Lower_RE_pc = quantile(RE_pc, 0.025, na.rm = TRUE),
    CI95_Upper_RE_pc = quantile(RE_pc, 0.975, na.rm = TRUE),
    Mean_NRE_pc = mean(NRE_pc, na.rm = TRUE),
    CI95_Lower_NRE_pc = quantile(NRE_pc, 0.025, na.rm = TRUE),
    CI95_Upper_NRE_pc = quantile(NRE_pc, 0.975, na.rm = TRUE),
    Mean_GFCF_pc = mean(GFCF_pc, na.rm = TRUE),
    CI95_Lower_GFCF_pc = quantile(GFCF_pc, 0.025, na.rm = TRUE),
    CI95_Upper_GFCF_pc = quantile(GFCF_pc, 0.975, na.rm = TRUE),
    Mean_AE_pc = mean(AE_pc, na.rm = TRUE),
    CI95_Lower_AE_pc = quantile(AE_pc, 0.025, na.rm = TRUE),
    CI95_Upper_AE_pc = quantile(AE_pc, 0.975, na.rm = TRUE),
    Mean_ED_pc = mean(ED_pc, na.rm = TRUE),
    CI95_Lower_ED_pc = quantile(ED_pc, 0.025, na.rm = TRUE),
    CI95_Upper_ED_pc = quantile(ED_pc, 0.975, na.rm = TRUE))

# Summary Stats using Pivot_longer for Country
long_data <- Primary_variables %>%
  pivot_longer(cols = c(RE_pc, NRE_pc, GFCF_pc, AE_pc, ED_pc), names_to = "Variable", values_to = "Value")
summary_stats_long <- long_data %>%
  group_by(Country, Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    CI95_Lower = quantile(Value, 0.025, na.rm = TRUE),
    CI95_Upper = quantile(Value, 0.975, na.rm = TRUE))

# Summary stats (and plot) for variables without Country 
summary_stats_all <- long_data %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    CI95_Lower = Mean - 1.96 * (SD / sqrt(n())),
    CI95_Upper = Mean + 1.96 * (SD / sqrt(n())))

ggplot(summary_stats_all, aes(x = Variable, y = Mean, color = Variable)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI95_Lower, ymax = CI95_Upper), width = 0.2) +
  labs(title = "Mean with 95% CI by Variable",
       x = "Variable",
       y = "Mean") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Summary Stats for region
aggregate <- Primary_variables %>%
  group_by(Region, Year) %>%
  summarise(
    Mean_RE_pc = mean(RE_pc, na.rm = TRUE),
    SD_RE_pc = replace_na(sd(RE_pc, na.rm = TRUE), 0),
    Mean_NRE_pc = mean(NRE_pc, na.rm = TRUE),
    SD_NRE_pc = replace_na(sd(NRE_pc, na.rm = TRUE), 0),
    Mean_GFCF_pc = mean(GFCF_pc, na.rm = TRUE),
    SD_GFCF_pc = replace_na(sd(GFCF_pc, na.rm = TRUE), 0),
    Mean_AE_pc = mean(AE_pc, na.rm = TRUE),
    SD_AE_pc = replace_na(sd(AE_pc, na.rm = TRUE), 0),
    Mean_ED_pc = mean(ED_pc, na.rm = TRUE),
    SD_ED_pc = replace_na(sd(ED_pc, na.rm = TRUE), 0),
    Count = n()) %>%
  mutate(across(starts_with("SD_"),~ round(., 1)))

# Summary Stats for OPEC & non-OPEC
Opec <- c("Algeria", "Congo", "Equatorial Guinea", "Gabon", "Iran", "Iraq",              # Assign countries to OPEC
          "Kuwait", "Libya", "Nigeria", "Saudi Arabia", "United Arab Emirates", "Venezuela")

Opec_Membership <- Primary_variables %>%
  mutate(OPEC_Status = case_when(
    Country %in% Opec ~ "OPEC",
    TRUE ~ "non-OPEC"))

average <- Opec_Membership %>%
  group_by(Year, OPEC_Status) %>%
  summarise(
    Avg_RE_pc = mean(RE_pc, na.rm = TRUE),
    Avg_NRE_pc = mean(NRE_pc, na.rm = TRUE),
    Avg_GFCF_pc = mean(GFCF_pc, na.rm = TRUE),
    Avg_AE_pc = mean(AE_pc, na.rm = TRUE),
    Avg_ED_pc = mean(ED_pc, na.rm = TRUE))

avg_table <- average %>%                                                        # WIder Format
  pivot_wider(
    names_from = OPEC_Status,
    values_from = c(Avg_RE_pc, Avg_NRE_pc),
    names_prefix = "Avg_")%>%
  rename(
    Avg_RE_pc_OPEC = `Avg_RE_pc_Avg_OPEC`,
    Avg_RE_pc_non_OPEC = `Avg_RE_pc_Avg_non-OPEC`,
    Avg_NRE_pc_OPEC = `Avg_NRE_pc_Avg_OPEC`,
    Avg_NRE_pc_non_OPEC = `Avg_NRE_pc_Avg_non-OPEC`
  )

# Graph colors
color_map <- c("Africa" = "yellow", "Americas" = "blue", "Asia" = "purple", "Europe" = "red", "Oceania" = "green")

# Making Scatter plot & fitted line of Renewable Energy Consumption to Gross Fixed Capital Formation with log transformation
ggplot(Primary_variables, aes(x = lGFCF_pc, y = lRE_pc)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "tomato") +
  labs(title = "Gross Fixed Capital Formation Vs Renewable Energy", 
       x = "lGFCF_pc", y = "lRE_pc") +
  theme_minimal()

# Making Scatter plot & fitted line of Renewable Energy Consumption to Nonrenewable Energy Consumption with log transformation
ggplot(Primary_variables, aes(x = lNRE_pc, y = lRE_pc)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(title = "Non-Renewable Energy Vs Renewable Energy", 
       x = "lNRE_pc", y = "lRE_pc") +
  theme_minimal()

# Making Scatter plot & fitted line of Renewable Energy Consumption to Access to Electricity with log transformation
ggplot(Primary_variables, aes(x = lAE_pc, y = lRE_pc)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Access to Electricity Vs Renewable Energy", 
       x = "lAE_pc", y = "lRE_pc") +
  theme_minimal()

# Making Scatter plot & fitted line of Renewable Energy Consumption to Electricity Demand with log transformation
ggplot(Primary_variables, aes(x = lED_pc, y = lRE_pc)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Electricity Demand Vs Renewable Energy", 
       x = "lED_ΜW_pc", y = "lRE_pc") +
  theme_minimal()

# Scatter Plot for countries for the most recent year (2020) using Geom Text
data_2020 <- subset(Primary_variables, Year == 2020)

ggplot(data_2020, aes(x = lNRE_pc, y = lRE_pc)) +
  geom_point() +
  geom_text(aes(label = Country), nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  labs(title = "Scatterplot of lNRE_pc vs lRE_pc for 2020", x = "lNRE_pc", y = "lRE_pc")

ggplot(data_2020, aes(x = lGFCF_pc, y = lRE_pc)) +
  geom_point() +
  geom_text(aes(label = Country), nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  labs(title = "Scatterplot of lGFCF_pc vs lRE_pc for 2020", x = "lGFCF_pc", y = "lRE_pc")

ggplot(data_2020, aes(x = lAE_pc, y = lRE_pc)) +
  geom_point() +
  geom_text(aes(label = Country), nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  labs(title = "Scatterplot of lAE_pc vs lRE_pc for 2020", x = "lAE_pc", y = "lRE_pc")

ggplot(data_2020, aes(x = lED_pc, y = lRE_pc)) +
  geom_point() +
  geom_text(aes(label = Country), nudge_x = 0.25, nudge_y = 0.01, check_overlap = TRUE) +
  labs(title = "Scatterplot of lED_pc vs lRE_pc for 2020", x = "lED_pc", y = "lRE_pc")

# 3D Scatter Plot for countries for the most recent year (2020)
data_20 <- Primary_variables %>%
  filter(Year == 2020)

plot3d(data_20$RE_pc, data_20$NRE_pc, data_20$GFCF_pc, col = "blue", type = "s", size = 2)
title3d(main = "3D Scatter Plot of RE_pc, NRE_pc, GFCF_pc for 2020")
grid3d("xyz")
view3d(theta = 30, phi = 30, fov = 70, zoom = 0.8)

# Creating a list to store the ggplot objects
plots <- list()

for (variable in lvariables) {                                     # Creating histograms for the logged variable
  plot <- ggplot(Primary_variables, aes(x = .data[[variable]])) +
    geom_histogram(fill = "skyblue", color = "black", bins = 20) +
    labs(title = paste("Histogram of", variable),
         x = variable,
         y = "Frequency") +
    theme_minimal()
  plots[[variable]] <- plot}
multiplot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 3))

# Plotly Example
nvariables <- c("lRE_pc", "lNRE_pc", "lGFCF_pc", "lAE_pc", "lED_pc")

for (variable in nvariables) {
  plot_data <- Primary_variables
  
  plot <- plot_ly(plot_data, x = ~.data[[variable]], type = "histogram") %>%
    layout(title = paste("Histogram of", variable),
           xaxis = list(title = variable),
           yaxis = list(title = "Frequency"))
  
  print(plot)
}

# Constructing Pairplots
pairs(Primary_variables[, c('lRE_pc', 'lNRE_pc','lGFCF_pc', 'lAE_pc', 'lED_pc')], 
      labels = c('lRE_pc', 'lNRE_pc','lGFCF_pc', 'lAE_pc', 'lED_pc'),
      col = 'blue', pch = 16,
      main = "Pair Plot of the Variables", cex.main = 0.5)

# Testing Relationships
NRE_to_RE <- ggplot(Primary_variables, aes(x = lNRE_pc, y = lRE_pc)) +
  geom_point(aes(fill = Region)) + 
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Asia", ], aes(x = lNRE_pc, y = lRE_pc, fill = "Asia"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Europe", ], aes(x = lNRE_pc, y = lRE_pc, fill = "Europe"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Americas", ], aes(x = lNRE_pc, y = lRE_pc, fill = "Americas"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Oceania", ], aes(x = lNRE_pc, y = lRE_pc, fill = "Oceania"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Africa", ], aes(x = lNRE_pc, y = lRE_pc, fill = "Africa"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  scale_fill_manual(name = "Region",
                    values = c(Asia = "black", Europe = "blue", Americas = "green", Oceania = "yellow", Africa = "magenta")) +
  guides(fill = guide_legend(title = "Region"))
NRE_to_RE

GFCF_to_RE <- ggplot(Primary_variables, aes(x = lGFCF_pc, y = lRE_pc)) +
  geom_point(aes(fill = Region)) + 
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Asia", ], aes(x = lGFCF_pc, y = lRE_pc, fill = "Asia"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Europe", ], aes(x = lGFCF_pc, y = lRE_pc, fill = "Europe"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Americas", ], aes(x = lGFCF_pc, y = lRE_pc, fill = "Americas"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Oceania", ], aes(x = lGFCF_pc, y = lRE_pc, fill = "Oceania"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Africa", ], aes(x = lGFCF_pc, y = lRE_pc, fill = "Africa"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  scale_fill_manual(name = "Region",
                    values = c(Asia = "black", Europe = "blue", Americas = "green", Oceania = "yellow", Africa = "magenta")) +
  guides(fill = guide_legend(title = "Region"))
GFCF_to_RE

AE_to_RE <- ggplot(Primary_variables, aes(x = lAE_pc, y = lRE_pc)) +
  geom_point(aes(fill = Region)) + 
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Asia", ], aes(x = lAE_pc, y = lRE_pc, fill = "Asia"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Europe", ], aes(x = lAE_pc, y = lRE_pc, fill = "Europe"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Americas", ], aes(x = lAE_pc, y = lRE_pc, fill = "Americas"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Oceania", ], aes(x = lAE_pc, y = lRE_pc, fill = "Oceania"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Africa", ], aes(x = lAE_pc, y = lRE_pc, fill = "Africa"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  scale_fill_manual(name = "Region",
                    values = c(Asia = "black", Europe = "blue", Americas = "green", Oceania = "yellow", Africa = "magenta")) +
  guides(fill = guide_legend(title = "Region"))
AE_to_RE

ED_to_RE <- ggplot(Primary_variables, aes(x = lED_pc, y = lRE_pc)) +
  geom_point(aes(fill = Region)) + 
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Asia", ], aes(x = lED_pc, y = lRE_pc, fill = "Asia"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Europe", ], aes(x = lED_pc, y = lRE_pc, fill = "Europe"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Americas", ], aes(x = lED_pc, y = lRE_pc, fill = "Americas"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Oceania", ], aes(x = lED_pc, y = lRE_pc, fill = "Oceania"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  stat_ellipse(data = Primary_variables[Primary_variables$Region == "Africa", ], aes(x = lED_pc, y = lRE_pc, fill = "Africa"), 
               geom = "polygon", level = 0.99, alpha = 0.15) +
  scale_fill_manual(name = "Region",
                    values = c(Asia = "black", Europe = "blue", Americas = "green", Oceania = "yellow", Africa = "magenta")) +
  guides(fill = guide_legend(title = "Region"))
ED_to_RE

# Primary Variable with imputed values for NA
PV <- Primary_variables %>%                    # Drop the logged variables from Primary_variables
  select(-starts_with("l"))

replace_na_with_mean_specific <- function(x) {
  if (is.numeric(x)) {
    mean_value <- mean(x, na.rm = TRUE)     # Define function to replace NA with column means for numeric variables
    replace(x, is.na(x), mean_value)
  } else {
    x
  }
}

Imputed_PV <- PV %>%
  mutate(across(c(RE_pc, NRE_pc, GFCF_pc, AE_pc, ED_pc), replace_na_with_mean_specific))

log_specific <- function(x) {               # computes ln of the inputs
  log(x)
}

LV <- Imputed_PV %>%
  mutate(across(c(RE_pc, NRE_pc, GFCF_pc, AE_pc, ED_pc), log_specific)) %>%  # further mutate and rename
  rename_with(~paste0("l_", .), c(RE_pc, NRE_pc, GFCF_pc, AE_pc, ED_pc))

Clean_PV <- left_join(Imputed_PV, LV) %>%                                    # merging with left_join
  select(Region, Country, Year, 
         RE_pc, l_RE_pc, 
         NRE_pc, l_NRE_pc, 
         GFCF_pc, l_GFCF_pc, 
         AE_pc, l_AE_pc, 
         ED_pc, l_ED_pc) %>%
  mutate_if(is.numeric, ~ round(., digits = 1))                              # Nesting

# Checking Clean_PV
str(Clean_PV)         # Check structure
any(is.na(Clean_PV))  # Check for missing values
summary(Clean_PV)     # Check basic descriptive stats
dim(Clean_PV)         # Check dimension

# Anova
Anova<-aov(RE_pc ~ NRE_pc + GFCF_pc + AE_pc + ED_pc, data = Clean_PV)
summary(Anova)

# Connected Scatterplots
Mean_vars <- Clean_PV %>%                                                       # mean of the variables for region & year
  filter(Region %in% c("Africa", "Americas", "Europe", "Asia", "Oceania")) %>%
  group_by(Region, Year) %>%
  summarise(
    mean_RE_pc = mean(RE_pc),
    mean_NRE_pc = mean(NRE_pc),
    mean_GFCF_pc = mean(GFCF_pc),
    mean_AE_pc = mean(AE_pc),
    mean_ED_pc = mean(ED_pc))
Mean_vars_long <- pivot_longer(Mean_vars,                                       # long format
                               cols = starts_with("mean_"), 
                               names_to = "Variable", 
                               values_to = "Mean_Value")  

Mean_vars_long$Variable <- factor(Mean_vars_long$Variable, levels = c("mean_RE_pc", "mean_NRE_pc", "mean_GFCF_pc", "mean_AE_pc", "mean_ED_pc"))                                              # Ordering variables accordingly

ggplot(Mean_vars_long, aes(x = Year, y = Mean_Value, group = Region, color = Region)) +
  geom_line() +
  geom_point() +
  labs(title = "Mean Values Over Years (2016-2020)",
       x = "Year",
       y = "Mean Value",
       color = "Region") +
  facet_wrap(~ Variable, scales = "free_y", nrow = 1)

#Imported Theme
solarized_colors <- c("#002b36", "#073642", "#586e75", "#657b83", "#839496", "#93a1a1", "#eee8d5", "#fdf6e3")
Theme <- theme_minimal() + theme(panel.background = element_rect(fill = solarized_colors[7]),plot.title = element_text(color = solarized_colors[2]),axis.title.x = element_text(color = solarized_colors[2]),axis.title.y = element_text(color = solarized_colors[2]),axis.text.x = element_text(color = solarized_colors[2]),axis.text.y = element_text(color = solarized_colors[2]),panel.grid.major = element_line(color = solarized_colors[3]),panel.grid.minor = element_blank())

# Plotting the distributions using economist theme
ggplot(data = Clean_PV, aes(x = RE_pc)) + geom_histogram(fill = "yellow") + theme_economist() + labs(title = "RE_pc", x = "kWh")
ggplot(data = Clean_PV, aes(x = NRE_pc)) + geom_histogram(fill = "blue") + theme_economist() + labs(title = "NRE_pc", x = "kWh")
ggplot(data = Clean_PV, aes(x = GFCF_pc)) + geom_histogram(fill = "purple") + theme_economist() + labs(title = "GFCF_pc", x = "$")
ggplot(data = Clean_PV, aes(x = AE_pc)) + geom_histogram(fill = "red") + theme_economist() + labs(title = "AE_pc", x = "kWh")
ggplot(data = Clean_PV, aes(x = ED_pc)) + geom_histogram(fill = "green") + theme_economist() + labs(title = "ED_pc", x = "kWh")

# Density charts for each variable and using grid arrange
density_plots <- list(
  ggplot(data = Clean_PV, aes(x = RE_pc)) +
    geom_density(fill = "yellow", color = "black", alpha = 0.8) +
    theme_economist() +
    labs(title = "Density of RE_pc", x = "kWh"),
  
  ggplot(data = Clean_PV, aes(x = NRE_pc)) +
    geom_density(fill = "blue", color = "black", alpha = 0.8) +
    theme_economist() +
    labs(title = "Density of NRE_pc", x = "kWh"),
  
  ggplot(data = Clean_PV, aes(x = GFCF_pc)) +
    geom_density(fill = "purple", color = "black", alpha = 0.8) +
    theme_economist() +
    labs(title = "Density of GFCF_pc", x = "$"),
  
  ggplot(data = Clean_PV, aes(x = AE_pc)) +
    geom_density(fill = "red", color = "black", alpha = 0.8) +
    theme_economist() +
    labs(title = "Density of AE_pc", x = "kWh"),
  
  ggplot(data = Clean_PV, aes(x = ED_pc)) +
    geom_density(fill = "green", color = "black", alpha = 0.8) +
    theme_economist() +
    labs(title = "Density of ED_pc", x = "kWh")
)
grid.arrange(grobs = density_plots, ncol = 2)

# Using facet_wrap and facet_grid
ggplot(data = Clean_PV, aes(x = RE_pc)) + 
  geom_histogram(fill = "yellow") + 
  theme_economist() + 
  labs(title = "RE_pc", x = "kWh") +
  facet_wrap(~ Region, ncol = 2)

ggplot(data = Clean_PV, aes(x = RE_pc)) + 
  geom_histogram(fill = "yellow") + 
  theme_economist() + 
  labs(title = "RE_pc", x = "kWh") +
  facet_grid(Region ~ .)

ggplot(data = Clean_PV, aes(x = NRE_pc)) + 
  geom_histogram(fill = "blue") + 
  theme_economist() + 
  labs(title = "NRE_pc", x = "kWh") +
  facet_wrap(~ Region, ncol = 2)

ggplot(data = Clean_PV, aes(x = NRE_pc)) + 
  geom_histogram(fill = "blue") + 
  theme_economist() + 
  labs(title = "NRE_pc", x = "kWh") +
  facet_grid(Region ~ .)

ggplot(data = Clean_PV, aes(x = GFCF_pc)) + 
  geom_histogram(fill = "purple") + 
  theme_economist() + 
  labs(title = "GFCF_pc", x = "$") +
  facet_wrap(~ Region, ncol = 2)

ggplot(data = Clean_PV, aes(x = GFCF_pc)) + 
  geom_histogram(fill = "purple") + 
  theme_economist() + 
  labs(title = "GFCF_pc", x = "$") +
  facet_grid(Region ~ .)

ggplot(data = Clean_PV, aes(x = AE_pc)) + 
  geom_histogram(fill = "red") + 
  theme_economist() + 
  labs(title = "AE_pc", x = "kWh") +
  facet_wrap(~ Region, ncol = 2)

ggplot(data = Clean_PV, aes(x = AE_pc)) + 
  geom_histogram(fill = "red") + 
  theme_economist() + 
  labs(title = "AE_pc", x = "kWh") +
  facet_grid(Region ~ .)

ggplot(data = Clean_PV, aes(x = ED_pc)) + 
  geom_histogram(fill = "green") + 
  theme_economist() + 
  labs(title = "ED_pc", x = "kWh") +
  facet_wrap(~ Region, ncol = 2)

ggplot(data = Clean_PV, aes(x = ED_pc)) + 
  geom_histogram(fill = "green") + 
  theme_economist() + 
  labs(title = "ED_pc", x = "kWh") +
  facet_grid(Region ~ .)

# Solarized graphs using ggtitle, and ggsave
plot1 <- ggplot(data = Clean_PV, aes(x = l_RE_pc)) + geom_histogram(fill = solarized_colors[3]) + Theme + ggtitle("l_RE_pc") + xlab("kWh")
plot2 <- ggplot(data = Clean_PV, aes(x = l_NRE_pc)) + geom_histogram(fill = solarized_colors[3]) + Theme + ggtitle("l_NRE_pc") + xlab("kWh")
plot3 <- ggplot(data = Clean_PV, aes(x = l_GFCF_pc)) + geom_histogram(fill = solarized_colors[3]) + Theme + ggtitle("l_GFCF_pc") + xlab("$")
plot4 <- ggplot(data = Clean_PV, aes(x = l_AE_pc)) + geom_histogram(fill = solarized_colors[3]) + Theme + ggtitle("l_AE_pc") + xlab("kWh")
plot5 <- ggplot(data = Clean_PV, aes(x = l_ED_pc)) + geom_histogram(fill = solarized_colors[3]) + Theme + ggtitle("l_ED_pc") + xlab("kWh")
combined_plots <- grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 2)
ggsave("results/combined_plots.png", combined_plots, width = 10, height = 8, units = "in")

# Correlation using jpeg & dev.off
jpeg(file ="results/Clean_PV_correlation.jpg", units = "in", height = 6, width = 6, res = 300) # Connecting to a JPEG file and specifying the dimensions
correlation_matrix <- cor(Clean_PV[, c("RE_pc", "NRE_pc", "GFCF_pc", "AE_pc", "ED_pc")])
corrplot(correlation_matrix, method = "color") # Correlation using color
dev.off() # closes the device connection saving to the JPEG file

# Database using Clean_PV
db_file <- "E:/Study/Spring 24/GIS/Assignments/Project/my_database.db"     # Setting path to SQLite DB file
DBconn <- dbConnect(RSQLite::SQLite(), dbname = db_file)                  # Connecting to DB
DBI::dbWriteTable(DBconn, name = "Clean_PV", value = Clean_PV, append = TRUE)    # Writing to DB
dbListTables(DBconn)                                                            # Listing tables in DB
query <- "SELECT * FROM Clean_PV"                                               # Defining SQL query
Clean_PV_from_db <- dbGetQuery(DBconn, query)                                # Execute query & retrieve data from DB

# Heatmap (Relativeness) using DB
?heatmap
mean_data <- dbReadTable(DBconn, "Clean_PV") %>%                      # Finding the means for the variables
  group_by(Region) %>%
  summarise(
    mean_RE_pc = mean(RE_pc),
    mean_NRE_pc = mean(NRE_pc),
    mean_GFCF_pc = mean(GFCF_pc),
    mean_AE_pc = mean(AE_pc),
    mean_ED_pc = mean(ED_pc)
  )
mat <- as.matrix(mean_data[, -1])             # Converting summarized data (excluding the first column) in a matrix
rownames(mat) <- mean_data$Region             # Setting row names from mean_data to the matrix, mat
heatmaply(mat, 
          dendrogram = "none",
          xlab = "", ylab = "", 
          main = "Mean Values by Region",
          scale = "column",
          margins = c(60, 100, 40, 20),
          grid_color = "white",
          grid_width = 0.00001,
          titleX = FALSE,
          hide_colorbar = TRUE,
          branches_lwd = 0.1,
          label_names = c("Region", "Variable:", "Mean Value"),
          fontsize_row = 12, fontsize_col = 12,
          labCol = colnames(mat),
          labRow = rownames(mat),
          heatmap_layers = theme(axis.line=element_blank()))

dbDisconnect(DBconn)                                            # Disconnecting DB

# Regression using Clean_PV
Regression <- lm(RE_pc ~ NRE_pc + GFCF_pc + AE_pc + ED_pc, data = Clean_PV)
summary(Regression)
Regression_file <- "Regression_summary.html"
stargazer(Regression, type = "html", out = paste0("results/", Regression_file))

# Regression using ML
response_var <- "RE_pc"
predictor_vars <- c("NRE_pc", "GFCF_pc", "AE_pc", "ED_pc")

# Split the data randomly into training and testing sets (70% training, 30% testing)
set.seed(123)                                                               # For reproducibility
train_index <- createDataPartition(Clean_PV[[response_var]], p = 0.7, list = FALSE) # Output should be a vector and not a list
train_data <- Clean_PV[train_index, ]
test_data <- Clean_PV[-train_index, ]

# Define the regression model (Random Forest), Predict & Evaluate
model <- randomForest(RE_pc ~ ., data = train_data[, c(predictor_vars, "RE_pc")], ntree = 100)
predictions <- predict(model, newdata = test_data)                     
rmse <- sqrt(mean((predictions - test_data[[response_var]])^2))        # Root Mean Squared Error
mae <- mean(abs(predictions - test_data[[response_var]]))              # Mean Absolute Error
r_squared <- cor(predictions, test_data[[response_var]])^2
print(paste("RMSE:", rmse))
print(paste("MAE:", mae))
print(paste("R-squared:", r_squared))

# Fit the regression model using ML
model_ml <- train(RE_pc ~ NRE_pc + GFCF_pc + AE_pc + ED_pc, data = train_data,
                  method = "lm")
predictions_ml <- predict(model_ml, newdata = test_data)       # Make predictions on the test data
par(mar = c(5, 5, 4, 2))                                       # Adjust the margin values
plot(test_data$RE_pc, predictions_ml, 
     xlab = "Actual RE_pc", ylab = "Predicted RE_pc",          # Plot the actual vs. predicted values
     main = "Actual vs. Predicted RE_pc using ML")
abline(lm(predictions_ml ~ test_data$RE_pc), col = "magenta", lwd = 3)  # Best-fitted line (with customized width)

# K-means Clustering
clustering_data <- Clean_PV[, c("RE_pc", "NRE_pc", "GFCF_pc", "AE_pc", "ED_pc")]
set.seed(123)
k <- 3                                                              # Number of clusters
kmeans_model <- kmeans(clustering_data, centers = k)                
kmeans_model$centers                        # View cluster centers (centroids of each cluster in the feature space)
clusters <- kmeans_model$cluster                #Assign each data point to a cluster based on the clustering results
Clean_PV_with_clusters <- cbind(Clean_PV, Cluster = clusters)       # Append cluster information to the Clean_PV 
plot(clustering_data, col = clusters, main = "k-means Clustering")  # Plot
points(kmeans_model$centers, col = 1:k, pch = 8, cex = 2)

# Map (Density for the 5 year average of the variables for each country on a world map)
world_map <- map_data("world")
merged_map <- merge(world_map, Clean_PV, by.x = "region", by.y = "Country", all.x = TRUE)

ggplot(merged_map, aes(long, lat, group = group, fill = RE_pc)) +
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "green", name = "RE_pc Density") +
  theme_void() +
  labs(title = "Average RE_pc Density by Country")

ggplot(merged_map, aes(long, lat, group = group, fill = NRE_pc)) +
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "green", name = "NRE_pc Density") +
  theme_void() +
  labs(title = "Average NRE_pc Density by Country")

ggplot(merged_map, aes(long, lat, group = group, fill = GFCF_pc)) +
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "green", name = "GFCF_pc Density") +
  theme_void() +
  labs(title = "Average GFCF_pc Density by Country")

ggplot(merged_map, aes(long, lat, group = group, fill = AE_pc)) +
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "green", name = "AE_pc Density") +
  theme_void() +
  labs(title = "Average AE_pc Density by Country")

ggplot(merged_map, aes(long, lat, group = group, fill = ED_pc)) +
  geom_polygon() +
  scale_fill_gradient(low = "red", high = "green", name = "ED_pc Density") +
  theme_void() +
  labs(title = "Average ED_pc Density by Country")

# Shiny Example: Interactive Data Explorer
ui <- fluidPage(
  titlePanel("Interactive Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Select Region:",
                  choices = c("All", unique(Clean_PV$Region))),
      selectInput("year", "Select Year:",
                  choices = c("All", unique(Clean_PV$Year)))
    ),
    mainPanel(
      plotlyOutput("scatterplot"),
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    data <- Clean_PV
    if (input$region != "All") {
      data <- data[data$Region == input$region, ]
    }
    if (input$year != "All") {
      data <- data[data$Year == input$year, ]
    }
    data
  })

  output$scatterplot <- renderPlotly({
    plot <- ggplot(filtered_data(), aes(x = RE_pc, y = NRE_pc, color = Region)) +
      geom_point() +
      theme_minimal()
    ggplotly(plot)
  })

  output$table <- renderDT({
    filtered_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Primary Variable without the rows containing NA values
Reduced_PV <- Primary_variables %>%
  filter(!is.na(RE_pc) & !is.na(GFCF_pc) & !is.na(NRE_pc) & !is.na(AE_pc) & !is.na(ED_pc))
View(Reduced_PV)

# Calculate average growth rate for each region from Reduced_PV
Growth_PV <- Reduced_PV %>%
  group_by(Region) %>%
  summarise(
    avg_RE_pc_growth = mean(((RE_pc - lag(RE_pc))/lag(RE_pc)) * 100, na.rm = TRUE),
    avg_NRE_pc_growth = mean(((NRE_pc - lag(NRE_pc))/lag(NRE_pc)) * 100, na.rm = TRUE),
    avg_GFCF_pc_growth = mean(((GFCF_pc - lag(GFCF_pc))/lag(GFCF_pc)) * 100, na.rm = TRUE),
    avg_AE_pc_growth = mean(((AE_pc - lag(AE_pc))/lag(AE_pc)) * 100, na.rm = TRUE),
    avg_ED_pc_growth = mean(((ED_pc - lag(ED_pc))/lag(ED_pc)) * 100, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(across(starts_with("avg_"), ~round(., 1)))

# Reshape the data from wide to long format
Growth_PV_long <- Growth_PV %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "Variable", values_to = "Average_Growth")

# Create bar plots for each variable
plots <- lapply(unique(Growth_PV_long$Variable), function(var) {
  ggplot(data = filter(Growth_PV_long, Variable == var), aes(x = Region, y = Average_Growth, fill = Region)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Average Growth Rate for", var), x = "Region", y = "Average Growth Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})
grid.arrange(grobs = plots, ncol = 2)

# Forecasting
all_predictions <- list()     # Creating list to store predictions for each country
Reduced_PV$Year <- as.numeric(Reduced_PV$Year)

# Loop through each unique country
for (country in unique(Reduced_PV$Country)) {
  country_data <- subset(Reduced_PV, Country == country)
  
  # Fit a linear regression model for each variable
  lm_models <- lapply(c("RE_pc", "NRE_pc", "GFCF_pc", "AE_pc", "ED_pc"), function(var) {
    model <- lm(as.formula(paste(var, " ~ Year")), data = country_data)
    return(model)})
  
  # Create prediction data for the years 2021 to 2025
  predict_data <- data.frame(Year = 2021:2025)
  
  # Make predictions for each variable
  predictions <- lapply(lm_models, function(model) {
    predict(model, newdata = predict_data)})
  
  # Store predictions for the current country
  country_predictions <- data.frame(
    Country = country,
    Year = predict_data$Year,
    RE_pc = predictions[[1]],
    NRE_pc = predictions[[2]],
    GFCF_pc = predictions[[3]],
    AE_pc = predictions[[4]],
    ED_pc = predictions[[5]])
  
  all_predictions[[country]] <- country_predictions}      # Append predictions to the list

all_predictions_df <- do.call(rbind, all_predictions)     # Combine predictions for all countries into a single DF

# Inspect Model Coefficients for the 5 predicted years
lapply(lm_models, coef)

# Evaluate Model Fit
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Set up a 2x2 layout for plots
for (i in seq_along(lm_models)) {
  plot(lm_models[[i]], which = i)  # Generate diagnostic plots for each model
}

# Filter data for Bangladesh
BD_predictions <- subset(all_predictions_df, Country == "Bangladesh")

# Plot for Bangladesh
plot_BD <- plot(BD_predictions$Year, BD_predictions$RE_pc, type = "l",
                ylim = range(c(BD_predictions$RE_pc, BD_predictions$NRE_pc, BD_predictions$GFCF_pc)),
                xlab = "Year", ylab = "Predicted Value",
                main = "Predicted Values for Bangladesh",
                col = "blue", lwd = 2)
lines(BD_predictions$Year, BD_predictions$NRE_pc, col = "red", lwd = 2)
lines(BD_predictions$Year, BD_predictions$GFCF_pc, col = "green", lwd = 2)

# Add legend for Bangladesh
legend_BD <- legend("topright", legend = c("RE_pc", "NRE_pc", "GFCF_pc"),
                    col = c("blue", "red", "green"), lwd = 2, bty = "n")

# Filter data for the United States
US_predictions <- subset(all_predictions_df, Country == "United States")

# Plot for the United States
plot_US <- plot(US_predictions$Year, US_predictions$RE_pc, type = "l",
                ylim = range(c(US_predictions$RE_pc, US_predictions$NRE_pc, US_predictions$GFCF_pc)),
                xlab = "Year", ylab = "Predicted Value",
                main = "Predicted Values for United States",
                col = "blue", lwd = 2)
lines(US_predictions$Year, US_predictions$NRE_pc, col = "red", lwd = 2)
lines(US_predictions$Year, US_predictions$GFCF_pc, col = "green", lwd = 2)

# Add legend for the United States
legend_US <- legend("topright", legend = c("RE_pc", "NRE_pc", "GFCF_pc"),
                    col = c("blue", "red", "green"), lwd = 2, bty = "n")

# Trying Animation on Reduced_CV
Reduced_PV$Year <- as.POSIXct(paste0(Reduced_PV$Year, "-01-01"))
p1_animated <- ggplot(Reduced_PV, aes(x = RE_pc, y = Region, size = RE_pc, color = Region)) +
  geom_point() +
  scale_size_continuous(range = c(1, 20)) +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'RE_pc', y = 'Region') +
  transition_time(Year) +
  ease_aes('linear')
p1_animated

p2_animated <- ggplot(Reduced_PV, aes(x = NRE_pc, y = Region, size = NRE_pc, color = Region)) +
  geom_point() +
  scale_size_continuous(range = c(1, 20)) +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'NRE_pc', y = 'Region') +
  transition_time(Year) +
  ease_aes('linear')
p2_animated

p3_animated <- ggplot(Reduced_PV, aes(x = GFCF_pc, y = Region, size = GFCF_pc, color = Region)) +
  geom_point() +
  scale_size_continuous(range = c(1, 20)) +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'GFCF_pc', y = 'Region') +
  transition_time(Year) +
  ease_aes('linear')
p3_animated

p4_animated <- ggplot(Reduced_PV, aes(x = AE_pc, y = Region, size = AE_pc, color = Region)) +
  geom_point() +
  scale_size_continuous(range = c(1, 20)) +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'AE_pc', y = 'Region') +
  transition_time(Year) +
  ease_aes('linear')
p4_animated

p5_animated <- ggplot(Reduced_PV, aes(x = ED_pc, y = Region, size = ED_pc, color = Region)) +
  geom_point() +
  scale_size_continuous(range = c(1, 20)) +
  theme_bw() +
  labs(title = 'Year: {frame_time}', x = 'ED_pc', y = 'Region') +
  transition_time(Year) +
  ease_aes('linear')
p5_animated