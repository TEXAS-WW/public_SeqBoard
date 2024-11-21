###########################################################
## Read in Data, Shapefiles and Mapped Objects
###########################################################

# Load required packages for data manipulation and visualization
#source("variables.R")  # Source global variables defined in external file for consistency
suppressPackageStartupMessages({
  # Load a series of libraries without showing any package startup messages
  library(feasts)  # For time series features
  library(tsibble)  # For handling time series data in a tidy format
  library(lubridate)  # For date-time manipulation
  library(ggplot2)  # For creating static visualizations
  library(viridis)  # For color scales that are perceptually uniform in both colour and greyscale
  library(ggExtra)  # For adding marginal histograms to ggplot2, and more ggplot2 enhancements
  library(tidyr)  # For data tidying
  library(dygraphs)  # For interactive time series charting
  library(xts)  # For handling time-series data
  library(tidyverse)  # For easy data manipulation and visualization
  library(readxl)  # For reading Excel files
  library(writexl)  # For writing Excel files
  library(janitor)  # For cleaning up messy data
  library(ggpubr)  # For 'ggplot2' based publication ready plots
  library(readr)  # For reading rectangular data
  library(stringi)  # For fast, correct string manipulation
  library(stringr)
  library(shinyjs)
  library(tidycensus)  # For loading US Census data
  library(tigris)  # For loading geographic data from the US Census Bureau
  library(sf)  # For simple features, a standardized way to encode spatial vector data
  library(rprojroot)  # For finding the root of a project directory
  library(data.table)  # For enhanced data manipulation
  library(wesanderson)  # For Wes Anderson color palettes
  library(Rtsne)  # For T-distributed Stochastic Neighbor Embedding (t-SNE)
  library(RColorBrewer)  # For color palettes
  library(gganimate)  # For animating ggplot2 visualizations
  library(gifski)  # For rendering animations to GIF
  library(shiny)  # For building interactive web apps straight from R
  library(shinydashboard)  # For creating dashboards with 'Shiny'
  library(shinyWidgets)  # For custom widgets like buttons, sliders, etc.
  library(leaflet)  # For interactive maps
  library(dplyr)  # For data manipulation
  library(plotly)  # For creating interactive web graphics via 'plotly.js'
  library(xtable)  # For exporting tables to LaTeX or HTML
  library(reactable)  # For creating interactive data tables
  library(DT)  # For rendering data tables
  library(shinythemes)  # For additional themes for 'Shiny' apps
  library(htmltools)  # For HTML rendering tools
  library(maps)  # For drawing geographical maps
  library(reactablefmtr)  # For formatting options for reactable
  library(sparkline)  # For generating sparklines in 'Shiny' applications
  library(dataui)  # For interactive web data components
  library(devtools)  # For development tools
  library(scales)  # For graphical scales mapping data to aesthetics
  library(cdlTools)  # For converting state FIPS codes to state names
  library(htmlwidgets)
  library(ggmap)
  library(shinyMobile) #For mobile optimization
  options(tigris_use_cache = TRUE)  # Use local caching for 'tigris' data
})

# Function to calculate moving averages
ma <- function(x, n = 3) {
  # Calculate moving averages using a simple mean over n periods
  stats::filter(x, rep(1 / n, n), sides = 1)
}

# Load data related to Wastewater Treatment Plant (WWTP) locations
# This section reads multiple Excel files containing site coding, merges them, and standardizes column names
WWTP_files <- list.files(sprintf(
  "%s/Data/site_coding", 
  find_rstudio_root_file()), 
  pattern = "location.*\\.xlsx$", full.names = TRUE)
WWTP <- rbindlist(lapply(WWTP_files, read_excel), fill = TRUE)
WWTP <- as.data.frame(WWTP)
colnames(WWTP) = c("State", "County", "City", "WWTP", "lat", "lon", "county_centroid_lat", "county_centroid_lon", "city_centroid_lat",
                   "city_centroid_lon", "Radius", "Color", "Weight", "FillOpacity")

# Read and replace real site names with codes for anonymization
code_dt <- read_excel(sprintf("%s/Data/site_coding/WWTP_codes1.xlsx", find_rstudio_root_file()))

#NOT USED AS OF 10/7/24
abbr_dt <- read_excel(sprintf("%s/Data/site_coding/Sites_and_abbreviations.xlsx", find_rstudio_root_file()))



# Aggregate data by county within Texas, merging with shapefiles for mapping 
#NOT USED AS OF 10/7/24
CountyWWTP <- WWTP %>% 
  group_by(County, county_centroid_lon, county_centroid_lat, Color, Radius, Weight, FillOpacity) %>% 
  summarize(totalWWTP = n_distinct(WWTP)) 
names(CountyWWTP)[1] = "NAMELSAD"
county_data_shp <- tigris::counties(state = "TX", year = 2021)
merged_CountyWWTP <- left_join(county_data_shp, CountyWWTP, by = "NAMELSAD")
merged_CountyWWTP$Weight <- replace(merged_CountyWWTP$Weight, is.na(merged_CountyWWTP$Weight), 0)
merged_CountyWWTP$Radius <- replace(merged_CountyWWTP$Radius, is.na(merged_CountyWWTP$Radius), 0)
merged_CountyWWTP$FillOpacity <- replace(merged_CountyWWTP$FillOpacity, is.na(merged_CountyWWTP$FillOpacity), 0)
merged_CountyWWTP$Color <- replace(merged_CountyWWTP$Color, is.na(merged_CountyWWTP$Color), "#F5DEB3")
merged_CountyWWTP$totalWWTP <- replace(merged_CountyWWTP$totalWWTP, is.na(merged_CountyWWTP$totalWWTP), 0)

# Read and process metadata for taxonomical and genomic data
tax_files <- list.files(sprintf(
  "%s/Data/taxonomical_profiles", 
  find_rstudio_root_file()), 
  pattern = "*.tax.tsv", full.names = TRUE)
comb_tax_table <- rbindlist(lapply(tax_files, fread))
comb_tax_table <- as.data.frame(comb_tax_table)
metadata_files <- list.files(sprintf(
  "%s/Data/metadata", 
  find_rstudio_root_file()), 
  pattern = "*.xlsx", full.names = TRUE)
comb_metadata_table <- rbindlist(lapply(metadata_files, read_excel))
comb_metadata_table <- as.data.frame(comb_metadata_table)
colnames(comb_metadata_table) = c("sample_ID", "Site", "City", "Date", "Flow", "PoolID")
comb_metadata_table$Date <- as.Date(comb_metadata_table$Date)

# Combine metadata with site codes and reformat table for further analysis
comb_metadata_table <- merge(comb_metadata_table, code_dt, by.x = "Site", by.y = "Name", all.x = TRUE)
comb_metadata_table$Site <- comb_metadata_table$Code
comb_metadata_table <- comb_metadata_table %>% 
  mutate(sample_ID = paste(sample_ID, PoolID, sep = ".")) %>%
  select(-c(Code, PoolID))

# Read genome coverage data and merge with taxonomical profiles
coverage_files <- list.files(sprintf(
  "%s/Data/genome_coverage", 
  find_rstudio_root_file()), 
  pattern = "*.mean_cov.tsv", full.names = TRUE)
comb_coverage_table <- rbindlist(
  lapply(coverage_files, 
         fread, header = FALSE, 
         col.names = c("sample_ID", "accession", "start_base", "end_base", "mean_depth")))
comb_coverage_table <- as.data.frame(comb_coverage_table)

# Load qPCR data, format, and prepare for analysis
qPCR_files <- list.files(sprintf(
  "%s/Data/qPCR", 
  find_rstudio_root_file()), 
  pattern = "*.csv", full.names = TRUE)
comb_qPCR_table <- rbindlist(lapply(qPCR_files, fread, colClasses = "character"))
comb_qPCR_table <- as.data.frame(comb_qPCR_table)
comb_qPCR_table$date_of_collection <- as.POSIXct(comb_qPCR_table$date_of_collection)
comb_qPCR_table <- merge(comb_qPCR_table, abbr_dt, by = "LocationAbbr") %>%
  mutate(Week = floor_date(as.Date(date_of_collection), "weeks", week_start = 1),
         copiesperml = as.numeric(copiesperml)) %>%
  group_by(LocationAbbr, CMMR_Barcode, Target, SampleName, Week, City) %>%
  summarize(average_genome_copies_L = mean(copiesperml)) %>%
  ungroup()



## calculate unique cities
virome_cities <- comb_metadata_table %>% 
  ungroup() %>%
  filter(City != "other") %>% 
  select(City) %>%
  unique()

qPCR_cities_dt <- comb_qPCR_table %>% 
  ungroup() %>%
  filter(City != "other") %>% 
  select(City) %>%
  unique()


total_cities <- merge(virome_cities, qPCR_cities_dt, by = "City", all = T)

WWTP_cities <- list(unique(total_cities$City))
WWTP_cities <- gsub(", TX", "", WWTP_cities[[1]])

#NOT USED AS OF 10/7/24
WWTP_citieslength <- length(unique(total_cities$City))

### Coordinates, Texas Cities with WWTPs in TEPHI program

invisible(capture.output(
  cities <- st_read(sprintf(
    "%s/Data/geographical_files/Texas_Cities/City.shp", 
    find_rstudio_root_file())) %>% 
    filter(CITY_NM %in% WWTP_cities)%>% 
    st_cast("POINT") %>% as("Spatial") 
))

# Calculate unique cities for virome and qPCR analysis
# This section creates distinct lists of cities based on metadata and qPCR data, ensuring data consistency across analyses
virome_cities <- comb_metadata_table %>%
  ungroup() %>%
  filter(City != "other") %>%
  select(City) %>%
  unique()

qPCR_cities_dt <- comb_qPCR_table %>%
  ungroup() %>%
  filter(City != "other") %>%
  select(City) %>%
  unique()

# Combine the city lists into a single dataset and remove duplicates
total_cities <- merge(virome_cities, qPCR_cities_dt, by = "City", all = TRUE)
WWTP_cities <- list(unique(total_cities$City))
WWTP_cities <- gsub(", TX", "", WWTP_cities[[1]])  # Remove state suffix for cleaner city names

# Coordinate handling for Texas cities involved in the TEPHI program
# This part reads shapefiles for Texas cities, filters them by cities with WWTPs, and converts them to spatial points for mapping
invisible(capture.output(
  cities <- st_read(sprintf(
    "%s/Data/geographical_files/Texas_Cities/City.shp", 
    find_rstudio_root_file())) %>% 
    filter(CITY_NM %in% WWTP_cities) %>% 
    st_cast("POINT") %>% as("Spatial")  # Convert shape data to simple POINT type for easier handling
))

# Prepare the data tables for pathogen analysis based on sequencing and qPCR data
# This includes mapping, averaging, and normalizing the data for further analysis
major_path_met_dt <- merge(comb_tax_table, comb_metadata_table, 
                           by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  group_by(City) %>%
  filter(n_distinct(Week) >= 3) %>%
  ungroup() %>%
  filter(species %in% c("Norwalk virus", "Enterovirus D", "Rotavirus A", 
                        "Influenza A virus", 
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus",
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3")) %>%
  group_by(sample_ID, City, Week, species) %>%
  summarize(RPKMF = sum(RPKMF)) %>%
  ungroup() %>%
  group_by(species, City) %>%
  mutate(rel_ab = RPKMF/sum(RPKMF)) %>%
  ungroup()

# Expand pathogen data to include all weeks sampled, filling gaps where no data exists
city_dates <- comb_metadata_table %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  select(c(City, Week)) %>%
  distinct()
major_path_expand_dt <- major_path_met_dt %>%
  group_by(Week, City, species) %>%
  summarize(rel_ab = sum(rel_ab),
            RPKMF = mean(RPKMF)) %>%
  ungroup() %>%
  complete(Week, City, species, fill = list(rel_ab = 0, RPKMF = 0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKMF)) %>%
  ungroup() %>%
  mutate(species = gsub("Enterovirus D", "Enterovirus D68", species),
         species = gsub("Norwalk virus", "Noroviruses", species),
         species = gsub("Severe acute respiratory syndrome-related coronavirus",
                        "SARS-CoV-2", species),
         species = gsub("Hepatovirus A", "Hepatitis A Virus", species),
         species = gsub("Human respirovirus 3", "Parainfluenza Virus 3", species),
         species = gsub("Human respirovirus 1", "Parainfluenza Virus 1", species),
         species = gsub("Human mastadenovirus B", "Human Adenovirus B", species),
         species = gsub("Respiratory syncytial virus", "Respiratory syncytial virus A", species),
         species = gsub("Human orthopneumovirus", "Respiratory syncytial virus B", species))

major_path_expand_dt <- merge(major_path_expand_dt, city_dates, by = c("City", "Week"))

# Date range calculations for interactive displays and filtering
# These calculations find the minimum and maximum dates for comprehensive deep sequencing and qPCR datasets
minDate_cds <- as.Date(min(major_path_expand_dt$Week, na.rm = TRUE))
maxDate_cds <- as.Date(max(major_path_expand_dt$Week, na.rm = TRUE))


# qPCR (Targeted): Quantification of Specific Pathogens in Wastewater

qPCR_ma_p <- comb_qPCR_table %>%
  filter(City != "other",
         Target %in% c("SARSCOV2N1", "INFLUENZAA", "INFLUENZAB", 
                       "NOROVIRUS", "MONKEYPOX")) %>%
  mutate(Target = gsub("SARSCOV2N1", "SARS-CoV-2", Target),
         Target = gsub("INFLUENZAA", "Influenza A virus", Target),
         Target = gsub("INFLUENZAB", "Influenza B virus", Target),
         Target = gsub("NOROVIRUS", "Norovirus GII", Target),
         Target = gsub("MONKEYPOX", "Monkeypox virus", Target)) %>%
  group_by(Week, City, Target) %>%
  summarize(average_genome_copies_L = mean(average_genome_copies_L)) %>%
  ungroup() %>%
  group_by(City, Target) %>%
  filter(n_distinct(Week) >= 3) %>%
  arrange(City, Target, Week) %>%
  mutate(moving_average = ma(average_genome_copies_L)) %>%
  ungroup()

print(qPCR_ma_p)

minDate_qpcr <- as.Date(min(qPCR_ma_p$Week, na.rm = TRUE))
maxDate_qpcr <- as.Date(max(qPCR_ma_p$Week, na.rm = TRUE))


# DATA PREPARATION FOR INTERACTIVE TABLE
# Format the coverage data for reactable displays
comb_coverage_table$mean_depth <- round(comb_coverage_table$mean_depth)  # Round mean depth values for cleaner display

# Aggregate coverage data by sample and accession to create a list of mean depth values
sum_coverage <- comb_coverage_table %>%
  group_by(sample_ID, accession) %>%
  summarize(coverage = list(mean_depth)) 

sum_coverage <- setDT(sum_coverage)  # Convert to data.table for better performance on large data sets

# Merge taxonomic profile, metadata, and coverage into a single dataset
genome_data <- merge(comb_tax_table, comb_metadata_table, by = "sample_ID") %>%
  filter(species %in% c("Enterovirus D", "Influenza A virus", 
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus", 
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3"))

combined_react_data <- merge(genome_data, sum_coverage, by = c("sample_ID", "accession"))

# Select and reformat the data for better readability in the interactive table
combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "accession", "sequence_name", "reference_length", "RPKMF", "covered_bases", "coverage"))
combined_react_data$Percent_covered <- combined_react_data$covered_bases / combined_react_data$reference_length  # Calculate percentage coverage
combined_react_data <- subset(combined_react_data, select = c("sample_ID", "Site", "City", "Date", "sequence_name", "accession", "reference_length", "Percent_covered", "RPKMF", "coverage"))
#remove , complete genome from every value in Sequence name
combined_react_data <- combined_react_data %>%
  mutate(`sequence_name` = str_remove(`sequence_name`, ", complete genome"))








###Load covid variant data 
freyja_sum_files <- list.files(sprintf(
  "%s/Data/sars_variants", 
  find_rstudio_root_file()), 
  pattern = "*freyja_sum.tsv", full.names = TRUE)

## combine all covid variants as one table
comb_frey_sum_table <- rbindlist(lapply(freyja_sum_files, fread))


#Format covid variant table
sars_lin_meta_dt <- merge(comb_frey_sum_table, comb_metadata_table,
                          by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  replace_na(list(coverage = 0)) %>%
  arrange(desc(Week)) %>%
  group_by(lineage, Week) %>%
  summarize(n_sites = n()) %>%
  ungroup() %>%
  group_by(lineage) %>%
  mutate(first_week = first(Week)) %>%
  ungroup()





### prepare sensitive read mapping data table for texas wide plot

########################Load data for texas wide sensitive ##########
#I belive Mike Tisza wrote this
## load virus info table for sensitive mapping taxonomy, etc
vir_info_dt <- fread(sprintf(
  "%s/Data/vir_info/Comp_viral_panel_w_curated_plus_extra_v2.0.2.all_metadata.tsv",
  find_rstudio_root_file()))



##load coverm files iteratively and combine

if (exists("sense_dataset")){
  rm(sense_dataset)
}


## list of relevant files in the directory
sense_files <- list.files(sprintf(
  "%s/Data/sensitive_taxonomy", 
  find_rstudio_root_file()), 
  pattern = "*.tsv", full.names = TRUE)



for (file in sense_files){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("sense_dataset")){
    sense_dataset <- fread(file, header=T, sep="\t") %>%
      filter(reads_mapped > 0)
    
  }
  
  # if the merged dataset does exist, append to it
  else if (exists("sense_dataset")){
    temp_dataset <-fread(file, header=T, sep="\t") %>%
      filter(reads_mapped > 0)
    
    sense_dataset<-rbind(sense_dataset, temp_dataset)
    rm(temp_dataset)
  }
}

sense_dataset <- sense_dataset %>%
  mutate(sample_ID = paste(Sample_ID, PoolID, sep = ".")) %>%
  select(-c(Sample_ID, PoolID))




read_sum_dt <- comb_tax_table %>%
  distinct(sample_ID, total_filtered_reads_in_sample)

sense_dt <- merge(sense_dataset, vir_info_dt, 
                  by = "accession")

sense_dt <- merge(sense_dt, read_sum_dt, 
                  by = "sample_ID") %>%
  mutate(RPKMF = (
    reads_mapped / (total_filtered_reads_in_sample/1e6)))



sense_path_met_dt <- merge(sense_dt, comb_metadata_table, 
                           by = "sample_ID") %>%
  mutate(Week = floor_date(Date, "weeks", week_start = 1)) %>%
  group_by(City) %>%
  filter(n_distinct(Week) >= 5) %>%
  ungroup() %>%
  filter(species %in% c("Norwalk virus", "Enterovirus D", "Rotavirus A", 
                        "Influenza A virus", "Influenza B virus",
                        "Severe acute respiratory syndrome-related coronavirus", 
                        "Monkeypox virus", "Respiratory syncytial virus",
                        "Human orthopneumovirus",
                        "Human mastadenovirus B", "Hepatovirus A", 
                        "Human respirovirus 1", "Human respirovirus 3",
                        "Human metapneumovirus", 
                        "Primate erythroparvovirus 1") | 
           subspecies %in% c("Echovirus E11")) %>%
  group_by(sample_ID, City, Week, species) %>%
  summarize(RPKMF = sum(RPKMF)) %>%
  ungroup() %>%
  group_by(species, City) %>%
  mutate(rel_ab = RPKMF/sum(RPKMF)) %>%
  ungroup()

## expand zeros
sense_path_expand_dt <- sense_path_met_dt %>%
  group_by(Week, City, species) %>%
  summarize(rel_ab = sum(rel_ab),
            RPKMF = mean(RPKMF)) %>%
  ungroup() %>%
  complete(Week, City, species, 
           fill = list(rel_ab = 0, RPKMF = 0)) %>%
  group_by(City, species) %>%
  arrange(City, species, Week) %>%
  mutate(moving_average = ma(RPKMF)) %>%
  ungroup() %>%
  mutate(species = gsub("Enterovirus D", "Enterovirus D68", species),
         species = gsub("Enterovirus B", "Echovirus E11", species),
         species = gsub("Norwalk virus", "Noroviruses", species),
         species = gsub("Severe acute respiratory syndrome-related coronavirus",
                        "SARS-CoV-2", 
                        species),
         species = gsub("Hepatovirus A", "Hepatitis A Virus", species),
         species = gsub("Human respirovirus 3", 
                        "Parainfluenza Virus 3", 
                        species),
         species = gsub("Human respirovirus 1", 
                        "Parainfluenza Virus 1", 
                        species),
         species = gsub("Human mastadenovirus B", 
                        "Human Adenovirus B", 
                        species),
         species = gsub("Monkeypox virus", "Mpox virus", species),
         species = gsub("Respiratory syncytial virus", 
                        "Respiratory syncytial virus A", 
                        species),
         species = gsub("Human orthopneumovirus", 
                        "Respiratory syncytial virus B", 
                        species),
         species = gsub("Primate erythroparvovirus 1",
                        "Parvovirus B19",
                        species)
  )

sense_path_expand_dt <- merge(sense_path_expand_dt, 
                              city_dates, by = c("City", "Week"))


#################################### Make a Texas object

# make a year ago variable
yearago <- as.Date(Sys.time())-365

#Aggregate the cities for a texas wide plot
tx_virus_select <- sense_path_expand_dt %>%
  # Group by species and Week
  group_by(species, Week) %>%
  # Summarize the data
  summarize(
    # Calculate the mean RPKMFS across cities
    RPKMF = mean(RPKMF, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Calculate the moving average for RPKMFS
  mutate(moving_average = ma(RPKMF)) %>%
  ungroup()




################qualitative metrics for overview
qual_eval <- sense_path_expand_dt %>%
  # Rename virus species
  mutate(species = case_when(
    species == "Hepatitis A Virus" ~ "Hepatitis A",
    species == "Influenza A virus" ~ "Influenza A",
    species == "Mpox virus" ~ "Mpox",
    species == "Influenza B virus" ~ "Influenza B",
    TRUE ~ species
  )) %>%
  
  # Group by species
  group_by(species) %>%
  
  # Calculate historic_median for each species, excluding zero values
  mutate(historic_median = median(moving_average[moving_average != 0], na.rm = TRUE)) %>%
  
  # Arrange by date in descending order
  arrange(desc(Week)) %>%
  
  # Get the 4 most recent observations
  slice_head(n = 4) %>%
  
  # Calculate the average of these 4 observations and the most recent value
  summarize(
    most_recent = first(moving_average),
    avg_last_4 = mean(moving_average),
    historic_median = first(historic_median),
    .groups = 'drop'
  ) %>%
  
  # Calculate the percentage difference and determine the trend
  mutate(
    percent_diff = (most_recent - avg_last_4) / avg_last_4 * 100,
    Trend = case_when(
      percent_diff >= 100 ~ "Increasing",
      percent_diff <= 0 ~ "Decreasing",
      TRUE ~ "None"
    ),
    
    # Add historic_context column based on the new historic_median
    historic_context = case_when(
      avg_last_4 <= (1/2 * historic_median) ~ "Low",
      avg_last_4 > (1/2 * historic_median) & most_recent <= (3.25 * historic_median) ~ "Medium",
      avg_last_4 > (3.25 * historic_median) ~ "High"
    )
  ) %>%
  
  # Add the new 'level' column
  mutate(
    level = case_when(
      historic_context == "High" ~ "High",
      historic_context == "Medium" & Trend == "Increasing" ~ "High",
      historic_context == "Medium" ~ "Medium",
      historic_context == "Low" & Trend == "Increasing" ~ "Medium",
      TRUE ~ "Low"
    )
  ) %>%
  
  # Rename columns for clarity
  rename(
    Species = species,
    MostRecentValue = most_recent,
    AverageLastFour = avg_last_4,
    PercentDifference = percent_diff,
    HistoricMedian = historic_median,
    HistoricContext = historic_context
  ) %>%
  
  # Select and order columns
  select(Species, MostRecentValue, AverageLastFour, PercentDifference, Trend, HistoricMedian, HistoricContext, level)




############# Generate the Texas-wide Pathogen Levels Plot
tx_wide_plot <- tx_virus_select %>%
  filter(Week >= yearago) %>%
  ggplot(aes(x = Week, y = moving_average)) +
  geom_line(color = "black", size = 1) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  facet_wrap(~species, scales = "free_y", ncol = 3) +
  theme_minimal() +
  labs(x = "", y = "Wastewater Abundance", title = " ") +
  theme(
    strip.background = element_rect(fill = "lightgrey", color = NA),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_text(angle = 90, vjust = 1, margin = margin(r = 10)),
    plot.margin = unit(c(1, 1, 1, 2), "cm")  # top, right, bottom, left
  )

# Convert to plotly object
tx_wide_plotly <- ggplotly(tx_wide_plot, tooltip = c("x", "y")) %>%
  layout(
    autosize = TRUE,
    height = 800,
    showlegend = FALSE,
    margin = list(l = 80, r = 20, t = 20, b = 20)  # Increased left margin
  ) %>%
  config(displayModeBar = FALSE)

# Save the plotly object, overwriting any existing file
saveRDS(tx_wide_plotly, "tx_wide_plotly.rds")





# Prepare the data for the genome coverage table
prepare_table_data <- function() {
  three_months_ago <- Sys.Date() - months(3)
  filtered_data <- combined_react_data %>%
    filter(Date >= three_months_ago) %>%
    arrange(desc(Date)) %>%
    select(-sample_ID, -Site, -reference_length)  # Remove these columns
  
  return(filtered_data)
}

# Generate and save the preprocessed table data
table_data <- prepare_table_data()
saveRDS(table_data, "preprocessed_table_data.rds")


















########### Mike Tisza wrote this code for the Covid variant plot, Nico adapted it for preprocessing
# Create color palette
pal <- wes_palette("GrandBudapest1", 
                   length(unique(sars_lin_meta_dt$lineage)),
                   type = "continuous")

# Create the ggplot object
sars_lineage_plot <- sars_lin_meta_dt %>%
  ggplot(aes(x = Week, 
             y = fct_reorder(lineage, first_week, .desc = TRUE), 
             size = n_sites, 
             color = lineage)) +
  geom_point() +
  geom_line(size = 0.25, na.rm = TRUE) +
  scale_color_manual(values = pal) +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  theme_light() +
  labs(x = "", y = "WHO designated lineage") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")

# Save the ggplot object
saveRDS(sars_lineage_plot, "sars_lineage_plot.rds")






####################Overview


# Define a custom ordering for level
level_order <- c("High", "Medium", "Low")

# Define the link map
link_map <- c(
  "Enterovirus D68" = "https://www.cdc.gov/non-polio-enterovirus/about/about-enterovirus-d68.html",
  "Hepatitis A" = "https://www.cdc.gov/hepatitis-a/index.html",
  "Human Adenovirus B" = "https://www.cdc.gov/adenovirus/about/index.html",
  "Influenza A" = "https://www.cdc.gov/flu/index.htm",
  "Mpox" = "https://www.cdc.gov/mpox/index.html",
  "Noroviruses" = "https://www.cdc.gov/norovirus/index.html",
  "Parvovirus B19" = "https://www.cdc.gov/parvovirus-b19/about/index.html",
  "Rotavirus A" = "https://www.cdc.gov/rotavirus/about/index.html",
  "Human metapneumovirus" = "https://www.cdc.gov/human-metapneumovirus/about/index.html",
  "Influenza B" = "https://www.cdc.gov/flu/about/viruses/types.htm",
  "Parainfluenza Virus 1" = "https://www.cdc.gov/parainfluenza/about/index.html",
  "Parainfluenza Virus 3" = "https://www.cdc.gov/parainfluenza/about/index.html",
  "Respiratory syncytial virus A" = "https://www.cdc.gov/rsv/index.html",
  "Respiratory syncytial virus B" = "https://www.cdc.gov/rsv/index.html",
  "SARS-CoV-2" = "https://www.cdc.gov/covid/index.html"
)

# Define color assignments
color_map <- c(
  "High" = "#FFDAB9",
  "Medium" = "#FFFFE0",
  "Low" = "#FFFFFF"
)

# Define trend arrows
trend_arrows <- c(
  "Increasing" = "&#x2191;",  # Up arrow
  "Decreasing" = "&#x2193;",  # Down arrow
  "None" = "&#x2194;"         # Left-right arrow
)

# Save these objects
saveRDS(list(
  level_order = level_order,
  link_map = link_map,
  color_map = color_map,
  trend_arrows = trend_arrows
), "species_summary_data.rds")





##########Create a map of watewater sites

# Color palette
color_palette <- c(
  "#0C2653", "#1C5AC4", "#34623F", "#721817",
  "#FF0000", "#C3A3FF", "#FF69B4"
)

# Cities data
cities <- data.frame(
  name = c("Austin", "Brownsville", "El Paso", "Houston", "Laredo", "Lubbock", "Wichita Falls"),
  lat = c(30.2672, 25.9017, 31.7619, 29.7604, 27.5064, 33.5779, 33.9137),
  lon = c(-97.7431, -97.4974, -106.4850, -95.3698, -99.5075, -101.8552, -98.4934),
  color = color_palette
)

# Text positions
text_positions <- data.frame(
  name = cities$name,
  hjust = case_when(
    cities$name %in% c("El Paso", "Laredo") ~ -0.1,
    TRUE ~ 0.5
  ),
  vjust = case_when(
    cities$name == "Wichita Falls" ~ 1.5,
    cities$name == "El Paso" ~ 0.5,
    cities$name == "Laredo" ~ -0.2,
    TRUE ~ -0.5
  )
)

# Get the map data for Texas
# <- map_data("state", region = "texas")
texas_map <- st_read("PHR.shp")


# Convert the sf object to a data frame that ggplot can use
texas_map_df <- texas_map %>%
  st_cast("MULTIPOLYGON") %>%  # Ensure all geometries are MULTIPOLYGON
  st_cast("POLYGON") %>%       # Cast to POLYGON
  mutate(id = row_number()) %>% # Add an id for each polygon
  st_cast("POINT") %>%         # Convert to points
  mutate(
    long = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  st_drop_geometry() %>%       # Remove the geometry column
  group_by(id) %>%
  mutate(group = cur_group_id()) %>%
  ungroup()


# Save all preprocessed data
saveRDS(list(
  color_palette = color_palette,
  cities = cities,
  text_positions = text_positions,
  texas_map = texas_map
), "map_data.rds")
