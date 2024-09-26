###############################
#    Packages installation    #
###############################

library(readxl)
library(ggplot2)
library(here)
library(tidyverse)

#############################
###   Data extraction     ###
#############################

######################################## For renewable energy sources df ##########################################

# Specify the file path
path_renew <- here::here("Data/Data_sustainable_energy.xlsx")

# Define the names of the excel sheets
Renewables_sheets <- c("Wind Generation - TWh", "Solar Generation - TWh", "Geo Biomass Other - TWh", "Hydro Generation - TWh", "Nuclear Generation - TWh")

# Read the specific sheets into a list of dataframes
Renewables_list <- lapply(Renewables_sheets, function(sheet_name) {
  read_xlsx(path_renew, sheet = sheet_name)
})

# Name the dataframes
names(Renewables_list) <- Renewables_sheets

# Bring the dataframes to the global environment
list2env(Renewables_list ,.GlobalEnv)


######################################## For CO2 emissions, PM2.5, and  Population df's ##########################################

# Read CO2 emissions csv file
path_co2 <- here::here("Data/CO2_emission_capita.csv")
co2_df <- read_csv(path_co2)

# Removing code column to have the same dimension as renewable energy df and it's useless
co2_df <- co2_df[ , -2]

# Read PM2.5 exposure csv file
path_pm2.5 <- here::here("Data/Data_PM2.5.csv")
PM_df <- read_csv(path_pm2.5)

# Removing column 1, 2, 4 to have the same dimension as renewable energy df and it's useless
PM_df <- PM_df[ ,-c(1, 2, 4)]

# Read Population exposure csv file
path_pop <- here::here("Data/Population_yearly.csv")
Pop_df <- read_csv(path_pop)

# Removing irrelevant columns just like for CO2 and PM2.5
Pop_df <- Pop_df[ ,-c(2, 3, 4)]

##############################
###     Data cleaning      ###
##############################

######################################## For renewable energy sources df ##########################################

#    Remove empty row     #
###########################

# Function to remove empty rows for each dataframe in the list
remove_empty_row <- function(df) {
  df <- df %>%
    filter_all(all_vars(!is.na(.)))
}

# Apply remove_empty to each dataframe in the list
Cleaned_Renewables_list <- lapply(Renewables_list, remove_empty_row)

#       Pivot our DF       #
############################

# Function to pivot our df
process_data <- function(df) {
  # Set the coerced years as column names
  colnames(df) <- as.integer(as.character(df[1, ]))  # Extract the years from the first row of the data frame and coerce them to integers
  
  # Remove the first row (useless)
  df <- df[-1,]
  
  # Rename the first column to "country"
  colnames(df)[1] <- "country"
  
  # We delete 3 columns (they have no interest and useless columns)
  df <- df[, head(seq_along(df), -3)]
  
  # Pivot of the data
  long_format_df <- df %>%
    tidyr::pivot_longer(
      cols = -country,   # Select all columns except the "country"
      names_to = "year", # Column names go into the "year" column
      values_to = "TWh"  # Values go into the "TWh" column
    )
  
  return(long_format_df)
}

# Use process_data to get our pivot df
wind_df <- process_data(Cleaned_Renewables_list[[1]])
solar_df <- process_data(Cleaned_Renewables_list[[2]])
geo_df <- process_data(Cleaned_Renewables_list[[3]])
hydro_df <- process_data(Cleaned_Renewables_list[[4]])
nuclear_df <- process_data(Cleaned_Renewables_list[[5]])

#      Merge our renewables energies df's    #
##############################################

# Rename columns in the original dataframes
wind_df <- wind_df %>%
  rename(wind_generation = TWh)
solar_df <- solar_df %>%
  rename(solar_generation = TWh)
geo_df <- geo_df %>%
  rename(geo_generation = TWh)
hydro_df <- hydro_df %>%
  rename(hydro_generation = TWh)
nuclear_df <- nuclear_df %>%
  rename(nuclear_generation = TWh)

# Sequentially merge the dataframes
merged_renew <- wind_df %>%
  merge(solar_df, by = c("country", "year"), all = FALSE) %>%
  merge(geo_df, by = c("country", "year"), all = FALSE) %>%
  merge(hydro_df, by = c("country", "year"), all = FALSE) %>%
  merge(nuclear_df, by = c("country", "year"), all = FALSE)

# Convert "year" to numeric in merged_renew
merged_renew <- merged_renew %>%
  mutate(year = as.numeric(year)) %>%
    filter(!(
      (country %in% c("Slovenia", "Croatia", "North Macedonia") & year < 1990) | # Remove year under 1990 for those 3 countries as they were part from yugoslavia so we can't have data before 1990
      (country %in% c("Azerbaijan", "Belarus", "Latvia", "Lithuania", "Kazakhstan", "Russian Federation", "Ukraine", "Estonia", "Turkemenistan", "Uzbekistan") & year < 1984) | # Remove year under 1984 for those 2 countries as they were part from USSR so we can't have data before 1984
      (country == "Bangladesh" & year < 1970) # Remove year under 1970 for Bangladesg as it was part from India so we can't have data before 1970
         )) 

######################################## For PM2.5, CO2 emissions and  Population df's ##########################################

#     PM_df      #
##################

# Rename column "country"
colnames(PM_df)[1] <- "country"

# Pivot of the data
PM_df <- PM_df %>%
  tidyr::pivot_longer(
    cols = -country,   # Select all columns except the "country"
    names_to = "year", # Column names go into the "year" column
    values_to = "PM_exposure"  # Values go into the "PM_exposure" column
  )

# Changing the format of the "year" column : "1960 [1960]" --> "1960"
PM_df$year <- gsub("\\s*\\[YR[0-9]{4}\\]", "", PM_df$year)

# Remove empty row
remove_empty_values <- function(df) {
  df <- df %>%
    mutate_all(~ifelse(. == "..", NA, .)) %>%  # Replace ".." with NA
    filter_all(all_vars(!is.na(.)))  # Filter out rows with NA values
  return(df)
}

# Apply remove_empty to PM_df
PM_df <- remove_empty_values(PM_df)

# Convert the "year" column to numeric in PM_df 
PM_df <- PM_df %>%
  mutate(year = as.numeric(year))

#      co2_df      #
####################

# Rename columns of co2_df
co2_df <- co2_df %>%
  rename(`CO2_emissions` = `Annual CO₂ emissions (per capita)`) %>%
  rename(country = Entity) %>%
  rename(year = Year)

# Convert the "year" column to numeric in co2_df
co2_df <- co2_df %>%
  mutate(year = as.numeric(year))

# Check if CO2_df contains missing values : NA or ".."
missing_values <- any(is.na(co2_df) | co2_df == "..")


#      Pop_df      #
####################

# Changing the column names and type
colnames(Pop_df)[1] <- "country"
colnames(Pop_df)[-1] <- gsub("\\s*\\[YR[0-9]{4}\\]", "", colnames(Pop_df)[-1])
Pop_df[, -1] <- sapply(Pop_df[, -1], as.numeric)


# Pivot of the data
Pop_df <- Pop_df %>%
  tidyr::pivot_longer(
    cols = -country,   # Select all columns except the "country"
    names_to = "year", # Column names go into the "year" column
    values_to = "Population"  # Values go into the "Population" column
  )

# Convert "year" to numeric in Pop_df
Pop_df <- Pop_df %>%
  mutate(year = as.numeric(year))

################################## Merging of co2_df, PM_df, Pop_df with merged_renew ##########################################


#      Country name replacements for Pop, CO2, PM dfs        #
##############################################################
# --> to standardize the country names in our final df as there is some change between the data sets

# List of country name replacements
country_replacements <- c(
  "United States" = "US", "Czechia" = "Czech Republic", "Egypt, Arab Rep." = "Egypt",
  "Iran, Islamic Rep." = "Iran", "Russia" = "Russian Federation",
  "Slovak Republic" = "Slovakia", "Korea, Rep." = "South Korea",
  "Trinidad and Tobago" = "Trinidad & Tobago", "Turkiye" = "Turkey",
  "Venezuela, RB" = "Venezuela")

# Define a function to apply country renaming to a dataset
rename_countries <- function(df) {
  df %>%
    mutate(country = ifelse(country %in% names(country_replacements),
                            country_replacements[country],
                            country))
}

# Apply the renaming to the Pop_df, CO2_df, and PM_df
Pop_df <- rename_countries(Pop_df)
co2_df <- rename_countries(co2_df)
PM_df <- rename_countries(PM_df)


#     Merge PM_df, CO2_df, Pop_df with merged_renew     #
#########################################################

raw_df <- merged_renew %>%
  left_join(PM_df, by = c("country", "year")) %>%
  left_join(co2_df, by = c("country", "year")) %>%
  left_join(Pop_df, by = c("country", "year"))
# raw_df contains all the energy generation, co2 emissions and PM exposure of all countries and all years from (1965-2022)

# Arrange column order to have dependent variables first
raw_df <- raw_df %>%
  select(1:2, CO2_emissions, PM_exposure, 3:ncol(.)) 

# Convert other columns in project_df to numeric
cols_to_convert <- setdiff(names(raw_df), c("country", "year", "CO2_emissions"))
for(colname in cols_to_convert) {
  raw_df[[colname]] <- as.numeric(raw_df[[colname]])
}

#  Drop entity that are not countries (Total europe, Western africa, ...)  #
############################################################################

# Create a list of non country entity in raw_df 
non_countries <- c("Central America", "China Hong Kong SAR", "Eastern Africa", "European Union#", "Middle Africa", "of which: OECD", "Non-OECD", "Other Caribbean", 
                   "Other CIS", "Other Europe", "Other South America", "Other Southern Africa", "Other Asia Pacific", "Other Middle East", "Taiwan",
                   "Total CIS", "Total Europe", "Total Africa", "Total Middle East", "Total Asia Pacific", "Total North America", 
                   "Total S. & Cent. America", "Total World", "USSR", "Western Africa")

# Remove the non country entities from raw_df
raw_df <- raw_df[!raw_df$country %in% non_countries,]

#  Drop any row without a Population  #
#######################################

all_df <- raw_df %>% 
  filter(complete.cases(Population)) # --> 77 countries
# We voluntarily created a "raw_df" to have the total of energy generation, co2 emissions and PM exposure for all the years (independent from the population)
# and then "raw_df" where we remove missing values of population to compute renewable energy generation per capita


#    Computing renewable energy generation variables per capita     #
#####################################################################


# Iterate through columns and divide by population
for (colname in colnames(all_df)) {
  # Skip the columns 'country', 'year', 'CO2_emissions', and 'Population'
  if (!colname %in% c('country', 'year', 'CO2_emissions', 'Population')) {
    
    # Identify rows where the column value is not NA
    valid_rows <- !is.na(all_df[[colname]])
    
    # Check if the entire column (excluding NA values) is numeric
    if (all(sapply(all_df[valid_rows, colname], is.numeric))) {
      
      # Only perform operation on those valid rows
      all_df[valid_rows, colname] <- all_df[valid_rows, colname] / all_df[valid_rows, 'Population']
    }
  }
}

######################## Apply filter on all_df to have different dfs for our analysis ######################## 
 
# raw_df not per capita !!! 
# all_df per capita !!!

#     df from 1990 to 2021       #
##################################

raw_1990 <- raw_df %>%
  filter(year >= 1990, year <= 2021)

all_1990 <- all_df %>%
  filter(year >= 1990, year <= 2021)
# We choose 1990 as before that date there wern't many renewable energy generations which doesn't help us to analyse a significant effect

#    df with only developped countries    #
###########################################


# Create a list of developed countries
developed_countries <- c("Australia", "Austria", "Belgium", "Canada", "Cyprus", 
                         "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                         "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Israel", 
                         "Italy", "Japan", "Latvia", "Lithuania", "Luxembourg", 
                         "Netherlands", "New Zealand", "Norway", "Poland", "Portugal", 
                         "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                         "United Kingdom", "US", "South Korea", "Singapore", "Qatar", 
                         "United Arab Emirates", "Mexico", "Saudi Arabia", "China")

developed_df <- all_df %>% filter(country %in% developed_countries) # --> 40 countries
rawdev_df <- raw_df %>% filter(country %in% developed_countries) # --> 40 countries

#    df with only developped countries from 1990    #
#####################################################

developed_1990 <- developed_df %>%
  filter(year >= 1990, year <= 2021)

rawdev_1990 <- rawdev_df %>%
  filter(year >= 1990, year <= 2021)

#    df with non developped countries    #
###########################################

nondev_df <- all_df[!all_df$country %in% developed_countries, ] # --> 37 countries
rawnondev_df <- raw_df[!raw_df$country %in% developed_countries, ] # --> 37 countries


#    df with non developped countries from 1990   #
###################################################

nondev_1990 <- nondev_df %>%
  filter(year >= 1990, year <= 2021)

#        multiply df by 100          #
######################################
# This will allow to make the log scaling more readable for our analysis as we have very low values per capita

# all100_df : mutliply all_df by 100
all100_df <- all_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))

# all100_1990 : keep only year from 1990 of all100_df
all100_1990 <- all100_df %>%
  filter(year >= 1990, year <= 2021)

# alldev100_1990 : keep only year from 1990 and dev country

alldev100_1990 <- developed_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))  %>%
  filter(year >= 1990, year <= 2021)

# allnondev100_1990 : keep only year from 1990 and nondev country

allnondev100_1990 <- nondev_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))  %>%
  filter(year >= 1990, year <= 2021)

# raw100_df : mutliply raw_df by 100
raw100_df <- raw_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))

# raw100_1990 : keep only year from 1990 of raw100_df
raw100_1990 <- raw100_df %>%
  filter(year >= 1990, year <= 2021)

# rawdev100_1990 : keep only year from 1990 and dev country

rawdev100_1990 <- rawdev_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))  %>%
  filter(year >= 1990, year <= 2021)

# rawnondev100_1990 : keep only year from 1990 and nondev country

rawnondev100_1990 <- rawnondev_df %>%
  mutate(across(
    .cols = 3:9,
    .fns = ~ . * 100
  ))  %>%
  filter(year >= 1990, year <= 2021)

# Remove the intermediate df / lists
rm(Pop_df, PM_df, co2_df, merged_renew, Cleaned_Renewables_list, geo_df, `Geo Biomass Other - TWh`, `Hydro Generation - TWh`, hydro_df, `Nuclear Generation - TWh`, nuclear_df, Renewables_list, `Solar Generation - TWh`, solar_df, `Wind Generation - TWh`, wind_df)

