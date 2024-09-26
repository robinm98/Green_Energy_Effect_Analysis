#######################################################################################
######                            Data analysis                                 #######
#######################################################################################

library(dplyr)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(plotly)
library(tidyr)
library(forcats)
library(ggridges)
library(corrplot)
library(hrbrthemes) 
library(viridis)
library(GGally)
library(ggrepel)
library(stargazer)
library(gridExtra)

# Set the working directory
setwd(here::here("Transcript"))

# Load and run the data extraction script
source("Data extraction.R")


#################################
###     Data distribution     ###
#################################

######### Histogram plot

### Facet grid for histogram

# Reshape our dataframes into a long format to create an histogram
longall_df <- melt(all100_1990[, -c(2,10)]) # for all country
longdev_df <- melt(alldev100_1990[, -c(2,10)]) # for developed country
longnondev_df <- melt(allnondev100_1990[, -c(2,10)]) # for non developed country

# Add a new column to each to indicate the development status
longall_df$Status <- 'All'
longdev_df$Status <- 'Developed'
longnondev_df$Status <- 'Non-Developed'

# Combine the two dataframes into one
hcombined_df <- rbind(longall_df,longdev_df, longnondev_df)

# Create histograms with facet_grid to ensure same scales
h <- ggplot(hcombined_df, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_grid(Status ~ variable, scales = "free_x") + 
  scale_x_log10() + # Apply log scale for x
  theme_minimal() +
  labs(x = "Value", y = "Count") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.x = element_text(size = 6), 
    strip.text.y = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Distribution of Variables by Development Status")

h

########### Violon plot

#### Combined violon plot

# Melt and ad development status
longdev_df <- melt(alldev100_1990[, -c(2, 4, 10)], variable.name = 'EnergySource', value.name = 'value') %>%
  mutate(DevelopmentStatus = 'Developed')

longnondev_df <- melt(allnondev100_1990[, -c(2, 4, 10)], variable.name = 'EnergySource', value.name = 'value') %>%
  mutate(DevelopmentStatus = 'Non-Developed')

# Set column names to be displayed in the violon plot 
col_names <- c("CO2_emissions" = "CO2 Emissions",
               "PM_exposure" = "PM2.5 Exposure",
               "wind_generation" = "Wind Generation",
               "solar_generation" = "Solar Generation",
               "geo_generation" = "Geo Generation",
               "hydro_generation" = "Hydro Generation",
               "nuclear_generation" = "Nuclear Generation")

# Rename columns in the melted data frames
longdev_df$EnergySource <- factor(longdev_df$EnergySource, levels = names(col_names), labels = col_names)
longnondev_df$EnergySource <- factor(longnondev_df$EnergySource, levels = names(col_names), labels = col_names)

# Combine the datasets
vcombined_df <- bind_rows(longdev_df, longnondev_df)

# Filter to remove extreme values applying IQR
vcombined_df <- vcombined_df %>%
  group_by(EnergySource) %>%
  mutate(
    lower_bound = quantile(value, 0.25) - 1.5 * IQR(value),
    upper_bound = quantile(value, 0.75) + 1.5 * IQR(value)
  ) %>%
  ungroup() %>%
  filter(value > lower_bound & value < upper_bound) %>%
  select(-lower_bound, -upper_bound)

# Create a violin plot with a facet grid
v <- vcombined_df %>%
  mutate(EnergySource = fct_reorder(EnergySource, value)) %>%

  ggplot(aes(x=EnergySource, y=value, fill=DevelopmentStatus)) +
  geom_violin(trim=FALSE, position=position_dodge(width=0.8), size=0.2) +
  scale_fill_viridis(discrete=TRUE, name="Development Status") +
  theme_ipsum() +
  facet_wrap(~ EnergySource, scales = "free") + 
  theme(
    axis.title.x = element_text(hjust = 0.5, size = 15), 
    axis.title.y = element_text(hjust = 0.5, size = 15),
    strip.text.x = element_blank(),
    legend.position = "bottom" 
  ) +
  labs(x = "Count", y = "TWh generations for energs sources / CO2 emissions for CO2") +
  scale_y_continuous(limits = c(0, NA)) + # Set the y-axis to not go below 0
  ggtitle("Violon Plot by Development Status")

v

#################################
###     Data correlation      ###
#################################

# All country

# Remove non-numeric columns and rows with NA values
alldf_numeric <- developed_df[, sapply(all_df, is.numeric)]
alldf_numeric <- na.omit(alldf_numeric[, -c(1, 9)])

# Calculate the correlation matrix for all country
allcor_matrix <- cor(alldf_numeric)

# Plot the correlation matrix
corrplot(allcor_matrix, method = "color", 
         type = "lower", 
         order = "original", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.pos = "lt", # Place text labels on the left and top
         tl.srt = 45, # Rotation of text labels
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200),
         cl.pos = "r",
         number.cex = 0.6, # Make the correlation numbers smaller
         tl.cex = 0.8, 
         title = "Correlation matrix for all country",
         mar = c(0, 0, 2, 0))

# Dev country
devdf_numeric <- developed_df[, sapply(developed_df, is.numeric)]
devdf_numeric <- na.omit(devdf_numeric[, -c(1, 9)])

# Calculate the correlation matrix for developed country
devcor_matrix <- cor(devdf_numeric)

p1 <- corrplot(devcor_matrix, method = "color", 
         type = "lower", 
         order = "original", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.pos = "lt", # Place text labels on the left and top
         tl.srt = 45, # Rotation of text labels
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), 
         cl.pos = "r",
         number.cex = 0.6, # Make the correlation numbers smaller
         tl.cex = 0.8, # Text label size
         title = "Correlation matrix for developed country",
         mar = c(0, 0, 2, 0)  
)

# Non dev country
nondevdf_numeric <- nondev_df[, sapply(nondev_df, is.numeric)]
nondevdf_numeric <- na.omit(nondevdf_numeric[, -c(1, 9)])

# Calculate the correlation matrix for non developed country
nondevcor_matrix <- cor(nondevdf_numeric)

# Plot the correlation matrix
p2 <- corrplot(nondevcor_matrix, method = "color", 
         type = "lower", 
         order = "original", 
         addCoef.col = "black", 
         tl.col = "black", 
         tl.pos = "lt", # Place text labels on the left and top
         tl.srt = 45, # Rotation of text labels
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200), 
         cl.pos = "r",
         number.cex = 0.6, # Make the correlation numbers smaller
         tl.cex = 0.8, # Text label size
         title = "Correlation matrix for developing country",
         mar = c(0, 0, 2, 0)  
)


#################################
###     Data description      ###
#################################

########################################### ENERGY DESCRIPTION ########################################### 

############  
#### Top 10 Cumulative Total Energy --> TOTAL 
###########

###    Stacked Bar Chart      ###
# Calculate cumulative total energy production by country

########## TOTAL 
raw_df_cumulative <- raw_df %>%
  group_by(country) %>%
  summarize(
    wind_total = sum(wind_generation, na.rm = TRUE),
    solar_total = sum(solar_generation, na.rm = TRUE),
    geo_total = sum(geo_generation, na.rm = TRUE),
    hydro_total = sum(hydro_generation, na.rm = TRUE),
    nuclear_total = sum(nuclear_generation, na.rm = TRUE),
    total_energy = sum(wind_total, solar_total, geo_total, hydro_total, nuclear_total)
  ) %>%
  ungroup() %>%
  arrange(total_energy) %>%
  top_n(10) ### 10 for 10 best or -10 for ten lowest

# Reorder the levels of the country factor
raw_df_cumulative$country <- reorder(raw_df_cumulative$country, raw_df_cumulative$total_energy)

# Reshape the data to long format for stacking
raw_df_cumulative_long <- raw_df_cumulative %>%
  pivot_longer(
    cols = c(wind_total, solar_total, geo_total, hydro_total, nuclear_total),
    names_to = "source",
    values_to = "cumulative_total"
  )

# Create the stacked bar chart
ggplot(raw_df_cumulative_long, aes(x = country, y = cumulative_total, fill = source)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 10 Countries with Historically the Largest Cumulative Renewable Energy Production",
    x = "Country",
    y = "Cumulative Energy Production (TWh)",
    fill = "Energy Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######### PER CAPITA
# Calculate average total energy production by country
all_df_average <- all_df %>%
  group_by(country) %>%
  summarize(
    wind_avg = sum(wind_generation, na.rm = TRUE) / n(),
    solar_avg = sum(solar_generation, na.rm = TRUE) / n(),
    geo_avg = sum(geo_generation, na.rm = TRUE) / n(),
    hydro_avg = sum(hydro_generation, na.rm = TRUE) / n(),
    nuclear_avg = sum(nuclear_generation, na.rm = TRUE) / n(),
    total_energy_avg = sum(wind_avg, solar_avg, geo_avg, hydro_avg, nuclear_avg)
  ) %>%
  ungroup() %>%
  arrange(total_energy_avg) %>%
  top_n(10) ### 10 for 10 best or -10 for ten lowest

# Reorder the levels of the country factor
all_df_average$country <- reorder(all_df_average$country, all_df_average$total_energy_avg)

# Reshape the data to long format for stacking
all_df_average_long <- all_df_average %>%
  pivot_longer(
    cols = c(wind_avg, solar_avg, geo_avg, hydro_avg, nuclear_avg),
    names_to = "source",
    values_to = "average_total"
  )

# Create the stacked bar chart
ggplot(all_df_average_long, aes(x = country, y = average_total, fill = source)) +
  geom_bar(stat = "identity") +
  labs(
    title = "10 Countries with Historically the Largest Average Renewable Energy Production Per Capita",
    x = "Country",
    y = "Average Energy Production (TWh)",
    fill = "Energy Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##############   
###### Annual Growth of Total Energy 
##############

###      Area Filled line graph      ###

# Calculate annual total energy production by source for each year
raw_df_annual <- raw_df %>%
  filter(year <= 2021) %>%
  pivot_longer(
    cols = c(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation),
    names_to = "source",
    values_to = "energy"
  ) %>%
  group_by(year, source) %>%
  summarize(
    total_energy = sum(energy, na.rm = TRUE)
  ) %>%
  ungroup()

###### Create an area plot
ggplot(raw_df_annual, aes(x = year, y = total_energy, fill = source)) +
  geom_area() +
  labs(
    title = "Annual Growth of Total Energy Production by Source",
    x = "Year",
    y = "Total Energy Production (TWh)",
    fill = "Energy Source"
  ) +
  scale_fill_manual(values = c("wind_generation" = "#a6cee3", "solar_generation" = "#FDBA74", "geo_generation" = "#b2df8a", "hydro_generation" = "#1f78b4", "nuclear_generation" = "#B39EB5"),
                    labels = c("wind_generation" = "Wind", "solar_generation" = "Solar", "geo_generation" = "Geothermal", "hydro_generation" = "Hydro", "nuclear_generation" = "Nuclear")) +
  theme_minimal() +
  theme(
      plot.title = element_text(size = 11),
      axis.title.y = element_text(size = 9),
      axis.title.x = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.4, "cm"),  
      legend.key.height = unit(0.4, "cm"))

################
##### Create a line plot ---> PER CAPITA
################

# Calculate annual total energy production by source for each year
all_df_annual <- all_df %>%
  filter(year <= 2021) %>%
  pivot_longer(
    cols = c(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation),
    names_to = "source",
    values_to = "energy"
  ) %>%
  group_by(year, source) %>%
  summarize(
    total_energy = sum(energy, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate annual per capita energy production by source for each year
ggplot(all_df_annual, aes(x = year, y = total_energy, color = source, group = source)) +
  geom_line() +
  labs(
    title = "Annual Growth of Total Energy Production by Source",
    x = "Year",
    y = "Total Energy Production (TWh)",
    color = "Energy Source"
  ) +
  scale_color_manual(values = c("wind_generation" = "#a6cee3", "solar_generation" = "#FDBA74", "geo_generation" = "#b2df8a", "hydro_generation" = "#1f78b4", "nuclear_generation" = "#B39EB5"),
                     labels = c("wind_generation" = "Wind", "solar_generation" = "Solar", "geo_generation" = "Geothermal", "hydro_generation" = "Hydro", "nuclear_generation" = "Nuclear")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Source"))

############## 
####### Top 10 Individual Country Growth
##############

###      Line Graph      ###

########################## TOTAL #######################################

# Calculate the annual total energy production for each country
raw_df_total_energy <- raw_df %>%
  group_by(country, year) %>%
  summarize(total_energy = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation)) %>%
  ungroup()

# Filter the data to include only the top 10 countries with the highest annual total energy production
top_10_countries <- raw_df_total_energy %>%
  group_by(country) %>%
  summarize(total_energy = sum(total_energy)) %>%
  ungroup() %>%
  arrange(desc(total_energy)) %>%
  slice_head(n = 10) %>% #### slice_tail for the lowest
  pull(country)

filtered_data <- raw_df_total_energy %>%
  filter(country %in% top_10_countries)

# Create an interactive line plot with hover labels
plot1 <- ggplot(filtered_data, aes(x = year, y = total_energy, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "Annual Total Renewable Energy Production of Top 10 Countries",
    x = "Year",
    y = "Energy Production (TWh)",
    color = "Country"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

# Convert the ggplot to a plotly object for interactivity
plotly_plot <- ggplotly(plot1, tooltip = "text")

# Display the interactive plot
plotly_plot

facet_plot1 <- plot1 + facet_wrap(~country, scales = 'free_y')
facet_plot1

########################## PER CAPITA #######################################
###      Line Graph      ###

# Calculate the annual total energy production for each country
all_df_total_energy <- all_df %>%
  group_by(country, year) %>%
  summarize(total_energy = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation)) %>%
  ungroup()

# Filter the data to include only the top 20 countries with the highest annual total energy production
top_20_countries <- all_df_total_energy %>%
  group_by(country) %>%
  summarize(total_energy = sum(total_energy)) %>%
  ungroup() %>%
  arrange(desc(total_energy)) %>%
  slice_head(n = 10) %>% #### slice_head for the 10 highest and slice_tail for the 10 lowest
  pull(country)

filtered_data <- all_df_total_energy %>%
  filter(country %in% top_20_countries)

# Create an interactive line plot with hover labels
plot2 <- ggplot(filtered_data, aes(x = year, y = total_energy, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "Annual Total Energy Production Over Time by Top 20 Countries",
    x = "Year",
    y = "Total Energy Production (TWh)",
    color = "Country"
  ) +
  theme_minimal()

# Convert the ggplot to a plotly object for interactivity
plotly_plot <- ggplotly(plot2, tooltip = "text")

# Display the interactive plot
plotly_plot

facet_plot2 <- plot2 + facet_wrap(~country, scales = 'free_y')
facet_plot2



########################################### CO2 DESCRIPTION ########################################### 

################
#### Mean CO2 Emissions 
################

#### DEV and Undev compared####

### PER CAPITA 

# Calculate the yearly mean for developed nations
developed_mean <- developed_df %>%
  group_by(year) %>%
  summarize(mean_CO2 = sum(CO2_emissions))

# Calculate the yearly mean for undeveloped nations
nondev_mean <- nondev_df %>%
  group_by(year) %>%
  summarize(mean_CO2 = sum(CO2_emissions))

# Combine the mean data frames
mean_df <- bind_rows(
  mutate(developed_mean, category = "Developed"),
  mutate(nondev_mean, category = "Undeveloped")
)

# Plot for Developed and Undeveloped Nations with Yearly Mean
ggplot(bind_rows(developed_df, nondev_df), aes(x = year, y = CO2_emissions, color = factor("developed"))) +
  geom_line(data = mean_df, aes(x = year, y = mean_CO2, color = category), size = 1) +
  labs(
    title = "CO2 Emissions Per Capita : Developed vs Undeveloped",
    x = "Year",
    y = "Per Capita CO2 Emissions (Tonnes)",
    color = "Category"
  ) +
  scale_color_manual(values = c("Developed" = "blue", "Undeveloped" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

#################  TOTAL


# Ensure population and CO2_emissions are numeric
developed_df1 <- all_df %>%
  mutate(product = Population * CO2_emissions)

nondev_df1 <- nondev_df %>%
  mutate(product = Population * CO2_emissions)

# Calculate the annual total for the product in developed nations
developed_total <- developed_df1 %>%
  group_by(year) %>%
  summarize(total_product = sum(product))

# Calculate the annual total for the product in undeveloped nations
nondev_total <- nondev_df1 %>%
  group_by(year) %>%
  summarize(total_product = sum(product))

# Combine the total data frames
total_product_df <- bind_rows(
  mutate(developed_total, category = "Developed"),
  mutate(nondev_total, category = "Undeveloped")
)

# Plot for Developed and Undeveloped Nations with Yearly Total (Product)
ggplot(bind_rows(developed_df, nondev_df), aes(x = year, y = product, color = factor("developed"))) +
  geom_line(data = total_product_df, aes(x = year, y = total_product, color = category), size = 1) +
  labs(
    title = "Total CO2 Emissions : Developed vs Undeveloped",
    x = "Year",
    y = "Total CO2 Emissions (Tonnes)",
    color = "Category"
  ) +
  scale_color_manual(values = c("Developed" = "blue", "Undeveloped" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

####################
##### Top 10 annual CO2 emissions growth (Line plot)
###################

# --> 1991 because of oil fire in Koweit !!!!

################## PER CAPITA

# Exclude the year 2022 from the dataset
raw_df_filtered <- developed_df %>%
  filter(!(year == 2022), !(country == "Kuwait" & year == 1991)) # purposely remove 1991 for kuwait 

# Calculate the annual total CO2 emissions for each country
raw_df_total_CO2 <- raw_df_filtered %>%
  group_by(country, year) %>%
  summarize(total_CO2_emissions = sum(CO2_emissions, na.rm = TRUE)) %>%
  ungroup()

# Filter the data to include only the top 10 countries with the highest annual total CO2 emissions
top_10_countries_CO2 <- raw_df_total_CO2 %>%
  group_by(country) %>%
  summarize(total_CO2_emissions = sum(total_CO2_emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_CO2_emissions)) %>%
  slice_head(n = 10) %>% #### slice_head for the 10 highest and slice_tail for the 10 lowest
  pull(country)

filtered_data_CO2 <- raw_df_total_CO2 %>%
  filter(country %in% top_10_countries_CO2)

# Create an interactive line plot with hover labels for CO2 emissions
plot_CO2 <- ggplot(filtered_data_CO2, aes(x = year, y = total_CO2_emissions, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "CO2 Emissions per capita Change for Developed Countries",
    x = "Year",
    y = "CO2 Emissions per capita (Tonnes)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

# Convert the ggplot to a plotly object for interactivity
plotly_CO2_capita <- ggplotly(plot_CO2, tooltip = "text")

# To display the plot in R, you would typically use the following command:
plotly_CO2_capita

###################### TOTAL

# Exclude the year 2022 from the dataset
all_df_filtered <- developed_df %>%
  filter(!(year == 2022), !(country == "Kuwait" & year == 1991), !(country == "China"), !(country == "US")) # purposely remove 1991 for kuwait 

# Calculate the annual total CO2 emissions for each country
all_df_total_CO2 <- all_df_filtered %>%
  group_by(country, year) %>%
  summarize(total_CO2_emissions = sum(CO2_emissions*Population, na.rm = TRUE)) %>%
  ungroup()

# Filter the data to include only the top 10 countries with the highest annual total CO2 emissions
top_10_countries_CO2 <- all_df_total_CO2 %>%
  group_by(country) %>%
  summarize(total_CO2_emissions = sum(total_CO2_emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_CO2_emissions)) %>%
  slice_head(n = 10) %>% #### slice_head for the 10 highest and slice_tail for the 10 lowest
  pull(country)

filtered_data_CO2 <- all_df_total_CO2 %>%
  filter(country %in% top_10_countries_CO2)

# Create an interactive line plot with hover labels for CO2 emissions
ggplot(filtered_data_CO2, aes(x = year, y = total_CO2_emissions, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "Total CO2 Emissions Change for Developing Countries",
    x = "Year",
    y = "Total CO2 Emissions (Tones)",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

# Convert the ggplot to a plotly object for interactivity
plotly_CO2_total <- ggplotly(plot_CO2, tooltip = "text")

# To display the plot in R, you would typically use the following command:
plotly_CO2_total


####################
###### CO2 emissions per capita / total ranking per country (bar plot)
####################

##### PER CAPITA

# Calculate the cumulative CO2 emissions per capita for each country
raw_df_cumulative_co2 <- developed_df %>%
  group_by(country) %>%
  summarize(
    co2_cumulative = sum(CO2_emissions, na.rm = TRUE)  
  ) %>%
  ungroup() %>%
  arrange(desc(co2_cumulative)) %>%
  top_n(10, co2_cumulative)  # Top 10 countries with the highest total CO2 emissions

# Create the bar chart for cumulative CO2 emissions per capita
ggplot(raw_df_cumulative_co2, aes(x = reorder(country, co2_cumulative), y = co2_cumulative, fill = co2_cumulative)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Cumulative CO2\nEmissions per capita") +
  labs(
    title = "Top 10 Countries with the Highest Cumulative CO2 Emissions per capita",
    x = "Country",
    y = "Cumulative CO2 Emissions per capita"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 12))

###### TOTAL

# Calculate the total CO2 emissions for each country by multiplying per capita emissions with population
raw_df_total_co2 <- raw_df %>%
  mutate(total_CO2_emissions = CO2_emissions * Population) %>%
  group_by(country) %>%
  summarize(
    co2_total = sum(total_CO2_emissions, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(co2_total)) %>%
  top_n(10, co2_total) # top_n with 10 to get the top 10

# Create the bar chart for total CO2 emissions with gradient fill
ggplot(raw_df_total_co2, aes(x = reorder(country, co2_total), y = co2_total, fill = co2_total)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Total CO2\nEmissions") +
  labs(
    title = "Top 10 Countries with the Highest Total CO2 Emissions",
    x = "Country",
    y = "Total CO2 Emissions (tons)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


########################################### PM DESCRIPTION ########################################### 

#################
##### Overall PM exposure evolution
#################

# Years for which we have PM exposure data
years_with_data <- c(1990, 1995, 2000, 2005, 2010:2019)

# # Calculate the annual total PM2.5 exposure product for developed nations for years with data
developed_pm <- rawdev_df %>%
  filter(year %in% years_with_data) %>%
  group_by(year) %>%
  summarize(total_pm = sum(PM_exposure, na.rm = TRUE)) %>%
  mutate(category = "Developed")

# # Calculate the annual total PM2.5 exposure product for non-developed nations for years with data
nondev_pm <- rawnondev_df %>%
  filter(year %in% years_with_data) %>%
  group_by(year) %>%
  summarize(total_pm = sum(PM_exposure, na.rm = TRUE)) %>%
  mutate(category = "Undeveloped")

# Combine the total data frames
total_pm_df <- bind_rows(developed_pm, nondev_pm)

# Plot for Developed and Undeveloped Nations with Yearly Total PM2.5 Exposure (Product)
plot_pm <- ggplot(total_pm_df, aes(x = year, y = total_pm, color = category, group = category)) +
  geom_line(size = 1) +
  labs(
    title = "Total PM2.5 Exposure : Developed vs Undeveloped",
    x = "Year",
    y = "Total PM2.5 Exposure (µg/m3)",
    color = "Category"
  ) +
  scale_color_manual(values = c("Developed" = "blue", "Undeveloped" = "red")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm")
  )

plot_pm


##########
#### PM exposure evolution for top 10 countries with highest PM exposure
##########

# Years for which we have PM exposure data
years_with_data <- c(1990, 1995, 2000, 2005, 2010:2019)

##### Dev
# Filter for only the years with PM exposure data
raw_df_filtered <- rawdev_df %>%
  filter(year %in% years_with_data)

# Calculate cumulative PM exposure for each country to select the 10 lowest
top_10_countries_pm <- raw_df_filtered %>%
  group_by(country) %>%
  summarize(cumulative_pm_exposure = sum(PM_exposure, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(cumulative_pm_exposure)) %>% # To have descending order
  slice_tail(n = 10) %>% # To have highest exposure
  pull(country)

# Keep only the top 10 countries with the lowest cumulative PM exposure
filtered_data_pm <- raw_df_filtered %>%
  filter(country %in% top_10_countries_pm)

# Create an interactive line plot with hover labels for PM exposure
plot_pmdev <- ggplot(filtered_data_pm, aes(x = year, y = PM_exposure, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "PM Exposure Change for Developed countries",
    x = "Year",
    y = "Annual PM Exposure",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 7),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6),
    axis.text.y = element_text(size = 5),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 5),
    axis.title.x = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm"),
    legend.margin = margin(0, 0, 0, 0),   
    legend.spacing.y = unit(0.1, "cm") 
    )

plot_pmdev
# Convert the ggplot to a plotly object for interactivity
plotly_pmdev <- ggplotly(plot_pm, tooltip = "text")

plotly_pmdev

##### Non-dev
# Filter for only the years with PM exposure data
raw_df_filtered <- rawnondev_df %>%
  filter(year %in% years_with_data)

# Calculate cumulative PM exposure for each country to select the 10 lowest
top_10_countries_pm <- raw_df_filtered %>%
  group_by(country) %>%
  summarize(cumulative_pm_exposure = sum(PM_exposure, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(cumulative_pm_exposure)) %>% # To have descending order
  slice_tail(n = 10) %>% # To have highest exposure
  pull(country)

# Keep only the top 10 countries with the lowest cumulative PM exposure
filtered_data_pm <- raw_df_filtered %>%
  filter(country %in% top_10_countries_pm)

# Create an interactive line plot with hover labels for PM exposure
plot_pmnondev <- ggplot(filtered_data_pm, aes(x = year, y = PM_exposure, group = country, color = country, text = country)) +
  geom_line() +
  labs(
    title = "PM Exposure Change for Undeveloped Countries",
    x = "Year",
    y = "Annual PM Exposure",
    color = "Country"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 7),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6),
    axis.text.y = element_text(size = 5),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 5),
    axis.title.x = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),  
    legend.key.height = unit(0.4, "cm"),
    legend.margin = margin(0, 0, 0, 0),   
    legend.spacing.y = unit(0.1, "cm") 
    )

plot_pmnondev

# Convert the ggplot to a plotly object for interactivity
plotly_pmnondev <- ggplotly(plot_pm, tooltip = "text")

plotly_pmnondev

grid.arrange(plot_pmdev, plot_pmnondev, ncol = 2)

####################
###### PM ranking per country (bar plot)
####################


# Calculate the cumulative PM exposure for each country
raw_df_cumulative_pm <- rawnondev_df %>%
  group_by(country) %>%
  summarize(
    pm_total = sum(PM_exposure, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(pm_total)) %>%
  top_n(10, pm_total) # Use top_n with 10 to get the top 10

# Reshape the data to long format for plotting
raw_df_cumulative_pm_long <- raw_df_cumulative_pm %>%
  pivot_longer(
    cols = pm_total,
    names_to = "source",
    values_to = "cumulative_total"
  )

# Create the bar chart for PM exposure
ggplot(raw_df_cumulative_pm_long, aes(x = reorder(country, cumulative_total), y = cumulative_total, fill = cumulative_total)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "red", name = "Total PM\nExposure") +
  labs(
    title = "Top 10 Countries with the Highest Cumulative PM Exposure",
    x = "Country",
    y = "Cumulative PM Exposure",
    fill = "PM Source"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


########################################### MULTIVARIATES RENEW VS CO2 ########################################### 

############################
#### Scatter by country % annual change
###########################

# Calculate yearly rate of change of total renewable energy as a percentage for each country
raw_df_rate_change <- raw_df %>%
  group_by(country, year) %>%
  summarize(
    total_renewable_energy = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation),
    CO2_emissions = sum(CO2_emissions)
  ) %>%
  group_by(country) %>%
  mutate(rate_change_renewable = c(NA, (diff(total_renewable_energy) / lag(total_renewable_energy)) * 100)[-1])

# Remove rows with NA or non-finite values in CO2_emissions or rate_change_renewable
raw_df_rate_change <- raw_df_rate_change %>%
  filter(!is.na(CO2_emissions), !is.na(rate_change_renewable)) %>%
  filter(is.finite(CO2_emissions), is.finite(rate_change_renewable))

# Create scatter plot with a single line of best fit for all data
gg_plot <- ggplot(raw_df_rate_change, aes(x = CO2_emissions, y = rate_change_renewable)) +
  geom_point(aes(color = country), size = 2) +  # Adjust point size here
  geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.8) +  # Single smooth line in black
  labs(
    title = "Scatter Plot with Line of Best Fit for Yearly Rate of Change of Total Renewable Energy",
    x = "CO2 Emissions",
    y = "Yearly Rate of Change of Total Renewable Energy (%)",
    caption = "Source: Your Data Source"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Convert ggplot to plotly for interactivity
plotly_plot <- ggplotly(gg_plot, tooltip = "text")

# Display the interactive plot
plotly_plot


########################
### Scatter Plot Total energy annual % change of renewable energy VS CO2
########################

# Calculate yearly rate of change of total renewable energy as a percentage
raw_df_rate_change <- raw_df %>%
  group_by(year) %>%
  summarize(
    total_renewable_energy = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation),
    CO2_emissions = sum(CO2_emissions)
  ) %>%
  mutate(rate_change_renewable = c(NA, (diff(total_renewable_energy) / lag(total_renewable_energy)) * 100)[-1]) %>%
  mutate(CO2_emissions_change = (CO2_emissions / lag(CO2_emissions) - 1) * 100)

# Create scatter plot with a line of best fit using ggplot
gg_plot <- ggplot(raw_df_rate_change, aes(x = CO2_emissions_change, y = rate_change_renewable)) +
  geom_point(aes(text = as.character(year)), size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +
  labs(
    title = "Scatter Of Total Annual Change (%)",
    x = "Yearly Rate of Change of CO2 Emissions (%)",
    y = "Yearly Rate of Change of Total Renewable Energy (%)",
  ) +
  theme_minimal()

# Convert ggplot to plotly for interactivity
plotly_plot <- ggplotly(gg_plot, tooltip = "text")

# Display the interactive plot
plotly_plot


##############
#### Avg. renewable energy generated VS CO2 emissions 
##############

# Regression line is flat because no relation for most countries but there is still a pattern for some countries
# Some countries with low renewable energy generations have high PM exposure (same that have CO2 emissions)
# Some countries with high renewable energy generations have low PM exposure

#### Per capita

# Calculate average renewable energy production across the years
avg_renewablesall <- all_df %>%
  group_by(country) %>%
  summarize(
    avg_wind = mean(wind_generation, na.rm = TRUE),
    avg_solar = mean(solar_generation, na.rm = TRUE),
    avg_geo = mean(geo_generation, na.rm = TRUE),
    avg_hydro = mean(hydro_generation, na.rm = TRUE),
    avg_nuclear = mean(nuclear_generation, na.rm = TRUE)
  ) %>%
  mutate(
    avg_renewables = rowMeans(select(., starts_with("avg")), na.rm = TRUE)
  )

# Calculate average CO2 emissions across the years
avg_co2 <- all_df %>%
  group_by(country) %>%
  summarize(avg_CO2 = mean(CO2_emissions, na.rm = TRUE))

# Merge the two datasets
merged_data <- merge(avg_renewablesall, avg_co2, by = "country")

# Create a scatter plot with hover labels and trend line
ggplot(merged_data, aes(x = avg_renewables, y = avg_CO2, label = country)) +
  geom_point() +
  geom_text_repel(box.padding = 0.5, segment.size = 0.2, max.overlaps = 30) +  # Add labels on hover
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(
    title = "Per Capita Average : Renewable Energy Generation vs CO2 Emissions",
    x = "Average Per Capita Renewable Energy Generation (TWh)",
    y = "Average Per Capita CO2 Emissions (Tonnes)"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    )

### Total 

# Calculate average renewable energy production across the years
avg_renewables <- raw_df %>%
  group_by(country) %>%
  summarize(
    avg_wind = mean(wind_generation, na.rm = TRUE),
    avg_solar = mean(solar_generation, na.rm = TRUE),
    avg_geo = mean(geo_generation, na.rm = TRUE),
    avg_hydro = mean(hydro_generation, na.rm = TRUE),
    avg_nuclear = mean(nuclear_generation, na.rm = TRUE)
  ) %>%
  mutate(
    avg_renewables = rowMeans(select(., starts_with("avg")), na.rm = TRUE)
  )

# Calculate average CO2 emissions across the years
avg_co2 <- raw_df %>%
  group_by(country) %>%
  summarize(avg_CO2 = mean(CO2_emissions * Population, na.rm = TRUE))

# Merge the two datasets
merged_data <- merge(avg_renewables, avg_co2, by = "country")

# Create a scatter plot with hover labels and trend line
ggplot(merged_data, aes(x = avg_CO2, y = avg_renewables, label = country)) +
  geom_point() +
  geom_text_repel(box.padding = 0.5, segment.size = 0.2) +  # Add labels on hover
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(
    title = "Total Average : Renewable Energy Generation vs CO2 Emissions",
    x = "Average Total Renewable Energy Generation (TWh)", 
    y = "Average Total CO2 Emissions (Tonnes)"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
  )

##########################
####### RENEWABLE ENERGY VS CO2 EMISSIONS PER COUNTRY (FACET GRID) for top 10
##########################


# Filter out the year 2022 as we don't have that observation for CO2
all_df_filtered <- all_df %>%
  filter(year != 2022)

# Calculate total renewable energy production for each country and year
total_renewables <- all_df_filtered %>%
  group_by(country, year) %>%
  summarize(
    total_renewables = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate CO2 emissions for each country and year
co2_emissions <- all_df_filtered %>%
  group_by(country, year) %>%
  summarize(CO2_emissions = sum(CO2_emissions, na.rm = TRUE)) %>%
  ungroup()

# Merge the total renewables data with the CO2 emissions data
merged_data <- merge(total_renewables, co2_emissions, by = c("country", "year"))

# Identify the top 10 countries with the highest total renewable energy production
top_countries <- merged_data %>%
  group_by(country) %>%
  summarize(total_renewables = sum(total_renewables, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_renewables)) %>%
  slice_head(n = 9) %>%
  pull(country)

# Filter the data for only these top countries
filtered_data <- merged_data %>%
  filter(country %in% top_countries)

# Normalize the data for CO2 and renewable energy to allow using a secondary y-axis
max_CO2 <- max(filtered_data$CO2_emissions, na.rm = TRUE)
max_renewables <- max(filtered_data$total_renewables, na.rm = TRUE)
filtered_data <- filtered_data %>%
  mutate(norm_CO2 = CO2_emissions / max_CO2,
         norm_renewables = total_renewables / max_renewables)

ggplot(filtered_data, aes(x = year)) +
  geom_line(aes(y = norm_renewables, group = country, color = country, linetype = "Renewable Energy")) +
  geom_line(aes(y = norm_CO2, group = country, color = country, linetype = "CO2 Emissions"), color = "grey") +
  facet_wrap(~ country, scales = 'free_x') + # Allows each country to have its own x-axis scale
  scale_y_continuous(
    "Normalized Renewable Energy Generation",
    sec.axis = sec_axis(~ ., name = "Normalized CO2 Emissions")
  ) +
  labs(
    title = "Renewable Energy Generation vs CO2 Emissions by Country and Year",
    x = "Year"
  ) +
  scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data$country))),
                     breaks = unique(filtered_data$country),
                     labels = unique(filtered_data$country),
                     name = "Country") +
  scale_linetype_manual(values = c("Renewable Energy" = "solid", "CO2 Emissions" = "dashed"),
                        name = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################### MULTIVARIATES RENEW VS PM ########################################## 

##############
##### AVG RENEWABLE ENERGY VS AVERAGE PM EXPOSURE
##############


# Calculate average renewable energy production across the years
avg_renewablesall <- all_df %>%
  group_by(country) %>%
  summarize(
    avg_wind = mean(wind_generation, na.rm = TRUE),
    avg_solar = mean(solar_generation, na.rm = TRUE),
    avg_geo = mean(geo_generation, na.rm = TRUE),
    avg_hydro = mean(hydro_generation, na.rm = TRUE),
    avg_nuclear = mean(nuclear_generation, na.rm = TRUE)
  ) %>%
  mutate(
    avg_renewables = rowMeans(select(., starts_with("avg")), na.rm = TRUE)
  )

# Calculate average PM exposure across the years
avg_pm <- raw_df %>%
  group_by(country) %>%
  summarize(avg_PM_exposure = mean(PM_exposure, na.rm = TRUE))

# Merge the two datasets
merged_data_pm <- merge(avg_renewablesall, avg_pm, by = "country")

# Create a scatter plot with hover labels and trend line
ggplot(merged_data_pm, aes(x = avg_renewables, y = avg_PM_exposure, label = country)) +
  geom_point() +
  geom_text_repel(aes(label = country), box.padding = 0.5, segment.size = 0.2, max.overlaps = 10) +  # Add labels to avoid overlap
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(
    title = "Average Renewable Energy Per Capita vs Average PM Exposure",
    x = "Average Renewable Energy Per Capita",
    y = "Average PM Exposure"
  ) +
  theme_minimal()  + 
  theme(
    plot.title = element_text(size = 11),
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
  )

########################
### Scatter Plot Total energy annual % change of renewable energy VS PM
########################

# Calculate yearly rate of change of total renewable energy as a percentage
raw_df_rate_change <- raw_df %>%
  group_by(year) %>%
  summarize(
    total_renewable_energy = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation),
    PM_exposure = sum(PM_exposure)
  ) %>%
  mutate(rate_change_renewable = c(NA, (diff(total_renewable_energy) / lag(total_renewable_energy)) * 100)[-1]) %>%
  mutate(PM_exposure_change = (PM_exposure / lag(PM_exposure) - 1) * 100)

# Create scatter plot with a line of best fit using ggplot
gg_plot <- ggplot(raw_df_rate_change, aes(x = PM_exposure_change, y = rate_change_renewable)) +
  geom_point(aes(text = as.character(year)), size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.5) +
  labs(
    title = "Scatter Of Total Annual Change (%)",
    x = "Yearly Rate of Change of PM Exposure (%)",
    y = "Yearly Rate of Change of Total Renewable Energy (%)",
  ) +
  theme_minimal()

# Convert ggplot to plotly for interactivity
plotly_plot <- ggplotly(gg_plot, tooltip = "text")

# Display the interactive plot
plotly_plot

##########################
####### RENEWABLE ENERGY VS PM EXPOSURE PER COUNTRY (FACET GRID) for top 10
##########################

# Years for which we have PM exposure data
years_with_data <- c(1990, 1995, 2000, 2005, 2010:2019)

# Calculate total renewable energy production for each country and year
total_renewables <- all_df %>%
  group_by(country, year) %>%
  summarize(
    total_renewables = sum(wind_generation, solar_generation, geo_generation, hydro_generation, nuclear_generation, na.rm = TRUE)
  ) %>%
  ungroup()

# Filter out the years we don't have data for PM exposure
raw_df_filtered <- raw_df %>%
  filter(year %in% years_with_data)

# Calculate PM exposure for each country and year
pm_exposure <- raw_df_filtered %>%
  group_by(country, year) %>%
  summarize(PM_exposure = mean(PM_exposure, na.rm = TRUE)) %>%
  ungroup()

# Merge the total renewables data with the PM exposure data
merged_data <- merge(total_renewables, pm_exposure, by = c("country", "year"))

# Identify the top 10 countries with the highest total renewable energy production
top_countries <- merged_data %>%
  group_by(country) %>%
  summarize(total_renewables = sum(total_renewables, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_renewables)) %>%
  slice_head(n = 9) %>%
  pull(country)

# Filter the data for only these top countries
filtered_data <- merged_data %>%
  filter(country %in% top_countries)

# Normalize the data for PM exposure and renewable energy to allow using a secondary y-axis
max_PM <- max(filtered_data$PM_exposure, na.rm = TRUE)
max_renewables <- max(filtered_data$total_renewables, na.rm = TRUE)
filtered_data <- filtered_data %>%
  mutate(norm_PM = PM_exposure / max_PM,
         norm_renewables = total_renewables / max_renewables)

# Create a line plot with two separate y-axes and legends for PM Exposure and CO2 Emissions
ggplot(filtered_data, aes(x = year)) +
  geom_line(aes(y = norm_renewables, group = country, color = country, linetype = "Renewable Energy")) +
  geom_line(aes(y = norm_PM, group = country, color = country, linetype = "PM Exposure"), color = "grey") +
  facet_wrap(~ country, scales = 'free_x') + # Allows each country to have its own x-axis scale
  scale_y_continuous(
    "Normalized Renewable Energy Generation",
    sec.axis = sec_axis(~ ., name = "Normalized PM Exposure")
  ) +
  labs(
    title = "Renewable Energy Generation vs PM Exposure by Country and Year",
    x = "Year"
  ) +
  scale_color_manual(values = scales::hue_pal()(length(unique(filtered_data$country))),
                     breaks = unique(filtered_data$country),
                     labels = unique(filtered_data$country),
                     name = "Country") +
  scale_linetype_manual(values = c("Renewable Energy" = "solid", "PM Exposure" = "dashed"),
                        name = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


########################################### MULTIVARIATES PM VS CO2 ########################################## 

##############
##### AVG CO2 EMISSIONS VS AVERAGE PM EXPOSURE
##############


# Calculate average CO2 emissions across the years for each country
avg_co2 <- all_df %>%
  group_by(country) %>%
  summarize(avg_CO2_emissions = mean(CO2_emissions, na.rm = TRUE))

# Calculate average PM exposure across the years for each country
avg_pm <- all_df %>%
  group_by(country) %>%
  summarize(avg_PM_exposure = mean(PM_exposure, na.rm = TRUE))

# Merge the two datasets on country
merged_data <- merge(avg_co2, avg_pm, by = "country")

# Create a scatter plot with hover labels and trend line
ggplot(merged_data, aes(x = avg_CO2_emissions, y = avg_PM_exposure, label = country)) +
  geom_point() +
  geom_text_repel(aes(label = country), box.padding = 0.5, segment.size = 0.2) +  # Add labels to avoid overlap
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(
    title = "Average CO2 Emissions vs Average PM Exposure",
    x = "Average CO2 Emissions",
    y = "Average PM Exposure"
  ) +
  theme_minimal()


##########################
####### CO2 EMISSIONS VS PM EXPOSURE PER COUNTRY (FACET GRID) for top 10
##########################

# Filter out the years we don't have data for PM exposure
all_df_filtered <- all_df %>%
  filter(year %in% years_with_data)

# Calculate CO2 emissions for each country and year
co2_emissions <- all_df_filtered %>%
  group_by(country, year) %>%
  summarize(CO2_emissions = sum(CO2_emissions, na.rm = TRUE)) %>%
  ungroup()

# Calculate PM exposure for each country and year
pm_exposure <- all_df_filtered %>%
  group_by(country, year) %>%
  summarize(PM_exposure = mean(PM_exposure*Population, na.rm = TRUE)) %>%
  ungroup()

# Merge the CO2 emissions data with the PM exposure data
merged_data <- merge(co2_emissions, pm_exposure, by = c("country", "year"))

# Identify the top 10 countries with the highest CO2 emissions
top_countries <- merged_data %>%
  group_by(country) %>%
  summarize(total_CO2 = sum(CO2_emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(total_CO2)) %>%
  slice_head(n = 9) %>%
  pull(country)

# Filter the data for only these top countries
filtered_data <- merged_data %>%
  filter(country %in% top_countries)

# Normalize the data for PM exposure and CO2 emissions to allow using a secondary y-axis
max_PM <- max(filtered_data$PM_exposure, na.rm = TRUE)
max_CO2 <- max(filtered_data$CO2_emissions, na.rm = TRUE)
filtered_data <- filtered_data %>%
  mutate(norm_PM = PM_exposure / max_PM,
         norm_CO2 = CO2_emissions / max_CO2)

# Create a line plot with two separate y-axes
ggplot(filtered_data, aes(x = year)) +
  geom_line(aes(y = norm_CO2, group = country), color = "red") +  # CO2 Emissions in red
  geom_line(aes(y = norm_PM, group = country), color = "blue") +  # PM Exposure in blue
  facet_wrap(~ country, scales = 'free_x') + # Allows each country to have its own x-axis scale
  scale_y_continuous(
    "Normalized CO2 Emissions",
    sec.axis = sec_axis(~ . * max_PM / max_CO2, name = "Normalized PM Exposure")
  ) +
  labs(
    title = "CO2 Emissions vs PM Exposure by Country and Year",
    x = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none") # Hide the legend for clarity

