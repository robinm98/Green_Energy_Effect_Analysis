#######################################################################################
######                            Data modelling                                #######
#######################################################################################

library(plm)
# Set the working directory
setwd(here::here("Transcript"))

# Load and run the data extraction script
source("Data extraction.R")

###########
### LOG MODELLING
###########

########### Linear model 

### FOR NON-DEV

# Add logarithmic transformations of energy and CO2 variables
scaling_factor <- 100000

log_nondev_1990 <- nondev_1990 %>%
  mutate(
    log_wind = log(wind_generation * scaling_factor),
    log_solar = log(solar_generation * scaling_factor),
    log_geo = log(geo_generation * scaling_factor),
    log_hydro = log(hydro_generation * scaling_factor),
    log_nuclear = log(nuclear_generation * scaling_factor),
    log_CO2 = log(CO2_emissions)
  )

# Assuming log_all_1990 is your data frame
log_nondev_1990 <- log_nondev_1990 %>%
  mutate_at(vars(-country), ~ ifelse(is.infinite(.), 0, .))


# Fit a multiple linear regression model
nondev_model <- lm(log_CO2 ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear, data = log_nondev_1990)

# Summarize the model
summary(nondev_model)

### FOR DEV

# Add logarithmic transformations of energy and CO2 variables
scaling_factor <- 100000

log_dev_1990 <- developed_1990 %>%
  mutate(
    log_wind = log(wind_generation * scaling_factor),
    log_solar = log(solar_generation * scaling_factor),
    log_geo = log(geo_generation * scaling_factor),
    log_hydro = log(hydro_generation * scaling_factor),
    log_nuclear = log(nuclear_generation * scaling_factor),
    log_CO2 = log(CO2_emissions)
  )

# Assuming log_all_1990 is your data frame
log_dev_1990 <- log_dev_1990 %>%
  mutate_at(vars(-country), ~ ifelse(is.infinite(.), 0, .))


# Fit a multiple linear regression model
dev_model <- lm(log_CO2 ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear, data = log_dev_1990)

# Summarize the model
summary(dev_model)


### FOR ALL

# Add logarithmic transformations of energy and CO2 variables
scaling_factor <- 100000

log_all_1990 <- all_1990 %>%
  mutate(
    log_wind = log(wind_generation * scaling_factor),
    log_solar = log(solar_generation * scaling_factor),
    log_geo = log(geo_generation * scaling_factor),
    log_hydro = log(hydro_generation * scaling_factor),
    log_nuclear = log(nuclear_generation * scaling_factor),
    log_CO2 = log(CO2_emissions)
  )

# Assuming log_all_1990 is your data frame
log_all_1990 <- log_all_1990 %>%
  mutate_at(vars(-country), ~ ifelse(is.infinite(.), 0, .))


# Fit a multiple linear regression model
all_model <- lm(log_CO2 ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear, data = log_all_1990)

# Summarize the model
summary(all_model)


########################################
#######Current best Model###############
########################################

######### WITH FE

### FOR DEVELOPED COUNTRIES 

# Creation of the data frame for the fixed effect
log_developed_1990_p <- pdata.frame(log_developed_1990, index = c("country", "year"))

#Creation of a model with time fixed effect
dev_model <- plm(CO2_emissions ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear,
                 data = log_developed_1990_p,
                 effect = "time", # This specifies time fixed effects
                 model = "within") # This chooses the within estimator, which is for fixed effects

#Visualization
summary(dev_model)


### FOR NON-DEV

# Creation of the data frame for the fixed effect
log_nondev_1990_p <- pdata.frame(log_nondev_1990, index = c("country", "year"))

#Creation of a model with time fixed effect
nondev_model <- plm(CO2_emissions ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear,
                    data = log_nondev_1990_p,
                    effect = "time", # This specifies time fixed effects
                    model = "within") # This chooses the within estimator, which is for fixed effects

#Visualization
summary(nondev_model)


### FOR ALL

# Creation of the data frame for the fixed effect
log_all_1990_p <- pdata.frame(log_all_1990, index = c("country", "year"))

#Creation of a model with time fixed effect
all_model <- plm(CO2_emissions ~ log_wind + log_solar + log_geo + log_hydro + log_nuclear,
                    data = log_all_1990_p,
                    effect = "time", # This specifies time fixed effects
                    model = "within") # This chooses the within estimator, which is for fixed effects

#Visualization
summary(all_model)
