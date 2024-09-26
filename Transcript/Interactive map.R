#######################################################################################
######                             Interactive map                                #######
#######################################################################################

library(plotly)
library(ggplot2)

# Set the working directory
setwd(here::here("Transcript"))

# Load and run the data extraction script
source("Data extraction.R")

#############
#### Energy Map 1
#############


filtered_data_raw <- raw_df %>% filter(year == 2019)
filtered_data_all <- all_df %>% filter(year == 2019)

# updatemenus component
updatemenus <- list(
  list(
    active = 0,
    x = 0.2,
    y = 0.99,
    buttons = list(
      list(
        label = "Solar",
        method = "update",
        args = list(list(visible = c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)))
      ),
      list(
        label = "Wind",
        method = "update",
        args = list(list(visible = c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE)))
      ),
      list(
        label = "Hydro",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)))
      ),
      list(
        label = "Nuclear",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE)))
      ),
      list(
        label = "Geo",
        method = "update",
        args = list(list(visible = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)))
      )
    )
  )
)

### TOTAL 

fig1 <- plot_geo(filtered_data_raw, locationmode = 'country names') %>%
  add_trace(
    z = ~solar_generation,
    locations = ~country,
    color = ~solar_generation,
    colors = 'Reds',
    name = 'Solar',
    colorbar = list(title = "Solar Generation (TWh)", len = 1, y = 0.1, orientation = "h")
  ) %>%
  add_trace(
    z = ~wind_generation,
    locations = ~country,
    color = ~wind_generation,
    name = 'Wind',
    colorbar = list(title = "Wind Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~hydro_generation,
    locations = ~country,
    color = ~hydro_generation,
    name = 'Hydro',
    colorbar = list(title = "Hydro Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~nuclear_generation,
    locations = ~country,
    color = ~nuclear_generation,
    name = 'Nuclear',
    colorbar = list(title = "Nuclear Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~geo_generation,
    locations = ~country,
    color = ~geo_generation,
    name = 'Geo',
    colorbar = list(title = "Geo Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    type = 'scattergeo',  # Specify the trace type
    mode = 'markers',     # Use markers
    locations = ~country, # Specify countries as locations
    marker = list(
      size = ~CO2_emissions,      # Size of the markers based on CO2 emissions
      color = 'black',                 # Marker color
      sizemode = 'area',               # The size of the marker represents an area
      sizeref = 0.08,                   # Adjust this value to scale marker sizes
      line = list(color = "rgb(40,40,40)", width = 0.5)
    ),
    name = 'Total CO2 Emissions'
  ) %>%
  add_trace(
    type = 'scattergeo',  # Specify the trace type
    mode = 'markers',     # Use markers
    locations = ~country, # Specify countries as locations
    marker = list(
      size = ~PM_exposure,      # Size of the markers based on CO2 emissions
      color = 'grey',                 # Marker color
      sizemode = 'area',               # The size of the marker represents an area
      sizeref = 0.2,                   # Adjust this value to scale marker sizes
      line = list(color = "rgb(40,40,40)", width = 0.5)
    ),
    name = 'PM Exposure'
  ) %>%
  layout(
    title = "Total Renewable Energy Generation (2019)",
    showlegend = TRUE,
    updatemenus = updatemenus,
    geo = list(
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    ),
    annotations = list(
      list(
        text = "<b>Click to hide circle</b>",  
        x = 1.04,  
        y = 0.19,     
        xref = "paper",
        yref = "paper",
        xanchor = 'left',  
        yanchor = 'top',   
        align = 'left',
        showarrow = FALSE,
        font = list(
          size = 12,  
          color = "black"  
        )
      )
    ),
    margin = list(
      t = 100  # Reduce the top margin to bring the title closer to the map
    )
  )

fig1

### PER CAPITA

fig2 <- plot_geo(filtered_data_all, locationmode = 'country names') %>%
  add_trace(
    z = ~solar_generation,
    locations = ~country,
    color = ~solar_generation,
    colors = 'Reds',
    name = 'Solar',
    colorbar = list(title = "Solar Generation (TWh)", len = 1, y = 0.1, orientation = "h")
  ) %>%
  add_trace(
    z = ~wind_generation,
    locations = ~country,
    color = ~wind_generation,
    name = 'Wind',
    colorbar = list(title = "Wind Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~hydro_generation,
    locations = ~country,
    color = ~hydro_generation,
    name = 'Hydro',
    colorbar = list(title = "Hydro Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~nuclear_generation,
    locations = ~country,
    color = ~nuclear_generation,
    name = 'Nuclear',
    colorbar = list(title = "Nuclear Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    z = ~geo_generation,
    locations = ~country,
    color = ~geo_generation,
    name = 'Geo',
    colorbar = list(title = "Geo Generation (TWh)", len = 1, y = 0.1, orientation = "h"),
    visible = FALSE
  ) %>%
  add_trace(
    type = 'scattergeo',  # Specify the trace type
    mode = 'markers',     # Use markers
    locations = ~country, # Specify countries as locations
    marker = list(
      size = ~CO2_emissions,      # Size of the markers based on CO2 emissions
      color = 'black',                 # Marker color
      sizemode = 'area',               # The size of the marker represents an area
      sizeref = 0.08,                   # Adjust this value to scale marker sizes
      line = list(color = "rgb(40,40,40)", width = 0.5)
    ),
    name = 'CO2 Emissions per capita'
  ) %>%  
  add_trace(
    type = 'scattergeo',  # Specify the trace type
    mode = 'markers',     # Use markers
    locations = ~country, # Specify countries as locations
    marker = list(
      size = ~(PM_exposure*Population),      # Size of the markers based on CO2 emissions
      color = 'grey',                 # Marker color
      sizemode = 'area',               # The size of the marker represents an area
      sizeref = 0.2,                   # Adjust this value to scale marker sizes
      line = list(color = "rgb(40,40,40)", width = 0.5)
    ),
    name = 'PM Exposure'
  ) %>%
  layout(
    title = "Renewable Energy Generation per capita (2019)",
    showlegend = TRUE,
    updatemenus = updatemenus,
    geo = list(
      showland = TRUE,
      landcolor = toRGB("gray95"),
      countrycolor = toRGB("gray80")
    ),
    annotations = list(
      list(
        text = "<b>Click to hide circle</b>",  
        x = 1.04,  
        y = 0.19,     
        xref = "paper",
        yref = "paper",
        xanchor = 'left',  
        yanchor = 'top',   
        align = 'left',
        showarrow = FALSE,
        font = list(
          size = 12,  
          color = "black"  
        )
      )
    ),
    margin = list(
      t = 100  # Reduce the top margin to bring the title closer to the map
    )
  )

fig2

