#############################################
## The following loads the needed packages ##
#############################################

# load the required packages
packages <- c(
  "knitr",
  "tidyverse", 
  "stargazer",
  "plm",
  "ggrepel", 
  "reshape2", 
  "corrplot", 
  "hrbrthemes", 
  "viridis", 
  "plotly"
)
purrr::walk(packages, library, character.only = TRUE)

######################################################
## The following sets a few option for nice reports ##
######################################################

# general options
options(
  digits = 3,
  str = strOptions(strict.width = "cut"),
  width = 70,
  tibble.width = 70,
  cli.unicode = FALSE
)

# ggplot options
theme_set(theme_light())

# knitr options
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  # cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "90%",
  out.height = "100%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618,
  fig.show = "hold",
  message = FALSE,
  echo = FALSE
)

