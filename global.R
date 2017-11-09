
# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
library(data.table)
library(DT)
library(ggplot2) # install with devtools::install_github('hadley/ggplot2') for compatibility with plotly
library(plotly)
library(forcats)


# Load required data
load("data/nutri_clean.RData")
load("data/nutri_wide.RData")
load("data/nutri_new.RData")