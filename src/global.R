
# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr, warn.conflicts = FALSE)
library(stringr)
library(data.table)
library(DT)
library(ggplot2) # install with devtools::install_github('hadley/ggplot2') for compatibility with plotly
library(plotly)
library(forcats)
library(ggthemes)
library(shinyjs)
library(here)



# Load required data
load(here("data/nutri_clean.RData"))
load(here("data/nutri_wide.RData"))
load(here("data/nutri_new.RData"))
load(here("data/nutri_choice.RData"))
load(here("data/food_wide.RData"))

## Define default nutrients for recipes section
RecipeNutrients <-  nutri_wide %>% 
        filter(str_detect(Nutrient, "ENERCC|\\(CHO\\)|PROT|\\(FAT\\)")) %>% 
        select(NutrientID) %>% 
        as_vector()


### Features

#A simple app but with a lot of working components

# Compare food ingredients and prepare recipes
# Add & Remove buttons change input_current$ingredients.
# input_current$ingredients is a reactive value, which means that 
# the dependants of this will be notified if this changes.
# nutri_filtered depends on input_current$ingredients.
# nutri_sum depends on nutri_filtered.
# output$RecipeTable depends on nutri_sum.
# There is a cascading effect of several things being updated
### Reference: See Pattern 3
# https://shiny.rstudio.com/articles/action-buttons.html
# https://www.youtube.com/watch?v=63KnV4XWsR0

# Note: nutri_filtered also depends on input$NutrientSub, but we want 
# it to update the table only when we press the AddIngredient. Since 
# the input$NutrientSub is reactive, we need to remove its reactiveness
# by using isolate(). This means that nutri_filtered wants to use the 
# NutrientSub value, but does not want to be notified as soon as the 
# value changes because this would cause a cascading effect up to the 
# table being updated.
### Reference: See Isolate
# https://shiny.rstudio.com/articles/isolation.html