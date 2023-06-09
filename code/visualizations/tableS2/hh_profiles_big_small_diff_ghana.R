# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: May 16, 2023
# Purpose: Compare the household demographic and livelihood characteristics for (1) households with 50+ percentage point differences and (2) households with < 10 percentage point differences when ranked by their PPI scores, DHS scores, and their regular expenditures in Ghana
# Inputs: Cleaned dataset (ghana_listing_selected_vars_cleaned.rds) generated by clean_ghana.R
# Outputs: Supplementary Table 2
# Sample sizes: Rural (n = 3,064), Urban (n = 872) households after removing PPI, DHS, and RegExp NAs

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggpubr)
library(table1)

# source relevant files ---------------------------------------------------

source(here("code", "cleaning", "clean_ghana.R"))

# import data -------------------------------------------------------------

ghana_listing_cleaned <- readRDS(here("data", "ghana", "ghana_listing_selected_vars_cleaned.rds"))

# created a combined, full sample dataset ----------------------------------------

ghana_profiles_data <- 
  ghana_listing_cleaned %>% 
  filter(
    is.na(ppi_prob_1.90) == FALSE, 
    is.na(wi_urbrrl) == FALSE, 
    is.na(regexppcpd) == FALSE, 
  ) %>% 
  
  # assign percentile ranks at the rural/urban level
  
  group_by(
    rur_urb
  ) %>%
  
  mutate(
    country = "ghana",
    district = ifelse(
      district == "Asutifi_North", "Asutifi North", "Wassa East"
    ),
    ppi_percentile = percent_rank(desc(ppi_prob_1.90)),
    dhs_percentile = percent_rank(wi_urbrrl),
    regexp_percentile = percent_rank(regexppcpd),
    abs_diff_ppi_dhs_percentile = abs(ppi_percentile - dhs_percentile)*100,
    abs_diff_ppi_regexp_percentile = abs(ppi_percentile - regexp_percentile)*100,
    abs_diff_dhs_regexp_percentile = abs(dhs_percentile - regexp_percentile)*100
  ) %>%
  
  ungroup() %>%
  
  select(
    ppi_prob_1.90,
    wi_urbrrl,
    regexppcpd,
    country,
    district,
    rur_urb,
    
    # percentiles data
    
    abs_diff_ppi_dhs_percentile,
    abs_diff_ppi_regexp_percentile,
    abs_diff_dhs_regexp_percentile,
    
    # socio demographic data
    
    hoh_completed_prim_school,
    fem_hoh,
    dependency_ratio,
    grew_crops,
    earned_nonfarm,
    remittances_friends_family
  ) %>% 
  
  mutate(
    
    # create variables indicating large or small change in percentiles
    
    abs_diff_ppi_dhs_percentile_size = ifelse(
      abs_diff_ppi_dhs_percentile < 10, "Small difference (< 10 pct pts)", ifelse(
        abs_diff_ppi_dhs_percentile > 50, "Large difference (> 50 pct pts)", "Moderate difference"
      )
    ),
    
    abs_diff_ppi_regexp_percentile_size = ifelse(
      abs_diff_ppi_regexp_percentile < 10, "Small difference (< 10 pct pts)", ifelse(
        abs_diff_ppi_regexp_percentile > 50, "Large difference (> 50 pct pts)", "Moderate difference"
      )
    ),
    
    abs_diff_dhs_regexp_percentile_size = ifelse(
      abs_diff_dhs_regexp_percentile < 10, "Small difference (< 10 pct pts)", ifelse(
        abs_diff_dhs_regexp_percentile > 50, "Large difference (> 50 pct pts)", "Moderate difference"
      )
    ),
    
    # label profile variables for ease of visualization in tables
    
    hoh_completed_prim_school = factor(
      hoh_completed_prim_school,
      levels = c(1,0),
      labels = c("Completed primary school",
                 "Didn't complete primary school")
    ),
    
    fem_hoh = factor(
      fem_hoh,
      levels = c(1,0),
      labels = c("Female",
                 "Male")
    ),
    
    grew_crops = factor(
      grew_crops,
      levels = c(1,0),
      labels = c("Grows crops","Does not grow crops")
    ),
    
    earned_nonfarm = factor(
      earned_nonfarm,
      levels = c(1,0),
      labels = c("Earns from non-farm activities", "Does not earn from non-farm activities")
    ),
    
    remittances_friends_family = factor(
      remittances_friends_family,
      levels = c(1,0),
      labels = c("Receives remittances", "Does not receive remittances")
    )
  ) %>%
  
  rename(
    `HOH completed primary school` = hoh_completed_prim_school,
    `Sex of HOH` = fem_hoh,
    `Dependency ratio (others: 15-64)` = dependency_ratio,
    `Crop cultivation` = grew_crops,
    `Non-farm livelihoods` = earned_nonfarm,
    `Remittances` = remittances_friends_family
  )

# profiles of rural households in Ghana ------------------------------

# profiles for rural households in Ghana (for big and small ppi-dhs delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_ppi_dhs_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "rural"
         )
)

# profiles for rural households in Ghana (for big and small ppi-regexp delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_ppi_regexp_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "rural"
         )
)

# profiles for rural households in Ghana (for big and small dhs-regexp delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_dhs_regexp_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "rural"
         )
)

# profiles of urban households in Ghana ------------------------------

# profiles for urban households in Ghana (for big and small ppi-dhs delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_ppi_dhs_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "urban"
         )
)

# profiles for urban households in Ghana (for big and small ppi-regexp delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_ppi_regexp_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "urban"
         )
)

# profiles for urban households in Ghana (for big and small dhs-regexp delta)
table1(~`HOH completed primary school` + `Sex of HOH` + `Dependency ratio (others: 15-64)` + `Crop cultivation` + `Non-farm livelihoods` + `Remittances`| abs_diff_dhs_regexp_percentile_size, 
       data = ghana_profiles_data %>% 
         filter(
           rur_urb == "urban"
         )
)
