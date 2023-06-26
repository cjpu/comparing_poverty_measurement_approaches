# header ------------------------------------------------------------------

# Author: Christine Pu (cjpu@stanford.edu)
# Date Created: February 23, 2023
# Purpose: Visualize accordance between "bottom 10" (i.e., households with the lowest 10 PPI scores) and "top 10" (i.e., households with the highest 10 PPI scores) households based on their PPI scores, DHS scores, regular expenditures per capita, Krishna's Stages of Progress in urban Ghana
# Inputs: Cleaned dataset (ghana_listing_selected_vars_cleaned.rds) produced by clean_ghana.R
# Outputs: Supplementary Figure 3
# Sample sizes: Urban Wassa East (n = 209)

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)
library(formattable)
library(RColorBrewer)

# source relevant files ---------------------------------------------------

source(here("code", "cleaning", "clean_ghana.R"))

# import data -------------------------------------------------------------

ghana_listing_cleaned <- readRDS(here("data", "ghana", "ghana_listing_selected_vars_cleaned.rds"))

# defining top 10 and bottom 10 ranks for urban wassa east households only  ----------------------------------------------

t10_dhs_urban_wassaeast = 200
b10_dhs_urban_wassaeast = 10
t10_ppi_urban_wassaeast = 194
b10_ppi_urban_wassaeast = 9
t10_regexppcpd_urban_wassaeast = 200
b10_regexppcpd_urban_wassaeast = 10

# comparing top 10 and bottom 10 ppi households in urban wassa east------------------------------------------------------------------

ghana_listing_cleaned %>% 
  
  # isolate analyses to relevant geographic scope
  
  filter( 
    rur_urb == "urban",
    district == "Wassa_East",
  ) %>% 
  
  # filter out households with NAs
  
  filter(
    is.na(wi_urbrrl) == FALSE & 
      is.na(ppi_prob_1.90) == FALSE & 
      is.na(regexppcpd) == FALSE & 
      is.na(num_hh_6to11) == FALSE & 
      !(is.na(num_6to11_attending_school) == TRUE & num_hh_6to11 != 0) & 
      is.na(enough_food) == FALSE & 
      is.na(enough_clothing) == FALSE &
      is.na(roof_finished_mat) == FALSE
  ) %>% 
  
  # create binary indicators for whether the household reaches each stage of progress 
  
  mutate(
    meets_krishna_stage1 = ifelse(
      is.na(enough_food) == TRUE, NA, ifelse( 
        enough_food == "always", 1, 0 
      )
    ), 
    meets_krishna_stage2 = ifelse(
      is.na(enough_clothing) == TRUE, NA, ifelse( 
        enough_clothing == "always", 1, 0
      )
    ), 
    meets_krishna_stage3_wkids = ifelse( 
      is.na(all_attend_prim_school) == FALSE & all_attend_prim_school == 1, 1, ifelse(
        is.na(all_attend_prim_school) == FALSE & all_attend_prim_school == 0, 0, NA
      )
    ),
    meets_krishna_stage4 = ifelse(
      is.na(roof_finished_mat) == FALSE & roof_finished_mat == 1, 1, ifelse(
        is.na(roof_finished_mat) == FALSE & roof_finished_mat == 0, 0, NA 
      )
    ), 
    
    # calculate the proportion of stages met, the denominators will be different depending on whether the household has a kid between the 6 to 11 age group
    
    prop_krishna_stages_met_wkids = ifelse(
      is.na(num_hh_6to11) == FALSE & num_hh_6to11 > 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage3_wkids + meets_krishna_stage4)/4*100, digits = 0), NA 
    ),
    prop_krishna_stages_met_nokids = ifelse(
      is.na(num_hh_6to11) == FALSE & num_hh_6to11 == 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage4)/3*100, digits = 0), NA 
    ),
    prop_krishna_stages_met = ifelse(
      is.na(num_hh_6to11) == FALSE & num_hh_6to11 > 0, prop_krishna_stages_met_wkids, ifelse(
        is.na(num_hh_6to11) == FALSE & num_hh_6to11 == 0, prop_krishna_stages_met_nokids, "error" 
      )
    )
  ) %>% 
  
  # isolate variables of interest
  
  select(
    wi_urbrrl,
    ppi_prob_1.90,
    regexppcpd,
    num_hh_6to11,
    prop_krishna_stages_met
  ) %>% 
  
  # rank the households based on ppi, dhs, and regular expenditures
  
  mutate(
    dhs_rank_rounded = round(rank(wi_urbrrl), digits = 0),
    ppi_rank_rounded = round(rank(-ppi_prob_1.90), digits = 0),
    regexppcpd_rank_rounded = round(rank(regexppcpd), digits = 0)
  ) %>% 
  
  filter(
    ppi_rank_rounded <= b10_ppi_urban_wassaeast
  ) %>% 
  
  arrange(
    ppi_rank_rounded 
  ) %>% 
  
  select(
    dhs_rank_rounded,
    ppi_rank_rounded,
    regexppcpd_rank_rounded,
    prop_krishna_stages_met
  ) %>% 
  
  rename(
    filler1 = dhs_rank_rounded,
    filler2 = ppi_rank_rounded,
    filler3 = regexppcpd_rank_rounded,
    filler4 = prop_krishna_stages_met
  ) %>% 
  
  # display results in a table with colour formatting
  
  formattable(
    list(
      filler1 = formatter("span",
                          style = ~style(display = "block",
                                         font.weight = "bold",
                                         color = "white",
                                         "border-radius" = "4px",
                                         "padding-right" = "4px",
                                         "background-color" = 
                                           ifelse(filler1 <= b10_dhs_urban_wassaeast, "red",
                                                  ifelse(filler1 >= t10_dhs_urban_wassaeast, "green", "lightgrey"
                                                  )
                                           )
                          )
      ),
      filler2 = formatter("span",
                          style = ~style(display = "block",
                                         font.weight = "bold",
                                         color = "white",
                                         "border-radius" = "4px",
                                         "padding-right" = "4px",
                                         "background-color" = 
                                           ifelse(filler2 <= b10_ppi_urban_wassaeast, "red",
                                                  ifelse(filler2 >= t10_ppi_urban_wassaeast, "green", "lightgrey"
                                                  )
                                           )
                          )
      ),
      filler3 = formatter("span",
                          style = ~style(display = "block",
                                         font.weight = "bold",
                                         color = "white",
                                         "border-radius" = "4px",
                                         "padding-right" = "4px",
                                         "background-color" = 
                                           ifelse(filler3 <= b10_regexppcpd_urban_wassaeast, "red",
                                                  ifelse(filler3 >= t10_regexppcpd_urban_wassaeast, "green", "lightgrey"
                                                  )
                                           )
                          )
      ),
      filler4 = formatter("span",
                          style = ~style(display = "block",
                                         font.weight = "bold",
                                         color = "white",
                                         "border-radius" = "4px",
                                         "padding-right" = "4px",
                                         "background-color" = 
                                           ifelse(filler4 == 0, "red",
                                                  ifelse(filler4 == 100, "green", "lightgrey"
                                                  )
                                           )
                          )
      )
    )
  )
