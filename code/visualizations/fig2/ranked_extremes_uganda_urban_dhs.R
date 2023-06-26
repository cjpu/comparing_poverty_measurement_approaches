# header ------------------------------------------------------------------

# Author: Christine Pu (cjpu@stanford.edu)
# Date Created: April 22, 2023
# Purpose: Visualize accordance between "bottom 10" (i.e., households with the lowest 10 DHS scores) and "top 10" (i.e., households with the highest 10 DHS scores) households based on their PPI scores, DHS scores, regular expenditures per capita, Krishna's Stages of Progress in urban Uganda
# Inputs: Cleaned dataset (uganda_listing_selected_vars_cleaned.rds) produced by clean_uganda.R
# Outputs: Figure 2
# Sample sizes: Urban Lira (n = 358), Urban Kabarole (n = 519) 

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggplot2)
library(ggpubr)
library(formattable)
library(RColorBrewer)

# source relevant files ---------------------------------------------------

source(here("code", "cleaning", "clean_uganda.R"))

# import data -------------------------------------------------------------

uganda_listing_cleaned <- readRDS(here("data", "uganda", "uganda_listing_selected_vars_cleaned.rds"))

# defining top 10 and bottom 10 ranks for urban lira households only  ----------------------------------------------

t10_dhs_urban_lira = 349
b10_dhs_urban_lira = 10
t10_ppi_urban_lira = 303
b10_ppi_urban_lira = 16
t10_regexppcpd_urban_lira = 349
b10_regexppcpd_urban_lira = 10

# defining top 10 and bottom 10 ranks for urban kabarole households only ----------------------------------------------

t10_dhs_urban_kabarole = 510
b10_dhs_urban_kabarole = 10
t10_ppi_urban_kabarole = 466
b10_ppi_urban_kabarole = 11
t10_regexppcpd_urban_kabarole = 510
b10_regexppcpd_urban_kabarole = 10

# comparing top 10 and bottom 10 dhs households in urban lira ------------------------------------------------------------------

uganda_listing_cleaned %>% 
  
  # isolate analyses to relevant geographic scope
  
  filter( 
    rur_urb == "urban",
    district == "lira"
  ) %>% 
  
  # filter out households with NAs 
  
  filter(
    is.na(wi_urbrrl) == FALSE & 
      is.na(ppi_prob_1.90) == FALSE & 
      is.na(regexppcpd) == FALSE & 
      is.na(num_hh_6to12) == FALSE & 
      is.na(enough_food) == FALSE & 
      is.na(enough_clothing) == FALSE &
      is.na(roof_finished_mat) == FALSE
  ) %>% 
  
  # create binary indicators for whether the household reaches each stage of progress 
  
  mutate(
    meets_krishna_stage1 = ifelse(
      is.na(enough_food) == FALSE & enough_food == "always", 1, ifelse(
        is.na(enough_food) == TRUE, NA, 0 
      )
    ), 
    meets_krishna_stage2 = ifelse(
      is.na(enough_clothing) == FALSE & enough_food == "always", 1, ifelse(
        is.na(enough_clothing) == TRUE, NA, 0
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
    
    # calculate the proportion of stages met, the denominators will be different depending on whether the household has a kid between the 6 to 12 age group
    
    prop_krishna_stages_met_wkids = ifelse(
      num_hh_6to12 > 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage3_wkids + meets_krishna_stage4)/4*100, digits = 0), NA 
    ),
    prop_krishna_stages_met_nokids = ifelse(
      num_hh_6to12 == 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage4)/3*100, digits = 0), NA 
    ),
    prop_krishna_stages_met = ifelse(
      num_hh_6to12 > 0, prop_krishna_stages_met_wkids, ifelse(
        num_hh_6to12 == 0, prop_krishna_stages_met_nokids, "error" 
      )
    )
  ) %>% 
  
  # isolate variables of interest
  
  select(
    wi_urbrrl,
    ppi_prob_1.90,
    regexppcpd,
    num_hh_6to12,
    prop_krishna_stages_met,
    
  ) %>% 
  
  # rank the households based on ppi, dhs, and regular expenditures
  
  mutate(
    dhs_rank_rounded = round(rank(wi_urbrrl), digits = 0),
    ppi_rank_rounded = round(rank(-ppi_prob_1.90), digits = 0),
    regexppcpd_rank_rounded = round(rank(regexppcpd), digits = 0)
  ) %>%
  
  filter(
    dhs_rank_rounded <= b10_dhs_urban_lira
  ) %>% 
  
  arrange(
    dhs_rank_rounded 
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
                                           ifelse(filler1 <= b10_dhs_urban_lira, "red",
                                                  ifelse(filler1 >= t10_dhs_urban_lira, "green", "lightgrey"
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
                                           ifelse(filler2 <= b10_ppi_urban_lira, "red",
                                                  ifelse(filler2 >= t10_ppi_urban_lira, "green", "lightgrey"
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
                                           ifelse(filler3 <= b10_regexppcpd_urban_lira, "red",
                                                  ifelse(filler3 >= t10_regexppcpd_urban_lira, "green", "lightgrey"
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


# comparing top 10 and bottom 10 dhs households in urban kabarole ------------------------------------------------------------------

uganda_listing_cleaned %>% 
  
  # isolate analyses to relevant geographic scope
  
  filter( 
    rur_urb == "urban",
    district == "kabarole"
  ) %>%
  
  # filter out households with NAs
  
  filter(
    is.na(wi_urbrrl) == FALSE & 
      is.na(ppi_prob_1.90) == FALSE & 
      is.na(regexppcpd) == FALSE & 
      is.na(num_hh_6to12) == FALSE & 
      is.na(enough_food) == FALSE & 
      is.na(enough_clothing) == FALSE &
      is.na(roof_finished_mat) == FALSE
  ) %>% 
  
  # create binary indicators for whether the household reaches each stage of progress 
  
  mutate(
    meets_krishna_stage1 = ifelse(
      is.na(enough_food) == FALSE & enough_food == "always", 1, ifelse(
        is.na(enough_food) == TRUE, NA, 0 
      )
    ), 
    meets_krishna_stage2 = ifelse(
      is.na(enough_clothing) == FALSE & enough_food == "always", 1, ifelse(
        is.na(enough_clothing) == TRUE, NA, 0 
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
    
    # calculate the proportion of stages met, the denominators will be different depending on whether the household has a kid between the 6 to 12 age group
    
    prop_krishna_stages_met_wkids = ifelse(
      num_hh_6to12 > 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage3_wkids + meets_krishna_stage4)/4*100, digits = 0), NA 
    ),
    prop_krishna_stages_met_nokids = ifelse(
      num_hh_6to12 == 0, round((meets_krishna_stage1 + meets_krishna_stage2 + meets_krishna_stage4)/3*100, digits = 0), NA 
    ),
    prop_krishna_stages_met = ifelse(
      num_hh_6to12 > 0, prop_krishna_stages_met_wkids, ifelse(
        num_hh_6to12 == 0, prop_krishna_stages_met_nokids, "error" 
      )
    )
  ) %>%
  
  # isolate variables of interest
  
  select(
    wi_urbrrl,
    ppi_prob_1.90,
    regexppcpd,
    num_hh_6to12,
    prop_krishna_stages_met,
    
  ) %>% 
  
  # rank the households based on ppi, dhs, and regular expenditures
  
  mutate(
    dhs_rank_rounded = round(rank(wi_urbrrl), digits = 0),
    ppi_rank_rounded = round(rank(-ppi_prob_1.90), digits = 0),
    regexppcpd_rank_rounded = round(rank(regexppcpd), digits = 0)
  ) %>%
  
  filter(
    dhs_rank_rounded <= b10_dhs_urban_kabarole
  ) %>% 
  
  arrange(
    dhs_rank_rounded 
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
                                           ifelse(filler1 <= b10_dhs_urban_kabarole, "red",
                                                  ifelse(filler1 >= t10_dhs_urban_kabarole, "green", "lightgrey"
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
                                           ifelse(filler2 <= b10_ppi_urban_kabarole, "red",
                                                  ifelse(filler2 >= t10_ppi_urban_kabarole, "green", "lightgrey"
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
                                           ifelse(filler3 <= b10_regexppcpd_urban_kabarole, "red",
                                                  ifelse(filler3 >= t10_regexppcpd_urban_kabarole, "green", "lightgrey"
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
