# header ------------------------------------------------------------------

# Author: Christine Pu (cjpu@stanford.edu)
# Date Created: May 9, 2023
# Purpose: Compare the distributions of the differences in percentile rankings among b20 households (defined by regular expenditures per capita) based on households' PPI scores, DHS scores, and regular expenditures per capita in Ethiopiga, Ghana, and Uganda
# Inputs: Cleaned datasets (ethiopia_listing_selected_vars_cleaned.rds, ghana_listing_selected_vars_cleaned.rds, uganda_listing_selected_vars_cleaned.rds) produced by clean_ethiopia.R, clean_ghana.R, and clean_uganda.R, respectively
# Outputs: Panel C of Supplementary Figure 4
# Sample sizes: Rural b20 regexp households [Dera (n = 501), Farta (n = 497), North Mecha (n = 504), Asutifi North (n = 241), Wassa East (n = 403), Kabarole (n = 335), and Lira (n = 285)]
# Sample sizes: Urban b20 regexp households [Dera (n = 92), Farta (n = 58), North Mecha (n = 84), Asutifi North (n = 147), Wassa East (n = 57), Kabarole (n = 110), and Lira (n = 72)]

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggpubr)
library(ggthemes)

# source relevant files ---------------------------------------------------

source(here("code", "cleaning", "clean_ethiopia.R"))
source(here("code", "cleaning", "clean_ghana.R"))
source(here("code", "cleaning", "clean_uganda.R"))

# import data -------------------------------------------------------------

ethiopia_listing_cleaned <- readRDS(here("data", "ethiopia", "ethiopia_listing_selected_vars_cleaned.rds")) 
ghana_listing_cleaned <- readRDS(here("data", "ghana", "ghana_listing_selected_vars_cleaned.rds"))
uganda_listing_cleaned <- readRDS(here("data", "uganda", "uganda_listing_selected_vars_cleaned.rds"))

# boxplot of differences in percentile rankings for b20 households based on their regexp scores -------------------

boxplot_percent_rankings_diff_b20_regexp <- ggboxplot(
  
  # isolate variable of interest and combine datasets from all three countries
  
  bind_rows(
    ethiopia_listing_cleaned %>% 
      filter(
        is.na(ppi_prob_1.90) == FALSE, 
        is.na(wi_urbrrl) == FALSE, 
        is.na(regexppcpd) == FALSE 
      ) %>% 
      group_by(
        rur_urb,
        district
      ) %>% 
      mutate(
        country = "ethiopia",
        PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
        DHS_percentile = percent_rank(wi_urbrrl)*100,
        RegExp_percentile = percent_rank(regexppcpd)*100,
        diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
        diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
        diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile)
      ) %>% 
      ungroup() %>% 
      select(
        RegExp_percentile,
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb,
      ) %>% 
      
      # isolate B20 households based on regexp scores
      
      filter(
        RegExp_percentile < 20
      ),
    
    ghana_listing_cleaned %>% 
      filter(
        is.na(ppi_prob_1.90) == FALSE,
        is.na(wi_urbrrl) == FALSE, 
        is.na(regexppcpd) == FALSE 
      ) %>% 
      group_by(
        rur_urb,
        district
      ) %>% 
      mutate(
        country = "ghana",
        PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
        DHS_percentile = percent_rank(wi_urbrrl)*100,
        RegExp_percentile = percent_rank(regexppcpd)*100,
        diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
        diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
        diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
        district = ifelse(
          district == "Asutifi_North", "Asutifi North", "Wassa East"
        )
      ) %>% 
      ungroup() %>% 
      select(
        RegExp_percentile,
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb
      ) %>% 
      
      # isolate B20 households based on regexp scores
      
      filter(
        RegExp_percentile < 20
      ),
    
    uganda_listing_cleaned %>% 
      filter(
        is.na(ppi_prob_1.90) == FALSE, 
        is.na(wi_urbrrl) == FALSE, 
        is.na(regexppcpd) == FALSE, 
      ) %>% 
      group_by(
        rur_urb,
        district
      ) %>% 
      mutate(
        country = "uganda",
        PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
        DHS_percentile = percent_rank(wi_urbrrl)*100,
        RegExp_percentile = percent_rank(regexppcpd)*100,
        diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
        diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
        diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
        district = ifelse(
          district == "lira", "Lira", "Kabarole"
        )
      ) %>%
      ungroup() %>% 
      select(
        RegExp_percentile,
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb
      ) %>% 
      
      # isolate B20 households based on regexp scores
      
      filter(
        RegExp_percentile < 20
      )
    
  ) %>% 
    mutate(
      district = 
        factor(
          district,
          levels = c("Dera", "Farta", "North Mecha", "Asutifi North", "Wassa East", "Kabarole", "Lira"
          )
        ),
      rur_urb = ifelse(
        rur_urb == "rural", "Rural households", "Urban households"
      )
    ) %>% 
    pivot_longer(
      diff_ppi_dhs:diff_regexp_ppi,
      names_to = "pairwise_comparison",
      values_to = "rank_difference"
    ) %>% 
    mutate(
      pairwise_comparison = ifelse(
        pairwise_comparison == "diff_dhs_regexp", "DHS versus RegExp", ifelse(
          pairwise_comparison == "diff_ppi_dhs", "PPI versus DHS", ifelse(
            pairwise_comparison == "diff_regexp_ppi", "RegExp versus PPI", "error"
          )
        )
      )
    ),
  x = "district",
  y = "rank_difference",
  fill = "country",
  palette = c("skyblue3", "springgreen4", "plum3"),
  ylab = "",
  xlab = ""
) +
  facet_grid(
    rows = vars(rur_urb),
    cols = vars(pairwise_comparison)
  ) +
  theme_few() +
  theme(
    legend.position = "none"
  ) +
  rotate_x_text(45) +
  theme(text = element_text(size = 12)) +
  font("xy.text", size = 10)

boxplot_percent_rankings_diff_b20_regexp

# sample sizes for b20 regexp analysis -------------------------------------------

bind_rows(
  ethiopia_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ethiopia",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile)
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  ghana_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE,
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ghana",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "Asutifi_North", "Asutifi North", "Wassa East"
      )
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  uganda_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE, 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "uganda",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "lira", "Lira", "Kabarole"
      )
    ) %>%
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    )
) %>% 
  group_by(
    rur_urb,
    district
  ) %>% 
  summarise(n())

# computing mean percentile difference in rankings = 26.41 (i.e., an entire quartile) across all urb/rur districts for b20 regexp households ------------------------------------------

bind_rows(
  ethiopia_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ethiopia",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile)
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  ghana_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE,
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ghana",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "Asutifi_North", "Asutifi North", "Wassa East"
      )
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  uganda_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE, 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "uganda",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "lira", "Lira", "Kabarole"
      )
    ) %>%
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    )
  
) %>% 
  mutate(
    district = 
      factor(
        district,
        levels = c("Dera", "Farta", "North Mecha", "Asutifi North", "Wassa East", "Kabarole", "Lira"
        )
      ),
    rur_urb = ifelse(
      rur_urb == "rural", "Rural households", "Urban households"
    )
  ) %>% 
  pivot_longer(
    diff_ppi_dhs:diff_regexp_ppi,
    names_to = "pairwise_comparison",
    values_to = "rank_difference"
  ) %>% 
  mutate(
    pairwise_comparison = ifelse(
      pairwise_comparison == "diff_dhs_regexp", "(1) DHS versus RegExp", ifelse(
        pairwise_comparison == "diff_ppi_dhs", "(2) PPI versus DHS", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "(3) RegExp versus PPI", "error"
        )
      )
    )
  ) %>%
  group_by(
    rur_urb,
    pairwise_comparison,
    district,
  ) %>% 
  summarise(
    mean = mean(rank_difference)
  ) %>%
  pull(mean) %>% 
  mean()

# computing minimum median percentile difference in rankings = 6.94 across all urb/rur districts for b20 regexp households ------------------------------------------

bind_rows(
  ethiopia_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ethiopia",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile)
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  ghana_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE,
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ghana",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "Asutifi_North", "Asutifi North", "Wassa East"
      )
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  uganda_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE, 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "uganda",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "lira", "Lira", "Kabarole"
      )
    ) %>%
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    )
  
) %>% 
  mutate(
    district = 
      factor(
        district,
        levels = c("Dera", "Farta", "North Mecha", "Asutifi North", "Wassa East", "Kabarole", "Lira"
        )
      ),
    rur_urb = ifelse(
      rur_urb == "rural", "Rural households", "Urban households"
    )
  ) %>% 
  pivot_longer(
    diff_ppi_dhs:diff_regexp_ppi,
    names_to = "pairwise_comparison",
    values_to = "rank_difference"
  ) %>% 
  mutate(
    pairwise_comparison = ifelse(
      pairwise_comparison == "diff_dhs_regexp", "(1) DHS versus RegExp", ifelse(
        pairwise_comparison == "diff_ppi_dhs", "(2) PPI versus DHS", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "(3) RegExp versus PPI", "error"
        )
      )
    )
  ) %>%
  group_by(
    rur_urb,
    pairwise_comparison,
    district,
  ) %>% 
  summarise(
    median = median(rank_difference)
  ) %>%
  pull(median) %>% 
  min()

# computing minimum median percentile difference in rankings = 39.04 across all urb/rur districts for b20 regexp households ------------------------------------------

bind_rows(
  ethiopia_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ethiopia",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile)
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  ghana_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE,
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "ghana",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "Asutifi_North", "Asutifi North", "Wassa East"
      )
    ) %>% 
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    ),
  
  uganda_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE, 
    ) %>% 
    group_by(
      rur_urb,
      district
    ) %>% 
    mutate(
      country = "uganda",
      PPI_percentile = percent_rank(desc(ppi_prob_1.90))*100,
      DHS_percentile = percent_rank(wi_urbrrl)*100,
      RegExp_percentile = percent_rank(regexppcpd)*100,
      diff_ppi_dhs = abs(PPI_percentile - DHS_percentile),
      diff_dhs_regexp = abs(DHS_percentile - RegExp_percentile),
      diff_regexp_ppi = abs(RegExp_percentile - PPI_percentile),
      district = ifelse(
        district == "lira", "Lira", "Kabarole"
      )
    ) %>%
    ungroup() %>% 
    select(
      RegExp_percentile,
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
    ) %>% 
    
    # isolate B20 households based on regexp scores
    
    filter(
      RegExp_percentile < 20
    )
  
) %>% 
  mutate(
    district = 
      factor(
        district,
        levels = c("Dera", "Farta", "North Mecha", "Asutifi North", "Wassa East", "Kabarole", "Lira"
        )
      ),
    rur_urb = ifelse(
      rur_urb == "rural", "Rural households", "Urban households"
    )
  ) %>% 
  pivot_longer(
    diff_ppi_dhs:diff_regexp_ppi,
    names_to = "pairwise_comparison",
    values_to = "rank_difference"
  ) %>% 
  mutate(
    pairwise_comparison = ifelse(
      pairwise_comparison == "diff_dhs_regexp", "(1) DHS versus RegExp", ifelse(
        pairwise_comparison == "diff_ppi_dhs", "(2) PPI versus DHS", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "(3) RegExp versus PPI", "error"
        )
      )
    )
  ) %>% 
  group_by(
    rur_urb,
    pairwise_comparison,
    district,
  ) %>% 
  summarise(
    median = median(rank_difference)
  ) %>%
  pull(median) %>% 
  max()
