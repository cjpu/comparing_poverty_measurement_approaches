# header ------------------------------------------------------------------

# Author: Christine Pu (cjpu@stanford.edu)
# Date Created: May 9, 2023
# Purpose: Compare the distributions of the differences in percentile rankings based on households' PPI scores, DHS scores, and regular expenditures per capita in Ethiopiga, Ghana, and Uganda
# Inputs: Cleaned datasets (ethiopia_listing_selected_vars_cleaned.rds, ghana_listing_selected_vars_cleaned.rds, uganda_listing_selected_vars_cleaned.rds) produced by clean_ethiopia.R, clean_ghana.R, and clean_uganda.R, respectively
# Outputs: Figure 3
# Sample sizes: Rural households [Dera (n = 2,477), Farta (n = 2,470), North Mecha (n = 2,520), Asutifi North (n = 1,146), Wassa East (n = 1,918), Kabarole (n = 1,353), and Lira (n = 1,354)]
# Sample sizes: Urban households [Dera (n = 461), Farta (n = 288), North Mecha (n = 414), Asutifi North (n = 655), Wassa East (n = 217), Kabarole (n = 519), and Lira (n = 358)]

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

# boxplot of differences in percentile rankings for the full sample ---------------------------

boxplot_percent_rankings_diff_full_sample <- 
  ggboxplot(
  
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
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb,
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
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb
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
        diff_ppi_dhs,
        diff_dhs_regexp,
        diff_regexp_ppi,
        country,
        district,
        rur_urb
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
        ),
      pairwise_comparison = factor(
        pairwise_comparison, 
        levels = c("DHS versus RegExp", "PPI versus DHS", "RegExp versus PPI")
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
  theme(text = element_text(size = 20)) +
  font("xy.text", size = 15)

boxplot_percent_rankings_diff_full_sample

# computing mean percentile difference in rankings = 25.13 (i.e., an entire quartile) across all urb/rur districts ------------------------------------------

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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      pairwise_comparison == "diff_ppi_dhs", "PPI versus DHS", ifelse(
        pairwise_comparison == "diff_dhs_regexp", "DHS versus Regular Expenditures", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "Regular Expenditures versus PPI", "error"
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

# computing minimum median percentile difference in rankings (13.3) across all urb/rur districts  ------------------------

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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      pairwise_comparison == "diff_ppi_dhs", "PPI versus DHS", ifelse(
        pairwise_comparison == "diff_dhs_regexp", "DHS versus Regular Expenditures", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "Regular Expenditures versus PPI", "error"
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

# computing maximum median percentile difference in rankings (29.1) across all urb/rur districts  ------------------------

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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb,
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      diff_ppi_dhs,
      diff_dhs_regexp,
      diff_regexp_ppi,
      country,
      district,
      rur_urb
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
      pairwise_comparison == "diff_ppi_dhs", "PPI versus DHS", ifelse(
        pairwise_comparison == "diff_dhs_regexp", "DHS versus Regular Expenditures", ifelse(
          pairwise_comparison == "diff_regexp_ppi", "Regular Expenditures versus PPI", "error"
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
