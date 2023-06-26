# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: April 24, 2023
# Purpose: Visualize Spearman rank correlation coefficients between the PPI, DHS, and Regular expenditures for urban and rural strata among b20 households (according to their DHS scores) at the country-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
# Inputs: Cleaned datasets (ethiopia_listing_selected_vars_cleaned.rds, ghana_listing_selected_vars_cleaned.rds, uganda_listing_selected_vars_cleaned.rds) produced by clean_ethiopia.R, clean_ghana.R, and clean_uganda.R, respectively
# Outputs: Panel D in Figure 1
# Sample sizes: All rural (n = 2,648), All urban (n = 583), Rural Ethiopia (n = 1,494), Urban Ethiopia (n = 233), Rural Ghana (n = 613), Urban Ghana (n = 175), Rural Uganda (n = 542), Urban Uganda (n = 176)
# Note: A (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggpubr)
library(RVAideMemoire)
library(ggthemes)

# source relevant files ---------------------------------------------------

source(here("code", "cleaning", "clean_ethiopia.R"))
source(here("code", "cleaning", "clean_ghana.R"))
source(here("code", "cleaning", "clean_uganda.R"))

# import data -------------------------------------------------------------

ethiopia_listing_cleaned <- readRDS(here("data", "ethiopia", "ethiopia_listing_selected_vars_cleaned.rds")) 
ghana_listing_cleaned <- readRDS(here("data", "ghana", "ghana_listing_selected_vars_cleaned.rds"))
uganda_listing_cleaned <- readRDS(here("data", "uganda", "uganda_listing_selected_vars_cleaned.rds"))

# create combined, full sample dataset ------------------------------------------------

ppi_dhs_regexp_allhhs <- bind_rows(
  ethiopia_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    mutate(
      country = "ethiopia"
    ) %>% 
    select(
      ppi_prob_1.90,
      wi_urbrrl,
      regexppcpd,
      country,
      district,
      rur_urb
    ),
  ghana_listing_cleaned %>%
    filter(
      is.na(ppi_prob_1.90) == FALSE,
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    mutate(
      country = "ghana",
      district = ifelse(
        district == "Asutifi_North", "Asutifi North", "Wassa East"
      )
    ) %>% 
    select(
      ppi_prob_1.90,
      wi_urbrrl,
      regexppcpd,
      country,
      district,
      rur_urb
    ),
  uganda_listing_cleaned %>% 
    filter(
      is.na(ppi_prob_1.90) == FALSE, 
      is.na(wi_urbrrl) == FALSE, 
      is.na(regexppcpd) == FALSE 
    ) %>% 
    mutate(
      country = "uganda",
      district = ifelse(
        district == "lira", "Lira", "Kabarole"
      )
    ) %>%       
    select(
      ppi_prob_1.90,
      wi_urbrrl,
      regexppcpd,
      country,
      district,
      rur_urb
    )
)

# calculate Spearman's rank correlation coefficients for all B20 urban households --------------------------------------

# ppi vs. dhs - all b20 urban households
cortest_ppi_dhs_allhhs_urban_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all b20 urban households
cortest_ppi_regexp_allhhs_urban_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all b20 urban households
cortest_dhs_regexp_allhhs_urban_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 urban households--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_urban_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_urban_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_urban_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)
# calculate Spearman's rank correlation coefficients for all B20 rural households --------------------------------------

# ppi vs. dhs - all b20 rural households
cortest_ppi_dhs_allhhs_rural_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all b20 rural households
cortest_ppi_regexp_allhhs_rural_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all b20 rural households
cortest_dhs_regexp_allhhs_rural_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 rural households--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_rural_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_rural_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_rural_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)
# calculate Spearman's rank correlation coefficients for all B20 rural households in Ethiopia --------------------------------------

# ppi vs. dhs - all rural households in ethiopia
cortest_ppi_dhs_allhhs_rural_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all rural households in ethiopia
cortest_ppi_regexp_allhhs_rural_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all rural households in ethiopia
cortest_dhs_regexp_allhhs_rural_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 rural households in Ethiopia--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_rural_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_rural_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_rural_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all B20 urban households in Ethiopia --------------------------------------

# ppi vs. dhs - all urban households in ethiopia
cortest_ppi_dhs_allhhs_urban_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all urban households in ethiopia
cortest_ppi_regexp_allhhs_urban_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all urban households in ethiopia
cortest_dhs_regexp_allhhs_urban_ethiopia_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 urban households in Ethiopia--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_urban_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_urban_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_urban_ethiopia_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all B20 rural households in Ghana --------------------------------------

# ppi vs. dhs - all rural households in ghana
cortest_ppi_dhs_allhhs_rural_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all rural households in ghana
cortest_ppi_regexp_allhhs_rural_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all rural households in ghana
cortest_dhs_regexp_allhhs_rural_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 rural households in Ghana--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_rural_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_rural_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_rural_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all B20 urban households in Ghana --------------------------------------

# ppi vs. dhs - all urban households in ghana
cortest_ppi_dhs_allhhs_urban_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all urban households in ghana
cortest_ppi_regexp_allhhs_urban_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all urban households in ghana
cortest_dhs_regexp_allhhs_urban_ghana_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 urban households in Ghana--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_urban_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_urban_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_urban_ghana_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)


# calculate Spearman's rank correlation coefficients for all B20 rural households in Uganda --------------------------------------

# ppi vs. dhs - all rural households in uganda
cortest_ppi_dhs_allhhs_rural_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all rural households in uganda
cortest_ppi_regexp_allhhs_rural_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all rural households in uganda
cortest_dhs_regexp_allhhs_rural_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 rural households in Uganda--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_rural_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_rural_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_rural_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "rural") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all B20 urban households in Uganda --------------------------------------

# ppi vs. dhs - all urban households in uganda
cortest_ppi_dhs_allhhs_urban_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all urban households in uganda
cortest_ppi_regexp_allhhs_urban_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all urban households in uganda
cortest_dhs_regexp_allhhs_urban_uganda_spearman_b20 <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all B20 urban households in Uganda--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_urban_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_urban_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_urban_uganda_spearman_ci_b20 <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda",
           rur_urb == "urban") %>%
    mutate(
      dhs_percentile = percent_rank(wi_urbrrl)
    ) %>% 
    filter(
      dhs_percentile < 0.2
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# create data frame for graphing coeffs and CIs ---------------------------

# note that a (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

spearmans_urbrur_country_b20_dhs <- tribble(
  ~spearman_coeff, ~comparison, ~lower_ci, ~upper_ci, ~sample, ~country, ~stratum,
  
  # all urban households across three countries
  -(cortest_ppi_dhs_allhhs_urban_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_urban_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_urban_spearman_ci_b20$conf.int[2]), "All Urban", "All Countries", "Urban Households",
  -(cortest_ppi_regexp_allhhs_urban_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_urban_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_urban_spearman_ci_b20$conf.int[2]), "All Urban", "All Countries", "Urban Households",
  cortest_dhs_regexp_allhhs_urban_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_urban_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_urban_spearman_ci_b20$conf.int[2], "All Urban", "All Countries", "Urban Households",
  
  # all rural households across three countries
  -(cortest_ppi_dhs_allhhs_rural_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_rural_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_rural_spearman_ci_b20$conf.int[2]), "All Rural", "All Countries", "Rural Households",
  -(cortest_ppi_regexp_allhhs_rural_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_rural_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_rural_spearman_ci_b20$conf.int[2]), "All Rural", "All Countries", "Rural Households",
  cortest_dhs_regexp_allhhs_rural_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_rural_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_rural_spearman_ci_b20$conf.int[2], "All Rural", "All Countries", "Rural Households",
  
  # all rural households in ethiopia
  -(cortest_ppi_dhs_allhhs_rural_ethiopia_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[2]), "Rural Ethiopia", "Ethiopia", "Rural Households",
  -(cortest_ppi_regexp_allhhs_rural_ethiopia_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[2]), "Rural Ethiopia", "Ethiopia", "Rural Households",
  cortest_dhs_regexp_allhhs_rural_ethiopia_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_rural_ethiopia_spearman_ci_b20$conf.int[2], "Rural Ethiopia", "Ethiopia", "Rural Households",
  
  # all urban households in ethiopia
  -(cortest_ppi_dhs_allhhs_urban_ethiopia_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[2]), "Urban Ethiopia", "Ethiopia", "Urban Households",
  -(cortest_ppi_regexp_allhhs_urban_ethiopia_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[2]), "Urban Ethiopia", "Ethiopia", "Urban Households",
  cortest_dhs_regexp_allhhs_urban_ethiopia_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_urban_ethiopia_spearman_ci_b20$conf.int[2], "Urban Ethiopia", "Ethiopia", "Urban Households",
  
  # all rural households in ghana
  -(cortest_ppi_dhs_allhhs_rural_ghana_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_rural_ghana_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_rural_ghana_spearman_ci_b20$conf.int[2]), "Rural Ghana", "Ghana", "Rural Households",
  -(cortest_ppi_regexp_allhhs_rural_ghana_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_rural_ghana_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_rural_ghana_spearman_ci_b20$conf.int[2]), "Rural Ghana", "Ghana", "Rural Households",
  cortest_dhs_regexp_allhhs_rural_ghana_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_rural_ghana_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_rural_ghana_spearman_ci_b20$conf.int[2], "Rural Ghana", "Ghana", "Rural Households",
  
  # all urban households in ghana
  -(cortest_ppi_dhs_allhhs_urban_ghana_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_urban_ghana_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_urban_ghana_spearman_ci_b20$conf.int[2]), "Urban Ghana", "Ghana", "Urban Households",
  -(cortest_ppi_regexp_allhhs_urban_ghana_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_urban_ghana_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_urban_ghana_spearman_ci_b20$conf.int[2]), "Urban Ghana", "Ghana", "Urban Households",
  cortest_dhs_regexp_allhhs_urban_ghana_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_urban_ghana_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_urban_ghana_spearman_ci_b20$conf.int[2], "Urban Ghana", "Ghana", "Urban Households",
  
  # all rural households in uganda
  -(cortest_ppi_dhs_allhhs_rural_uganda_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_rural_uganda_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_rural_uganda_spearman_ci_b20$conf.int[2]), "Rural Uganda", "Uganda", "Rural Households",
  -(cortest_ppi_regexp_allhhs_rural_uganda_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_rural_uganda_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_rural_uganda_spearman_ci_b20$conf.int[2]), "Rural Uganda", "Uganda", "Rural Households",
  cortest_dhs_regexp_allhhs_rural_uganda_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_rural_uganda_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_rural_uganda_spearman_ci_b20$conf.int[2], "Rural Uganda", "Uganda", "Rural Households",
  
  # all urban households in uganda
  -(cortest_ppi_dhs_allhhs_urban_uganda_spearman_b20$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_urban_uganda_spearman_ci_b20$conf.int[1]), -(cortest_ppi_dhs_allhhs_urban_uganda_spearman_ci_b20$conf.int[2]), "Urban Uganda", "Uganda", "Urban Households",
  -(cortest_ppi_regexp_allhhs_urban_uganda_spearman_b20$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_urban_uganda_spearman_ci_b20$conf.int[1]), -(cortest_ppi_regexp_allhhs_urban_uganda_spearman_ci_b20$conf.int[2]), "Urban Uganda", "Uganda", "Urban Households",
  cortest_dhs_regexp_allhhs_urban_uganda_spearman_b20$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_urban_uganda_spearman_ci_b20$conf.int[1], cortest_dhs_regexp_allhhs_urban_uganda_spearman_ci_b20$conf.int[2], "Urban Uganda", "Uganda", "Urban Households",
) %>% 
  mutate(
    sample = 
      factor(
        sample,
        levels = c("All Rural", "All Urban", "Rural Ethiopia", "Urban Ethiopia", "Rural Ghana",  "Urban Ghana", "Rural Uganda",    
                   "Urban Uganda"
        )
      )
  )

# graph the correlation coefficients with confidence intervals ------------

spearmans_urbrur_country_b20_dhs_graph <- ggplot(
  data = spearmans_urbrur_country_b20_dhs, 
  aes(
    x = as.factor(sample),
    color = country,
    shape = stratum
  )
) +
  geom_errorbar(
    aes(ymax = upper_ci, ymin = lower_ci), 
    width = 0.2
  ) +
  geom_point(
    aes(
      y=spearman_coeff,
      size = 25
    )
  ) +
  scale_shape_manual(values = c(15, 18)) +
  facet_wrap(~comparison) +
  theme_few() +
  rotate_x_text(45) +
  scale_color_manual(
    values = c("darkgrey", "skyblue3", "springgreen4", "plum3")
  ) + 
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  geom_hline(yintercept = -0.3, linetype = "dashed") +
  scale_y_continuous(breaks = c(-0.7, -0.5, -0.3, 0, 0.3, 0.5, 0.7),
                     limits = c(-0.66, 0.66)) +
  theme(text = element_text(size = 10)) +
  ylab("") +
  xlab("") +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 15))

spearmans_urbrur_country_b20_dhs_graph
