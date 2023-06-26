# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: April 24, 2023
# Purpose: Visualize Spearman rank correlation coefficients between the PPI, DHS, and Regular expenditures at the full sample, country-level, and district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
# Inputs: Cleaned datasets (ethiopia_listing_selected_vars_cleaned.rds, ghana_listing_selected_vars_cleaned.rds, uganda_listing_selected_vars_cleaned.rds) produced by clean_ethiopia.R, clean_ghana.R, and clean_uganda.R, respectively
# Outputs: Panel A in Figure 1
# Sample sizes: All households (n = 16,150), All Ethiopia (n = 8,630), Dera (n = 2,938), Farta (n = 2,758), North Mecha (n = 2,934), All Ghana (n = 3,936), Asutifi North (n = 1,801), Wassa East (n = 2,135), All Uganda (n = 3,584), Kabarole (n = 1,872), Lira (n = 1,712)
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

# calculate Spearman's rank correlation coefficients for all households --------------------------------------

# ppi vs. dhs - all households
cortest_ppi_dhs_allhhs_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all households
cortest_ppi_regexp_allhhs_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all households
cortest_dhs_regexp_allhhs_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all households--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all households in Ethiopia --------------------------------------

# ppi vs. dhs - all households in Ethiopia
cortest_ppi_dhs_allhhs_ethiopia_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all households in Ethiopia
cortest_ppi_regexp_allhhs_ethiopia_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all households in Ethiopia
cortest_dhs_regexp_allhhs_ethiopia_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all households in Ethiopia--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_ethiopia_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_ethiopia_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ethiopia") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_ethiopia_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ethiopia") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all households in Ghana --------------------------------------

# ppi vs. dhs - all households in ghana
cortest_ppi_dhs_allhhs_ghana_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all households in ghana
cortest_ppi_regexp_allhhs_ghana_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all households in ghana
cortest_dhs_regexp_allhhs_ghana_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all households in Ghana--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_ghana_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_ghana_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "ghana") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_ghana_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "ghana") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all households in Uganda --------------------------------------

# ppi vs. dhs - all households in uganda
cortest_ppi_dhs_allhhs_uganda_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- all households in uganda
cortest_ppi_regexp_allhhs_uganda_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp - all households in uganda
cortest_dhs_regexp_allhhs_uganda_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for all households in Uganda--------------------------------------

# ppi vs. dhs
cortest_ppi_dhs_allhhs_uganda_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp
cortest_ppi_regexp_allhhs_uganda_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(country == "uganda") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp
cortest_dhs_regexp_allhhs_uganda_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(country == "uganda") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# calculate Spearman's rank correlation coefficients for all households by district --------------------------------------

# Asutifi North

# ppi vs. dhs - Asutifi North
cortest_ppi_dhs_allhhs_asutifi_north_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Asutifi North
cortest_ppi_regexp_allhhs_asutifi_north_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Asutifi North
cortest_dhs_regexp_allhhs_asutifi_north_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# Dera

# ppi vs. dhs - Dera
cortest_ppi_dhs_allhhs_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Dera
cortest_ppi_regexp_allhhs_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Dera
cortest_dhs_regexp_allhhs_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# Farta

# ppi vs. dhs - Farta
cortest_ppi_dhs_allhhs_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Farta
cortest_ppi_regexp_allhhs_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Farta
cortest_dhs_regexp_allhhs_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# Kabarole

# ppi vs. dhs - Kabarole
cortest_ppi_dhs_allhhs_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Kabarole
cortest_ppi_regexp_allhhs_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Kabarole
cortest_dhs_regexp_allhhs_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# Lira

# ppi vs. dhs - Lira
cortest_ppi_dhs_allhhs_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Lira
cortest_ppi_regexp_allhhs_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Lira
cortest_dhs_regexp_allhhs_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# North Mecha

# ppi vs. dhs - North Mecha
cortest_ppi_dhs_allhhs_north_mecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- North Mecha
cortest_ppi_regexp_allhhs_north_mecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- North Mecha
cortest_dhs_regexp_allhhs_north_mecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(regexppcpd),
  method = c("spearman")
)

# Wassa East

# ppi vs. dhs - Wassa East
cortest_ppi_dhs_allhhs_wassa_east_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. regexp -- Wassa East
cortest_ppi_regexp_allhhs_wassa_east_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(regexppcpd),
  method = c("spearman")
) 

# dhs vs. regexp -- Wassa East
cortest_dhs_regexp_allhhs_wassa_east_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(regexppcpd),
  method = c("spearman")
)

# calculate bootstrapped (n = 1000) confidence intervals for all households by district--------------------------------------

# Asutifi North

# ppi vs. dhs -- Asutifi North
cortest_ppi_dhs_allhhs_asutifi_north_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Asutifi North
cortest_ppi_regexp_allhhs_asutifi_north_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Asutifi North") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Asutifi North") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Asutifi North
cortest_dhs_regexp_allhhs_asutifi_north_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Asutifi North") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# Dera

# ppi vs. dhs -- Dera
cortest_ppi_dhs_allhhs_dera_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Dera
cortest_ppi_regexp_allhhs_dera_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Dera") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Dera") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Dera
cortest_dhs_regexp_allhhs_dera_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Dera") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# Farta

# ppi vs. dhs -- Farta
cortest_ppi_dhs_allhhs_farta_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Farta
cortest_ppi_regexp_allhhs_farta_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Farta") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Farta") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Farta
cortest_dhs_regexp_allhhs_farta_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Farta") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# Kabarole

# ppi vs. dhs -- Kabarole
cortest_ppi_dhs_allhhs_kabarole_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Kabarole
cortest_ppi_regexp_allhhs_kabarole_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Kabarole") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Kabarole") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Kabarole
cortest_dhs_regexp_allhhs_kabarole_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Kabarole") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# Lira

# ppi vs. dhs -- Lira
cortest_ppi_dhs_allhhs_lira_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Lira
cortest_ppi_regexp_allhhs_lira_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Lira") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Lira") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Lira
cortest_dhs_regexp_allhhs_lira_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Lira") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# North Mecha

# ppi vs. dhs -- North Mecha
cortest_ppi_dhs_allhhs_north_mecha_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- North Mecha
cortest_ppi_regexp_allhhs_north_mecha_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "North Mecha") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "North Mecha") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- North Mecha
cortest_dhs_regexp_allhhs_north_mecha_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "North Mecha") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# Wassa East

# ppi vs. dhs -- Wassa East
cortest_ppi_dhs_allhhs_wassa_east_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
)

# ppi vs. regexp -- Wassa East
cortest_ppi_regexp_allhhs_wassa_east_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Wassa East") %>%
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>%
    filter(district == "Wassa East") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# dhs vs. regexp -- Wassa East
cortest_dhs_regexp_allhhs_wassa_east_spearman_ci <- spearman.ci( 
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(wi_urbrrl),
  ppi_dhs_regexp_allhhs %>% 
    filter(district == "Wassa East") %>%
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
)

# create data frame for graphing coeffs and CIs ---------------------------

# note that a (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

spearmans_country_district <- tribble(
  ~spearman_coeff, ~comparison, ~lower_ci, ~upper_ci, ~sample, ~country, ~stratum,
  
  # all households across three countries
  -(cortest_ppi_dhs_allhhs_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_spearman_ci$conf.int[2]), "All Households", "All Countries", "All Households",
  -(cortest_ppi_regexp_allhhs_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_spearman_ci$conf.int[2]), "All Households", "All Countries", "All Households",
  cortest_dhs_regexp_allhhs_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_spearman_ci$conf.int[2], "All Households", "All Countries", "All Households",
  
  # all households in ethiopia
  -(cortest_ppi_dhs_allhhs_ethiopia_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_ethiopia_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_ethiopia_spearman_ci$conf.int[2]), "All Ethiopia", "Ethiopia", "All Households",
  -(cortest_ppi_regexp_allhhs_ethiopia_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_ethiopia_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_ethiopia_spearman_ci$conf.int[2]), "All Ethiopia", "Ethiopia", "All Households",
  cortest_dhs_regexp_allhhs_ethiopia_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_ethiopia_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_ethiopia_spearman_ci$conf.int[2], "All Ethiopia", "Ethiopia", "All Households",
  
  # all households in ghana
  -(cortest_ppi_dhs_allhhs_ghana_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_ghana_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_ghana_spearman_ci$conf.int[2]), "All Ghana", "Ghana", "All Households",
  -(cortest_ppi_regexp_allhhs_ghana_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_ghana_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_ghana_spearman_ci$conf.int[2]), "All Ghana", "Ghana", "All Households",
  cortest_dhs_regexp_allhhs_ghana_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_ghana_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_ghana_spearman_ci$conf.int[2], "All Ghana", "Ghana", "All Households",
  
  # all households in uganda
  -(cortest_ppi_dhs_allhhs_uganda_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_uganda_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_uganda_spearman_ci$conf.int[2]), "All Uganda", "Uganda", "All Households",
  -(cortest_ppi_regexp_allhhs_uganda_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_uganda_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_uganda_spearman_ci$conf.int[2]), "All Uganda", "Uganda", "All Households",
  cortest_dhs_regexp_allhhs_uganda_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_uganda_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_uganda_spearman_ci$conf.int[2], "All Uganda", "Uganda", "All Households",
  
  # all asutifi north
  -(cortest_ppi_dhs_allhhs_asutifi_north_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_asutifi_north_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_asutifi_north_spearman_ci$conf.int[2]), "Asutifi North", "Ghana", "All Households",
  -(cortest_ppi_regexp_allhhs_asutifi_north_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_asutifi_north_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_asutifi_north_spearman_ci$conf.int[2]), "Asutifi North", "Ghana", "All Households",
  cortest_dhs_regexp_allhhs_asutifi_north_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_asutifi_north_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_asutifi_north_spearman_ci$conf.int[2], "Asutifi North", "Ghana", "All Households",
  
  # all dera
  -(cortest_ppi_dhs_allhhs_dera_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_dera_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_dera_spearman_ci$conf.int[2]), "Dera", "Ethiopia", "All Households",
  -(cortest_ppi_regexp_allhhs_dera_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_dera_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_dera_spearman_ci$conf.int[2]), "Dera", "Ethiopia", "All Households",
  cortest_dhs_regexp_allhhs_dera_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_dera_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_dera_spearman_ci$conf.int[2], "Dera", "Ethiopia", "All Households",
  
  # all farta
  -(cortest_ppi_dhs_allhhs_farta_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_farta_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_farta_spearman_ci$conf.int[2]), "Farta", "Ethiopia", "All Households",
  -(cortest_ppi_regexp_allhhs_farta_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_farta_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_farta_spearman_ci$conf.int[2]), "Farta", "Ethiopia", "All Households",
  cortest_dhs_regexp_allhhs_farta_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_farta_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_farta_spearman_ci$conf.int[2], "Farta", "Ethiopia", "All Households",
  
  # all kabarole
  -(cortest_ppi_dhs_allhhs_kabarole_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_kabarole_spearman_ci$conf.int[2]), "Kabarole", "Uganda", "All Households",
  -(cortest_ppi_regexp_allhhs_kabarole_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_kabarole_spearman_ci$conf.int[2]), "Kabarole", "Uganda", "All Households",
  cortest_dhs_regexp_allhhs_kabarole_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_kabarole_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_kabarole_spearman_ci$conf.int[2], "Kabarole", "Uganda", "All Households",
  
  # all lira
  -(cortest_ppi_dhs_allhhs_lira_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_lira_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_lira_spearman_ci$conf.int[2]), "Lira", "Uganda", "All Households",
  -(cortest_ppi_regexp_allhhs_lira_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_lira_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_lira_spearman_ci$conf.int[2]), "Lira", "Uganda", "All Households",
  cortest_dhs_regexp_allhhs_lira_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_lira_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_lira_spearman_ci$conf.int[2], "Lira", "Uganda", "All Households",
  
  # all north mecha
  -(cortest_ppi_dhs_allhhs_north_mecha_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_north_mecha_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_north_mecha_spearman_ci$conf.int[2]), "North Mecha", "Ethiopia", "All Households",
  -(cortest_ppi_regexp_allhhs_north_mecha_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_north_mecha_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_north_mecha_spearman_ci$conf.int[2]), "North Mecha", "Ethiopia", "All Households",
  cortest_dhs_regexp_allhhs_north_mecha_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_north_mecha_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_north_mecha_spearman_ci$conf.int[2], "North Mecha", "Ethiopia", "All Households",
  
  # all wassa east
  -(cortest_ppi_dhs_allhhs_wassa_east_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_allhhs_wassa_east_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_allhhs_wassa_east_spearman_ci$conf.int[2]), "Wassa East", "Ghana", "All Households",
  -(cortest_ppi_regexp_allhhs_wassa_east_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_allhhs_wassa_east_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_allhhs_wassa_east_spearman_ci$conf.int[2]), "Wassa East", "Ghana", "All Households",
  cortest_dhs_regexp_allhhs_wassa_east_spearman$estimate, "DHS vs RegExp", cortest_dhs_regexp_allhhs_wassa_east_spearman_ci$conf.int[1], cortest_dhs_regexp_allhhs_wassa_east_spearman_ci$conf.int[2], "Wassa East", "Ghana", "All Households",
  
  
) %>% 
  mutate(
    sample = 
      factor(
        sample,
        levels = c("All Households", "All Ethiopia", "Dera", "Farta", 
                   "North Mecha", "All Ghana", "Asutifi North", "Wassa East","All Uganda", "Kabarole", "Lira"
        )
      )
  )

# graph the correlation coefficients with confidence intervals ------------

spearmans_country_district_graph <- ggplot(
  data = spearmans_country_district, 
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
  scale_shape_manual(values = c(16, 15, 18)) +
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

spearmans_country_district_graph
