# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: May 29, 2023
# Purpose: Visualize Spearman rank correlation coefficients between the PPI, DHS, and Regular expenditures for urban and rural strata at the district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
# Inputs: Cleaned datasets (ethiopia_listing_selected_vars_cleaned.rds, ghana_listing_selected_vars_cleaned.rds, uganda_listing_selected_vars_cleaned.rds) produced by clean_ethiopia.R, clean_ghana.R, and clean_uganda.R, respectively
# Outputs: Panel A in Supplementary Figure 2
# Sample sizes: rural Dera (n = 2,477), rural Farta (n = 2,470), rural North Mecha (n = 2,520), rural Asutifi North (n = 1,146), rural Wassa East (n = 1,918), rural Kabarole (n = 1,353), rural Lira (n = 1,354), urban Dera (n = 461), urban Farta (n = 288), urban North Mecha (n = 414), urban Asutifi North (n = 655), urban Wassa East (n = 217), urban Kabarole (n = 519), urban Lira (n = 358)
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


# calculate Spearman's rank correlation coefficients for rural households by district - dhs vs. ppi --------------------------------------

# ppi vs. dhs - rural dera
cortest_ppi_dhs_rural_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - rural Farta
cortest_ppi_dhs_rural_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - rural north mecha
cortest_ppi_dhs_rural_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - rural asutifi north
cortest_ppi_dhs_rural_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - rural wassa east
cortest_ppi_dhs_rural_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - rural kabarole
cortest_ppi_dhs_rural_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 


# ppi vs. dhs - rural lira
cortest_ppi_dhs_rural_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 


# calculate Spearman's rank correlation coefficients for urban households by district - dhs vs. ppi--------------------------------------

# ppi vs. dhs - urban dera
cortest_ppi_dhs_urban_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban farta
cortest_ppi_dhs_urban_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban north mecha
cortest_ppi_dhs_urban_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban asutifi north
cortest_ppi_dhs_urban_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban wassa east
cortest_ppi_dhs_urban_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban kabarole
cortest_ppi_dhs_urban_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# ppi vs. dhs - urban lira
cortest_ppi_dhs_urban_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for rural households by district - dhs vs. ppi--------------------------------------

# ppi vs. dhs - rural dera
cortest_ppi_dhs_rural_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural farta
cortest_ppi_dhs_rural_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural north mecha
cortest_ppi_dhs_rural_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural asutifi north
cortest_ppi_dhs_rural_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural wassa east
cortest_ppi_dhs_rural_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural kabarole
cortest_ppi_dhs_rural_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - rural lira
cortest_ppi_dhs_rural_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# calculate bootstrapped (n = 1000) confidence intervals for urban households by district - dhs vs. ppi--------------------------------------

# ppi vs. dhs - urban dera
cortest_ppi_dhs_urban_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - urban farta
cortest_ppi_dhs_urban_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - urban north mecha
cortest_ppi_dhs_urban_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - urban asutifi north
cortest_ppi_dhs_urban_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - urban wassa east
cortest_ppi_dhs_urban_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - urban kabarole
cortest_ppi_dhs_urban_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. dhs - lira
cortest_ppi_dhs_urban_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# calculate Spearman's rank correlation coefficients for rural households by district - regexp vs. ppi --------------------------------------

# ppi vs. regexp - rural dera
cortest_ppi_regexp_rural_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - rural Farta
cortest_ppi_regexp_rural_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - rural north mecha
cortest_ppi_regexp_rural_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - rural asutifi north
cortest_ppi_regexp_rural_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - rural wassa east
cortest_ppi_regexp_rural_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - rural kabarole
cortest_ppi_regexp_rural_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 


# ppi vs. regexp - rural lira
cortest_ppi_regexp_rural_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 


# calculate Spearman's rank correlation coefficients for urban households by district - regexp vs. ppi--------------------------------------

# ppi vs. regexp - urban dera
cortest_ppi_regexp_urban_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban farta
cortest_ppi_regexp_urban_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban north mecha
cortest_ppi_regexp_urban_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban asutifi north
cortest_ppi_regexp_urban_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban wassa east
cortest_ppi_regexp_urban_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban kabarole
cortest_ppi_regexp_urban_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# ppi vs. regexp - urban lira
cortest_ppi_regexp_urban_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for rural households by district - regexp vs. ppi--------------------------------------

# ppi vs. regexp - rural dera
cortest_ppi_regexp_rural_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural farta
cortest_ppi_regexp_rural_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural north mecha
cortest_ppi_regexp_rural_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural asutifi north
cortest_ppi_regexp_rural_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural wassa east
cortest_ppi_regexp_rural_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural kabarole
cortest_ppi_regexp_rural_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - rural lira
cortest_ppi_regexp_rural_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# calculate bootstrapped (n = 1000) confidence intervals for urban households by district - regexp vs. ppi--------------------------------------

# ppi vs. regexp - urban dera
cortest_ppi_regexp_urban_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - urban farta
cortest_ppi_regexp_urban_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - urban north mecha
cortest_ppi_regexp_urban_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - urban asutifi north
cortest_ppi_regexp_urban_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - urban wassa east
cortest_ppi_regexp_urban_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - urban kabarole
cortest_ppi_regexp_urban_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 

# ppi vs. regexp - lira
cortest_ppi_regexp_urban_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(ppi_prob_1.90),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  nrep = 1000,
  conf.level = 0.95
) 
# calculate Spearman's rank correlation coefficients for rural households by district - regexp vs. dhs --------------------------------------

# regexp vs. dhs - rural dera
cortest_regexp_dhs_rural_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - rural Farta
cortest_regexp_dhs_rural_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - rural north mecha
cortest_regexp_dhs_rural_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - rural asutifi north
cortest_regexp_dhs_rural_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - rural wassa east
cortest_regexp_dhs_rural_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - rural kabarole
cortest_regexp_dhs_rural_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 


# regexp vs. dhs - rural lira
cortest_regexp_dhs_rural_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 


# calculate Spearman's rank correlation coefficients for urban households by district - regexp vs. dhs--------------------------------------

# regexp vs. dhs - urban dera
cortest_regexp_dhs_urban_dera_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban farta
cortest_regexp_dhs_urban_farta_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban north mecha
cortest_regexp_dhs_urban_northmecha_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban asutifi north
cortest_regexp_dhs_urban_asutifinorth_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban wassa east
cortest_regexp_dhs_urban_wassaeast_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban kabarole
cortest_regexp_dhs_urban_kabarole_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# regexp vs. dhs - urban lira
cortest_regexp_dhs_urban_lira_spearman <- cor.test(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  method = c("spearman")
) 

# calculate bootstrapped (n = 1000) confidence intervals for rural households by district - regexp vs. dhs--------------------------------------

# regexp vs. dhs - rural dera
cortest_regexp_dhs_rural_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural farta
cortest_regexp_dhs_rural_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural north mecha
cortest_regexp_dhs_rural_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural asutifi north
cortest_regexp_dhs_rural_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural wassa east
cortest_regexp_dhs_rural_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural kabarole
cortest_regexp_dhs_rural_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - rural lira
cortest_regexp_dhs_rural_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "rural",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# calculate bootstrapped (n = 1000) confidence intervals for urban households by district - regexp vs. dhs--------------------------------------

# regexp vs. dhs - urban dera
cortest_regexp_dhs_urban_dera_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Dera"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - urban farta
cortest_regexp_dhs_urban_farta_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Farta"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - urban north mecha
cortest_regexp_dhs_urban_northmecha_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "North Mecha"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - urban asutifi north
cortest_regexp_dhs_urban_asutifinorth_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Asutifi North"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - urban wassa east
cortest_regexp_dhs_urban_wassaeast_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Wassa East"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - urban kabarole
cortest_regexp_dhs_urban_kabarole_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Kabarole"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# regexp vs. dhs - lira
cortest_regexp_dhs_urban_lira_spearman_ci <- spearman.ci(
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(regexppcpd),
  ppi_dhs_regexp_allhhs %>% 
    filter(
      rur_urb == "urban",
      district == "Lira"
    ) %>% 
    pull(wi_urbrrl),
  nrep = 1000,
  conf.level = 0.95
) 

# create data frame for graphing coeffs and CIs ---------------------------

# note that a (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

spearmans_urbrur_district <- tribble(
  ~spearman_coeff, ~comparison, ~lower_ci, ~upper_ci, ~sample, ~country, ~stratum,
  
  # PPI VS DHS:
  
  # rural households in dera
  -(cortest_ppi_dhs_rural_dera_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_dera_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_dera_spearman_ci$conf.int[2]), "Rural Dera", "Ethiopia", "Rural Households",
  
  # rural households in farta
  -(cortest_ppi_dhs_rural_farta_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_farta_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_farta_spearman_ci$conf.int[2]), "Rural Farta", "Ethiopia", "Rural Households",
  
  # rural households in north mecha
  -(cortest_ppi_dhs_rural_northmecha_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_northmecha_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_northmecha_spearman_ci$conf.int[2]), "Rural North Mecha", "Ethiopia", "Rural Households",
  
  # rural households in asutifi north
  -(cortest_ppi_dhs_rural_asutifinorth_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_asutifinorth_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_asutifinorth_spearman_ci$conf.int[2]), "Rural Asutifi North", "Ghana", "Rural Households",
  
  # rural households in wassa east
  -(cortest_ppi_dhs_rural_wassaeast_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_wassaeast_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_wassaeast_spearman_ci$conf.int[2]), "Rural Wassa East", "Ghana", "Rural Households",
  
  # rural households in kabarole
  -(cortest_ppi_dhs_rural_kabarole_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_kabarole_spearman_ci$conf.int[2]), "Rural Kabarole", "Uganda", "Rural Households",
  
  # rural households in lira
  -(cortest_ppi_dhs_rural_lira_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_rural_lira_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_rural_lira_spearman_ci$conf.int[2]), "Rural Lira", "Uganda", "Rural Households",
  
  # urban households in dera
  -(cortest_ppi_dhs_urban_dera_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_dera_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_dera_spearman_ci$conf.int[2]), "Urban Dera", "Ethiopia", "Urban Households",
  
  # urban households in farta
  -(cortest_ppi_dhs_urban_farta_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_farta_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_farta_spearman_ci$conf.int[2]), "Urban Farta", "Ethiopia", "Urban Households",
  
  # urban households in north mecha
  -(cortest_ppi_dhs_urban_northmecha_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_northmecha_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_northmecha_spearman_ci$conf.int[2]), "Urban North Mecha", "Ethiopia", "Urban Households",
  
  # urban households in asutifi north
  -(cortest_ppi_dhs_urban_asutifinorth_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_asutifinorth_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_asutifinorth_spearman_ci$conf.int[2]), "Urban Asutifi North", "Ghana", "Urban Households",
  
  # urban households in wassa east
  -(cortest_ppi_dhs_urban_wassaeast_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_wassaeast_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_wassaeast_spearman_ci$conf.int[2]), "Urban Wassa East", "Ghana", "Urban Households",
  
  # urban households in kabarole
  -(cortest_ppi_dhs_urban_kabarole_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_kabarole_spearman_ci$conf.int[2]), "Urban Kabarole", "Uganda", "Urban Households",
  
  # urban households in lira
  -(cortest_ppi_dhs_urban_lira_spearman$estimate), "PPI vs DHS", -(cortest_ppi_dhs_urban_lira_spearman_ci$conf.int[1]), -(cortest_ppi_dhs_urban_lira_spearman_ci$conf.int[2]), "Urban Lira", "Uganda", "Urban Households",
  
  # PPI VS REGEXP:
  
  # rural households in dera
  -(cortest_ppi_regexp_rural_dera_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_dera_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_dera_spearman_ci$conf.int[2]), "Rural Dera", "Ethiopia", "Rural Households",
  
  # rural households in farta
  -(cortest_ppi_regexp_rural_farta_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_farta_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_farta_spearman_ci$conf.int[2]), "Rural Farta", "Ethiopia", "Rural Households",
  
  # rural households in north mecha
  -(cortest_ppi_regexp_rural_northmecha_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_northmecha_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_northmecha_spearman_ci$conf.int[2]), "Rural North Mecha", "Ethiopia", "Rural Households",
  
  # rural households in asutifi north
  -(cortest_ppi_regexp_rural_asutifinorth_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_asutifinorth_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_asutifinorth_spearman_ci$conf.int[2]), "Rural Asutifi North", "Ghana", "Rural Households",
  
  # rural households in wassa east
  -(cortest_ppi_regexp_rural_wassaeast_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_wassaeast_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_wassaeast_spearman_ci$conf.int[2]), "Rural Wassa East", "Ghana", "Rural Households",
  
  # rural households in kabarole
  -(cortest_ppi_regexp_rural_kabarole_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_kabarole_spearman_ci$conf.int[2]), "Rural Kabarole", "Uganda", "Rural Households",
  
  # rural households in lira
  -(cortest_ppi_regexp_rural_lira_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_rural_lira_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_rural_lira_spearman_ci$conf.int[2]), "Rural Lira", "Uganda", "Rural Households",
  
  # urban households in dera
  -(cortest_ppi_regexp_urban_dera_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_dera_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_dera_spearman_ci$conf.int[2]), "Urban Dera", "Ethiopia", "Urban Households",
  
  # urban households in farta
  -(cortest_ppi_regexp_urban_farta_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_farta_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_farta_spearman_ci$conf.int[2]), "Urban Farta", "Ethiopia", "Urban Households",
  
  # urban households in north mecha
  -(cortest_ppi_regexp_urban_northmecha_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_northmecha_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_northmecha_spearman_ci$conf.int[2]), "Urban North Mecha", "Ethiopia", "Urban Households",
  
  # urban households in asutifi north
  -(cortest_ppi_regexp_urban_asutifinorth_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_asutifinorth_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_asutifinorth_spearman_ci$conf.int[2]), "Urban Asutifi North", "Ghana", "Urban Households",
  
  # urban households in wassa east
  -(cortest_ppi_regexp_urban_wassaeast_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_wassaeast_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_wassaeast_spearman_ci$conf.int[2]), "Urban Wassa East", "Ghana", "Urban Households",
  
  # urban households in kabarole
  -(cortest_ppi_regexp_urban_kabarole_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_kabarole_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_kabarole_spearman_ci$conf.int[2]), "Urban Kabarole", "Uganda", "Urban Households",
  
  # urban households in lira
  -(cortest_ppi_regexp_urban_lira_spearman$estimate), "PPI vs RegExp", -(cortest_ppi_regexp_urban_lira_spearman_ci$conf.int[1]), -(cortest_ppi_regexp_urban_lira_spearman_ci$conf.int[2]), "Urban Lira", "Uganda", "Urban Households",
  
  # DHS VS REGEXP:
  
  # rural households in dera
  (cortest_regexp_dhs_rural_dera_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_dera_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_dera_spearman_ci$conf.int[2]), "Rural Dera", "Ethiopia", "Rural Households",
  
  # rural households in farta
  (cortest_regexp_dhs_rural_farta_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_farta_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_farta_spearman_ci$conf.int[2]), "Rural Farta", "Ethiopia", "Rural Households",
  
  # rural households in north mecha
  (cortest_regexp_dhs_rural_northmecha_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_northmecha_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_northmecha_spearman_ci$conf.int[2]), "Rural North Mecha", "Ethiopia", "Rural Households",
  
  # rural households in asutifi north
  (cortest_regexp_dhs_rural_asutifinorth_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_asutifinorth_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_asutifinorth_spearman_ci$conf.int[2]), "Rural Asutifi North", "Ghana", "Rural Households",
  
  # rural households in wassa east
  (cortest_regexp_dhs_rural_wassaeast_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_wassaeast_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_wassaeast_spearman_ci$conf.int[2]), "Rural Wassa East", "Ghana", "Rural Households",
  
  # rural households in kabarole
  (cortest_regexp_dhs_rural_kabarole_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_kabarole_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_kabarole_spearman_ci$conf.int[2]), "Rural Kabarole", "Uganda", "Rural Households",
  
  # rural households in lira
  (cortest_regexp_dhs_rural_lira_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_rural_lira_spearman_ci$conf.int[1]), (cortest_regexp_dhs_rural_lira_spearman_ci$conf.int[2]), "Rural Lira", "Uganda", "Rural Households",
  
  # urban households in dera
  (cortest_regexp_dhs_urban_dera_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_dera_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_dera_spearman_ci$conf.int[2]), "Urban Dera", "Ethiopia", "Urban Households",
  
  # urban households in farta
  (cortest_regexp_dhs_urban_farta_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_farta_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_farta_spearman_ci$conf.int[2]), "Urban Farta", "Ethiopia", "Urban Households",
  
  # urban households in north mecha
  (cortest_regexp_dhs_urban_northmecha_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_northmecha_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_northmecha_spearman_ci$conf.int[2]), "Urban North Mecha", "Ethiopia", "Urban Households",
  
  # urban households in asutifi north
  (cortest_regexp_dhs_urban_asutifinorth_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_asutifinorth_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_asutifinorth_spearman_ci$conf.int[2]), "Urban Asutifi North", "Ghana", "Urban Households",
  
  # urban households in wassa east
  (cortest_regexp_dhs_urban_wassaeast_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_wassaeast_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_wassaeast_spearman_ci$conf.int[2]), "Urban Wassa East", "Ghana", "Urban Households",
  
  # urban households in kabarole
  (cortest_regexp_dhs_urban_kabarole_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_kabarole_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_kabarole_spearman_ci$conf.int[2]), "Urban Kabarole", "Uganda", "Urban Households",
  
  # urban households in lira
  (cortest_regexp_dhs_urban_lira_spearman$estimate), "DHS vs RegExp", (cortest_regexp_dhs_urban_lira_spearman_ci$conf.int[1]), (cortest_regexp_dhs_urban_lira_spearman_ci$conf.int[2]), "Urban Lira", "Uganda", "Urban Households"
  
) %>% 
  mutate(
    sample = 
      factor(
        sample,
        levels = c("Rural Dera", "Rural Farta", "Rural North Mecha", "Rural Asutifi North", "Rural Wassa East", "Rural Kabarole", "Rural Lira", "Urban Dera", "Urban Farta", "Urban North Mecha", "Urban Asutifi North", "Urban Wassa East", "Urban Kabarole", "Urban Lira"
        )
      )
  ) %>% 
  mutate(
    comparison = ifelse(
      comparison == "DHS vs RegExp", "DHS vs RegExp", ifelse(
        comparison == "PPI vs DHS", "PPI vs DHS", ifelse(
          comparison == "PPI vs RegExp", "RegExp vs PPI", "error"
        )
      )
    )
  )

# graph the correlation coefficients with confidence intervals ------------

spearmans_urbrur_district_graph <- ggplot(
  data = spearmans_urbrur_district, 
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
    values = c("skyblue3", "springgreen4", "plum3")
  ) + 
  geom_hline(yintercept = 0.7, linetype = "dashed") +
  geom_hline(yintercept = -0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 0.0, linetype = "dashed") +
  geom_hline(yintercept = -0.3, linetype = "dashed") +
  scale_y_continuous(breaks = c(-0.3, 0, 0.3, 0.5, -0.5, 0.7),
                     limits = c(-0.5, 0.8)) +
  theme(text = element_text(size = 10)) +
  ylab("") +
  xlab("") +
  theme(legend.position="none") +
  theme(strip.text.x = element_text(size = 15))

spearmans_urbrur_district_graph
