# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: May 26, 2023
# Purpose: Combine graphs displaying Spearman rank's correlation coefficients between:
  # Panel A: PPI, DHS, and Regular expenditures at the full sample, country-level, and district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel B: PPI, DHS, and Regular expenditures for urban and rural strata at the country-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel C: PPI, DHS, and Regular expenditures among b20 households (according to their DHS scores) at the full sample, country-level, and district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel D: PPI, DHS, and Regular expenditures for urban and rural strata among b20 households (according to their DHS scores) at the country-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
# Inputs: Graphs (spearmans_country_district_graph, spearmans_country_district_b20_dhs_graph, spearmans_urbrur_country_graph, spearmans_urbrur_country_b20_dhs_graph) generated from spearmans_country_district.R, spearmans_country_district_b20_dhs.R, spearmans_urbrur_country.R, and spearmans_urbrur_country_b20_dhs.R, respectively
# Outputs: Figure 1
# Note: A (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggpubr)
library(RVAideMemoire)
library(ggthemes)

# source relevant files ---------------------------------------------------

source(here("code", "visualizations", "fig1", "spearmans_country_district.R"))
source(here("code", "visualizations", "fig1", "spearmans_country_district_b20_dhs.R"))
source(here("code", "visualizations", "fig1", "spearmans_urbrur_country.R"))
source(here("code", "visualizations", "fig1", "spearmans_urbrur_country_b20_dhs.R"))

# combine graphs into one figure ------------------------------------------

combine_spearmans_country_district_urbrur_b20_dhs <- 
  ggarrange(
    spearmans_country_district_graph,
    spearmans_urbrur_country_graph,
    spearmans_country_district_b20_dhs_graph,
    spearmans_urbrur_country_b20_dhs_graph,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"),
    font.label=list(color="black",size=20)
  )

combine_spearmans_country_district_urbrur_b20_dhs
