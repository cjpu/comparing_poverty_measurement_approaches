# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: May 29, 2023
# Purpose: Combine graphs displaying Spearman rank's correlation coefficients between:
  # Panel A: PPI, DHS, and Regular expenditures among b20 households (according to their PPI scores) at the full sample, country-level, and district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel B: PPI, DHS, and Regular expenditures for urban and rural strata among b20 households (according to their PPI scores) at the country-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel C: PPI, DHS, and Regular expenditures among b20 households (according to their regular expenditures) at the full sample, country-level, and district-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
  # Panel D: PPI, DHS, and Regular expenditures for urban and rural strata among b20 households (according to their regular expenditures) at the country-level in Ethiopia, Ghana, and Uganda with bootstrapped confidence intervals
# Inputs: Graphs (spearmans_country_district_b20_ppi_graph, spearmans_urbrur_country_b20_ppi_graph, spearmans_country_district_b20_regexp_graph, spearmans_urbrur_country_b20_regexp_graph) generated from spearmans_country_district_b20_ppi.R, spearmans_urbrur_country_b20_ppi.R, spearmans_country_district_b20_regexp.R, and spearmans_urbrur_country_b20_regexp.R, respectively
# Outputs: Supplementary Figure 1
# Note: A (-1) factor was applied to the spearman's correlation coefficients and confidence intervals associated with comparisons with the PPI for ease of visualization

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)
library(ggpubr)
library(RVAideMemoire)
library(ggthemes)

# source relevant files ---------------------------------------------------

source(here("code", "visualizations", "figS1", "spearmans_country_district_b20_ppi.R"))
source(here("code", "visualizations", "figS1","spearmans_country_district_b20_regexp.R"))
source(here("code", "visualizations", "figS1", "spearmans_urbrur_country_b20_ppi.R"))
source(here("code", "visualizations", "figS1", "spearmans_urbrur_country_b20_regexp.R"))

# combine graphs into one figure ------------------------------------------

combine_spearmans_country_district_urbrur_b20_ppi_regexp <-
  ggarrange(
    spearmans_country_district_b20_ppi_graph,
    spearmans_urbrur_country_b20_ppi_graph,
    spearmans_country_district_b20_regexp_graph,
    spearmans_urbrur_country_b20_regexp_graph,
    ncol = 2,
    nrow = 2,
    labels = c("A", "B", "C", "D"),
    font.label=list(color="black",size=20)
  )

combine_spearmans_country_district_urbrur_b20_ppi_regexp