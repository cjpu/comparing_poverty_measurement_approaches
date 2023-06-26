# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: February 21, 2023
# Purpose: Cleans raw, primary data collected by Stanford University and the Ethiopian Public Health Association (EPHA) from June to July 2022 in Ethiopia 
# Inputs: ethiopia_listing_selected_vars.rds (raw data collected by Stanford and EPHA, which can be accessed here: https://osf.io/h2swb/, DOI 10.17605/OSF.IO/H2SWB), ethiopia_ppi_1.90_probabilities.csv (scorecard from the Poverty Probability Index which can be accessed here: https://www.povertyindex.org/country/ethiopia)
# Outputs: ethiopia_listing_selected_vars_cleaned.rds (a cleaned dataset)

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# import data -------------------------------------------------------------

ethiopia_listing_selected_vars_raw <- as_tibble(readRDS(here("data", "ethiopia", "ethiopia_listing_selected_vars.rds")))
ethiopia_ppi_1.90_index <- read.csv(here("data", "ethiopia", "ethiopia_ppi_1.90_probabilities.csv"))

# reclassify responses based on NAs and "other" responses----------------------------------------------------

# reclassify "-88" as NA for transport
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$transport == -88,]$transport <- NA

# reclassify "-88" and "-99" as NA for airtime
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$airtime == -88 | ethiopia_listing_selected_vars_raw$airtime == -99,]$airtime <- NA

# reclassify "do not know" and "no response" as NA for had_enough_food
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$had_enough_food == "No response" | ethiopia_listing_selected_vars_raw$had_enough_food == "Do not know",]$had_enough_food <- NA

# reclassify "no response" as NA for had_enough_clothing
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$had_enough_clothing == "No response",]$had_enough_clothing <- NA

# reclassify "do not know" as NA for mbr_edu_lvl_1
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$mbr_edu_lvl_1 == "Do not know",]$mbr_edu_lvl_1 <- NA

# reclassify "other" responses for education level of the hoh (other_elvl_1_en)
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$other_elvl_en_1 == "Diploma Teaching",]$mbr_edu_lvl_1 <- "Technical & vocational (TVET)"
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$other_elvl_en_1 == "Attended adult literacy classes",]$mbr_edu_lvl_1 <- "Adult literacy program"
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$other_elvl_en_1 == "Spiritual education" | ethiopia_listing_selected_vars_raw$other_elvl_en_1 == "Spiritual education to become a Priest" ,]$mbr_edu_lvl_1 <- "Spiritual education to become Priest"

# reclassify "do not know" as NA for consume_any_beef
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$consume_any_beef == "Do not know",]$consume_any_beef <- NA

# reclassify "do not know" as NA for consume_any_horse_bean
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$consume_any_horse_bean == "Do not know",]$consume_any_horse_bean <- NA

# reclassify "other" responses for type of toilet facility
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$tf_other_en == "Composting toilet",]$toilet_facility_type <- "Composting toilet"
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$tf_other_en == "Pit latrine with slab",]$toilet_facility_type <- "Pit latrine-Pit latrine with slab"

# reclassify "other" responses for source of light
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$sol_other_en == "Light from dry cell with switch",]$source_of_light <- "Light from dry cell with switch"
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$sol_other_en == "Local kerosene lamp (Kuraz)",]$source_of_light <- "Local kerosene lamp (Kuraz)"

# reclassify "other" responses for type of fuel for cooking
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$tof_other_en == "Collected firewood",]$type_of_fuel_used <- "Collected firewood"

# reclassify "do not know" as NA for number_of_axes
ethiopia_listing_selected_vars_raw[ethiopia_listing_selected_vars_raw$number_of_axes == "Do not know",]$number_of_axes <- NA

# create new / modify existing variables ----------------------------------

ethiopia_listing_cleaned_premerge <- 
  ethiopia_listing_selected_vars_raw %>% 

  # filter for eligible and consenting respondents
  
  filter(
    hh_member == "Yes, respondent is household member", 
    hh_relationship != "Other (specify)", 
    eighteen_older == "Yes", 
    able_to_answer == "Yes", 
    consent == "Yes", 
    consent_obtained == "Yes" 
  ) %>% 
  
  # rename variables to be consistent with other countries
  
  rename(
    hh_size = num_hh_members,
    enough_food = had_enough_food,
    enough_clothing = had_enough_clothing
  ) %>% 
  
  # recode rur_urb variable
  
  mutate(
    rur_urb = case_when(
      rur_urb == "1" ~ "urban", 
      rur_urb == "2" ~ "rural"
      ),
    
  # regular expenditures -- calculate total regular expenditures per capita per day (based on food, airitme, and transport expenditures in the last 2 weeks)
  
    regexp_2wks = food + airtime + transport,
    regexppcpd = regexp_2wks / hh_size / 14, 
  
  # krishna -- create binary variable for roof with iron sheets and other finished materials
  
  roof_finished_mat = ifelse(
    is.na(roof_material) == TRUE, NA, ifelse( 
      roof_material == "Finished Roofing-Metal/Corrugated Iron" | roof_material == "Finished Roofing-Wood" | roof_material == "Finished Roofing-Cement", 1 , 0
      )
    )
  ) %>% 
  
  # krishna -- create a variable that counts the total number of primary school going children between 7-14 
    
  rowwise() %>% 
  mutate(
    num_7to14_children_1 = sum(mbr_age_1 >=7 & mbr_age_1 <= 14, na.rm = TRUE),
    num_7to14_children_2 = sum(mbr_age_2 >=7 & mbr_age_2 <= 14, na.rm = TRUE),
    num_7to14_children_3 = sum(mbr_age_3 >=7 & mbr_age_3 <= 14, na.rm = TRUE),
    num_7to14_children_4 = sum(mbr_age_4 >=7 & mbr_age_4 <= 14, na.rm = TRUE),
    num_7to14_children_5 = sum(mbr_age_5 >=7 & mbr_age_5 <= 14, na.rm = TRUE),
    num_7to14_children_6 = sum(mbr_age_6 >=7 & mbr_age_6 <= 14, na.rm = TRUE),
    num_7to14_children_7 = sum(mbr_age_7 >=7 & mbr_age_7 <= 14, na.rm = TRUE),
    num_7to14_children_8 = sum(mbr_age_8 >=7 & mbr_age_8 <= 14, na.rm = TRUE),
    num_7to14_children_9 = sum(mbr_age_9 >=7 & mbr_age_9 <= 14, na.rm = TRUE),
    num_7to14_children_10 = sum(mbr_age_10 >=7 & mbr_age_10 <= 14, na.rm = TRUE),
    num_7to14_children_11 = sum(mbr_age_11 >=7 & mbr_age_11 <= 14, na.rm = TRUE),
    num_7to14_children_12 = sum(mbr_age_12 >=7 & mbr_age_12 <= 14, na.rm = TRUE),
    num_7to14_children_13 = sum(mbr_age_13 >=7 & mbr_age_13 <= 14, na.rm = TRUE),
    num_attended_school_1 = sum(regulary_attended_scl_1 == "Yes", na.rm = TRUE),
    num_attended_school_2 = sum(regulary_attended_scl_2 == "Yes", na.rm = TRUE),
    num_attended_school_3 = sum(regulary_attended_scl_3 == "Yes", na.rm = TRUE),
    num_attended_school_4 = sum(regulary_attended_scl_4 == "Yes", na.rm = TRUE),
    num_attended_school_5 = sum(regulary_attended_scl_5 == "Yes", na.rm = TRUE),
    num_attended_school_6 = sum(regulary_attended_scl_6 == "Yes", na.rm = TRUE),
    num_attended_school_7 = sum(regulary_attended_scl_7 == "Yes", na.rm = TRUE),
    num_attended_school_8 = sum(regulary_attended_scl_8 == "Yes", na.rm = TRUE),
    num_attended_school_9 = sum(regulary_attended_scl_9 == "Yes", na.rm = TRUE),
    num_attended_school_10 = sum(regulary_attended_scl_10 == "Yes", na.rm = TRUE),
    num_attended_school_11 = sum(regulary_attended_scl_11 == "Yes", na.rm = TRUE),
    num_attended_school_12 = sum(regulary_attended_scl_12 == "Yes", na.rm = TRUE),
    num_attended_school_13 = sum(regulary_attended_scl_13 == "Yes", na.rm = TRUE),
    num_hh_7to14 = num_7to14_children_1 + num_7to14_children_2 + num_7to14_children_3 + num_7to14_children_4 + num_7to14_children_5 + num_7to14_children_6 + num_7to14_children_7 + num_7to14_children_8 + num_7to14_children_9 + num_7to14_children_10 + num_7to14_children_11 + num_7to14_children_12 + num_7to14_children_13,
    num_7to14_attending_school = num_attended_school_1 + num_attended_school_2 + num_attended_school_3 + num_attended_school_4 + num_attended_school_5 + num_attended_school_6 + num_attended_school_7 + num_attended_school_8 + num_attended_school_9 + num_attended_school_10 + num_attended_school_11 + num_attended_school_12 + num_attended_school_13,
    all_attend_prim_school = ifelse(
      is.na(num_hh_7to14) == TRUE, NA, ifelse( 
        num_hh_7to14 == 0, NA, ifelse( 
          is.na(num_7to14_attending_school) == TRUE, NA, ifelse( 
            num_hh_7to14 == num_7to14_attending_school, 1, 0
          )
          )
        )
      )
    ) %>% 
  ungroup() %>% 

  mutate(

    # krishna -- create binary variable to represent Krishna's stages of progress (food, clothing, attending primary school, finished roof)
    
    poor_krishna = ifelse(

    # assign poverty status = 0 for households that meet all stages of progress with at least 1 child between 7 to 14 years of age

    is.na(enough_food) == FALSE & 
      enough_food == "Every day or almost every day" &
      is.na(enough_clothing) == FALSE & 
      enough_clothing  == "Every day or almost every day" &
      is.na(num_hh_7to14) == FALSE & 
      num_hh_7to14 != 0 &
      is.na(all_attend_prim_school) == FALSE & 
      all_attend_prim_school == 1 &
      is.na(roof_finished_mat) == FALSE & 
      roof_finished_mat == 1, 0,

    # assign poverty status for households with no children between 7 to 14 years of age but meet all other criteria

    ifelse(
      is.na(enough_food) == FALSE & 
        enough_food == "Every day or almost every day" &
        is.na(enough_clothing) == FALSE & 
        enough_clothing  == "Every day or almost every day" &
        is.na(num_hh_7to14) == FALSE & 
        num_hh_7to14 == 0 &
        is.na(roof_finished_mat) == FALSE & 
        roof_finished_mat == 1, 0, 
      
      # assign NA to households with incomplete information

      ifelse(
        is.na(enough_food) == TRUE |
          is.na(enough_clothing) == TRUE |
          is.na(num_hh_7to14) == TRUE |
          num_hh_7to14 != 0 & is.na(num_7to14_attending_school) == TRUE | 
          is.na(roof_finished_mat) == TRUE, NA, 1 
      )
    )
  ),

  # ppi_1.90 -- create ppi score for "in which region does the household live?" 
  
  ppi_score_region = ifelse(
    is.na(region) == TRUE, NA, ifelse( 
      region == "Amhara", 0, "error" 
    )
  ),
  
  # ppi_1.90 -- create ppi score for "how many members are there in the household?"
  
  ppi_score_hhmem = ifelse(
    is.na(hh_size) == TRUE, NA, ifelse( 
      hh_size == 1 | hh_size == 2 | hh_size == 3 | hh_size == 4, 24, ifelse(
        hh_size == 5 | hh_size == 6 | hh_size == 7, 12, ifelse(
          hh_size >= 8, 0, "error"
        )
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "what is the highest grade that the household head completed"
  
  ppi_score_hoh_edu = ifelse(
    is.na(mbr_edu_lvl_1) == TRUE, NA, ifelse( 
      mbr_edu_lvl_1 == "5th to 8th grade" | mbr_edu_lvl_1 == "9th or 10th grade (general secondary)" | mbr_edu_lvl_1 == "11th or 12th (upper secondary)" | mbr_edu_lvl_1 == "Bachelor's, Master's, or PhD degree" | mbr_edu_lvl_1 == "Technical & vocational (TVET)" | mbr_edu_lvl_1 == "Adult literacy program", 11, ifelse(
        mbr_edu_lvl_1 == "1st to 4th grade", 0, ifelse(
          hh_head_read_write_1 == "Yes, can read AND write", 11, ifelse(
            hh_head_read_write_1 == "Can read, but NOT write", 0, ifelse(
              hh_head_read_write_1 == "No, cannot read or write", 0, "error"
              )
            )
          )
        )
      )
    ), 
  
  # ppi_1.90 -- create ppi score for "over the past 7 days, did you or others in your household consume any beef?"
  
  ppi_score_beef = ifelse(
    is.na(consume_any_beef) == TRUE, NA, ifelse( 
      consume_any_beef == "Yes", 11, ifelse(
        consume_any_beef == "No", 0, "error" 
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "over the past 7 days, did you or others in your household consume any horse beans?"
  
  ppi_score_horsebeans = ifelse(
    is.na(consume_any_horse_bean) == TRUE, NA, ifelse( 
      consume_any_horse_bean == "Yes", 9, ifelse(
        consume_any_horse_bean == "No", 0, "error" 
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "the roof of the main dwelling is predominantly made of what material?"
  
  ppi_score_roof = ifelse( 
    is.na(roof_material) == TRUE, NA, ifelse( 
      roof_material == "Finished Roofing-Cement", 6, ifelse(
        roof_material == "Finished Roofing-Metal/Corrugated Iron", 6, ifelse(
          roof_material == "Finished Roofing-Wood", 0, ifelse( 
            roof_material == "Natural materials-Mud and wood", 0, ifelse(
              roof_material == "Natural materials-Thatch/Mud", 0, ifelse(
                roof_material == "Rudimentary roofing-Cardboard", 0, ifelse(
                  roof_material == "Rudimentary roofing-Palm/Bamboo", 0, ifelse(
                    roof_material == "Rudimentary roofing-Plastic canvas", 0, ifelse(
                      roof_material == "Rudimentary roofing-Wood planks", 0, "error"
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "what type of toilet facility does the household use?"
  
  ppi_score_toilet = ifelse( 
    is.na(toilet_facility_type) == TRUE, NA, ifelse( 
      toilet_facility_type == "Composting toilet", 0, ifelse(
        toilet_facility_type == "Flush or pour flush toilet-Flush to pit latrine" | toilet_facility_type == "Flush or pour flush toilet-Flush to septic tank" | toilet_facility_type == "Flush or pour flush toilet-Flush to somewhere else", 5, ifelse(
          toilet_facility_type == "No facility/bush/field/forest", 0, ifelse(
            toilet_facility_type == "Pit latrine-Open pit", 0, ifelse(
              toilet_facility_type == "Pit latrine-Pit latrine with slab", 5, ifelse(
                toilet_facility_type == "Pit latrine-Pit latrine without slab", 0, ifelse(
                  toilet_facility_type == "Pit latrine-Ventilated improved pit latrine", 5, "error" 
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "what is the main source of light for the household?"
  
  ppi_score_light = ifelse(
    is.na(source_of_light) == TRUE, NA, ifelse( 
      source_of_light == "Biogas", 0, ifelse(
        source_of_light == "Candle / wax", 0, ifelse(
          source_of_light == "Electrical battery", 0, ifelse(
            source_of_light == "Electricity meter-private", 10, ifelse(
              source_of_light == "Electricity meter-shared", 10, ifelse(
                source_of_light == "Firewood", 0, ifelse(
                  source_of_light == "Kerosene light lamp (imported)", 0, ifelse(
                    source_of_light == "Lantern", 10, ifelse(
                      source_of_light == "Light from dry cell with switch", 0, ifelse(
                        source_of_light == "Local kerosene lamp (Kuraz)", 0, ifelse(
                          source_of_light == "Solar energy", 10, ifelse(
                            source_of_light == "Other (Specify)", 0, "error" 
                            )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "what is the main source of cooking fuel?"
  
  ppi_score_fuel = ifelse(
    is.na(type_of_fuel_used) == TRUE, NA, ifelse( 
      type_of_fuel_used == "Agricultural crop residue", 0, ifelse(
        type_of_fuel_used == "Animal dung / manure", 0, ifelse(
          type_of_fuel_used == "Biogas", 0, ifelse(
            type_of_fuel_used == "Charcoal", 7, ifelse(
              type_of_fuel_used == "Collected firewood", 0, ifelse(
                type_of_fuel_used == "Electricity", 7, ifelse(
                  type_of_fuel_used == "Kerosene", 7, ifelse(
                    type_of_fuel_used == "None / No food cooked in household", 0, ifelse(
                      type_of_fuel_used == "Purchased firewood", 7, ifelse(
                        type_of_fuel_used == "Solar energy", 0, ifelse(
                          type_of_fuel_used == "Straw/shrubs/grass/leaves", 0, "error" 
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  # ppi_1.90 -- create ppi score for "how many axes (gejera) does your household own?"
  
  ppi_score_axes = ifelse(
    is.na(number_of_axes) == TRUE, NA, ifelse( 
      number_of_axes == "Zero", 7, ifelse(
        number_of_axes == "One or more", 0, "error" 
        )
      )
    ),
  
  # calculate total ppi score
  
  ppi_score_total = 
    as.double(ppi_score_region) + 
    as.double(ppi_score_hhmem) +
    as.double(ppi_score_hoh_edu) +
    as.double(ppi_score_beef) +
    as.double(ppi_score_horsebeans) +
    as.double(ppi_score_roof) +
    as.double(ppi_score_toilet) +
    as.double(ppi_score_light) +
    as.double(ppi_score_fuel) +
    as.double(ppi_score_axes),
  
  # create variable "hoh_read_and_write" -- binary variable indicating whether the head of household can read AND write (yes to both = 1)
  
  hoh_read_and_write = case_when(
    hh_head_read_write_1 == "Yes, can read AND write" ~ 1,
    (hh_head_read_write_1 == "No, cannot read or write" | hh_head_read_write_1 == "Can read, but NOT write") ~ 0,
    (hh_head_read_write_1 == "Do not know" | is.na(hh_head_read_write_1)) ~ NA_real_
  ),
  
  # create variable "hoh_attended_school" -- binary variable indicating whether the head of household ever attended formal school (yes = 1)
  
  hoh_attended_school = case_when(
    mbr_edu_lvl_1 == "None: No formal/regular schooling" ~ 0,
    mbr_edu_lvl_1 == "Spiritual education to become Priest" ~ 0,
    is.na(mbr_edu_lvl_1) ~ NA_real_,
    TRUE ~ 1
  ),
  
  # create variable "hoh_completed_prim_school" -- binary variable indicating whether the head of household completed primary school i.e., 1st to 8th grade (yes = 1)
  
  hoh_completed_prim_school = case_when(
    mbr_edu_lvl_1 %in% c("9th or 10th grade (general secondary)", "11th or 12th (upper secondary)", "Technical & vocational (TVET)", "Bachelor's, Master's, or PhD degree") ~ 1,
    mbr_edu_lvl_1 %in% c("1st to 4th grade", "5th to 8th grade","None: No formal/regular schooling", "Spiritual education to become Priest","Adult literacy program") ~ 0,
    TRUE ~ NA_real_
  ),
  
  # create variable "fem_hoh" -- binary variable indicating whether the head of household is female (yes = 1)
  
  fem_hoh = case_when(
    mbr_sex_1 == "Female" ~ 1,
    mbr_sex_1 == "Male" ~ 0,
    TRUE ~ NA_real_
  ),
  
  # create variable "marital_status" -- categorical variable indicating the marital status of the head of household
  
  hoh_marital_status = case_when(
    mbr_marital_sts_1 == "Married" ~ "Married",
    mbr_marital_sts_1 == "Divorced / separated" ~ "Divorced", 
    mbr_marital_sts_1 == "Living with a partner" ~ "Living with a partner",
    mbr_marital_sts_1 == "Widow / widower" ~ "Widow / widower",
    TRUE ~ NA_character_
  ),
  
  # create variable "plant_crops" -- categorical variable indicating when the household planted crops
  
  plant_crops = case_when(
    plant_crops == "Yes, we planted crops in the DRY SEASON" ~ "Plants in dry season only",
    plant_crops == "Yes, we planted crops in the RAINY SEASON" ~ "Plants in rainy season only",
    plant_crops == "Yes, we planted crops in BOTH SEASONS" ~ "Plants in both seasons",
    plant_crops == "NO, we did not plant any crops between June 2021 to May 2022" ~ "Does not plant",
    TRUE ~ NA_character_
  ),
  
  # create variable "earned_nonfarm" -- binary variable indicating whether the household engaged in a non-farm livelihood activity
  
  earned_nonfarm = case_when(
    payment_type_professional == "Yes" |
      payment_type_skilled == "Yes" |
      payment_type_unskilled == "Yes" |
      payment_type_wholesale == "Yes" |
      payment_type_retail == "Yes" |
      payment_type_military == "Yes" ~ 1,
    payment_type_professional == "No" &
      payment_type_skilled == "No" &
      payment_type_unskilled == "No" &
      payment_type_wholesale == "No" &
      payment_type_retail == "No" &
      payment_type_military == "No" ~ 0,
    payment_type_professional == "Do not know" &
      payment_type_skilled == "Do not know" &
      payment_type_unskilled == "Do not know" &
      payment_type_wholesale == "Do not know" &
      payment_type_retail == "Do not know" &
      payment_type_military == "Do not know" ~ NA_integer_,
    TRUE ~ 0 
    )
  ) %>% 
  
  # create variable representing dependency ratio (# hh members outside of the 15-64 range / # hh members between 15-64 years old)
  
  # create "hh_member_64yrs" variable indicating the number of household members that are 64 years old or younger
  rowwise() %>%
  mutate(
    num_hh_member_64yrs_or_younger = 
      sum(if_else(is.na(mbr_age_1) | mbr_age_1 <= 64, 0, 1),
          if_else(is.na(mbr_age_2) | mbr_age_2 <= 64, 0, 1),
          if_else(is.na(mbr_age_3) | mbr_age_3 <= 64, 0, 1),
          if_else(is.na(mbr_age_4) | mbr_age_4 <= 64, 0, 1),
          if_else(is.na(mbr_age_5) | mbr_age_5 <= 64, 0, 1),
          if_else(is.na(mbr_age_6) | mbr_age_6 <= 64, 0, 1),
          if_else(is.na(mbr_age_7) | mbr_age_7 <= 64, 0, 1),
          if_else(is.na(mbr_age_8) | mbr_age_8 <= 64, 0, 1),
          if_else(is.na(mbr_age_9) | mbr_age_9 <= 64, 0, 1),
          if_else(is.na(mbr_age_10) | mbr_age_10 <= 64, 0, 1),
          if_else(is.na(mbr_age_11) | mbr_age_11 <= 64, 0, 1),
          if_else(is.na(mbr_age_12) | mbr_age_12 <= 64, 0, 1),
          if_else(is.na(mbr_age_13) | mbr_age_13 <= 64, 0, 1)),
    
    # create "hh_member_15yrs" variable indicating the number of household members that are older than 15 years old
    
    num_hh_member_older_than_15yrs =
      sum(if_else(is.na(mbr_age_1) | mbr_age_1 > 15 | mbr_age_1 == -88, 0, 1), 
          if_else(is.na(mbr_age_2) | mbr_age_2 > 15, 0, 1),
          if_else(is.na(mbr_age_3) | mbr_age_3 > 15, 0, 1),
          if_else(is.na(mbr_age_4) | mbr_age_4 > 15, 0, 1),
          if_else(is.na(mbr_age_5) | mbr_age_5 > 15, 0, 1),
          if_else(is.na(mbr_age_6) | mbr_age_6 > 15, 0, 1),
          if_else(is.na(mbr_age_7) | mbr_age_7 > 15, 0, 1),
          if_else(is.na(mbr_age_8) | mbr_age_8 > 15, 0, 1),
          if_else(is.na(mbr_age_9) | mbr_age_9 > 15, 0, 1),
          if_else(is.na(mbr_age_10) | mbr_age_10 > 15, 0, 1),
          if_else(is.na(mbr_age_11) | mbr_age_11 > 15, 0, 1),
          if_else(is.na(mbr_age_12) | mbr_age_12 > 15, 0, 1),
          if_else(is.na(mbr_age_13) | mbr_age_13 > 15, 0, 1)),
    
    # create "num_hh_members" variable indicating the total number of members in the household
    
    num_hh_members =
      sum(if_else(is.na(mbr_age_1) | mbr_age_1 == -88, 0, 1), 
          if_else(is.na(mbr_age_2), 0, 1),
          if_else(is.na(mbr_age_3), 0, 1),
          if_else(is.na(mbr_age_4), 0, 1),
          if_else(is.na(mbr_age_5), 0, 1),
          if_else(is.na(mbr_age_6), 0, 1),
          if_else(is.na(mbr_age_7), 0, 1),
          if_else(is.na(mbr_age_8), 0, 1),
          if_else(is.na(mbr_age_9), 0, 1),
          if_else(is.na(mbr_age_10), 0, 1),
          if_else(is.na(mbr_age_11), 0, 1),
          if_else(is.na(mbr_age_12), 0, 1),
          if_else(is.na(mbr_age_13), 0, 1)),
    
    # compute numerator (# of household members over 64 and younger than 15)
    
    dependency_ratio_numerator =
      (as.double(num_hh_member_64yrs_or_younger) + as.double(num_hh_member_older_than_15yrs)),
    
    # compute denominator (# of household members between 15 to 64 years of age)
    
    dependency_ratio_denominator =
      (as.double(num_hh_members) - as.double(num_hh_member_64yrs_or_younger) - as.double(num_hh_member_older_than_15yrs)),
    
    # compute dependency ratio 
    
    dependency_ratio = dependency_ratio_numerator / dependency_ratio_denominator,
    dependency_ratio = if_else(dependency_ratio < 0, NA_real_, dependency_ratio),
    dependency_ratio = if_else(dependency_ratio_denominator == 0, NA_real_, dependency_ratio)
    
  ) %>%
  
  ungroup() %>%
  
  # remove variables that have been converted and recoded
  
  select(
    - hh_member,
    - hh_relationship,
    - hh_rship_other,
    - hh_rship_other_en,
    - eighteen_older,
    - able_to_answer,
    - consent,
    - consent_obtained,
    - food,
    - airtime,
    - transport,
    - water,
    - medicine,
    - number_of_axes,
    - type_of_fuel_used,
    - source_of_light,
    - toilet_facility_type, 
    - consume_any_horse_bean,
    - consume_any_beef,
    - hh_head_read_write_1,
    - mbr_edu_lvl_1,
    - other_elvl_1,
    - other_elvl_en_1,
    - rm_other,
    - tf_other,
    - tf_other_en,
    - sol_other,
    - sol_other_en,
    - tof_other,
    - tof_other_en,
    - mbr_age_1,
    - mbr_age_2,
    - mbr_age_3,
    - mbr_age_4,
    - mbr_age_5,
    - mbr_age_6,
    - mbr_age_7,
    - mbr_age_8,
    - mbr_age_9,
    - mbr_age_10,
    - mbr_age_11,
    - mbr_age_12,
    - mbr_age_13,
    - num_7to14_children_1,
    - num_7to14_children_2,
    - num_7to14_children_3,
    - num_7to14_children_4,
    - num_7to14_children_5,
    - num_7to14_children_6,
    - num_7to14_children_7,
    - num_7to14_children_8,
    - num_7to14_children_9,
    - num_7to14_children_10,
    - num_7to14_children_11,
    - num_7to14_children_12,
    - num_7to14_children_13,
    - regulary_attended_scl_1,
    - regulary_attended_scl_2,
    - regulary_attended_scl_3,
    - regulary_attended_scl_4,
    - regulary_attended_scl_5,
    - regulary_attended_scl_6,
    - regulary_attended_scl_7,
    - regulary_attended_scl_8,
    - regulary_attended_scl_9,
    - regulary_attended_scl_10,
    - regulary_attended_scl_11,
    - regulary_attended_scl_12,
    - regulary_attended_scl_13,
    - num_attended_school_1,
    - num_attended_school_2,
    - num_attended_school_3,
    - num_attended_school_4,
    - num_attended_school_5,
    - num_attended_school_6,
    - num_attended_school_7,
    - num_attended_school_8,
    - num_attended_school_9,
    - num_attended_school_10,
    - num_attended_school_11,
    - num_attended_school_12,
    - num_attended_school_13,
    - ppi_score_region,
    - ppi_score_hhmem,
    - ppi_score_hoh_edu,
    - ppi_score_beef,
    - ppi_score_horsebeans,
    - ppi_score_roof,
    - ppi_score_toilet,
    - ppi_score_light,
    - ppi_score_fuel,
    - ppi_score_axes,
    - mbr_rship_1, 
    - hh_head_read_write_1,
    - mbr_edu_lvl_1,
    - mbr_sex_1,
    - payment_type_professional,
    - payment_type_skilled,
    - payment_type_unskilled,
    - payment_type_wholesale,
    - payment_type_retail,
    - payment_type_military,
    - mbr_age_1,
    - mbr_age_2,
    - mbr_age_3,
    - mbr_age_4,
    - mbr_age_5,
    - mbr_age_6,
    - mbr_age_7,
    - mbr_age_8,
    - mbr_age_9,
    - mbr_age_10,
    - mbr_age_11,
    - mbr_age_12,
    - mbr_age_13,
    - num_hh_member_64yrs_or_younger,
    - num_hh_member_older_than_15yrs,
    - dependency_ratio_numerator,
    - dependency_ratio_denominator,
    - region,
    - hh_size,
    - roof_material,
    - other_mbr_ship_1,
    - regexp_2wks,
    - num_7to14_attending_school,
    - num_hh_members,
    - mbr_marital_sts_1
  ) %>% 

  # move geographic identifiers to the beginning

  relocate(
    district,
    rur_urb
  ) %>% 
  
  # convert krishna to a factor
  
  mutate(
    poor_krishna = as.factor(poor_krishna)
  )

# merge ppi probabilities -------------------------------------------------

ethiopia_listing_cleaned <- left_join(
  ethiopia_listing_cleaned_premerge,
  ethiopia_ppi_1.90_index,
  by = c("ppi_score_total" = "score")
) %>% 
  rename(
    ppi_prob_1.90 = probability
  ) %>% 
  select(
    - ppi_score_total
  )

# remove "%" from the merged probabilities and convert variable type to a double

ethiopia_listing_cleaned$ppi_prob_1.90 = substr(ethiopia_listing_cleaned$ppi_prob_1.90, 1, nchar(ethiopia_listing_cleaned$ppi_prob_1.90)-1)
ethiopia_listing_cleaned$ppi_prob_1.90 = as.double(ethiopia_listing_cleaned$ppi_prob_1.90)

# export data -------------------------------------------------------------

saveRDS(ethiopia_listing_cleaned, here("data", "ethiopia", "ethiopia_listing_selected_vars_cleaned.rds"))
