# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu)
# Date Created: September 9, 2022
# Purpose: Cleans raw, primary data collected by Stanford University and the International Growth Research and Evaluation Center (IGREC) from February to March 2022 in Uganda
# Inputs: uganda_listing_selected_vars.rds (raw data collected by Stanford and IGREC, which can be accessed here: https://osf.io/h2swb/, DOI 10.17605/OSF.IO/H2SWB)
# Outputs: uganda_listing_selected_vars_cleaned.rds (a cleaned dataset)

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# import data -------------------------------------------------------------

uganda_listing_selected_vars_raw <- as_tibble(readRDS(here("data", "uganda", "uganda_listing_selected_vars.rds")))

# reclassify other responses ----------------------------------------------

# reclassify "g57_rooftype_ot" (other roof type responses)
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g57_rooftype_ot == "Both iron sheet and grassthatch",]$g57_rooftype <- 8
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g57_rooftype_ot == "Both iron sheet and  grassthatch",]$g57_rooftype <- 8
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g57_rooftype_ot == "Many houses others iron sheet and grassthatch",]$g57_rooftype <- 8
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g57_rooftype_ot == "Banana fibers",]$g57_rooftype <- 1
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g57_rooftype_ot == "Unburnt mud blocks with mud",]$g57_rooftype <- 2

# reclassify "g58_walltype_ot" (other wall type responses) 
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Both burnt brick and cement and mud block",]$g58_walltype <- 13
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Both burnt brick with cement and mud block",]$g58_walltype <- 13
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Both burnt brick with cement and mud brick",]$g58_walltype <- 13
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Both burnt brick with cement and unburnt brick with mud",]$g58_walltype <- 13
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Both burnt brick with half plusterd wall and unburnt brick with mud",]$g58_walltype <- 5
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Mud blocks with mud",]$g58_walltype <- 5
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Other part is burnt brick with cement and other is unburnt brick with mud",]$g58_walltype <- 13
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Poles with cement and sand",]$g58_walltype <- 3
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Poles with mud but plastered with cement and sand",]$g58_walltype <- 3
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Poles with sand and cement",]$g58_walltype <- 3
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Sand",]$g58_walltype <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Unburnt blocks with mud",]$g58_walltype <- 5
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Unburnt bricks with mud",]$g58_walltype <- 5
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$g58_walltype_ot == "Unburnt mud blocks with mud",]$g58_walltype <- 5

# reclassify e40_fuelcooking_ot (other main fuel for cooking responses)
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e40_fuelcooking_ot == "The respondent doesn't cook, He eats from the restaurant.",]$e40_fuelcooking <- 10

# reclassify f46_toilettype_ot (other sanitation type responses) 
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Does not have a toilet and uses the one for the neighbor",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Have Vip outside the compound and flush toilet inside the house",]$f46_toilettype <- 4
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "No facility they share with another household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "No facility,she shares with the neibours",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "No facility,the respondent uses the neighbor's latrine",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Share with brother's household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Share with neighbors",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared toilet",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared with brother's household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shares with father's household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared with neighbor",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared with son's household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared. Covered pit latrine with slab",]$f46_toilettype <- 7
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Shared with father's household",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Use neighbor's facility",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Uses for his father",]$f46_toilettype <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$f46_toilettype_ot == "Using other household's toilet",]$f46_toilettype <- NA

# reclassify e44_workdone_ot (other nonfarm work done responses)
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Assistance from children",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Church contribution",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Farming",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Fishing.",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "From Village Saving",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Her children give her money to run the household, she said she doesn't enguge herself in any activity that brings money to her household.",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Parents",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "Remittance from family abroad",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "She is now very old and can't do much and well wishers and her children just support her.",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "She's not working at the moment.",]$e44_workdone <- "8"
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$e44_workdone_ot == "The respondent gets financial support from her children who are nolonger members of the household.",]$e44_workdone <- "8"

# reclassify b20_educlevel_ot (other responses for highest level of education completed)
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Didn't attend school",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Didn't attend school",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Didn't attend school",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Informal education",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Informal education",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Informal education",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Never Attended any school",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Never Attended any school",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Never Attended any school",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "No formal education.",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "No formal education.",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "No formal education.",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Non informal education",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Non informal education",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Non informal education",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Not educated",]$b19_attendsch <- 2
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Not educated",]$b20_educlevel <- NA
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Not educated",]$b20_educlevel_ot <- ""

uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Not sure",]$b20_educlevel <- 99
uganda_listing_selected_vars_raw[uganda_listing_selected_vars_raw$b20_educlevel_ot == "Not sure",]$b20_educlevel_ot <- ""

# create new / modify existing variables ----------------------------------

uganda_listing_cleaned <- 
  uganda_listing_selected_vars_raw %>% 
  
  # filter for eligible and consenting respondents
  
  filter(
    a13_hhmember == 1, 
    a15_relationhead != 12 & a15_relationhead != 13 & a15_relationhead != 14,
    a14_18yrsandabove == 1, 
    a16_able2answer == 1, 
    a17_consent == 1 
  ) %>% 

  # rename variables
  
  rename(
    hh_size = c23_totalhhmem,
    num_hh_6to12 = c24_agebrackets6to12,
    num_6to12_attending_school = c25_childsch
  ) %>% 

  mutate(
    
    # regular expenditures -- reclassify values as NA for 99 and 999 values (which signify "don't know") for regular household expenditures
    
    regexp_2wks = ifelse(
    g55_spenton_basic == 99, NA, ifelse( 
      g55_spenton_basic == 999, NA, g55_spenton_basic 
      )
    ),
    
    # regular expenditures -- calculate total regular expenditures per capita and per day
  
    regexppcpd = regexp_2wks/hh_size/14, 
  
    # krishna -- rename and recode e39_enuffood
    
    enough_food = case_when( 
      e39_enuffood == "1" ~ "always",
      e39_enuffood == "2" ~ "most days",
      e39_enuffood == "3" ~ "some days",
      e39_enuffood == "4" ~ "rarely"
    ),
    
    # krishna -- rename and recode e41_hhclothing
    
    enough_clothing = case_when( 
      e41_hhclothing == "1" ~ "always",
      e41_hhclothing == "2" ~ "most days",
      e41_hhclothing == "3" ~ "some days",
      e41_hhclothing == "4" ~ "rarely"
    ),
    
    # krishna-- rename and recode g57_rooftype
    
    main_roof_mat = case_when(
      g57_rooftype == "1" ~ "thatch_palm", 
      g57_rooftype == "2" ~ "mud", 
      g57_rooftype == "3" ~ "rustic_mat",
      g57_rooftype == "4" ~ "tins",
      g57_rooftype == "5" ~ "wood_planks", 
      g57_rooftype == "6" ~ "cardboard",
      g57_rooftype == "7" ~ "tarpaulin",
      g57_rooftype == "8" ~ "ironsheets",
      g57_rooftype == "9" ~ "wood",
      g57_rooftype == "10" ~ "asbestos",
      g57_rooftype == "11" ~ "tiles",
      g57_rooftype == "12" ~ "concrete",
      g57_rooftype == "96" ~ "other"
    ),
    
    # krishna -- create binary variable for roof with iron sheets and other finished materials
    
    roof_finished_mat = ifelse(
      main_roof_mat == "ironsheets" | main_roof_mat == "concrete" | main_roof_mat == "tiles" | main_roof_mat == "wood", 1 , 0
    ),  
    
    # krishna -- create binary variable for sending all children between 6 to 12 to primary school 
    
    all_attend_prim_school = ifelse(
      is.na(num_hh_6to12) == TRUE, NA, ifelse( 
        num_hh_6to12 == 0, NA, ifelse( 
          is.na(num_6to12_attending_school) == TRUE, NA, ifelse( 
            num_6to12_attending_school == num_hh_6to12, 1, 0
          )
        )
      )
    ),

    # krishna -- create binary variable to represent Krishna's stages of progress (enough food, enough clothing, all children to primary school, iron roof)
    
    poor_krishna = ifelse(
      
      # assign poverty status = 0 for households that meet all stages of progress with at least 1 child between 6 to 12 years of age
      
      is.na(enough_food) == FALSE & 
        enough_food == "always" & 
        is.na(enough_clothing) == FALSE & 
        enough_clothing == "always" & 
        is.na(all_attend_prim_school) == FALSE &
        all_attend_prim_school == 1 & 
        is.na(roof_finished_mat) == FALSE & 
        roof_finished_mat == 1, 0,
 
      
      # assign poverty status for households with no children between 6 to 12 years of age but meet all other criteria
      
      ifelse(
        is.na(enough_food) == FALSE & 
          enough_food == "always" & 
          is.na(enough_clothing) == FALSE &
          enough_clothing == "always" &  
          is.na(num_hh_6to12) == FALSE & 
          num_hh_6to12 == 0 & 
          is.na(roof_finished_mat) == FALSE &
          roof_finished_mat == 1, 0,
        
        # assign NA to households with incomplete information
        
        ifelse(
          is.na(enough_food) == TRUE | 
            is.na(enough_clothing) == TRUE | 
            is.na(num_hh_6to12) == TRUE | 
            num_hh_6to12 != 0 & is.na(num_6to12_attending_school) == TRUE | 
            is.na(roof_finished_mat) == TRUE, NA, 1 
        ) 
      )
    ), 
    
    # ppi_1.90 -- create poverty probability index score for "how many members does the household have?"
    
    ppi_score_hhmem = ifelse(
      is.na(hh_size) == TRUE, NA, ifelse(
        hh_size == 1, 28, ifelse(
          hh_size == 2, 21, ifelse(
            hh_size == 3, 12, ifelse(
              hh_size == 4, 8, ifelse(
                hh_size == 5 | hh_size == 6, 6, ifelse(
                  hh_size == 7, 4, ifelse(
                    hh_size == 8, 3, 0
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # ppi_1.90 -- create ppi score for "are all household members ages 6 to 12 currently in school?"
    
    ppi_score_6to12school = ifelse(
      is.na(all_attend_prim_school) == FALSE & all_attend_prim_school == 1, 2, ifelse(
        is.na(all_attend_prim_school) == FALSE & all_attend_prim_school == 0, 0, ifelse(
          num_hh_6to12 == 0, 5, "error" # 0 errors
        )
      )
    ),
    
    # ppi_1.90 -- rename and recode b22_readwrite
    
    oldest_fem_read_write = case_when( 
      b22_readwrite == "1" ~ "yes",
      b22_readwrite == "2" ~ "no female head/spouse",
      b22_readwrite == "3" ~ "no"
    ),
    
    # ppi_1.90 -- create ppi score for "can the (oldest) female head / spouse read and write with understanding in any language?
    
    ppi_score_femread = ifelse(
      oldest_fem_read_write == "yes", 3, ifelse(
        oldest_fem_read_write == "no", 0, ifelse(
          oldest_fem_read_write == "no female head/spouse", 0, NA
        )
      )
    ),

    # ppi_1.90 -- rename and recode g58_walltype
    
    main_wall_mat = case_when(
      g58_walltype == "1" ~ "thatched_straw", 
      g58_walltype == "2" ~ "dirt", 
      g58_walltype == "3" ~ "poles_mud",
      g58_walltype == "4" ~ "stone_mud",
      g58_walltype == "5" ~ "unburnt_bricks_mud", 
      g58_walltype == "6" ~ "plywood",
      g58_walltype == "7" ~ "reused_wood",
      g58_walltype == "8" ~ "cardboard",
      g58_walltype == "9" ~ "unburnt_bricks_plaster",
      g58_walltype == "10" ~ "burnt_bricks_mud",
      g58_walltype == "11" ~ "cement",
      g58_walltype == "12" ~ "stone_cement_lime",
      g58_walltype == "13" ~ "burnt_bricks_cement",
      g58_walltype == "14" ~ "unburnt_bricks_cement",
      g58_walltype == "15" ~ "cement_blocks",
      g58_walltype == "16" ~ "wood_planks_shingles",
      g58_walltype == "17" ~ "tin_iron_sheets",
      g58_walltype == "18" ~ "no_walls",
      g58_walltype == "19" ~ "cant_determine",
      g58_walltype == "96" ~ "other" 
    ),
    
    # ppi_1.90 -- create ppi score for "what type of material is mainly used for construction of the wall of the dwelling?
    
    ppi_score_wall = ifelse(
      main_wall_mat == "thatched_straw" |
        main_wall_mat == "dirt" |
        main_wall_mat == "poles_mud" |
        main_wall_mat == "stone_mud" | 
        main_wall_mat == "unburnt_bricks_mud", 0, 
      ifelse(
        main_wall_mat == "plywood" | 
          main_wall_mat == "reused_wood" |
          main_wall_mat == "unburnt_bricks_plaster" |
          main_wall_mat == "burnt_bricks_mud" | 
          main_wall_mat == "cement" | 
          main_wall_mat == "stone_cement_lime" | 
          main_wall_mat == "burnt_bricks_cement" |
          main_wall_mat == "unburnt_bricks_cement" |
          main_wall_mat == "cement_blocks" |
          main_wall_mat == "wood_planks_shingles" |
          main_wall_mat == "tin_iron_sheets", 4, "error"
      )
    ),
  
    # ppi_1.90 -- create ppi score for "what type of material is mainly used for construction of the roof of the dwelling?"
    
    ppi_score_roof = ifelse(
      main_roof_mat == "thatch_palm" |
        main_roof_mat == "mud" | 
        main_roof_mat == "rustic_mat" |
        main_roof_mat == "tins" |
        main_roof_mat == "tarpaulin", 0, 

      ifelse(
        main_roof_mat == "ironsheets" | 
          main_roof_mat == "wood" | 
          main_roof_mat == "tiles" | 
          main_roof_mat == "concrete", 5, "error" 
      )
    ),
    
    # ppi_1.90 -- rename and recode e40_fuelcooking
    
    main_fuel_cooking = case_when(
      e40_fuelcooking == "1" ~ "electricity",
      e40_fuelcooking == "2" ~ "lpg_cylinder_gas",
      e40_fuelcooking == "3" ~ "biogas", 
      e40_fuelcooking == "4" ~ "kerosene_paraffin",
      e40_fuelcooking == "5" ~ "charcoal", 
      e40_fuelcooking == "6" ~ "wood",
      e40_fuelcooking == "7" ~ "straw_shrubs_grass",
      e40_fuelcooking == "8" ~ "crop_residue",
      e40_fuelcooking == "9" ~ "animal_dung",
      e40_fuelcooking == "10" ~ "no_food_cooked_in_house",
      e40_fuelcooking == "96" ~ "other"
    ),
    
    # ppi_1.90 -- create ppi score for "what source of energy does the household mainly use for cooking?"
    
    ppi_score_cookingfuel = ifelse(
      main_fuel_cooking == "electricity" |
        main_fuel_cooking == "lpg_cylinder_gas" |
        main_fuel_cooking == "biogas" |
        main_fuel_cooking == "kerosene_paraffin" |
        main_fuel_cooking == "charcoal" |
        main_fuel_cooking == "no_food_cooked_in_house", 6,  
      ifelse(
        main_fuel_cooking == "wood" |
          main_fuel_cooking == "straw_shrubs_grass" |
          main_fuel_cooking == "crop_residue" |
          main_fuel_cooking == "animal_dung" , 0, "error" 
      )
    ),
    
    # ppi_1.90 -- rename and recode main sanitation values
    
    toilet = case_when(
      f46_toilettype == "1" ~ "flush_piped_sewer",
      f46_toilettype == "2" ~ "flush_septic_tank",
      f46_toilettype == "3" ~ "flush_pit_latrine",
      f46_toilettype == "4" ~ "flush_somewhere_else",
      f46_toilettype == "5" ~ "flush_dk_where",
      f46_toilettype == "6" ~ "vip",
      f46_toilettype == "7" ~ "covered_pit_slab",
      f46_toilettype == "8" ~ "covered_pit_no_slab",
      f46_toilettype == "9" ~ "uncovered_pit",
      f46_toilettype == "10" ~ "composting",
      f46_toilettype == "11" ~ "bucket",
      f46_toilettype == "12" ~ "hanging",
      f46_toilettype == "13" ~ "field",
      f46_toilettype == "96" ~ "other"
      ),
    
    # ppi_1.90 -- create ppi score for "what type of toilet facility does the household mainly use?"
    
    ppi_score_toilet = ifelse(
      toilet == "flush_piped_sewer" |
        toilet == "flush_septic_tank" |
        toilet == "flush_pit_latrine" |
        toilet == "flush_somewhere_else" |
        toilet == "flush_dk_where" |
        toilet == "vip", 11, 
      ifelse(
        toilet == "covered_pit_slab", 6,
        ifelse(
          toilet == "covered_pit_no_slab" |
            toilet == "uncovered_pit" | 
            toilet == "composting", 4, 
          ifelse(
            toilet == "bucket" |
              toilet == "hanging" |
              toilet == "field" , 0, "error" 
          )
        )
      )
    ),
    
    # ppi_1.90 -- rename and recode based on g49_phone and g50_phonesowned
    
    num_mobile_owned = ifelse(
      g49_phone == 1 & g50_phonesowned == 99, NA, ifelse(
        g49_phone == 1 & g50_phonesowned == 1, "one", ifelse(
          g49_phone == 1 & g50_phonesowned == 2, "two", ifelse(
            g49_phone == 1 & g50_phonesowned >= 3, "three or more", ifelse(
              g49_phone == 0, "none", "error" 
            )
          )
        )
      )
    ),
    
    # ppi_1.90 -- create ppi score for "how many mobile phones do members of your household own?"
    
    ppi_score_phones = ifelse(
      is.na(num_mobile_owned) == TRUE, NA, ifelse(
        num_mobile_owned == "none", 0, ifelse(
          num_mobile_owned == "one", 7, ifelse(
            num_mobile_owned == "two", 12, ifelse(
              num_mobile_owned == "three or more", 22, "error" 
            )
          )
        )
      )
    ),
    
    # ppi_1.90 -- rename and recode g48_radio
    
    own_radio = case_when(
      g48_radio == "1" ~ "yes",
      g48_radio == "0" ~ "no"
    ),
    
    # ppi_1.90 -- create ppi score for "does any member of your household own a radio?"
    
    ppi_score_radio = ifelse(
      own_radio == "no", 0, ifelse(
        own_radio == "yes", 7, "error" 
      )
    ),
    
    # ppi_1.90 -- rename and recode shoes
    
    own_one_pair_shoes = case_when( 
      shoes == "1" ~ "yes",
      shoes == "0" ~ "no"
    ),
    
    # ppi_1.90 -- create ppi score for "does every member of the household have at least one pair of shoes?"
    
    ppi_score_shoes = ifelse(
      own_one_pair_shoes == "no", 0, ifelse(
        own_one_pair_shoes == "yes", 9, "error" 
      )
    ),
    
    # ppi_1.90 -- calculate total ppi score
    
    ppi_score_total = 
      as.double(ppi_score_hhmem) + 
      as.double(ppi_score_6to12school) +
      as.double(ppi_score_femread) +
      as.double(ppi_score_wall) +
      as.double(ppi_score_roof) +
      as.double(ppi_score_cookingfuel) +
      as.double(ppi_score_toilet) +
      as.double(ppi_score_phones) +
      as.double(ppi_score_radio) +
      as.double(ppi_score_shoes), 
    
    # ppi_1.90 -- classify probability that household lives under $1.90/day 2011 PPP (according to their PPI scores based on the scorecards from the Poverty Probability Index website which can be accessed here: https://www.povertyindex.org/country/uganda)
    
    ppi_prob_1.90 = ifelse(
      is.na(ppi_score_total) == TRUE, NA, ifelse(
        ppi_score_total >= 0 & ppi_score_total <= 4, 96.7, ifelse(
            ppi_score_total >= 5 & ppi_score_total <= 10, 92.5, ifelse(
              ppi_score_total >= 10 & ppi_score_total <= 14, 81.1, ifelse(
                ppi_score_total >= 15 & ppi_score_total <= 19, 73.5, ifelse(
                  ppi_score_total >= 20 & ppi_score_total <= 24, 68.3, ifelse(
                    ppi_score_total >= 25 & ppi_score_total <= 29, 54.5, ifelse(
                      ppi_score_total >= 30 & ppi_score_total <= 34, 37.5, ifelse(
                        ppi_score_total >= 35 & ppi_score_total <= 39, 29.7, ifelse(
                          ppi_score_total >= 40 & ppi_score_total <= 44, 26.0, ifelse(
                            ppi_score_total >= 45 & ppi_score_total <= 49, 16.7, ifelse(
                              ppi_score_total >= 50 & ppi_score_total <= 54, 8.1, ifelse(
                                ppi_score_total >= 55 & ppi_score_total <= 59, 4.0, ifelse(
                                  ppi_score_total >= 60 & ppi_score_total <= 64, 0.6, ifelse(
                                    ppi_score_total >= 65 & ppi_score_total <= 69, 0.4, ifelse(
                                      ppi_score_total >= 70 & ppi_score_total <= 74, 0, ifelse(
                                        ppi_score_total >= 75 & ppi_score_total <= 79, 0, ifelse(
                                          ppi_score_total >= 80 & ppi_score_total <= 84, 0, ifelse(
                                            ppi_score_total >= 85 & ppi_score_total <= 90, 0, ifelse(
                                              ppi_score_total >= 90 & ppi_score_total <= 94, 0, ifelse(
                                                ppi_score_total >= 95 & ppi_score_total <= 100, 0, "error"
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
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # recode district values
    
    district = case_when(
      a05_district == "1" ~ "lira",
      a05_district == "2" ~ "kabarole"
    ),
    
    # recode region values
    
    region = case_when(
      a04_region == "1" ~ "northern",
      a04_region == "2" ~ "western"
    ),
    
    # convert to correct variable type
    
    ppi_prob_1.90 = as.double(ppi_prob_1.90),
    poor_krishna = as.factor(poor_krishna)
  
    ) %>%
  
  mutate(

    # create new variable with value = 1 if HOH ever attended school and value = 0 if HOH never attended school
    
    hoh_attended_school = ifelse(
      b19_attendsch == 1, 1, ifelse(
        b19_attendsch == 2, 0, ifelse(
          b19_attendsch == 99, NA, "error" 
        )
      )
    ),
    
    # create new variable with value = 1 if HOH completed primary school and value = 0 if HOH did not complete primary school
    
    hoh_completed_prim_school = ifelse(
      b19_attendsch == 2, 0, ifelse( 
        is.na(b20_educlevel) == TRUE, NA, ifelse( 
          b20_educlevel == 1 | b20_educlevel == 2 | b20_educlevel == 3 | b20_educlevel == 4 | b20_educlevel == 5 | b20_educlevel == 6, 0, ifelse(
            b20_educlevel == 99, NA, 1 
          )
        )
      )
    ),
    
    # create new variable with value = 1 if sex of respondent is female and value = 0 if sex of respondent is male 
    
    fem_resp = ifelse(
      a19_gender == 1, 0, ifelse(
        a19_gender == 2, 1, ifelse(
          is.na(a19_gender) == TRUE, NA, "error" 
        )
      )
    ),
    
    # create new variable with value = 1 if sex of HOH is female and value = 0 if sex of HOH is male 

    fem_hoh = ifelse(
     is.na(b21_femhh) == TRUE & a15_relationhead == 1, fem_resp, ifelse( 
        b21_femhh == 1, 1, ifelse(
          b21_femhh == 2, 0, "error"
        )
      )
    ),

    # create new variable computing the dependency ratio (# of individuals 65 and over and under 15 years : # of individuals between 15 to 64)
    
    dependency_ratio = ifelse(
      c24_agebrackets15_64 != 0 , round((c24_agebrackets64 + c24_agebrackets15 + num_hh_6to12 + c24_agebrackets5) / c24_agebrackets15_64, digits = 2), NA 
    ),
    
    # create new variable illustrating whether households grew crops and the purpose of growing those crops
    
    grew_crops_purpose = ifelse(
      d28_grewcrops == 99, NA, ifelse(
        d28_grewcrops == 2, "For consumption only", ifelse(
          d28_grewcrops == 3, "Did not grow crops", ifelse(
            d28_grewcrops == 1, "For selling only", "For consumption and selling"
          )
        )
      )
    ),

    # create new variable with value = 1 if the hh earned money / in-kind payments from any farm activities and value = 0 if household did not
    
    earned_farm = ifelse(
      e37_earnedmoney == 99, NA, ifelse(
        e37_earnedmoney == 1, 1, 0 
      )
    ),

    # create new variable with value = 1 if the hh earned money / in-kind payments from any non-farm activities and value = 0 if household did not
    
    earned_nonfarm = ifelse(
      e44_workdone == 99, NA, ifelse(
        e44_workdone == 8, 0, 1 
      )
    ),
    
    # create new variable with value = 1 if the hh earned more from non-farm than from farm activities. 
    
    main_livelihood = ifelse(
      e45_earnmoless == 1, "Earned more from farm", ifelse(
        e45_earnmoless == 2, "Earned more from non-farm", ifelse(
          e45_earnmoless == 3, "Earned the same from both", NA
            )
          )
        ),

    # create new variable with value = 1 if hh received remittances from friends and family not living with them
    
    remittances_friends_family = ifelse(
      e38_outearnedmoney == 99, NA, ifelse(
        e38_outearnedmoney == 1, 1, ifelse(
          e38_outearnedmoney == 2, 0, "error"
        )
      )
    )
  ) %>% 
  
    # remove variable names that have been converted and recoded
    
    select(
      - a13_hhmember,
      - a14_18yrsandabove,
      - a15_relationhead,
      - a16_able2answer,
      - a17_consent,
      - a04_region,
      - region,
      - a05_district,
      - f46_toilettype,
      - f46_toilettype_ot,
      - g55_spenton_basic,
      - b22_readwrite,
      - shoes, 
      - g48_radio,
      - e40_fuelcooking,
      - e40_fuelcooking_ot,
      - g50_phonesowned,
      - g57_rooftype,
      - e39_enuffood,
      - e41_hhclothing,
      - g58_walltype,
      - ppi_score_hhmem,
      - ppi_score_6to12school,
      - ppi_score_femread,
      - ppi_score_wall,
      - ppi_score_roof,
      - ppi_score_cookingfuel,
      - ppi_score_toilet,
      - ppi_score_phones,
      - ppi_score_radio,
      - ppi_score_shoes,
      - g49_phone,
      - g57_rooftype_ot,
      - g58_walltype_ot,
      - oldest_fem_read_write,
      - main_wall_mat,
      - main_fuel_cooking,
      - toilet,
      - num_mobile_owned,
      - own_radio,
      - own_one_pair_shoes,
      - b19_attendsch,
      - b20_educlevel,
      - a19_gender,
      - b21_femhh,
      - fem_resp,
      - c24_agebrackets15_64,
      - c24_agebrackets64,
      - c24_agebrackets15,
      - c24_agebrackets5,
      - d28_grewcrops,
      - e37_earnedmoney,
      - e44_workdone,
      - e45_earnmoless,
      - e38_outearnedmoney,
      - regexp_2wks,
      - num_6to12_attending_school,
      - main_roof_mat,
      - ppi_score_total,
      - b20_educlevel_ot,
      - e44_workdone_ot,
      - hh_size
    ) %>% 

  # move geographic identifiers to the beginning
  
  relocate(
    district,
    rur_urb,
  )

# export data -------------------------------------------------------------

saveRDS(uganda_listing_cleaned, here("data", "uganda", "uganda_listing_selected_vars_cleaned.rds"))
