# header ------------------------------------------------------------------

# Authors: Christine Pu (cjpu@stanford.edu), Hadassah Betapudi
# Date Created: July 8, 2022
# Purpose: Cleans raw, primary data collected by Stanford University and the Kwame Nkrumah University of Science and Technology (KNUST) from January to February 2022 in Ghana
# Inputs: ghana_listing_selected_vars.rds (raw data collected by Stanford and KNUST, which can be accessed here: https://osf.io/h2swb/, DOI 10.17605/OSF.IO/H2SWB), ghana_ppi_1.90_probabilities.csv (scorecard from the Poverty Probability Index which can be accessed here: https://www.povertyindex.org/country/ghana)
# Outputs: ghana_listing_selected_vars_cleaned.rds (a cleaned dataset)

# load packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(here)

# import data -------------------------------------------------------------

ghana_listing_selected_vars_raw <- readRDS(here("data", "ghana", "ghana_listing_selected_vars.rds")) %>% 
  filter(
    is.na(wi_urbrrl) == FALSE 
  )

# ppi scores index
ghana_ppi_1.90_index <- read.csv(here("data", "ghana", "ghana_ppi_1.90_probabilities.csv")) %>% 
  mutate(
    score = as.double(score)
  )

# reclassify responses based on NAs and "other" responses----------------------------------------------------

# reclassify "-99", 99" and "99999" as NA for regexp_4wks
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$regexp_4wks == -99 | ghana_listing_selected_vars_raw$regexp_4wks == 99 | ghana_listing_selected_vars_raw$regexp_4wks == 99999, ]$regexp_4wks <- NA

# assign value of NA to "n/a" values in hh_member_6_11yrs
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$hh_member_6_11yrs == "n/a", ]$hh_member_6_11yrs <- NA

# assign value of NA for "num_children_primary" if the response to hh_member_6_11yrs is 0 or NA or if num_children_primary has value of "" (blank) or if num_children_primary is "n/a"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$hh_member_6_11yrs == "0" | is.na(ghana_listing_selected_vars_raw$hh_member_6_11yrs) == TRUE | ghana_listing_selected_vars_raw$num_children_primary == "" | ghana_listing_selected_vars_raw$num_children_primary == "n/a", ]$num_children_primary <- NA

# reclassify "other" responses for "oth_cook_fuel_type" 
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "Electric stove", ]$cook_fuel_type <- "electricity"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "He doesn't cook always buy food from outside.", ]$cook_fuel_type <- "no_food_cooked"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "Rice Cooker", ]$cook_fuel_type <- "electricity"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "Rice cooker", ]$cook_fuel_type <- "electricity"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "She eats  from  outside", ]$cook_fuel_type <- "no_food_cooked"
ghana_listing_selected_vars_raw[ghana_listing_selected_vars_raw$oth_cook_fuel_type == "buying food outside", ]$cook_fuel_type <- "no_food_cooked"

# create new / modify existing variables ----------------------------------

ghana_listing_cleaned_premerge <- 
  ghana_listing_selected_vars_raw %>% 
  
  # filter for eligible and consenting respondents
  
  filter(
    resp_livin_fulltime == "yes",
    yesno_consent1 == "yes", 
    able_answer_quest == "yes", 
    is.na(res_sign) == FALSE | is.na(checkbox) == FALSE, 
    yesno_consent3 == "yes",
    relate_hh != "friend" & relate_hh != "househelp",
    other_relate_hh != "Tenant" & other_relate_hh != "Tenants" 
    
    ) %>%

  # rename variables to be consistent with other countries
  
  rename(
    enough_food = freq_hh_had_food,
    district = districts
  ) %>% 
  
  mutate(
    
    # recode rur_urb variable
    
    rur_urb = case_when(
      urban == "1" ~ "urban", 
      urban == "0" ~ "rural"
    ), 
    
    # convert variables into the "double" type
    
    regexp_4wks = as.double(regexp_4wks),
    hh_size = as.double(num_hh_members),
    num_hh_6to11 = as.double(hh_member_6_11yrs),
    num_6to11_attending_school = as.double(num_children_primary),
    
    # regular expenditures -- calculate regular expenditures per capita per day (based on airtime, food, and transport expenditures in the last 4 weeks)
    
    regexppcpd = regexp_4wks / hh_size / 30, 
    
    # krishna -- create binary variable for roof with iron sheets and other finished materials
    
    roof_finished_mat = ifelse(
      is.na(main_roof_mat) == TRUE, NA, ifelse( 
        main_roof_mat == "Finished_roof", 1 , 0
      )
    ),
    
    # krishna -- create a variable representing whether all children between 6-11 are currently attending primary school
    
    all_attend_prim_school = ifelse(
      is.na(num_hh_6to11) == TRUE, NA, ifelse( 
        num_hh_6to11 == 0, NA, ifelse( 
          is.na(num_6to11_attending_school) == TRUE, NA, ifelse( 
            num_hh_6to11 <= num_6to11_attending_school, 1, 0 
            )
          )
        )
      ),
    
    # krishna -- create binary variable to represent Krishna's stages of progress (food, clothing, attending primary school, finished roof)
    
    poor_krishna = ifelse(
      
    # assign poverty status = 0 for households that meet all stages of progress with at least 1 child between 6 to 11 years of age
      
      is.na(enough_food) == FALSE & 
        enough_food == "always" &
        is.na(enough_clothing) == FALSE &
        enough_clothing  == "always" &
        is.na(num_hh_6to11) == FALSE & 
        num_hh_6to11 != 0 &
        is.na(all_attend_prim_school) == FALSE & 
        all_attend_prim_school == 1 &
        is.na(roof_finished_mat) == FALSE & 
        roof_finished_mat == 1, 0, 
      
    # assign poverty status for households with no children between 6 to 11 years of age but meet all other criteria
      
      ifelse(
        is.na(enough_food) == FALSE & 
          enough_food == "always" &
          is.na(enough_clothing) == FALSE &
          enough_clothing  == "always" &
          is.na(num_hh_6to11) == FALSE & 
          num_hh_6to11 == 0 &
          is.na(roof_finished_mat) == FALSE &
          roof_finished_mat == 1, 0, 
        
    # assign NA to households with incomplete information
        
        ifelse(
          is.na(enough_food) == TRUE |
            is.na(enough_clothing) == TRUE | 
            is.na(num_hh_6to11) == TRUE | 
            num_hh_6to11 != 0 & is.na(num_6to11_attending_school) == TRUE |
            is.na(roof_finished_mat) == TRUE, NA, 1 
          ) 
        )
      ),

  # ppi_1.90 -- create ppi score for "in which region does the household live?"

  ppi_score_region = ifelse(
    is.na(region) == TRUE, NA, ifelse( 
      region == "Western", 27, ifelse(
        region == "Ahafo", 12, "error" 
      )
    )
  ),
  
  # ppi_1.90 - create ppi score for "How many members are there in the household?" 

  ppi_score_hhmem = ifelse(
    is.na(hh_size) == TRUE, NA, ifelse(
      hh_size == 1 | hh_size == 2 | hh_size == 3 | hh_size == 4, 13, ifelse(
        hh_size > 4, 0, "error" 
        )
      )
    ),

  # ppi_1.90 - create ppi score for "In the past month, have you purchased any chicken eggs (fresh or single)?" 
  
  ppi_score_eggs = ifelse(
    is.na(buy_chicken_egg) == TRUE, NA, ifelse( 
      buy_chicken_egg == "yes", 7, ifelse(
        buy_chicken_egg == "no", 0, "error" 
      )
    )
  ),

  # ppi_1.90 - create ppi score for "In the past month, have you purchased any raw or corned beef?"
  
  ppi_score_beef = ifelse(
    buy_beef == "dk_refuse", NA, ifelse( 
      buy_beef == "yes", 7, ifelse(
        buy_beef == "no", 0, "error" 
      )
    )
  ),

  # ppi_1.90 - create ppi score for "What is the main construction material used for the outer wall?" 
  
  ppi_score_wall = ifelse(
    is.na(main_wall_mat) == TRUE, NA, 0 
  ),
  
  # ppi_1.90 - create ppi score for "What is the main fuel used by the household for cooking?" 
  
  ppi_score_fuel = ifelse(
    is.na(cook_fuel_type) == TRUE, NA, ifelse( 
      cook_fuel_type == "Wood", 0, ifelse(
        cook_fuel_type == "Charcoal" | cook_fuel_type == "Cooking_fuel" | cook_fuel_type == "LPG" | cook_fuel_type == "natural_gas" | cook_fuel_type == "no_food_cooked" | cook_fuel_type == "electricity", 11, "error" 
      )
    )
  ),

  # ppi_1.90 - create ppi score for "Does any member of the household own a gas stove?" 
  
  ppi_score_gasstove = ifelse(
    gas_stove == "dk", NA, ifelse( 
      gas_stove == "yes", 2, ifelse(
        gas_stove == "no", 0, "error" 
        )
      )
    ),

  # ppi_1.90 - create ppi score for "Does any member of the household own a refrigerator?" 
  
  ppi_score_refrigerator = ifelse(
    refrigerator == "dk", NA, ifelse( 
      refrigerator == "yes", 2, ifelse(
        refrigerator == "no", 0, "error" 
      )
    )
  ),

  # ppi_1.90 - create ppi score for "Does any member of the household own a fan?" 
  
  ppi_score_fan = ifelse(
    fan == "dk", NA, ifelse(
      fan == "yes", 2, ifelse(
        fan == "no", 0, "error" 
      )
    )
  ),

  # ppi_.190 - create ppi score for "Does any member of the household own a television?" 
  ppi_score_tv = ifelse(
    tv == "dk", NA, ifelse( 
      tv == "yes", 4, ifelse(
        tv == "no", 0, "error" 
        )
      )
    ),

  # calculate total ppi score
  
  ppi_score_total = 
    as.double(ppi_score_region) + 
    as.double(ppi_score_hhmem) +
    as.double(ppi_score_eggs) +
    as.double(ppi_score_beef) +
    as.double(ppi_score_wall) +
    as.double(ppi_score_fuel) +
    as.double(ppi_score_gasstove) +
    as.double(ppi_score_refrigerator) +
    as.double(ppi_score_fan) +
    as.double(ppi_score_tv)
  ) %>% 
  
  # create and recode variables for household demographics and livelihood characteristics
  
  mutate(
    
    # create and recode variables for head of household attending education (yes = 1)
    
    hoh_attended_school = 
      ifelse(yesno_hoh_educ == "yes", 1, 
             ifelse(yesno_hoh_educ == "no", 0, NA)),
    
    # create and recode variable for head of household completing primary school (yes = 1)
    
    hoh_completed_prim_school = 
      ifelse(yesno_hoh_educ == "dk_refuse",NA,
             ifelse(yesno_hoh_educ == "yes" & 
                      ((edu_level == "jhs_middle" | 
                          edu_level == "shs" | 
                          edu_level == "tertiary") | 
                         (edu_level == "primary" & 
                            primary_level == "grade_6")), 1,
                    ifelse(yesno_hoh_educ == "yes" &
                             ((edu_level == "other" &
                                other_edu_level == "A level") |
                                (edu_level == "other" &
                                   other_edu_level == "Advance Diploma") |
                                (edu_level == "other" &
                                   other_edu_level == "Commercial college") |
                                (edu_level == "other" &
                                   other_edu_level == "DIPLOMA IN NURSING") |
                                (edu_level == "other" &
                                   other_edu_level == "Diploma") |
                                (edu_level == "other" &
                                   other_edu_level == "Diploma in  Basic Education") |
                                (edu_level == "other" &
                                   other_edu_level == "Diploma in Basic Education") |
                                (edu_level == "other" &
                                   other_edu_level == "Diploma in Nursing") |
                                (edu_level == "other" &
                                   other_edu_level == "He completed o level") |
                                (edu_level == "other" &
                                   other_edu_level == "Intermediate certificate in Mines Engineering") |
                                (edu_level == "other" &
                                other_edu_level == "Middle  school certificate") |
                                (edu_level == "other" &
                                   other_edu_level == "Middle school certificate") |
                                (edu_level == "other" &
                                   other_edu_level == "Middle school certificate.") |
                                (edu_level == "other" &
                                   other_edu_level == "Nursing Certificate") |
                                (edu_level == "other" &
                                   other_edu_level == "O level") |
                                (edu_level == "other" &
                                   other_edu_level == "O' Level") |
                                (edu_level == "other" &
                                   other_edu_level == "Standard 7") |
                                (edu_level == "other" &
                                   other_edu_level == "Tertiary") |
                                (edu_level == "other" &
                                   other_edu_level == "Vocational  school") |
                                (edu_level == "other" &
                                   other_edu_level == "Vocational School")), 1, 0
                           )
                    )
             ),
    
    # create and recode variable for female head of household (yes = 1)
    
    fem_hoh = 
      ifelse(female_hoh == "yes", 1, 
           ifelse(female_hoh == "no", 0, NA)),
    
    # create a variable that computes the dependency ratio (# of hh members outside of 15-64 : # of hh members aged 15-64)
    
    # compute numerator (# of individuals above 64 and below 15)
    
    dependency_ratio_numerator =
      (as.double(hh_member_64yrs) + as.double(hh_member_15yrs)),
    
    # compute denominator (# of individuals in the 15 to 64 range)
    
    dependency_ratio_denominator =
      (as.double(num_hh_members)-as.double(hh_member_64yrs)-as.double(hh_member_15yrs)),
    
    # compute actual ratio (# of individuals above 64 and below 15 / # individuals in the 15 to 64 range)
    
    dependency_ratio = dependency_ratio_numerator / dependency_ratio_denominator,
    
    # if the dependency ratio is negative, assign a "NA" value
    
    dependency_ratio = if_else(
      dependency_ratio < 0, NA_real_, dependency_ratio
      ),
    
    # if there are no adults in the 15 to 64 range, then assign a "NA" value
    
    dependency_ratio = if_else(
      dependency_ratio_denominator == 0, NA_real_, dependency_ratio
      ),
    
    # recode variable for households growing crops (1 = yes)
    
    grew_crops = 
      ifelse(hh_grow_crop_sell_eat == "yes", 1, 
             ifelse(hh_grow_crop_sell_eat == "no", 0, NA)),
    
    # recode variable for households earning non-farm income (1 = yes)
    
    earned_nonfarm =
      ifelse(hh_income_non_farm == "yes", 1, 
             ifelse(hh_income_non_farm == "no", 0, NA)),
    
    # recode variable for households receiving remittances from friends and family (1 = yes)
    
    remittances_friends_family =
      ifelse(hh_recieve_money == "yes", 1, 
             ifelse(hh_recieve_money == "no", 0, NA))
  ) %>%

  # remove variables that have been converted and recoded

  select(
    - resp_livin_fulltime,
    - yesno_consent1,
    - able_answer_quest,
    - res_sign,
    - checkbox,
    - yesno_consent3,
    - relate_hh,
    - other_relate_hh,
    - urban,
    - num_hh_members,
    - oth_enough_clothing,
    - buy_chicken_egg,
    - buy_beef,
    - main_wall_mat,
    - cook_fuel_type, 
    - oth_cook_fuel_type,
    - gas_stove,
    - refrigerator,
    - fan,
    - tv,
    - ppi_score_region,
    - ppi_score_hhmem,
    - ppi_score_eggs, 
    - ppi_score_beef, 
    - ppi_score_wall,
    - ppi_score_fuel,
    - ppi_score_gasstove,
    - ppi_score_refrigerator,
    - ppi_score_fan,
    - ppi_score_tv,
    - hh_member_6_11yrs,
    - num_children_primary,
    - yesno_hoh_educ,
    - female_hoh,
    - edu_level,
    - other_edu_level,
    - primary_level,
    - hh_member_64yrs,
    - hh_member_15yrs,
    - dependency_ratio_numerator,
    - dependency_ratio_denominator,
    - hh_grow_crop_sell_eat,
    - hh_income_non_farm,
    - hh_recieve_money,
    - oth_hh_recieve_money,
    - main_roof_mat,
    - regexp_4wks,
    - hh_size,
    - region
  ) %>% 

  # convert krishna into a factor
  
  mutate(
    poor_krishna = as.factor(poor_krishna)
  ) %>% 
  
  # move geographic identifiers to the beginning
  
  relocate(
    district,
    rur_urb
    )
  
# merge ppi probabilities -------------------------------------------------

ghana_listing_cleaned <- left_join(
  ghana_listing_cleaned_premerge,
  ghana_ppi_1.90_index,
  by = c("ppi_score_total" = "score")
) %>% 
  rename(
    ppi_prob_1.90 = probability 
  ) %>% 
  select(
    -ppi_score_total
  )

# remove "%" from the merged probabilities and convert to a double

ghana_listing_cleaned$ppi_prob_1.90 = substr(ghana_listing_cleaned$ppi_prob_1.90, 1, nchar(ghana_listing_cleaned$ppi_prob_1.90)-1)
ghana_listing_cleaned$ppi_prob_1.90 = as.double(ghana_listing_cleaned$ppi_prob_1.90)

# export data -------------------------------------------------------------

saveRDS(ghana_listing_cleaned, here("data", "ghana", "ghana_listing_selected_vars_cleaned.rds"))
