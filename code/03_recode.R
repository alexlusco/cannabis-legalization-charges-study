###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to recode violations, age groups, correct
# handful of presumed incorrectly entered charge data, calculate year
# diff for 04_figures.R, and augment file with population estimates
# for adults (>=18 years) and youths (12-17)
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-cleaned.csv")

charges_to_drop <- c("Operation while impaired (alcohol)", 
                     "Operation while impaired causing bodily harm (alcohol)", 
                     "Operation while impaired (unspecified)",
                     "Operation while impaired causing bodily harm (unspecified)", 
                     "Operation while impaired causing death (alcohol)", 
                     "Operation while impaired causing death (unspecified)", 
                     "Failure or refusal to comply with demand (alcohol)", 
                     "Failure or refusal to comply with demand (unspecified)", 
                     "Failure or refusal to comply with demand, accident resulting in bodily harm (alcohol)", 
                     "Failure or refusal to comply with demand, accident resulting in bodily harm (unspecified)", 
                     "Failure or refusal to comply with demand, accident resulting in death (alcohol)", 
                     "Failure or refusal to comply with demand, accident resulting in death (unspecified)", 
                     "Operation while prohibited")

df <- df |> 
  filter(age_group != "Total" & gender != "Total") |>
  filter(!violation %in% charges_to_drop) |> 
  mutate(violation_type = case_when(
    # possession
    violation %in% c("Cannabis- Possession (Pre Legalization)", 
                     "Possession of illicit or over 30g dried cannabis (or equivalent) by adult", 
                     "Possession of over 5g dried cannabis (or equivalent) by youth") ~ "Possession",
    # trafficking (include possession by org?)
    violation %in% c("Cannabis- trafficking (Pre Legalization)", 
                     "Possession, sale, etc., for use in production of or trafficking in substance", 
                     "Possession of cannabis by organization", 
                     "Distribution of illicit, over 30g dried cannabis (or equivalent), or to an organization, by adult", 
                     "Distribution of cannabis to youth, by adult", 
                     "Distribution of illicit, over 30g dried cannabis (or equivalent), or to an organization, by adult", 
                     "Distribution of over 5g dried cannabis (or equivalent), or to an organization, by youth", 
                     "Distribution of cannabis by organization", "Possession of cannabis for purpose of distributing", 
                     "Sale of cannabis to adult", "Sale of cannabis to youth", "Sale of cannabis to an organization", 
                     "Possession of cannabis for purpose of selling") ~ "Trafficking",
    # import export
    violation %in% c("Cannabis- Importation and exportation (Pre Legalization)", 
                     "Importation and exportation of cannabis", 
                     "Possession of cannabis for purpose of exportation") ~ "Importation-exportation",
    # production and cultivation
    violation %in% c("Cannabis- Production (Pre Legalization)", 
                     "Possession of budding or flowering plants, or more than four cannabis plants", 
                     "Distribution of budding or flowering plants, or more than four cannabis plants", 
                     "Cultivate, propagate or harvest cannabis by adult", 
                     "Cultivate, propagate or harvest cannabis by youth or organization", 
                     "Possess, produce, sell, distribute or import anything for use in production or distribution of illicit cannabis", 
                     "Obtain, offer to obtain, alter or offer to alter cannabis", "Other Cannabis Act") ~ "Production and cultivation",
    # drug impaired driving
    violation %in% c("Operation while impaired (drugs)", "Operation- low blood drug concentration", "peration- low blood drug concentration", "Operation while impaired (alcohol and drugs)", "Operation while impaired causing bodily harm (drugs)", "Operation while impaired causing bodily harm (alcohol and drugs)", "Operation while impaired causing death (drugs)", "Operation while impaired causing death (alcohol and drugs)") ~ "Drug impaired driving",
    TRUE ~ "Other"
  ))

# add youth variable
df <- df |> 
  mutate(youth_adult = ifelse(age_group == "12 to 17", "Youth", "Adult"))

df <- df |> 
  mutate(youth_adult = case_when(
    violation == "Possession of over 5g dried cannabis (or equivalent) by youth" ~ "Youth", # overriding some incorrectly entered age group information: 67 charges for this youth offence labelled 28 to 24, 7 labelled 25 to 34, 2 labelled 35 to 110, 5  labelled 35 to 89. We presume the age field rather than the charge field to misentered, preferring to overestimate charges for youth than underestimate
    TRUE ~ youth_adult
  ))

# add population statistics for rate per 100,000 calculations
df <- df |> 
  mutate(population = case_when( # using 2021 population estimates for Canadians age 12-17 and 18+ from https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1710000501
    region == "Newfoundland and Labrador" & youth_adult == "Youth" ~ 31422,
    region == "Newfoundland and Labrador" & youth_adult == "Adult" ~ 436162,
    region == "Prince Edward Island" & youth_adult == "Youth" ~ 10973,
    region == "Prince Edward Island" & youth_adult == "Adult" ~ 134763,
    region == "Nova Scotia" & youth_adult == "Youth" ~ 57999,
    region == "Nova Scotia" & youth_adult == "Adult" ~ 825690,
    region == "New Brunswick" & youth_adult == "Youth" ~ 48486,
    region == "New Brunswick" & youth_adult == "Adult" ~ 654634,
    region == "Quebec" & youth_adult == "Youth" ~ 530147,
    region == "Quebec" & youth_adult == "Adult" ~ 6998140,
    region == "Ontario" & youth_adult == "Youth" ~ 961924,
    region == "Ontario" & youth_adult == "Adult" ~ 12059243,
    region == "Manitoba" & youth_adult == "Youth" ~ 101605,
    region == "Manitoba" & youth_adult == "Adult" ~ 1081274,
    region == "Saskatchewan" & youth_adult == "Youth" ~ 89410,
    region == "Saskatchewan" & youth_adult == "Adult" ~ 908718,
    region == "Alberta" & youth_adult == "Youth" ~ 322691,
    region == "Alberta" & youth_adult == "Adult" ~ 3470048,
    region == "British Columbia" & youth_adult == "Youth" ~ 303652,
    region == "British Columbia" & youth_adult == "Adult" ~ 4329311,
    region == "Yukon" & youth_adult == "Youth" ~ 2621,
    region == "Yukon" & youth_adult == "Adult" ~ 34823,
    region == "Northwest Territories" & youth_adult == "Youth" ~ 3519,
    region == "Northwest Territories" & youth_adult == "Adult" ~ 34904,
    region == "Nunavut" & youth_adult == "Youth" ~ 4444,
    region == "Nunavut" & youth_adult == "Adult" ~ 25141,
    region == "Canada" & youth_adult == "Youth" ~ 2468893,
    region == "Canada" & youth_adult == "Adult" ~ 30992851,
  ))

# add variable with year_diff info for charts
df <- df |> 
  mutate(year_diff = year - 2019)

# write result as csv
write_csv(df, "data/police-reported-cannabis-offences-final.csv")