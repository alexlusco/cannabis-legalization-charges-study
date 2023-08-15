###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to run robustness checks on analyses conducted in
# 04_analysis.R
# by Statistics Canada
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-cleaned.csv")

charges_to_drop <- c("Operation while impaired (alcohol)", 
                     "Operation while impaired causing bodily harm (alcohol)", 
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
  filter(age_group != "Total" & gender != "Total" & region != "Canada") |> # regional rather than national level data
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
                     "Cultivate, propagate or harvest cannabis by adult", "Cultivate, propagate or harvest cannabis by youth or organization", 
                     "Possess, produce, sell, distribute or import anything for use in production or distribution of illicit cannabis", 
                     "Obtain, offer to obtain, alter or offer to alter cannabis", "Other Cannabis Act") ~ "Production and cultivation",
    # drug impaired driving
    violation %in% c("Operation while impaired (drugs)", 
                     "Operation while impaired (unspecified)", 
                     "Operation- low blood drug concentration", 
                     "peration- low blood drug concentration", # intentional - typo in original data file
                     "Operation while impaired (alcohol and drugs)", 
                     "Operation while impaired causing bodily harm (drugs)", 
                     "Operation while impaired causing bodily harm (alcohol and drugs)", 
                     "Operation while impaired causing death (drugs)", 
                     "Operation while impaired causing death (alcohol and drugs)") ~ "Drug impaired driving",
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
    region == "Nunavut" & youth_adult == "Adult" ~ 25141
  ))
      
before_after_means_charges <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    #filter(region == prov_terr) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> filter(year < 2019) |> reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region))
  df_after <- df_filtered |> filter(year >= 2019) |> reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region))
  #df_before <- df_filtered |> filter(year < 2019) |> reframe(total_charges = sum(total, na.rm = TRUE), .by = year)
  #df_after <- df_filtered |> filter(year >= 2019) |> reframe(total_charges = sum(total, na.rm = TRUE), .by = year)
  
  
  t_test_results <- t.test(x = df_after$total_charges, y = df_before$total_charges)
  
  return(t_test_results)
}

before_after_cohens_charges <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    #filter(region == prov_terr) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> filter(year < 2019) |> reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region))
  df_after <- df_filtered |> filter(year >= 2019) |> reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region))
  #df_before <- df_filtered |> filter(year < 2019) |> reframe(total_charges = sum(total, na.rm = TRUE), .by = year)
  #df_after <- df_filtered |> filter(year >= 2019) |> reframe(total_charges = sum(total, na.rm = TRUE), .by = year)
  
  
  cohens_results <- cohen.d(df_after$total_charges, df_before$total_charges)
  
  return(cohens_results)
}

output <- list()

for (i in unique(df$youth_adult)){
  
  #for (m in unique(df$region)){
  
  for (j in unique(df$violation_type)){
    
    ttest_results <- before_after_means_charges(df, i, j)
    
    cohens_results <- before_after_cohens_charges(df, i, j)
    
    results_df <- tibble(
      youth_adult = i,
      violation_type = j,
      #region = m,
      statistic = ttest_results$statistic,
      p_value = ttest_results$p.value,
      conf_interval_upper = ttest_results$conf.int[1],
      conf_interval_lower = ttest_results$conf.int[2],
      estimate_before = ttest_results$estimate[2],
      estimate_after = ttest_results$estimate[1],
      method = ttest_results$method,
      cohens = cohens_results$estimate
    )
    
    output[[paste(i, j)]] <- results_df
  #}
  }
}

charges_results <- bind_rows(output)

results_charges_table <- charges_results |> 
  select(-method) |> 
  mutate(across(statistic:cohens, round, 2)) |> 
  mutate(diff = round(estimate_after - estimate_before, 2),
         diff = paste(diff, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  select(-conf_interval_lower, -conf_interval_upper) |> 
  mutate(cohens = case_when(
    abs(cohens) >= 0.2 & abs(cohens) < 0.5 ~ paste(cohens, "(small)"),
    abs(cohens) >= 0.5 & abs(cohens) < 0.8 ~ paste(cohens, "(medium)"),
    abs(cohens) >= 0.8 ~ paste(cohens, "(large)"),
    TRUE ~ as.character(cohens)
  )) |> 
  mutate(p_value_sig = case_when(
    p_value <= 0.00 ~ "***",
    p_value > 0.00 & p_value <= 0.01 ~ "**",
    p_value > 0.01 & p_value <= 0.05 ~ "*",
    p_value > 0.05 & p_value <= 0.10 ~ "†",
    TRUE ~ "-"
  )) |> 
  relocate(diff, .before = cohens) |> 
  relocate(p_value_sig, .after = p_value) |> 
  relocate(violation_type, .before = youth_adult) |> 
  #relocate(region, .before = violation_type) |> 
  relocate(estimate_after, .after = estimate_before)

before_after_means_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2019) |> 
    summarize(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    summarize(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region))
  
  t_test_results <- t.test(df_after$prop_charged, df_before$prop_charged)
  
  return(t_test_results)
}

before_after_cohens_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2019) |> 
    summarize(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    summarize(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region))
  
  cohens_results <- cohen.d(df_after$prop_charged, df_before$prop_charged)
  
  return(cohens_results)
}

output <- list()

for (i in unique(df$youth_adult)){
  
  for (j in unique(df$violation_type)){
    
    ttest_results <- before_after_means_disposition(df, i, j)
    
    cohens_results <- before_after_cohens_disposition(df, i, j)
    
    results_df <- tibble(
      youth_adult = i,
      violation_type = j,
      statistic = ttest_results$statistic,
      p_value = ttest_results$p.value,
      conf_interval_upper = ttest_results$conf.int[1],
      conf_interval_lower = ttest_results$conf.int[2],
      estimate_before = ttest_results$estimate[2],
      estimate_after = ttest_results$estimate[1],
      method = ttest_results$method,
      cohens = cohens_results$estimate
    )
    
    output[[paste(i, j)]] <- results_df
    
  }
}

disposition_results <- bind_rows(output)

disposition_results_table <- disposition_results |> 
  select(-method) |> 
  mutate(across(statistic:cohens, round, 2)) |> 
  mutate(diff = round(estimate_after - estimate_before, 2),
         diff = paste(diff, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  select(-conf_interval_lower, -conf_interval_upper) |> 
  mutate(cohens = case_when(
    abs(cohens) >= 0.2 & abs(cohens) < 5.0 ~ paste(cohens, "(small)"),
    abs(cohens) >= 0.5 & abs(cohens) < 8.0 ~ paste(cohens, "(medium)"),
    abs(cohens) >= 0.8 ~ paste(cohens, "(large)"),
    TRUE ~ as.character(cohens)
  )) |> 
  mutate(p_value_sig = case_when(
    p_value <= 0.00 ~ "***",
    p_value > 0.00 & p_value <= 0.01 ~ "**",
    p_value > 0.01 & p_value <= 0.05 ~ "*",
    p_value > 0.05 & p_value <= 0.10 ~ "†",
    TRUE ~ "-"
  )) |> 
  relocate(diff, .before = cohens) |> 
  relocate(p_value_sig, .after = p_value) |> 
  relocate(violation_type, .before = youth_adult) |> 
  relocate(estimate_after, .after = estimate_before)

results_charges_table |> 
  arrange(violation_type) |> 
  #clipr::write_clip() |> 
  group_by(violation_type) |> 
  gt()

disposition_results_table |> 
  arrange(violation_type) |> 
  #clipr::write_clip() |> 
  group_by(violation_type) |> 
  gt()
