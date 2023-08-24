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

df <- read_csv("data/police-reported-cannabis-offences-final.csv")

df <- df |> filter(region != "Canada")

before_after_means_charges <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  t_test_results <- t.test(x = df_after$total_charges, y = df_before$total_charges)
  
  return(t_test_results)
}

before_after_cohens_charges <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  cohens_results <- cohen.d(df_after$total_charges, df_before$total_charges)
  
  return(cohens_results)
}

output <- list()

for (i in unique(df$youth_adult)){
  
  for (j in unique(df$violation_type)){
    
    ttest_results <- before_after_means_charges(df, i, j)
    
    cohens_results <- before_after_cohens_charges(df, i, j)
    
    results_df <- tibble(
      youth_adult = i,
      violation_type = j,
      statistic = ttest_results$statistic,
      p_value = ttest_results$p.value,
      conf_interval_upper = ttest_results$conf.int[1],
      conf_interval_lower = ttest_results$conf.int[2],
      estimate_before = ttest_results$estimate[2],
      estimate_after = ttest_results$estimate[1],
      #mean_difference = ttest_results$estimate[1],
      method = ttest_results$method,
      cohens = cohens_results$estimate
    )
    
    output[[paste(i, j)]] <- results_df
    
  }
}

charges_results <- bind_rows(output)

results_charges_table <- charges_results |> 
  #select(-method) |> 
  relocate(method, .after = cohens) |> 
  mutate(across(statistic:cohens, round, 2)) |> 
  mutate(diff = round(estimate_after - estimate_before, 2),
         diff = paste(diff, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  #mutate(mean_difference = paste(mean_difference, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
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
  relocate(estimate_after, .after = estimate_before)

before_after_means_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
  
  t_test_results <- t.test(x = df_after$prop_charged, y = df_before$prop_charged)
  
  return(t_test_results)
}

before_after_cohens_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
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
      #ean_difference = ttest_results$estimate[1],
      method = ttest_results$method,
      cohens = cohens_results$estimate
    )
    
    output[[paste(i, j)]] <- results_df
    
  }
}

disposition_results <- bind_rows(output)

disposition_results_table <- disposition_results |> 
  #select(-method) |> 
  relocate(method, .after = cohens) |> 
  mutate(across(statistic:cohens, round, 2)) |> 
  mutate(diff = round(estimate_after - estimate_before, 2),
         diff = paste(diff, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  #mutate(mean_difference = paste(mean_difference, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
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
  relocate(estimate_after, .after = estimate_before)

table1 <- results_charges_table |> 
  mutate(violation_type = factor(violation_type, levels = c("Possession", "Drug impaired driving", "Trafficking", "Production and cultivation", "Importation-exportation", "Other")),
         youth_adult = factor(youth_adult, levels = c("Adult", "Youth"))) |>
  arrange(violation_type) |> 
  rename(`charge type` = "violation_type",
         `age group` = "youth_adult",
         `t-statistic` = "statistic",
         `p-value` = "p_value",
         `significance` = "p_value_sig",
         "estimate before" = "estimate_before",
         "estimate after" = "estimate_after",
         "difference [95% conf. interval]" = "diff",
         `cohen's d` = "cohens") |> 
  gt()

table2 <- disposition_results_table |> 
  mutate(violation_type = factor(violation_type, levels = c("Possession", "Drug impaired driving", "Trafficking", "Production and cultivation", "Importation-exportation", "Other")),
         youth_adult = factor(youth_adult, levels = c("Adult", "Youth"))) |>
  arrange(violation_type) |> 
  rename(`charge type` = "violation_type",
         `age group` = "youth_adult",
         `t-statistic` = "statistic",
         `p-value` = "p_value",
         `significance` = "p_value_sig",
         "estimate before" = "estimate_before",
         "estimate after" = "estimate_after",
         "difference [95% conf. interval]" = "diff",
         `cohen's d` = "cohens") |> 
  gt()

gtsave(table1, "tables/charges_table_welchs.png")
gtsave(table2, "tables/dispositions_table_welchs.png")

