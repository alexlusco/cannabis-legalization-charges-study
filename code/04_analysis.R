###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to compute before-after t statistics, 
# and cohens d on total charges per 100,000 population and 
# proportion of charges cleared by charge vs diversion / other 
# means
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-final.csv")

df <- df |> filter(region != "Canada")

ttest_charges <- function(dat, group, charge_type) {
  
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
  
  t_test_results <- t.test(x = df_after$total_charges, y = df_before$total_charges, paired = TRUE)
  
  return(t_test_results)
}

cohens_charges <- function(dat, group, charge_type) {
  
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

before_after_means_charges <- function(dat) {
  
  out <- dat |> 
    filter(year != 2018) |> 
    mutate(period = case_when(
      year < 2018 ~ "estimate before",
      year >= 2019 ~ "estimate after",
    ))
  
  out <- out |> 
    reframe(charge_rate = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region, period, youth_adult, violation_type)) |>
    distinct(year, region, period, charge_rate, youth_adult, violation_type, .keep_all = TRUE)
  
  out <- out |> 
    summarize(charge_rate = mean(charge_rate, na.rm = TRUE), .by = c(period, youth_adult, violation_type)) |> 
    mutate(violation_type = factor(violation_type),
           youth_adult = factor(youth_adult)) |> 
    arrange(violation_type, youth_adult)
  
  out <- out |> 
    group_by(youth_adult, violation_type) |> 
    #mutate(diff = charge_rate[1] - charge_rate[2]) |> 
    ungroup() |> 
    spread(period, charge_rate) |> 
    relocate("estimate after", .after = "estimate before")
  
  return(out)
  
}

output <- list()

for (i in unique(df$youth_adult)){
  
  for (j in unique(df$violation_type)){
    
    ttest_results <- ttest_charges(df, i, j)
    
    cohens_results <- cohens_charges(df, i, j)
    
    results_df <- tibble(
      youth_adult = i,
      violation_type = j,
      statistic = ttest_results$statistic,
      p_value = ttest_results$p.value,
      conf_interval_upper = ttest_results$conf.int[1],
      conf_interval_lower = ttest_results$conf.int[2],
      #estimate_before = ttest_results$estimate[2],
      #estimate_after = ttest_results$estimate[1],
      mean_difference = ttest_results$estimate[1],
      method = ttest_results$method,
      cohens = cohens_results$estimate
    )
    
    output[[paste(i, j)]] <- results_df
    
  }
}

missing_means <- before_after_means_charges(df)

charges_results <- bind_rows(output) |> left_join(missing_means, by = c("youth_adult", "violation_type"))

results_charges_table <- charges_results |> 
  select(-method) |> 
  #relocate(method, .after = cohens) |> 
  mutate(across(c(statistic:`estimate after`, -p_value), round, 2)) |>
  mutate(p_value = round(p_value, 3)) |> 
  #mutate(diff = round(estimate_after - estimate_before, 2),
  #       diff = paste(diff, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  mutate(mean_difference = paste(mean_difference, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
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
  #relocate(diff, .before = cohens) |> 
  relocate(p_value_sig, .after = p_value) |> 
  relocate(violation_type, .before = youth_adult) |> 
  relocate(`estimate before`, .before = mean_difference) |> 
  relocate(`estimate after`, .after = `estimate before`)
  #relocate(estimate_after, .after = estimate_before)

ttest_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  t_test_results <- t.test(x = df_after$prop_charged, y = df_before$prop_charged)
  
  return(t_test_results)
}

cohens_disposition <- function(dat, group, charge_type) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group) |> 
    filter(violation_type == charge_type)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  cohens_results <- cohen.d(df_after$prop_charged, df_before$prop_charged)
  
  return(cohens_results)
}

output <- list()

for (i in unique(df$youth_adult)){
  
  for (j in unique(df$violation_type)){
    
    ttest_results <- ttest_disposition(df, i, j)
    
    cohens_results <- cohens_disposition(df, i, j)
    
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

disposition_results <- bind_rows(output)

disposition_results_table <- disposition_results |> 
  #select(-method) |> 
  relocate(method, .after = cohens) |> 
  mutate(across(c(statistic:cohens, -p_value), round, 2)) |>
  mutate(p_value = round(p_value, 3)) |> 
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
  relocate(violation_type, .before = youth_adult)
  #relocate(estimate_after, .after = estimate_before)

table1 <- results_charges_table |> 
  #select(-method) |> 
  mutate(violation_type = factor(violation_type, levels = c("Possession", "Drug impaired driving", "Trafficking", "Production and cultivation", "Importation-exportation", "Other")),
         youth_adult = factor(youth_adult, levels = c("Adult", "Youth"))) |>
  arrange(violation_type) |> 
  rename(`charge type` = "violation_type",
         `age group` = "youth_adult",
         `t-statistic` = "statistic",
         `p-value` = "p_value",
         `significance` = "p_value_sig",
         #"estimate before" = "estimate_before",
         #"estimate after" = "estimate_after",
         "difference [95% conf. interval]" = "mean_difference",
         `cohen's d` = "cohens") |> 
  group_by(`charge type`) |> 
  gt() |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

table2 <- disposition_results_table |> 
  select(-method) |> 
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
  group_by(`charge type`) |> 
  gt() |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gtsave(table1, "tables/charges_table_paired.png")
gtsave(table2, "tables/dispositions_table_welchs.png")

#####################################################

ttest_all_charges <- function(dat, group) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region)) |> 
    distinct(year, region, total_charges, .keep_all = TRUE)
  
  t_test_results <- t.test(x = df_after$total_charges, y = df_before$total_charges, paired = TRUE)
  
  return(t_test_results)
}

cohens_all_charges <- function(dat, group) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group)
  
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

before_after_means_all_charges <- function(dat) {
  
  out <- dat |> 
    filter(year != 2018) |> 
    mutate(period = case_when(
      year < 2018 ~ "estimate before",
      year >= 2019 ~ "estimate after",
    ))
  
  out <- out |> 
    reframe(charge_rate = sum(total, na.rm = TRUE)/population*100000, .by = c(year, region, period, youth_adult)) |>
    distinct(year, region, period, charge_rate, youth_adult, .keep_all = TRUE)
  
  out <- out |> 
    summarize(charge_rate = mean(charge_rate, na.rm = TRUE), .by = c(period, youth_adult)) |> 
    mutate(youth_adult = factor(youth_adult)) |> 
    arrange(youth_adult)
  
  out <- out |> 
    group_by(youth_adult) |> 
    #mutate(diff = charge_rate[1] - charge_rate[2]) |> 
    ungroup() |> 
    spread(period, charge_rate) |> 
    relocate("estimate after", .after = "estimate before")
  
  return(out)
  
}

ttest_all_disposition <- function(dat, group) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group)
  
  df_before <- df_filtered |> 
    filter(year < 2018) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  df_after <- df_filtered |> 
    filter(year >= 2019) |> 
    reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(year, region)) |> 
    distinct(year, region, prop_charged, .keep_all = TRUE) |> 
    filter(!is.na(prop_charged))
    #mutate(prop_charged = ifelse(is.na(prop_charged), 0, prop_charged))
  
  t_test_results <- t.test(x = df_after$prop_charged, y = df_before$prop_charged)
  
  return(t_test_results)
}

cohens_all_disposition <- function(dat, group) {
  
  df_filtered <- dat |> 
    filter(youth_adult == group)
  
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
  
  ttest_results1 <- ttest_all_charges(df, i)
  
  cohens_results1 <- cohens_all_charges(df, i)
  
  ttest_results2 <- ttest_all_disposition(df, i)
  
  cohens_results2 <- cohens_all_disposition(df, i)
  
  results_df1 <- tibble(
    outcome = "Charges",
    youth_adult = i,
    statistic = ttest_results1$statistic,
    p_value = ttest_results1$p.value,
    conf_interval_upper = ttest_results1$conf.int[1],
    conf_interval_lower = ttest_results1$conf.int[2],
    #estimate_before = ttest_results1$estimate[2],
    #estimate_after = ttest_results1$estimate[1],
    mean_difference = ttest_results1$estimate[1],
    #method = ttest_results1$method,
    cohens = cohens_results1$estimate
  )
  
  results_df2 <- tibble(
    outcome = "Punitiveness",
    youth_adult = i,
    statistic = ttest_results2$statistic,
    p_value = ttest_results2$p.value,
    conf_interval_upper = ttest_results2$conf.int[1],
    conf_interval_lower = ttest_results2$conf.int[2],
    estimate_before = ttest_results2$estimate[2],
    estimate_after = ttest_results2$estimate[1],
    #mean_difference = ttest_results2$estimate[1],
    #method = ttest_results2$method,
    cohens = cohens_results2$estimate
  )
  
  output[[i]] <- list(results_df1, results_df2)
  
}

missing_means_all_charges <- before_after_means_all_charges(df)

charge_output_youth <- output$Youth[1]
charge_output_adult <- output$Adult[1]
charge_output <- bind_rows(charge_output_youth, charge_output_adult) |> left_join(missing_means_all_charges, by = c("youth_adult"))

charge_output_for_table <- charge_output |> 
  mutate(across(c(statistic:`estimate after`, -p_value), round, 2)) |> 
  mutate(mean_difference = paste(mean_difference, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
  select(-conf_interval_lower, -conf_interval_upper) |> 
  mutate(cohens = case_when(
    abs(cohens) >= 0.2 & abs(cohens) < 0.5 ~ paste(cohens, "(small)"),
    abs(cohens) >= 0.5 & abs(cohens) < 0.8 ~ paste(cohens, "(medium)"),
    abs(cohens) >= 0.8 ~ paste(cohens, "(large)"),
    TRUE ~ as.character(cohens)
  )) |> 
  mutate(p_value_sig = case_when(
    round(p_value, 3) <= 0.00 ~ "***",
    round(p_value, 3) > 0.00 & round(p_value, 3) <= 0.01 ~ "**",
    round(p_value, 3) > 0.01 & round(p_value, 3) <= 0.05 ~ "*",
    round(p_value, 3) > 0.05 & round(p_value, 3) <= 0.10 ~ "†",
    TRUE ~ "-"
  )) |> 
  mutate(p_value = ifelse(p_value_sig == "***", "0.000", p_value)) |> # replace unnecessary scientific notation for final table, rounding to 3 decimal points
  mutate(outcome = factor(outcome, levels = c("Charges", "Punitiveness")),
         youth_adult = factor(youth_adult, levels = c("Adult", "Youth"))) |>
  arrange(outcome) |> 
  relocate(`estimate before`, .before = mean_difference) |> 
  relocate(`estimate after`, .after = `estimate before`) |> 
  rename(`age group` = "youth_adult",
         `t-statistic` = "statistic",
         `p-value` = "p_value",
         `significance` = "p_value_sig",
         #"estimate before" = "estimate_before",
         #"estimate after" = "estimate_after",
         "difference [95% conf. interval]" = "mean_difference",
         `cohen's d` = "cohens") |> 
  relocate(`significance`, .after = `p-value`)

disposition_output_youth <- output$Youth[2]
disposition_output_adult <- output$Adult[2]
disposition_output <- bind_rows(disposition_output_youth, disposition_output_adult)

disposition_output_for_table <- disposition_output |> 
  mutate(across(c(statistic:cohens, -p_value), round, 2)) |> 
  #mutate(mean_difference = paste(mean_difference, " [", conf_interval_upper, ", ", conf_interval_lower, "]", sep = "")) |> 
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
    round(p_value, 3) <= 0.00 ~ "***",
    round(p_value, 3) > 0.00 & round(p_value, 3) <= 0.01 ~ "**",
    round(p_value, 3) > 0.01 & round(p_value, 3) <= 0.05 ~ "*",
    round(p_value, 3) > 0.05 & round(p_value, 3) <= 0.10 ~ "†",
    TRUE ~ "-"
  )) |> 
  mutate(p_value = ifelse(p_value_sig == "***", "0.000", p_value)) |> # replace unnecessary scientific notation for final table, rounding to 3 decimal points
  mutate(outcome = factor(outcome, levels = c("Charges", "Punitiveness")),
         youth_adult = factor(youth_adult, levels = c("Adult", "Youth"))) |>
  arrange(outcome) |> 
  rename(`age group` = "youth_adult",
         `t-statistic` = "statistic",
         `p-value` = "p_value",
         `significance` = "p_value_sig",
         "estimate before" = "estimate_before",
         "estimate after" = "estimate_after",
         "difference [95% conf. interval]" = "diff",
         `cohen's d` = "cohens") |> 
  relocate(`difference [95% conf. interval]`, .before = `cohen's d`) |> 
  relocate(`significance`, .after = `p-value`)

table3 <- bind_rows(charge_output_for_table, disposition_output_for_table)

table3 <- mutate(table3, outcome = ifelse(outcome == "Charges", "Outcome 1: Charge rates (paired samples t-test)", "Outcome 2: Charge outcome severity (Welch's unequal variances t-test)"))

table3 <- table3 |> 
  group_by(outcome) |> 
  gt() |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gtsave(table3, "tables/charges_severity_all.png")


