###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to obtain misc descriptive statistics
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-final.csv")

df <- df |> filter(region != "Canada")

# breakdown of total charges by violation with counts
df |> 
  group_by(youth_adult) |> 
  mutate(total_charges = sum(total, na.rm = TRUE)) |> 
  group_by(violation_type, youth_adult) |> 
  reframe(total_perc = sum(total, na.rm = TRUE)/total_charges*100) |> 
  distinct(violation_type, youth_adult, .keep_all = TRUE) |> 
  spread(youth_adult, total_perc)
 # adult = 92.7
 # youth = 98.81

# list of charges in each category
df_tmp <- mutate(df, violation_type = factor(violation_type, levels = c("Possession", 
                                        "Drug impaired driving", 
                                        "Trafficking", 
                                        "Production and cultivation", 
                                        "Importation-exportation", 
                                        "Other"))) |> 
  arrange(violation_type) |> 
  filter(year != 2018) |> 
  mutate(timepoint = ifelse(year < 2018, "Before", "After"))

df_before <- filter(df_tmp, timepoint == "Before")
df_after <- filter(df_tmp, timepoint == "After")

list_of_violations_before <- list()
list_of_violations_after <- list()

for (i in unique(df_before$violation_type)){
  
  counts <- df_before |> 
    filter(violation_type == i) |> 
    summarize(total = sum(total, na.rm = TRUE), .by = violation) |> 
    arrange(-total) |> 
    mutate(violation_type = i) |> 
    relocate(violation_type, .before = violation) |> 
    filter(total > 0)
  
  list_of_violations_before[[i]] <- counts
  
}

for (i in unique(df_after$violation_type)){
  
  counts <- df_after |> 
    filter(violation_type == i) |> 
    summarize(total = sum(total, na.rm = TRUE), .by = violation) |> 
    arrange(-total) |> 
    mutate(violation_type = i) |> 
    relocate(violation_type, .before = violation) |> 
    filter(total > 0)
  
  list_of_violations_after[[i]] <- counts
  
}

violations_table_before <- 
  bind_rows(list_of_violations_before) |> 
  group_by(violation_type) |> 
  gt() |> 
  tab_header("Before legalization charge counts, 2015-2017") |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

violations_table_after <- 
  bind_rows(list_of_violations_after) |> 
  group_by(violation_type) |>
  gt() |> 
  tab_header("After legalization charge counts, 2019-2021") |> 
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_row_groups()
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

gtsave(violations_table_before, "tables/supplementary/suppl_violations_list_before.png")
gtsave(violations_table_after, "tables/supplementary/suppl_violations_list_after.png")

# supplementary analysis -- charge rates by region, testing for interaction effect between region:timepoint using
# mixed-design ANOVA
df_filtered_youth <- df |> 
  #filter(violation_type %in% c("Possession", "Drug impaired driving", "Trafficking")) |>
  filter(year != 2018) |> 
  filter(youth_adult == "Youth") |> 
  mutate(timepoint = ifelse(year < 2018, "Before", "After")) |> 
  #mutate(total = ifelse(is.na(total), 0, total)) |> 
  reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(region, timepoint, violation_type)) |> 
  distinct(region, total_charges, timepoint, violation_type, .keep_all = TRUE) |> 
  #filter(!is.na(prop_charged)) |> 
  #group_by(region) |> 
  #filter(!n() < 6) |> # for youth, drops yukon, NWT, NB, PEI, Nunavut due to NAs
  group_by(region, violation_type) |> 
  mutate(subject = cur_group_id()) |> 
  ungroup()

df_filtered_adult <- df |> 
  #filter(violation_type %in% c("Possession", "Drug impaired driving", "Trafficking")) |>
  filter(year != 2018) |> 
  filter(youth_adult == "Adult") |> 
  mutate(timepoint = ifelse(year < 2018, "Before", "After")) |> 
  mutate(total = ifelse(is.na(total), 0, total)) |> 
  reframe(total_charges = sum(total, na.rm = TRUE)/population*100000, .by = c(region, timepoint, violation_type)) |> 
  distinct(region, total_charges, timepoint, violation_type, .keep_all = TRUE) |> 
  #filter(!is.na(prop_charged)) |> 
  #group_by(region) |>
  #filter(!n() < 6) |>
  group_by(region, violation_type) |> 
  mutate(subject = cur_group_id()) |> 
  ungroup()

results_youth <- ezANOVA(
  data = df_filtered_youth,
  dv = total_charges,          # dependent variable
  wid = subject,            # subject identifier
  within = timepoint,       # within-subjects factor
  between = region,         # between-subjects factor
  detailed = TRUE           # will provide detailed output
)

results_adult <- ezANOVA(
  data = df_filtered_adult,
  dv = total_charges,          # dependent variable
  wid = subject,            # subject identifier
  within = timepoint,       # within-subjects factor
  between = region,         # between-subjects factor
  detailed = TRUE           # will provide detailed output
)

results_youth_rates_table <- results_youth$ANOVA |> gt() |> tab_header("Youth")
results_adult_rates_table <- results_adult$ANOVA |> gt() |> tab_header("Adult")

gtsave(results_youth_rates_table, "tables/supplementary/suppl_anova_youth_rates_table.png")
gtsave(results_adult_rates_table, "tables/supplementary/suppl_anova_adult_rates_table.png")

# supplementary analysis -- charge outcome severity by region, testing for interaction effect between region:timepoint using
# mixed-design ANOVA
df_filtered_youth <- df |> 
  filter(violation_type %in% c("Possession", "Drug impaired driving", "Trafficking")) |> 
  filter(year != 2018) |> 
  filter(youth_adult == "Youth") |> 
  mutate(timepoint = ifelse(year < 2018, "Before", "After")) |> 
  reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(region, timepoint, violation_type)) |>
  distinct(region, prop_charged, timepoint, violation_type, .keep_all = TRUE) |> 
  filter(!is.na(prop_charged)) |> 
  group_by(region) |> 
  filter(!n() < 6) |> # for youth, drops ...
  group_by(region, violation_type) |> 
  mutate(subject = cur_group_id()) |> 
  ungroup()

df_filtered_adult <- df |> 
  filter(violation_type %in% c("Possession", "Drug impaired driving", "Trafficking")) |> 
  filter(year != 2018) |> 
  filter(youth_adult == "Adult") |> 
  mutate(timepoint = ifelse(year < 2018, "Before", "After")) |> 
  reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100, .by = c(region, timepoint, violation_type)) |>
  distinct(region, prop_charged, timepoint, violation_type, .keep_all = TRUE) |> 
  filter(!is.na(prop_charged)) |> 
  group_by(region) |>
  filter(!n() < 6) |>
  group_by(region, violation_type) |> 
  mutate(subject = cur_group_id()) |> 
  ungroup()

results_youth <- ezANOVA(
  data = df_filtered_youth,
  dv = prop_charged,        # dependent variable
  wid = subject,            # subject identifier
  within = timepoint,       # within-subjects factor
  between = region,         # between-subjects factor
  detailed = TRUE           # will provide detailed output
)

results_adult <- ezANOVA(
  data = df_filtered_adult,
  dv = prop_charged,        # dependent variable
  wid = subject,            # subject identifier
  within = timepoint,       # within-subjects factor
  between = region,         # between-subjects factor
  detailed = TRUE           # will provide detailed output
)
  
results_youth_severity_table <- results_youth$ANOVA |> gt() |> tab_header("Youth")
results_adult_severity_table <- results_adult$ANOVA |> gt() |> tab_header("Adult")

gtsave(results_youth_severity_table, "tables/supplementary/suppl_anova_youth_severity_table.png")
gtsave(results_adult_severity_table, "tables/supplementary/suppl_anova_adult_severity_table.png")


