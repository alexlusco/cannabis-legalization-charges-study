###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to clean and wrangle cannabis charges data provided
# by Statistics Canada
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

sheet_url <- "data/police-reported-cannabis-offences.xlsx"

# define a function to add date_range column and repeat the value from the 3rd row of "cleared_by_charge"
add_date_range_col <- function(data) {
  date_range <- data$cleared_by_charge[3]
  data |> 
    mutate(date_range = date_range)
}

# write get data function to apply iteratively on each sheet
get_data <- function(sheet_url, sheet_name){
  
  # read in data, specifying sheet url and sheet name
  df <- readxl::read_xlsx(sheet_url, sheet = sheet_name, col_names = FALSE)
  
  # add region column using name and rename three key columns shared across years
  df <- df |> 
    mutate(region = sheet_name) |> 
    rename("violation" = `...1`,
           "age_group" = `...2`,
           "gender" = `...3`) |> 
    
    # fill NAs downward on violation and age_group
    fill(violation, .direction = "down") |> 
    fill(age_group, .direction = "down") |> 
    
    # reloacte violation, age group, and gender to end, important to do this before the split
    relocate(violation, .after = region) |> 
    relocate(age_group, .after = violation) |> 
    relocate(gender, .after = age_group)
  
  # split data into separate tibbles every 4 columns
  list_of_dfs <- df |> 
    split.default(rep(1:9, each = 4))
  
  # separate out tibble 9 in list, contains violation:gender cols to add back to data later
  other_data <- list_of_dfs[9]
  
  # rename columns in each tibble in the list
  new_column_names <- c("cleared_by_charge", "diversionary_program", "other_cleared_otherwise", "total")
  list_of_dfs <- lapply(list_of_dfs[1:8], setNames, nm = new_column_names)
  
  # bind columsn in other data to each tibble in the list
  list_of_dfs <- lapply(list_of_dfs, bind_cols, other_data)
  
  # apply add_new_column function to each tibble in the list
  list_of_dfs <- lapply(list_of_dfs, add_date_range_col)
  
  # remove top 5 rows from each tibble in list
  list_of_dfs <- lapply(list_of_dfs, function(df) {
    df %>% slice(-(1:5))
  })
  
  # merge list objects by binding rows, creating single tibble output
  output <- bind_rows(list_of_dfs)
  
  # do some relocation of key vars to clean up the output
  output <- output |> 
    relocate(gender, .before = cleared_by_charge) |> 
    relocate(age_group, .before = gender) |> 
    relocate(date_range, .before = age_group) |> 
    relocate(violation, .before = date_range) |> 
    relocate(region, .after = violation)
  
  return(output)
  
}

# create list of regions to iterate function over in for loop  
regions <- c("Newfoundland and Labrador", "Prince Edward Island", "Nova Scotia", "New Brunswick", 
             "Quebec", "Ontario", "Manitoba", "Saskatchewan", "Alberta", "British Columbia", "Yukon", 
             "Northwest Territories", "Nunavut", "Canada")

# create empty list for output of for loop
output <- list()

# define for loop
for(i in regions){
  
  output[[i]] <- get_data(sheet_url, i)
  
}

# merge tibbles in list output by binding rows
final_df <- bind_rows(output)

# correct some variable classes
final_df <- final_df |> 
  mutate(across(cleared_by_charge:total, as.double))

# create year var
final_df <- final_df |> 
  mutate(year = as.double(str_extract(date_range, "\\d{4}")))

# drop numbers at end of violation strings
final_df <- final_df |> 
  mutate(violation = str_remove(violation, "[0-9]+$"))

# save final_df as csv for analysis
write_csv(final_df, "data/police-reported-cannabis-offences-cleaned.csv")