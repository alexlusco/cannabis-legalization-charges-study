###########################
# Author:
# Date: August 14, 2023
# Description: R script to clean and wrangle cannabis charges data provided
# by Statistics Canada
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-final.csv")

df <- df |> filter(region != "Canada")

# Figure 1: total charges per 100,000 population

df |> 
  #mutate(year_diff = year_diff + 1) |> 
  group_by(year_diff, violation_type, region, youth_adult) |> 
  reframe(total_charges = sum(total, na.rm = TRUE)/population*100000) |> 
  ungroup() |> 
  distinct(year_diff, violation_type, region, youth_adult, total_charges) |> 
  group_by(year_diff, violation_type, youth_adult) |> 
  summarize(total_charges = mean(total_charges)) |> 
  mutate(total_charges = ifelse(year_diff == -1, NA, total_charges)) |> 
  ggplot(aes(x = year_diff, y = total_charges, group = youth_adult, colour = youth_adult)) +
  geom_line(linewidth = 0.75) +
  #geom_smooth() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_log10() +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(1, 400)) +
  facet_wrap(~factor(violation_type, levels = c("Possession", 
                                                "Drug impaired driving", 
                                                "Trafficking", 
                                                "Production and cultivation", 
                                                "Importation-exportation", 
                                                "Other"))) +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Total annual charges per 100,000 population for adults (>=18 years) and youths (12-17 years)\nbetween 2015-2017 and 2019-2021",
       y = "Total charges per 100,000\npopulation (log scale) \n",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

#extrafont::loadfonts()
ggsave("figures/adult_youth_charges_by_violation_type.png", width = 10, height = 6)

# Figure 2: charge dispositions

df |> 
  group_by(violation_type, year_diff, region, youth_adult) |> 
  reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100) |> 
  ungroup() |> 
  distinct(year_diff, violation_type, region, youth_adult, prop_charged) |> 
  group_by(year_diff, violation_type, youth_adult) |> 
  summarize(prop_charged = mean(prop_charged, na.rm = TRUE)) |> 
  mutate(prop_charged = ifelse(year_diff == -1, NA, prop_charged)) |> 
  ggplot(aes(x = year_diff, y = prop_charged, group = youth_adult, colour = youth_adult)) +
  geom_line(linewidth = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(0, 400)) +
  facet_wrap(~factor(violation_type, levels = c("Possession", 
                                                "Drug impaired driving", 
                                                "Trafficking", 
                                                "Production and cultivation", 
                                                "Importation-exportation", 
                                                "Other"))) +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Total proportion of offences cleared by criminal charge rather than diversion or other means for\nadults (>=18 years) and youths (12 to 17 years) between 2015-2017 and 2019-2021",
       y = "Total proportion of offences\ncleared by criminal charge\n",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

#extrafont::loadfonts()
ggsave("figures/adult_youth_charges_by_clearance_and_violation_type.png", width = 10, height = 6)


df |> 
  mutate(year_diff = year_diff + 1) |> 
  filter(year_diff <= -1 | year_diff >= 1) |> 
  group_by(year_diff, violation_type, region, youth_adult) |> 
  reframe(total_charges = sum(total, na.rm = TRUE)/population*100000) |> 
  ungroup() |> 
  distinct(year_diff, violation_type, region, youth_adult, total_charges) |> 
  group_by(year_diff, violation_type, region, youth_adult) |>
  summarize(total_charges = mean(total_charges)) |> 
  ungroup() |> 
  mutate(total_charges = ifelse(year_diff == -1, NA, total_charges)) |> 
  ggplot(aes(x = year_diff, y = total_charges, group = violation_type, colour = violation_type)) +
  geom_line(linewidth = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  scale_y_log10() +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(1, 400)) +
  facet_wrap(region ~ youth_adult, nrow = 7) +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Region stratified total annual charges per 100,000 population for adults (>=18 years) and\nyouths (12-17 years) between 2015-2017 and 2019-2021",
       y = "Total charges per 100,000\npopulation (log scale)\n",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

ggsave("figures/supplementary/suppl_adult_youth_charges_by_region.png", width = 9, height = 12)

df |> 
  mutate(year_diff = year_diff + 1) |> 
  filter(year_diff <= -1 | year_diff >= 1) |> 
  group_by(year_diff, violation_type, region, youth_adult) |> 
  reframe(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100) |> 
  ungroup() |> 
  distinct(year_diff, violation_type, region, youth_adult, prop_charged) |> 
  group_by(year_diff, violation_type, region, youth_adult) |>
  summarize(prop_charged = mean(prop_charged, na.rm = TRUE)) |> 
  ungroup() |> 
  mutate(prop_charged = ifelse(year_diff == -1, NA, prop_charged)) |> 
  ggplot(aes(x = year_diff, y = prop_charged, group = violation_type, colour = violation_type)) +
  geom_line(linewidth = 0.75) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(1, 400)) +
  facet_wrap(region ~ youth_adult, nrow = 7) +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Region stratified total annual charges per 100,000 population for adults (>=18 years) and\nyouths (12-17 years) between 2015-2017 and 2019-2021",
       y = "Total charges per 100,000\npopulation\n",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

ggsave("figures/supplementary/suppl_adult_youth_disposition_by_region.png", width = 9, height = 12)
