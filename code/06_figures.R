###########################
# Author: Alex Luscombe
# Date: August 14, 2023
# Description: R script to clean and wrangle cannabis charges data provided
# by Statistics Canada
# Notes: 
#   - 01_config needs to be run first to load required libraries
###########################

# script starts here

df <- read_csv("data/police-reported-cannabis-offences-final.csv")

# Figure 1: total charges per 100,000 population

df |> 
  group_by(year_diff, violation_type, youth_adult) |> 
  reframe(total_charges = sum(total, na.rm = TRUE)/population*100000) |> 
  ungroup() |> 
  ggplot(aes(x = year_diff, y = total_charges, group = youth_adult, colour = youth_adult)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(1, 400)) +
  facet_wrap(~factor(violation_type, levels = c("Possession", 
                                                "Drug impaired driving", 
                                                "Trafficking", 
                                                "Production and cultivation", 
                                                "Importation-exportation", 
                                                "Other")), scales = "free_y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Total charges per year per 100,000 population for adults (>=18 years) and youths (12-17 years)",
       y = "Total charges per 100,000 population",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

#extrafont::loadfonts()
ggsave("figures/adult_youth_charges_by_violation_type.png", width = 10, height = 6)

# Figure 2: charge dispositions

df |> 
  group_by(violation_type, year_diff, youth_adult) |> 
  summarize(prop_charged = sum(cleared_by_charge, na.rm = TRUE)/sum(total, na.rm = TRUE)*100) |> 
  ggplot(aes(x = year_diff, y = prop_charged, group = youth_adult, colour = youth_adult)) +
  geom_line() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  #geom_label(aes(label = youth_adult), data = df %>% filter(year_diff == 0)) +
  scale_x_continuous(n.breaks = 10) +
  #scale_y_log10(n.breaks = 6) +
  #expand_limits(y = c(1, 400)) +
  facet_wrap(~factor(violation_type, levels = c("Possession", 
                                                "Drug impaired driving", 
                                                "Trafficking", 
                                                "Production and cultivation", 
                                                "Importation-exportation", 
                                                "Other")), scales = "free_y") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "italic")) +
  #theme(text = element_text(family = "Times New Roman")) +
  theme(legend.position = "bottom") +
  labs(title = "Total proportion of violations cleared by criminal charge rather than diversion or other means for adults\n(>=18 years) and youths (12 to 17 years)",
       y = "Total proportion of violations cleared by criminal charge",
       x = "\nNumber of years before and after legalization",
       colour = "",
  )

#extrafont::loadfonts()
ggsave("figures/adult_youth_charges_by_clearance_and_violation_type.png", width = 10, height = 6)