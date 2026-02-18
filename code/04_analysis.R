#### 1) Load and clean data ####
df <- read_csv("data/police-reported-cannabis-offences-final.csv") %>%
  filter(region != "Canada") %>%
  mutate(
    time = year - 2014,
    post = ifelse(year >= 2019, 1, 0)
  )

stopifnot(all(c("year", "region", "youth_adult", "violation_type", "total", "cleared_by_charge", "population") %in% names(df)))

age_groups <- unique(df$youth_adult)
charge_types <- unique(df$violation_type)

#### 2) OUTCOME 1: Charge Rate (Negative Binomial) ####
results_rate <- list()

for (age in age_groups) {
  for (ctype in charge_types) {
    
    df_sub <- df %>%
      filter(youth_adult == age, violation_type == ctype, !is.na(total), !is.na(population)) %>%
      filter(sum(total, na.rm = TRUE) > 0)
    
    if (nrow(df_sub) < 20) next
    
    model_nb <- tryCatch({
      glm.nb(
        formula = total ~ time + post + time:post + factor(region) + offset(log(population)),
        data = df_sub,
        control = glm.control(maxit = 100)
      )
    }, warning = function(w) {
      message("Warning in model for ", age, " - ", ctype, ": ", conditionMessage(w))
      return(NULL)
    }, error = function(e) {
      message("Error in model for ", age, " - ", ctype, ": ", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(model_nb)) next
    
    coefs <- summary(model_nb)$coefficients
    exp_coefs <- exp(coefs[, 1])
    pvals <- coefs[, 4]
    
    for (term in c("time", "post", "time:post")) {
      if (term %in% rownames(coefs)) {
        results_rate[[length(results_rate) + 1]] <- tibble(
          Age_Group = age,
          Charge_Type = ctype,
          Term = term,
          IRR = round(exp_coefs[term], 3),
          P_Value = round(pvals[term], 4)
        )
      }
    }
    
    # DHARMa diagnostics
    sim <- tryCatch({
      simulateResiduals(model_nb)
    }, error = function(e) {
      message("DHARMa failed for ", age, " - ", ctype, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(sim)) {
      print(plot(sim))
      tryCatch({
        testOutliers(sim, type = "bootstrap")
      }, error = function(e) {
        message("Outlier test failed for ", age, " - ", ctype, ": ", e$message)
      })
    }
  }
}

df_rate <- bind_rows(results_rate) %>%
  pivot_wider(
    names_from = Term,
    values_from = c(IRR, P_Value),
    names_glue = "{Term}_{.value}"
  ) %>%
  relocate(Age_Group, Charge_Type)

print(gt(df_rate) %>% tab_header(title = "Outcome 1: Charge Rate (IRRs)"))

# WORD VERSION
df_out <- as.data.frame(df_rate)
clipr::write_clip(df_out, object_type = "table")

#### 3) OUTCOME 2: Charge Outcome Severity (Binomial Logit) ####
results_sev <- list()

for (age in age_groups) {
  for (ctype in charge_types) {
    
    df_sub <- df %>%
      filter(
        youth_adult == age,
        violation_type == ctype,
        !is.na(total), !is.na(cleared_by_charge),
        year != 2018
      ) %>%
      mutate(
        n_charged = cleared_by_charge,
        n_diverted = total - cleared_by_charge
      ) %>%
      filter((n_charged + n_diverted) > 0)
    
    if (nrow(df_sub) < 20 || any(df_sub$n_charged < 0 | df_sub$n_diverted < 0)) next
    
    model_bin <- tryCatch({
      glm(
        formula = cbind(n_charged, n_diverted) ~ time + post + time:post + factor(region),
        data = df_sub,
        family = binomial(link = "logit")
      )
    }, error = function(e) {
      message("Binomial model failed for ", age, " - ", ctype, ": ", e$message)
      return(NULL)
    })
    
    if (is.null(model_bin)) next
    
    coefs <- summary(model_bin)$coefficients
    exp_coefs <- exp(coefs[, 1])
    pvals <- coefs[, 4]
    
    for (term in c("time", "post", "time:post")) {
      if (term %in% rownames(coefs)) {
        results_sev[[length(results_sev) + 1]] <- tibble(
          Age_Group = age,
          Charge_Type = ctype,
          Term = term,
          OR = round(exp_coefs[term], 3),
          P_Value = round(pvals[term], 4)
        )
      }
    }
    
    sim <- tryCatch({
      simulateResiduals(model_bin)
    }, error = function(e) {
      message("DHARMa failed for ", age, " - ", ctype, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(sim)) {
      print(plot(sim))
      tryCatch({
        testOutliers(sim, type = "bootstrap")
      }, error = function(e) {
        message("Outlier test failed for ", age, " - ", ctype, ": ", e$message)
      })
    }
  }
}

df_sev <- bind_rows(results_sev) %>%
  pivot_wider(
    names_from = Term,
    values_from = c(OR, P_Value),
    names_glue = "{Term}_{.value}"
  ) %>%
  relocate(Age_Group, Charge_Type)

print(gt(df_sev) %>% tab_header(title = "Outcome 2: Charge Outcome Severity (ORs)"))

# WORD VERSION
df_out <- as.data.frame(df_sev)
clipr::write_clip(df_out, object_type = "table")

########### CHARGE RATES FIGURE ##########

key_types <- c("Possession", "Trafficking", "Drug impaired driving")

# Charge rate trends (per 100k)
p_charge_rate <- df %>%
  filter(!is.na(total), !is.na(population), region != "Canada") %>%
  filter(violation_type %in% c("Possession", "Drug impaired driving", "Trafficking")) %>%
  mutate(
    year_diff = year - 2018,  # Panel ITS: Center year_diff at legalization (2018 = 0)
    charge_rate = total / population * 100000
  ) %>%
  group_by(year_diff, violation_type, region, youth_adult) %>%
  summarize(charge_rate = sum(charge_rate, na.rm = TRUE), .groups = "drop") %>%
  group_by(year_diff, violation_type, youth_adult) %>%
  summarize(avg_charge_rate = mean(charge_rate), .groups = "drop") %>%
  mutate(
    avg_charge_rate = ifelse(year_diff == 0, NA, avg_charge_rate)  # Drop 2018 (transition year)
  ) %>%
  ggplot(aes(x = year_diff, y = avg_charge_rate, color = youth_adult)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_y_log10(labels = scales::comma_format(), n.breaks = 6) +
  scale_x_continuous(breaks = seq(-4, 3, 1)) +
  facet_wrap(~ violation_type, ncol = 1, scales = "free_y") +
  labs(
    title = "Charge Rates by Years Before and After Cannabis Legalization",
    #subtitle = "Total annual charges per 100,000 population, by charge type and age group",
    x = "Years before and after legalization (2018 = 0)",
    y = "Charges per 100,000 (log scale)",
    color = "Age group"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold.italic", size = 13),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = "figures/charge_rate_trends_by_violation_and_age.png",
  plot = p_charge_rate,
  width = 7.5,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)

######## DISPOSITIONS ######

key_types <- c("Possession", "Trafficking", "Drug impaired driving")

p_charge_disp <- df %>%
  filter(region != "Canada") %>%
  filter(violation_type %in% key_types) %>%
  filter(!is.na(total), !is.na(cleared_by_charge)) %>%
  mutate(
    year_diff = year - 2018
  ) %>%
  group_by(year_diff, violation_type, region, youth_adult) %>%
  summarize(
    total_offences = sum(total, na.rm = TRUE),
    total_charged  = sum(cleared_by_charge, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_charged = ifelse(total_offences > 0, (total_charged / total_offences) * 100, NA_real_)
  ) %>%
  group_by(year_diff, violation_type, youth_adult) %>%
  summarize(
    avg_prop_charged = mean(prop_charged, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    avg_prop_charged = ifelse(year_diff == 0, NA, avg_prop_charged)  # drop 2018 transition year
  ) %>%
  ggplot(aes(x = year_diff, y = avg_prop_charged, color = youth_adult)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = seq(-4, 3, 1)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  facet_wrap(~ violation_type, ncol = 1, scales = "free_y") +
  labs(
    title = "Charge Dispositions by Years Before and After Cannabis Legalization",
  #  subtitle = "Average percent of offences cleared by charge (vs. diversion/other), by charge type and \nage group",
    x = "Years before and after legalization (2018 = 0)",
    y = "Percent cleared by charge",
    color = "Age group"
  ) +
  theme_bw(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold.italic", size = 13),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  filename = "figures/charge_disposition_by_violation_and_age.png",
  plot = p_charge_disp,
  width = 7.5,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)



