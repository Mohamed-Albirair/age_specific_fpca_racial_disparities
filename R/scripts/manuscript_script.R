
### Manuscript: "Age-Specific Racial Disparities in the Incidence of Fatal Prostate Cancer: An Analytic Deconstruction"
### author: Mohamed Albirair, MBBS, MPH, PhD

## Packages
library(tidyverse)
library(flextable)

## Functions
# this is a code for the theme of ggplot visualizations
source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/main/goldenScatterCAtheme.R")

# Absolute metrics visualizations
source("R/functions/absViz.R")

# Convert absolute to relative metrics
source("R/functions/abs_to_rel_2.R")

# Relative metrics visualizations
source("R/functions/relViz.R")

# Running linear regression for trend analysis
source("R/functions/lm_reg.R")

# Display regression outputs
source("R/functions/kableOutput.R")

# Extract regression outcomes
source("https://raw.githubusercontent.com/Mohamed-Albirair/my-R-functions/main/modOutput.R")


## Settings
disease <- "Prostate cancer"
ageGroups_label <- c("45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84")
mod_params <- c("Intercept", "Age group", "Era", "Age group \u00D7 era")


## Read files
# Incidence
pcinc <- pcinc_personal_loc %>% 
      read.csv() %>% 
      mutate_at(4:6, readr::parse_number) %>% 
      mutate(AgeGroup = str_remove(AgeGroup, " years"),
             Period   = factor(Period, levels = c("Pre-PSA (1980-1989)",
                                                  "Post-PSA (2000-2009)"),
                               labels = c("Pre-PSA era (1980-1989)",
                                          "PSA era (2000-2009)")),
             Var      = Count / Pop ^ 2 * 1e5 ^ 2,
             LCL      = Rate - 1.96 * sqrt(Var),
             UCL      = Rate + 1.96 * sqrt(Var)) %>% 
      rename(metric = Rate)

# Survival
pcsurvdataNet <- read.csv(pcsurv_personal_loc) %>% 
      mutate_at(5:8, readr::parse_number) %>% 
      filter(SurvPeriod == "120 mo") %>% 
      rename(metric = Cause.Spec,
             SE     = SE.Cause.Spec,
             LCL    = LCL,
             UCL    = UCL) %>% 
      mutate(SurvPeriod = factor(SurvPeriod,
                                 levels = c("12 mo", "24 mo", "36 mo", "48 mo",
                                            "60 mo", "72 mo", "84 mo", "96 mo",
                                            "108 mo", "120 mo")),
             AgeGroup   = str_remove(AgeGroup, " years"),
             Period     = factor(Period, levels = c("Pre-PSA (1980-1989)",
                                                    "Post-PSA (2000-2009)"),
                                 labels = c("Pre-PSA era (1980-1989)",
                                            "PSA era (2000-2009)")),
             Var        = SE ^ 2)
  
# Prob death
pcDeathProb <- pcsurvdataNet %>% 
      mutate(metric = 100 - metric, UCLn = 100 - LCL, LCLn = 100 - UCL) %>% 
      select(-c(LCL, UCL)) %>% 
      rename(LCL = LCLn, UCL = UCLn)


pc_incid_DP <- pcDeathProb %>% 
      select(AgeGroup, Race, Period, metric, Var) %>% 
      rename(DeathProb = metric, Var_surv = Var) %>% 
      full_join(pcinc %>% 
                      select(AgeGroup, Race, Period, Rate = metric, Var_inc = Var),
                .) %>% 
      mutate(metric = Rate * DeathProb,
             Var = Rate ^ 2 * Var_surv + DeathProb ^ 2 * Var_inc,
             LCL = metric - 1.96 * sqrt(Var),
             UCL = metric + 1.96 * sqrt(Var))


### Results

## Annual Prostate Cancer Incidence Rates by Age Group
annincmod <- abs_to_rel_2(dat         = pcinc,
                          metric      = metric,
                          group_var   = c(AgeGroup, Period),
                          cmpr_var    = Race,
                          variance    = Var,
                          numerator   = "Black",
                          denominator = "White") %>% 
      
      lm_reg(data      = .)

anninctrend <- annincmod %>% modOutput(.) %>% 
      mutate(across(1, ~ mod_params))

# Figure 1, supplementary
# "Annual prostate cancer incidence rates by era, race, and age group. Figure shows observed incidence rates per 100,000 individuals with 95% confidence intervals."
absViz(data      = pcinc,
       ageCount  = 8,
       each_val  = 4,
       metric    = metric,
       LCL       = LCL,
       UCL       = UCL,
       xlabel    = ageGroups_label,
       cmpr_var  = Race,
       facet_var = Period,
       title     = NULL) +
      scale_y_continuous(name = "Annual incidence rate\n(per 100,000)",
                         labels = scales::comma)

# Figure #1
# "Prostate cancer Black-to-White annual incidence rate ratios by era and age group. Figure shows observed rate ratios with 95% confidence intervals and a fitted trend line based on a weighted linear regression model.", crop = TRUE}
abs_to_rel_2(dat         = pcinc,
             metric      = metric,
             group_var   = c(AgeGroup, Period),
             cmpr_var    = Race,
             variance    = Var,
             numerator   = "Black",
             denominator = "White") %>% 
      
      mutate(fitted = predict(annincmod, .)) %>% 
      
      relViz(data         = .,
             ageCount     = 8,
             each_val     = 2,
             xlabel       = ageGroups_label,
             color_choice = "blue",
             se           = FALSE,
             fill_choice  = NULL,
             facet_var    = Period,
             title        = NULL) +
      scale_y_continuous(name = "Incidence rate ratio (IRR)")

## Table #1
# YAML specifications: https://stackoverflow.com/questions/71008461/flextable-label-not-created-in-rmarkdown
# https://stackoverflow.com/questions/68761680/flextable-r-how-to-keep-columns-width-after-adding-a-header
anninctrend %>% flextable() %>% 
      set_caption(caption = "Fitted linear regression of prostate cancer incidence rate ratios by age group at diagnosis and calendar period. Reference categories: 45-49 age group, and the pre-PSA era.") %>% 
      bold(part = "header") %>% width(j = 1:5, 1)


## 10-Year Probability of Prostate Cancer-Specific Death
probdeathmod <- abs_to_rel_2(dat         = pcDeathProb,
                             metric      = metric,
                             group_var   = c(AgeGroup, Period),
                             cmpr_var    = Race,
                             variance    = Var,
                             numerator   = "Black",
                             denominator = "White") %>% 
      lm_reg(data      = .)

probdeathtrend <- probdeathmod %>% modOutput(.) %>% 
      mutate(across(1, ~ mod_params))

# Figure #2, supplementary
# "10-year prostate cancer probability of death by era, race, and age group. Figure shows observed incidence rates with 95% confidence intervals.", crop = TRUE}
pcDeathProb %>% 
      absViz(data      = .,
             ageCount  = 8,
             each_val  = 4,
             metric    = metric,
             LCL       = UCL,
             UCL       = LCL,
             xlabel    = ageGroups_label,
             cmpr_var  = Race,
             facet_var = Period,
             title     = NULL) +
      scale_y_continuous(name = "10-year probability of death (%)",
                         limits = c(0, 100), breaks = seq(0, 100, 20))


# Figure #2
# "Prostate cancer Black-to-White 10-year probability of death ratios by era and age group. Figure shows observed rate ratios with 95% confidence intervals and a fitted trend line based on a weighted linear regression model.", crop = TRUE}
abs_to_rel_2(dat         = pcDeathProb,
             metric      = metric,
             group_var   = c(AgeGroup, Period),
             cmpr_var    = Race,
             variance    = Var,
             numerator   = "Black",
             denominator = "White") %>% 
      
      mutate(fitted = predict(probdeathmod, .)) %>% 
      
      relViz(data         = .,
             ageCount     = 8,
             each_val     = 2,
             xlabel       = ageGroups_label,
             color_choice = "blue",
             se           = FALSE,
             fill_choice  = NULL,
             facet_var    = Period,
             title        = NULL) +
      scale_y_continuous(name = "Ratio of 10-year\nprobability of death")


# Table #2
# YAML specifications: https://stackoverflow.com/questions/71008461/flextable-label-not-created-in-rmarkdown
# https://stackoverflow.com/questions/68761680/flextable-r-how-to-keep-columns-width-after-adding-a-header
probdeathtrend %>% flextable() %>% 
      set_caption(caption = "Fitted linear regression of relative ratios of 10-year probability of prostate cancer death by age group at diagnosis and calendar period. Reference categories: 45-49 age group, and the pre-PSA era.") %>% 
      bold(part = "header") %>% width(j = 1:5, 1)


## Incidence of Fatal Prostate Cancer
fpcamod <- abs_to_rel_2(dat         = pc_incid_DP,
                        metric      = metric,
                        group_var   = c(AgeGroup, Period),
                        cmpr_var    = Race,
                        variance    = Var,
                        numerator   = "Black",
                        denominator = "White") %>% 
      lm_reg(data = .)

fpcatrend <- fpcamod %>% modOutput(.) %>% 
      mutate(across(1, ~ mod_params))


# Figure #3, supplementary
# "Annual incidence rates of fatal prostate cancer by era, race, and age group. Figure shows observed incidence rates with 95% confidence intervals", crop = TRUE}
pc_incid_DP %>% 
      absViz(data      = .,
             ageCount  = 8,
             each_val  = 4,
             metric    = metric / 100,
             LCL       = LCL / 100,
             UCL       = UCL / 100,
             xlabel    = ageGroups_label,
             cmpr_var  = Race,
             facet_var = Period,
             title     = NULL) +
      scale_y_continuous(name = "Annual incidence of fatal\ndisease (per 100,000)",
                         labels = scales::comma)


# Figure #3
# "Black-to-White annual incidence rate ratios of fatal prostate cancer incidence by era and age group. Figure shows observed rate ratios with 95% confidence intervals and a fitted trend line based on a weighted linear regression model.", crop = TRUE}
abs_to_rel_2(dat         = pc_incid_DP,
             metric      = metric,
             group_var   = c(AgeGroup, Period),
             cmpr_var    = Race,
             variance    = Var,
             numerator   = "Black",
             denominator = "White") %>% 
      
      mutate(fitted = predict(fpcamod, .)) %>% 
      
      relViz(data         = .,
             ageCount     = 8,
             each_val     = 2,
             xlabel       = ageGroups_label,
             color_choice = "blue",
             se           = FALSE,
             fill_choice  = NULL,
             facet_var    = Period,
             title        = NULL) +
      scale_y_continuous(name = "fPCa incidence rate ratio (IRR)")


# Table #3
# YAML specifications: https://stackoverflow.com/questions/71008461/flextable-label-not-created-in-rmarkdown
# https://stackoverflow.com/questions/68761680/flextable-r-how-to-keep-columns-width-after-adding-a-header
fpcatrend %>% flextable() %>% 
      set_caption(caption = "Fitted linear regression of Black-to-White incidence rate ratios of fatal prostate cancer by age group at diagnosis and calendar period. Reference categories: 45-49 age group, and the pre-PSA era.") %>% 
      bold(part = "header") %>% width(j = 1:5, 1)


## B. Incidence of Fatal Disease within 17 years of Diagnosis
# Read data
pcsurvdataNet17 <- read.csv(pcsurv17_personal_loc) %>%
      mutate_at(5:9, readr::parse_number) %>%
      mutate(AgeGroup = str_remove(AgeGroup, " years"),
             Period = factor(Period,
                             levels = c("Pre-PSA (1980-1989)", "Post-PSA (2000-2009)"),
                             labels = c("Pre-PSA era (1980-1989)", "PSA era (2000-2009)")),
             DeathProb_17 = 100 - Survival_17,
             Var_surv_17 = SE_surv_17 ^ 2)

pcIncidSurvNet17 <- pcsurvdataNet17 %>% 
      full_join(pcinc %>% rename(Rate = metric, Var_inc = Var), .) %>%
      mutate(IncidDeathProb = Rate * DeathProb_17,
             Var_prod = Rate ^ 2 * Var_surv_17 + DeathProb_17 ^ 2 * Var_inc,
             LCL_prod = IncidDeathProb - 1.96 * sqrt(Var_prod),
             UCL_prod = IncidDeathProb + 1.96 * sqrt(Var_prod))

fpcamod17 <- abs_to_rel_2(dat         = pcIncidSurvNet17,
                          metric      = IncidDeathProb,
                          group_var   = c(AgeGroup, Period),
                          cmpr_var    = Race,
                          variance    = Var_prod,
                          numerator   = "Black",
                          denominator = "White") %>% 
      lm_reg(data = .)

fpcatrend17 <- fpcamod17 %>% modOutput(.) %>% 
      mutate(across(1, ~ mod_params))

# Figure #4, supplementary
# "Annual incidence rates of fatal prostate cancer within 17 years of diagnosis by era, race, and age group. Figure shows observed incidence rates with 95% confidence intervals", crop = TRUE}
pcIncidSurvNet17 %>% 
      absViz(data      = .,
             ageCount  = 8,
             each_val  = 4,
             metric    = IncidDeathProb / 100,
             LCL       = LCL_prod / 100,
             UCL       = UCL_prod / 100,
             xlabel    = ageGroups_label,
             cmpr_var  = Race,
             facet_var = Period,
             title     = NULL) +
      scale_y_continuous(name = "Incidence of fatal disease",
                         labels = scales::comma)

# Figure #5, supplementary
# "Black-to-White annual incidence rate ratios of fatal prostate cancer incidence within 17 years of diagnosis by era and age group. Figure shows observed rate ratios with 95% confidence intervals and a fitted trend line based on a weighted linear regression model.", crop = TRUE}
abs_to_rel_2(dat         = pcIncidSurvNet17,
             metric      = IncidDeathProb,
             group_var   = c(AgeGroup, Period),
             cmpr_var    = Race,
             variance    = Var_prod,
             numerator   = "Black",
             denominator = "White") %>% 
      
      mutate(fitted = predict(fpcamod17, .)) %>% 
      
      relViz(data         = .,
             ageCount     = 8,
             each_val     = 2,
             xlabel       = ageGroups_label,
             color_choice = "blue",
             se           = FALSE,
             fill_choice  = NULL,
             facet_var    = Period,
             title        = NULL) +
      scale_y_continuous(name = "fPCa incidence rate ratio (IRR)")


# Table 4, supplementary
# YAML specifications: https://stackoverflow.com/questions/71008461/flextable-label-not-created-in-rmarkdown
# https://stackoverflow.com/questions/68761680/flextable-r-how-to-keep-columns-width-after-adding-a-header
fpcatrend17 %>% flextable() %>% 
      set_caption(caption = "Fitted linear regression of Black-to-White incidence rate ratios of fatal prostate cancer within 17 years of diagnosis by age group at diagnosis and calendar period. Reference categories: 45-49 age group, and the pre-PSA era.") %>% 
      bold(part = "header") %>% width(j = 1:5, 1)
