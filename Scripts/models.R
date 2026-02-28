### Load packages

library(tidyverse)
library(haven)
library(foreign)
library(ggthemes)
library(vdemdata)
library(lme4)
library(readxl)
library(rstanarm)
library(Matrix)
library(zoo)
library(plm)
library(reghelper)
library(datawizard)
library(stargazer)
library(data.table)
library(corrplot)
library(splines)
library(glmmTMB)
library(car)
library(modelsummary)

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

# Compute Bayes Factor approximation from BIC values
# BF > 1 favors model 1 over model 0; BF < 1 favors model 0
# Interpretation (Kass & Raftery 1995):
#   BF 1-3: barely worth mentioning
#   BF 3-20: positive evidence
#   BF 20-150: strong evidence
#   BF >150: very strong evidence
bic_bayes_factor <- function(bic0, bic1) {
  exp((bic0 - bic1) / 2)
}

# Compare a list of models on AIC, BIC, and pairwise Bayes factors vs baseline
compare_models <- function(model_list) {
  comparison <- tibble(
    model = names(model_list),
    AIC   = sapply(model_list, AIC),
    BIC   = sapply(model_list, BIC)
  )

  bic_baseline <- comparison$BIC[1]
  comparison <- comparison %>%
    mutate(BF_vs_baseline = bic_bayes_factor(bic_baseline, BIC))

  comparison
}

# Coefficient plot for any mixed model
plot_model_coefficients <- function(model, model_name) {
  broom.mixed::tidy(model, effects = "fixed") %>%
    filter(term != "(Intercept)") %>%
    ggplot(aes(x = estimate, y = term)) +
    geom_point() +
    geom_pointrange(aes(xmin = estimate - 1.96 * std.error,
                       xmax = estimate + 1.96 * std.error),
                   height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
      title = paste("Coefficient Plot:", model_name),
      x = "Estimate (with 95% CI)",
      y = NULL
    ) +
    theme_minimal()
}

# =============================================================================
# LOAD DATA
# =============================================================================

cses_test <- readRDS(file = "Data/cses_test.Rda")

# =============================================================================
# DIAGNOSTICS: SUMMARY STATISTICS, CORRELATIONS, AND MISSINGNESS
# =============================================================================

#### Summary statistics for key model variables

model_vars <- c("party_fav", "partyrep",
                "lag_sys_pop_within_c", "lag_sys_pop_between_c",
                "lag_opp_pop_within_c", "lag_opp_pop_between_c",
                "lag_gov_pop_within_c", "lag_gov_pop_between_c",
                "lag_ipop_within_c", "lag_ipop_between_c",
                "v2elloeldm_c",
                "unemploy_t0_within_c", "unemploy_t0_between_c",
                "age_squ_c", "numofparties_within_c", "numofparties_between_c",
                "gender", "v2elparlel", "edu",
                "party_sys_age_c", "lr_within_c", "lr_between_c",
                "lr_sq_within_c", "lr_sq_between_c")

summary_data <- cses_test %>%
  select(all_of(model_vars)) %>%
  mutate(gender = as.factor(gender),
         v2elparlel = as.factor(v2elparlel),
         edu = as.factor(edu)) %>%
  na.omit()

summary(summary_data)

#### Correlation matrix for continuous fixed-effect variables

fixed_vars <- c("lag_opp_pop_within_c", "lag_opp_pop_between_c",
                "lag_ipop_within_c", "lag_ipop_between_c",
                "lag_sys_pop_between_c", "lag_sys_pop_within_c",
                "v2elloeldm_c", "party_sys_age_c",
                "unemploy_t0_within_c", "unemploy_t0_between_c",
                "gini_within_c", "gini_between_c",
                "numofparties_within_c", "numofparties_between_c",
                "age_squ_c",
                "lr_within_c", "lr_between_c",
                "lr_sq_within_c", "lr_sq_between_c")

cor_data <- cses_test %>%
  select(all_of(fixed_vars)) %>%
  na.omit()

cor_matrix <- cor(cor_data)
print(round(cor_matrix, 2))
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Some correlation between measures, but not worrisome. Highly correlated
# measures are not in the same model (e.g., ipop and gov)

#### Missingness diagnostics

miss_vars <- c(
  "newpop_closest_within_c", "newpop_closest_between_c",
  "lag_opp_pop_within_c", "lag_opp_pop_between_c",
  "lag_ipop_within_c", "lag_ipop_between_c",
  "v2elloeldm_c", "v2elparlel", "party_sys_age_c",
  "unemploy_t0_within_c", "unemploy_t0_between_c",
  "gini_within_c", "gini_between_c",
  "numofparties_within_c", "numofparties_between_c",
  "age_squ_c",
  "lr_within_c", "lr_between_c",
  "lr_sq_within_c", "lr_sq_between_c",
  "lr_missing", "gender", "edu",
  "newpop_closest_missing", "partisan_type"
)

missing_counts <- sapply(miss_vars, function(var) sum(is.na(cses_test[[var]])))
missing_df <- data.frame(Variable = names(missing_counts), Missing = missing_counts)
print(missing_df)

# =============================================================================
# MODEL FITTING
# =============================================================================

# Full control set used across most models:
#   Institutional: v2elloeldm_c, v2elparlel, party_sys_age_c
#   Economic: unemploy_t0 (within/between), gini (within/between)
#   Party system: numofparties (within/between)
#   Individual: age_squ_c, lr (within/between), lr_sq (within/between),
#               lr_missing, gender, edu

# =============================================================================
# BASELINE MODELS (m0 series): System populism only, no controls
# =============================================================================

## Party Representation (binary: feels represented by a party)
m0a <- glmer(partyrep ~
               sys_pop_within_c +
               sys_pop_between_c +
               (1 | country_id/ye ar_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m0a)

## Party Favorability (continuous: feeling thermometer for preferred party)
m0b <- lmer(party_fav ~
               sys_pop_within_c +
               sys_pop_between_c +
               (1 | country_id/year_fact),
             data = cses_test)

summary(m0b)

# =============================================================================
# M1 SERIES: System populism with full controls
# =============================================================================

## Party Representation: System Populism
m1a <- glmer(partyrep ~
               lag_sys_pop_within_c +
               lag_sys_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test, family = binomial(link = "logit"))

summary(m1a)

## Party Favorability: System Populism
m1b <- lmer(party_fav ~
               lag_sys_pop_within_c +
               lag_sys_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
             data = cses_test)

summary(m1b)

# =============================================================================
# M2 SERIES: Opp + Incumbent split, with time trends
# =============================================================================

## Party Representation: Opp + Incumbent (linear time)
m2a <- glmer(partyrep ~
             lag_opp_pop_within_c +
             lag_opp_pop_between_c +
             lag_ipop_within_c +
             lag_ipop_between_c +
             v2elloeldm_c +
             as.factor(v2elparlel) +
             party_sys_age_c +
             unemploy_t0_within_c +
             unemploy_t0_between_c +
             gini_within_c +
             gini_between_c +
             numofparties_within_c +
             numofparties_between_c +
             age_squ_c +
             lr_within_c +
             lr_between_c +
             lr_sq_within_c +
             lr_sq_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             time +
            (1 + time | country_id) + (1 | country_id:year_fact),
           data = cses_test, family = binomial(link = "logit"))

## Party Representation: Opp + Incumbent (spline time)
m2a_spline <- glmer(partyrep ~
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              spline1 + spline2 + spline3 +
              (spline1 + spline2 + spline3 | country_id) +
              (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

## Party Favorability: Opp + Incumbent (linear time)
m2b <- lmer(party_fav ~
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

## Party Favorability: Opp + Incumbent (spline time)
m2b_spline <- lmer(party_fav ~
    lag_opp_pop_within_c +
    lag_opp_pop_between_c +
    lag_ipop_within_c +
    lag_ipop_between_c +
    v2elloeldm_c +
    as.factor(v2elparlel) +
    party_sys_age_c +
    unemploy_t0_within_c +
    unemploy_t0_between_c +
    gini_within_c +
    gini_between_c +
    numofparties_within_c +
    numofparties_between_c +
    age_squ_c +
    lr_within_c +
    lr_between_c +
    lr_sq_within_c +
    lr_sq_between_c +
    as.factor(lr_missing) +
    as.factor(gender) +
    as.factor(edu) +
    spline1 + spline2 + spline3 +
    (spline1 + spline2 + spline3 | country_id) +
    (1 | country_id:year_fact),
  data = cses_test
)

summary(m2a)
summary(m2a_spline)
summary(m2b)
summary(m2b_spline)

mlist <- list(
  "Time Logit" = m2a,
  "Spline Logit" = m2a_spline,
  "Time MEOLS" = m2b,
  "Spline MEOLS" = m2b_spline
)

modelsummary::modelsummary(mlist, output = "latex")

# =============================================================================
# M3 SERIES: Opposition populism only
# =============================================================================

## Party Representation: Opposition only
m3a <- glmer(partyrep ~
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m3a)

# =============================================================================
# M4 SERIES: Incumbent populism only
# =============================================================================

## Party Representation: Incumbent only
m4a <- glmer(partyrep ~
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m4a)

# =============================================================================
# M5 SERIES: Government populism only
# =============================================================================

## Party Representation: Government only
m5a <- glmer(partyrep ~
             lag_gov_pop_within_c +
             lag_gov_pop_between_c +
             v2elloeldm_c +
             as.factor(v2elparlel) +
             party_sys_age_c +
             unemploy_t0_within_c +
             unemploy_t0_between_c +
             gini_within_c +
             gini_between_c +
             numofparties_within_c +
             numofparties_between_c +
             age_squ_c +
             lr_within_c +
             lr_between_c +
             lr_sq_within_c +
             lr_sq_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m5a)

# =============================================================================
# M6 SERIES: Opp + Gov split (full governing coalition, not just incumbent)
# =============================================================================

## Party Representation: Opp + Gov
m6a <- glmer(partyrep ~
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               lag_gov_pop_within_c +
               lag_gov_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m6a)

# =============================================================================
# M7 SERIES: Individual-level party populism only (continuous)
# =============================================================================

## Party Representation: Individual closest-party populism
m7a <- glmer(partyrep ~
             newpop_closest_within_c +
             newpop_closest_between_c +
             v2elloeldm_c +
             as.factor(v2elparlel) +
             party_sys_age_c +
             unemploy_t0_within_c +
             unemploy_t0_between_c +
             gini_within_c +
             gini_between_c +
             numofparties_within_c +
             numofparties_between_c +
             age_squ_c +
             lr_within_c +
             lr_between_c +
             lr_sq_within_c +
             lr_sq_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test, family = binomial(link = "logit"))

summary(m7a)

# =============================================================================
# M8 SERIES: Combined individual + system populism (additive, no interaction)
# =============================================================================

## Party Representation: Individual + System (opp + incumbent)
m8a <- glmer(partyrep ~
               newpop_closest_within_c +
               newpop_closest_between_c +
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               as.factor(newpop_closest_missing) +
               (1 | country_id/year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m8a)

# =============================================================================
# M9 SERIES: CROSS-LEVEL INTERACTIONS — Opposition populism x own party populism
# Does the effect of opposition populism on representation depend on
# whether the respondent's own party is populist?
# NOTE: newpop_closest is only defined for partisans — these interaction
# estimates are conditional on having a closest party.
# =============================================================================

## Party Representation: Opp populism x own party populism (linear time)
m9a <- glmer(partyrep ~
               lag_opp_pop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m9a)

## Party Representation: Opp populism x own party populism (spline time)
m9a_spline <- glmer(partyrep ~
               lag_opp_pop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              spline1 + spline2 + spline3 +
              (spline1 + spline2 + spline3 | country_id) +
              (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m9a_spline)

## Party Favorability: Opp populism x own party populism (linear time)
m9b <- lmer(party_fav ~
               lag_opp_pop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m9b)

## Party Favorability: Opp populism x own party populism (spline time)
m9b_spline <- lmer(party_fav ~
               lag_opp_pop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              spline1 + spline2 + spline3 +
              (spline1 + spline2 + spline3 | country_id) +
              (1 | country_id:year_fact),
             data = cses_test)

summary(m9b_spline)

# =============================================================================
# M10 SERIES: CROSS-LEVEL INTERACTIONS — Incumbent populism x own party populism
# =============================================================================

## Party Representation: Incumbent populism x own party populism (linear time)
m10a <- glmer(partyrep ~
               lag_ipop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_ipop_between_c +
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m10a)

## Party Favorability: Incumbent populism x own party populism (linear time)
m10b <- lmer(party_fav ~
               lag_ipop_within_c * newpop_closest_within_c +
               newpop_closest_between_c +
               lag_ipop_between_c +
               lag_opp_pop_within_c +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
              time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m10b)

# =============================================================================
# M11 SERIES: Party alignment x Opposition populism
# Does the effect of opposition populism differ by whether the respondent's
# closest party is in government, opposition, or no party?
# =============================================================================

## Party Representation: Opposition populism x party alignment (linear time)
m11a <- glmer(partyrep ~
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m11a)

## Party Favorability: Opposition populism x party alignment (linear time)
m11b <- lmer(party_fav ~
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               lag_ipop_within_c +
               lag_ipop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m11b)

# =============================================================================
# M12 SERIES: Party alignment x Incumbent populism
# Does the effect of incumbent populism differ by whether the respondent's
# closest party is in government, opposition, or no party?
# =============================================================================

## Party Representation: Incumbent populism x party alignment (linear time)
m12a <- glmer(represent ~
               lag_ipop_within_c * party_alignment_lag +
               lag_ipop_between_c +
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m12a)

## Party Favorability: Incumbent populism x party alignment (linear time)
m12b <- lmer(party_fav ~
               lag_gov_pop_within_c * party_alignment_lag +
               lag_gov_pop_between_c +
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m12b)

modelsummary(m12b, stars = T)

# =============================================================================
# M13 SERIES: BETA REGRESSION — Party favorability (bounded 0-10)
# Beta regression handles ceiling effects by modeling the outcome on (0,1)
# with a logit link, so estimates near the boundaries are naturally compressed.
# Rescale party_fav to (0,1) with Smithson-Verkuilen squeeze to avoid exact
# 0s and 1s, which beta regression cannot handle.
# =============================================================================

# Rescale party_fav to (0, 1) open interval
n_beta <- sum(!is.na(cses_test$party_fav))
cses_test$party_fav_beta <- (cses_test$party_fav / 10 * (n_beta - 1) + 0.5) / n_beta

## Party Favorability (beta): Opp + Incumbent with party alignment interactions (spline time)
m13a <- glmmTMB(party_fav_beta ~
               lag_ipop_within_c * party_alignment_lag +
               lag_ipop_between_c +
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               spline1 + spline2 + spline3 +
               (spline1 + spline2 + spline3 | country_id) +
               (1 | country_id:year_fact),
             data = cses_test,
             family = beta_family(link = "logit"))

summary(m13a)

## Party Favorability (beta): Opp + Incumbent with party alignment interactions (linear time)
m13b <- glmmTMB(party_fav_beta ~
               lag_ipop_within_c * party_alignment_lag +
               lag_ipop_between_c +
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
               (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test,
             family = beta_family(link = "logit"))

summary(m13b)

# =============================================================================
# PLACEBO: Unemployment x party alignment
# The rally effect should be specific to populism, not a generic response to
# any time-varying country condition. We estimate two models:
#   m14a — non-interactive: party alignment enters additively alongside
#          populism (no interactions), establishing the average effect
#   m14b — placebo: unemployment replaces incumbent populism as the
#          interacted variable, while populism interactions are retained
#          in the same model as a horse race
# =============================================================================

## Non-interactive: Party alignment + populism (no interactions, linear time)
m14a <- lmer(party_fav ~
               lag_ipop_within_c + lag_ipop_between_c +
               lag_opp_pop_within_c + lag_opp_pop_between_c +
               party_alignment +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               unemploy_t0_within_c +
               unemploy_t0_between_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m14a)

linearHypothesis(m14a,
                 "party_alignmentgovernment = party_alignmentopposition")

## Placebo: Unemployment x party alignment (populism interactions retained)
m14b <- lmer(party_fav ~
               unemploy_t0_within_c * party_alignment_lag +
               unemploy_t0_between_c +
               lag_ipop_within_c * party_alignment_lag +
               lag_ipop_between_c +
               lag_opp_pop_within_c * party_alignment_lag +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
              (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m14b)

m15b <- lmer(party_fav ~
               unemploy_t0_within_c * party_alignment +
               unemploy_t0_between_c +
               lag_ipop_within_c * party_alignment +
               lag_ipop_between_c +
               lag_opp_pop_within_c * party_alignment +
               lag_opp_pop_between_c +
               v2elloeldm_c +
               as.factor(v2elparlel) +
               party_sys_age_c +
               gini_within_c +
               gini_between_c +
               numofparties_within_c +
               numofparties_between_c +
               age_squ_c +
               lr_within_c +
               lr_between_c +
               lr_sq_within_c +
               lr_sq_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               time +
               (1 + time | country_id) + (1 | country_id:year_fact),
             data = cses_test)

summary(m15b)

# =============================================================================
# MODEL SUMMARIES
# =============================================================================

# Print all partyrep model summaries
summary(m1a)
summary(m2a)
summary(m3a)
summary(m4a)
summary(m5a)
summary(m6a)
summary(m7a)

# =============================================================================
# MODEL COMPARISON: AIC, BIC, AND BAYES FACTORS
# Only compare models within the same DV family (same likelihood)
# =============================================================================

# Compare partyrep (binary) models
partyrep_comparison <- compare_models(list(
  "m1a: System"             = m1a,
  "m2a: Opp+Inc (time)"    = m2a,
  "m3a: Opp only"          = m3a,
  "m4a: Inc only"          = m4a,
  "m5a: Gov only"          = m5a,
  "m6a: Opp+Gov"           = m6a,
  "m7a: Individual"        = m7a,
  "m8a: Indiv+System"      = m8a,
  "m9a: Opp x Indiv (time)" = m9a,
  "m10a: Inc x Indiv (time)" = m10a,
  "m11a: Opp x Alignment (time)" = m11a,
  "m12a: Inc x Alignment (time)" = m12a
))
print(partyrep_comparison)

# Compare party_fav (continuous) models
partyfav_comparison <- compare_models(list(
  "m0b: Baseline"            = m0b,
  "m1b: System"              = m1b,
  "m2b: Opp+Inc (time)"     = m2b,
  "m9b: Opp x Indiv (time)" = m9b,
  "m10b: Inc x Indiv (time)" = m10b,
  "m11b: Opp x Alignment (time)" = m11b,
  "m12b: Inc x Alignment (time)" = m12b
))
print(partyfav_comparison)

# Compare time trend specifications within partyrep
time_comparison_partyrep <- compare_models(list(
  "m2a: Linear time"  = m2a,
  "m2a_spline: Spline" = m2a_spline,
  "m9a: Linear time"  = m9a,
  "m9a_spline: Spline" = m9a_spline
))
print(time_comparison_partyrep)

# Compare time trend specifications within party_fav
time_comparison_partyfav <- compare_models(list(
  "m2b: Linear time"  = m2b,
  "m2b_spline: Spline" = m2b_spline,
  "m9b: Linear time"  = m9b,
  "m9b_spline: Spline" = m9b_spline
))
print(time_comparison_partyfav)

# =============================================================================
# COEFFICIENT PLOTS
# =============================================================================

plot_model_coefficients(m1a, "PartyRep: System Populism")
plot_model_coefficients(m6a, "PartyRep: Opp + Gov Populism")
plot_model_coefficients(m9a, "PartyRep: Opp x Own Party (time)")
plot_model_coefficients(m9b, "PartyFav: Opp x Own Party (time)")
plot_model_coefficients(m10a, "PartyRep: Inc x Own Party (time)")
plot_model_coefficients(m10b, "PartyFav: Inc x Own Party (time)")
plot_model_coefficients(m11a, "PartyRep: Opp x Alignment (time)")
plot_model_coefficients(m11b, "PartyFav: Opp x Alignment (time)")
plot_model_coefficients(m12a, "PartyRep: Inc x Alignment (time)")
plot_model_coefficients(m12b, "PartyFav: Inc x Alignment (time)")










broom.mixed::tidy(m12b, effects = "fixed") %>%
  filter(str_detect(term, "gov_pop|opp_pop|alignment")) %>%
  mutate(term = fct_inorder(term)) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_point(size = 2) +
  geom_pointrange(aes(xmin = estimate - 1.96 * std.error,
                      xmax = estimate + 1.96 * std.error)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Beta Regression: Populism x Party Alignment",
    subtitle = "Party Favorability (logit scale)",
    x = "Estimate (with 95% CI)",
    y = NULL
  ) +
  theme_minimal()


library(broom.mixed)

# Extract coefficients and variance-covariance matrix
coefs <- fixef(m12b)$cond
vcov_mat <- vcov(m12b)$cond

# --- Incumbent populism effect by alignment ---
gov_pop_effects <- tibble(
  group = c("No Party", "Government", "Opposition"),
  estimate = c(
    coefs["lag_gov_pop_within_c"],
    coefs["lag_gov_pop_within_c"] + coefs["lag_gov_pop_within_c:party_alignment_laggovernment"],
    coefs["lag_gov_pop_within_c"] + coefs["lag_gov_pop_within_c:party_alignment_lagopposition"]
  ),
  se = c(
    sqrt(vcov_mat["lag_gov_pop_within_c", "lag_gov_pop_within_c"]),
    sqrt(vcov_mat["lag_gov_pop_within_c", "lag_gov_pop_within_c"] +
           vcov_mat["lag_gov_pop_within_c:party_alignment_laggovernment", "lag_gov_pop_within_c:party_alignment_laggovernment"] +
           2 * vcov_mat["lag_gov_pop_within_c", "lag_gov_pop_within_c:party_alignment_laggovernment"]),
    sqrt(vcov_mat["lag_gov_pop_within_c", "lag_gov_pop_within_c"] +
           vcov_mat["lag_gov_pop_within_c:party_alignment_lagopposition", "lag_gov_pop_within_c:party_alignment_lagopposition"] +
           2 * vcov_mat["lag_gov_pop_within_c", "lag_gov_pop_within_c:party_alignment_lagopposition"])
  ),
  predictor = "Incumbent Populism"
)

# --- Opposition populism effect by alignment ---
opp_effects <- tibble(
  group = c("No Party", "Government", "Opposition"),
  estimate = c(
    coefs["lag_opp_pop_within_c"],
    coefs["lag_opp_pop_within_c"] + coefs["party_alignment_laggovernment:lag_opp_pop_within_c"],
    coefs["lag_opp_pop_within_c"] + coefs["party_alignment_lagopposition:lag_opp_pop_within_c"]
  ),
  se = c(
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"]),
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"] +
           vcov_mat["party_alignment_laggovernment:lag_opp_pop_within_c", "party_alignment_laggovernment:lag_opp_pop_within_c"] +
           2 * vcov_mat["lag_opp_pop_within_c", "party_alignment_laggovernment:lag_opp_pop_within_c"]),
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"] +
           vcov_mat["party_alignment_lagopposition:lag_opp_pop_within_c", "party_alignment_lagopposition:lag_opp_pop_within_c"] +
           2 * vcov_mat["lag_opp_pop_within_c", "party_alignment_lagopposition:lag_opp_pop_within_c"])
  ),
  predictor = "Opposition Populism"
)

# --- Combine and plot ---
bind_rows(ipop_effects, opp_effects) %>%
  mutate(group = factor(group, levels = c("No Party", "Government", "Opposition"))) %>%
  ggplot(aes(x = estimate, y = group, color = group)) +
  geom_point(size = 3) +
  geom_pointrange(aes(xmin = estimate - 1.96 * se,
                      xmax = estimate + 1.96 * se)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ predictor, ncol = 1, scales = "free_x") +
  labs(
    title = "Effect of Populism on Party Favorability by Party Alignment",
    subtitle = "Beta regression (logit scale), 95% CIs",
    x = "Marginal Effect",
    y = NULL,
    color = "Alignment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



names(fixef(m12b))


coefs <- fixef(m12b)
vcov_mat <- vcov(m12b)

# --- Incumbent populism effect by alignment ---
ipop_effects <- tibble(
  group = c("No Party", "Government", "Opposition"),
  estimate = c(
    coefs["lag_ipop_within_c"],
    coefs["lag_ipop_within_c"] + coefs["lag_ipop_within_c:party_alignment_laggovernment"],
    coefs["lag_ipop_within_c"] + coefs["lag_ipop_within_c:party_alignment_lagopposition"]
  ),
  se = c(
    sqrt(vcov_mat["lag_ipop_within_c", "lag_ipop_within_c"]),
    sqrt(vcov_mat["lag_ipop_within_c", "lag_ipop_within_c"] +
           vcov_mat["lag_ipop_within_c:party_alignment_laggovernment", "lag_ipop_within_c:party_alignment_laggovernment"] +
           2 * vcov_mat["lag_ipop_within_c", "lag_ipop_within_c:party_alignment_laggovernment"]),
    sqrt(vcov_mat["lag_ipop_within_c", "lag_ipop_within_c"] +
           vcov_mat["lag_ipop_within_c:party_alignment_lagopposition", "lag_ipop_within_c:party_alignment_lagopposition"] +
           2 * vcov_mat["lag_ipop_within_c", "lag_ipop_within_c:party_alignment_lagopposition"])
  ),
  predictor = "Incumbent Populism"
)

# --- Opposition populism effect by alignment ---
opp_effects <- tibble(
  group = c("No Party", "Government", "Opposition"),
  estimate = c(
    coefs["lag_opp_pop_within_c"],
    coefs["lag_opp_pop_within_c"] + coefs["party_alignment_laggovernment:lag_opp_pop_within_c"],
    coefs["lag_opp_pop_within_c"] + coefs["party_alignment_lagopposition:lag_opp_pop_within_c"]
  ),
  se = c(
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"]),
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"] +
           vcov_mat["party_alignment_laggovernment:lag_opp_pop_within_c", "party_alignment_laggovernment:lag_opp_pop_within_c"] +
           2 * vcov_mat["lag_opp_pop_within_c", "party_alignment_laggovernment:lag_opp_pop_within_c"]),
    sqrt(vcov_mat["lag_opp_pop_within_c", "lag_opp_pop_within_c"] +
           vcov_mat["party_alignment_lagopposition:lag_opp_pop_within_c", "party_alignment_lagopposition:lag_opp_pop_within_c"] +
           2 * vcov_mat["lag_opp_pop_within_c", "party_alignment_lagopposition:lag_opp_pop_within_c"])
  ),
  predictor = "Opposition Populism"
)

# --- Combine and plot ---
bind_rows(ipop_effects, opp_effects) %>%
  mutate(group = factor(group, levels = c("No Party", "Government", "Opposition"))) %>%
  ggplot(aes(x = estimate, y = group, color = group)) +
  geom_point(size = 3) +
  geom_pointrange(aes(xmin = estimate - 1.96 * se,
                      xmax = estimate + 1.96 * se)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  facet_wrap(~ predictor, ncol = 1, scales = "free_x") +
  labs(
    title = "Effect of Populism on Party Favorability by Party Alignment",
    subtitle = "Linear mixed model (raw scale), 95% CIs",
    x = "Marginal Effect",
    y = NULL,
    color = "Alignment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
