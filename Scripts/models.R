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
library(ordinal)
library(data.table)
library(corrplot)

#### Load data

cses_test <- readRDS(file = "Data/cses_test.Rda")

#### Define all variables used in the model

model_vars <- c("how_rep", "lag_sys_pop_within_c", "lag_sys_pop_between_c",
                "lag_opp_pop_within_c",
                "lag_opp_pop_between_c",
                "lag_gov_pop_within_c",
                "lag_gov_pop_between_c",
                "lag_ipop_within_c",
                "lag_ipop_between_c",
                "v2elloeldm_c",
                "unemploy_t0_within_c", "unemploy_t0_between_c",
                "age_squ_c", "numofparties_c",
                "gender", "v2elparlel", "edu",
                "party_sys_age", "lr_within_c",
                "lr_between_c",
                "radical_between_c",
                "radical_within_c")

#### Filter dataset to relevant variables

summary_data <- cses_test %>% 
  select(all_of(model_vars))

#### Convert categorical variables to factors

summary_data <- summary_data %>%
  mutate(gender = as.factor(gender),
         v2elparlel = as.factor(v2elparlel),
         edu = as.factor(edu),
         how_rep_cata = as.factor(how_rep)) %>%
  na.omit()

summary(summary_data)

#### Now the same process for the dictmous question

#### Define all variables used in the model

model_vars <- c("how_rep", "lag_sys_pop_within_c", "lag_sys_pop_between_c",
                "lag_opp_pop_within_c",
                "lag_opp_pop_between_c",
                "lag_gov_pop_within_c",
                "lag_gov_pop_between_c",
                "lag_ipop_within_c",
                "lag_ipop_between_c",
                "v2elloeldm_c",
                "unemploy_t0_within_c", "unemploy_t0_between_c",
                "age_squ_c", "numofparties_c",
                "gender", "v2elparlel", "edu",
                "party_sys_age", "lr_within_c",
                "lr_between_c",
                "radical_between_c",
                "radical_within_c")
#### Filter dataset to relevant variables

summary_data <- cses_test %>% 
  select(all_of(model_vars))

#### Convert categorical variables to factors

summary_data <- summary_data %>%
  mutate(gender = as.factor(gender),
         v2elparlel = as.factor(v2elparlel),
         edu = as.factor(edu)) %>%
  na.omit()

summary(summary_data)


#### List of continuous fixed-effect variables

fixed_vars <- c("lag_opp_pop_within_c" ,
                  "lag_opp_pop_between_c" ,
                  "lag_ipop_within_c" ,
                  "lag_ipop_between_c" ,
                "lag_sys_pop_between_c",
                "lag_sys_pop_within_c",
                  "v2elloeldm_c" ,
                  "party_sys_age_c" ,
                  "unemploy_t0_within_c" ,
                  "unemploy_t0_between_c" ,
                  "gini_within_c" ,
                  "gini_between_c",
                  "numofparties_within_c" ,
                  "numofparties_between_c" ,
                  "age_squ_c" ,
                "lr_within_c",
                "lr_between_c",
                "radical_between_c",
                "radical_within_c")

#### Omit missing data

cor_data <- cses_test %>%
  select(all_of(fixed_vars)) %>%
  na.omit()


# Compute correlation matrix

cor_matrix <- cor(cor_data)

print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


#### There seems to be some correlation between worrisome measures, but not enough to be worrisome. The highly correlated measures are not in the same mode (ipop and gov, for example)

# Vector of variable names as character strings
vars <- c(
  "newpop_closest_within_c",
  "newpop_closest_between_c",
  "lag_opp_pop_within_c",
  "lag_opp_pop_between_c",
  "lag_ipop_within_c",
  "lag_ipop_between_c",
  "v2elloeldm_c",
  "v2elparlel",
  "party_sys_age_c",
  "unemploy_t0_within_c",
  "unemploy_t0_between_c",
  "gini_within_c",
  "gini_between_c",
  "numofparties_within_c",
  "numofparties_between_c",
  "age_squ_c",
  "lr_within_c",
  "lr_between_c",
  "radical_within_c",
  "radical_between_c",
  "lr_missing",
  "gender",
  "edu",
  "newpop_closest_missing"
)

# Count number of missing observations per variable
missing_counts <- sapply(vars, function(var) sum(is.na(cses_test[[var]])))

# Convert to a data frame for better readability
missing_df <- data.frame(Variable = names(missing_counts), Missing = missing_counts)

# Print the result
print(missing_df)

missing <- cses_test %>%
  filter(is.na(lag_ipop) == T)


#### Model fitting

#### Simple Baseline models

## Partisan Strength

m0 <- clmm(how_rep ~ 
             sys_pop_within_c +
             sys_pop_between_c +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m0)

## Party Representation

m0a <- glmer(partyrep ~ 
               sys_pop_within_c +
               sys_pop_between_c +
               (1 | country_id/year_fact),
             data = cses_test, family = binomial(link = "logit"))

summary(m0a)

m0b <- lmer(feel_max ~ 
               sys_pop_within_c +
               sys_pop_between_c +
               (1 | country_id/year_fact),
             data = cses_test)

summary(m0b)

broom.mixed::tidy(m0b)



## Partisan Strength : System Populism

m1 <- clmm(how_rep ~ 
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
             radical_within_c +
             radical_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m1)

## Party Representation : System Populism


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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test, family = binomial(link = "logit"))

summary(m1a)


m1b <- lmer(feel_max ~ 
               sys_pop_within_c +
               sys_pop_between_c +
               as.factor(v2elparlel) +
               v2elloeldm_c +
               numofparties_c +
               party_sys_age_c +
               age_squ_c +
               lr +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
             data = cses_test)

summary(m1b)

## Partisan Strength: System populism- split between opposition and government

m2 <- clmm(how_rep ~
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
             radical_within_c +
             radical_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)
summary(m2)

## Party Representation: System populism- split between opposition and government

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
             radical_within_c +
             radical_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test, family = binomial(link = "logit"))

summary(m2a)


m2b <- lmer(feel_max ~
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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
             data = cses_test)

summary(m2b)

## Partisan Strength: Opposition only

m3 <- clmm(how_rep ~ 
             opp_pop_within_c +
             opp_pop_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m3)

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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
           data = cses_test, 
           family = binomial(link = "logit"))

summary(m3a)

## Partisan Strength: Incumbent Only

m4 <- clmm(how_rep ~
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
             radical_within_c +
             radical_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m4)

## Party Representation: Incumbent Only

m4a <- glmer(partyrep ~ lag_ipop_within_c +
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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m4a)

## Partisan Strength: Government only

m5 <- clmm(how_rep ~ gov_pop_within_c +
             gov_pop_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

summary(m5)

## Party Representation: Government only

m5a <- glmer(how_rep ~
             gov_pop_within_c +
             gov_pop_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m5a)

## Partisan Strength: System populism- Split between opposition and government

m6 <- clmm(how_rep ~ 
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
             radical_within_c +
             radical_between_c +
             as.factor(lr_missing) +
             as.factor(gender) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m6)

## Party Representation: System populism- Split between opposition and government

m6a <- glmer(how_rep ~ 
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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test,
           family = binomial(link = "logit"))

summary(m6a)

#### Partisan Strength: Indy Model

m7 <- clmm(how_rep ~ 
             newpop_closest_within_c +
             newpop_closest_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test)

summary(m7)

#### Party Representation: Indy Model

m7a <- glmer(partyrep ~ 
             newpop_closest_within_c +
             newpop_closest_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1 | country_id/year_fact),
           data = cses_test, family = binomial(link = "logit"))

#### Party Representation: Indy-system mix model

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
               radical_within_c +
               radical_between_c +
               as.factor(lr_missing) +
               as.factor(gender) +
               as.factor(edu) +
               as.factor(newpop_closest_missing) +
               (1 | country_id/year_fact),
             data = cses_test, family = binomial(link = "logit"))


summary(m8a)


summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
summary(m7)


summary(m1a)
summary(m2a)
summary(m3a)
summary(m4a)
summary(m5a)
summary(m6a)
summary(m7a)


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

plot_model_coefficients(m1a, "PartyRep: System Populism")
plot_model_coefficients(m6, "HowRep: System Populism")


plots <- list(
  plot_model_coefficients(model_partyrep_syspop, "PartyRep: System Populism"),
  plot_model_coefficients(model_partyrep_govopp, "PartyRep: Gov & Opp Populism"),
  plot_model_coefficients(model_partyrep_opposition, "PartyRep: Opposition Populism"),
  plot_model_coefficients(model_howrep_syspop, "HowRep: System Populism"),
  plot_model_coefficients(model_howrep_govopp, "HowRep: Gov & Opp Populism"),
  plot_model_coefficients(model_howrep_opposition, "HowRep: Opposition Populism")
)





