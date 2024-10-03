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
library(psych)


data_start <- vdem %>%
  select(country_name,
         country_id, year,
         regturn = v2eltrnout,
         vapturn = v2elvaptrn,
         compvote = v2elcomvot,
         leg_ele = v2eltype_0,
         exe_ele = v2eltype_6,
         dist_mag = v2elloeldm,
         v2x_polyarchy)

gov_party <- vparty %>%
  filter(v2pagovsup == 0) %>%
  select(country_name, party_name = v2paenname, 
         country_id, country_text_id, year, 
         v2paid,
         v2xpa_popul, v2paseatshare)

gov_data_sep <- left_join(gov_party, data_start, by = c("year", "country_id")) %>%  
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(tenure_id = cumsum(v2paid != dplyr::lag(v2paid, default = first(v2paid))) + 1) %>%
  ungroup() %>%
  group_by(country_id, tenure_id) %>%
  mutate(pop0 = first(v2xpa_popul, na_rm = T),
         ss0 = first(v2paseatshare, na_rm = T),
         term = row_number(),
         poly_first = first(v2x_polyarchy, na_rm = T)) %>%
  ungroup() %>%
  group_by(tenure_id, country_id) %>%
  mutate(poly_max = max(v2x_polyarchy, na.rm = T),
         pop_avg = mean(v2xpa_popul, na_rm = T),
         id = paste(country_id, tenure_id, sep = "_"))
  

gov_data_sep <- cbind(gov_data_sep, 
                  datawizard::demean(gov_data_sep,
                                     select = c("v2x_polyarchy", "dist_mag", "term", "exe_ele", "leg_ele"), 
                                     group = "country_id")) %>%
  filter(v2x_polyarchy >= .42)


m1 <- lmer(vapturn ~ pop0*v2x_polyarchy_within + v2x_polyarchy_between + exe_ele + leg_ele + ss0 + term_within + term_between + as.factor(compvote) + (1 | country_id/tenure_id), data = gov_data_sep)

summary(m1)


gov_data_con <- left_join(gov_party, data_start, by = c("year", "country_id")) %>%  
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(tenure_id = cumsum(v2paid != dplyr::lag(v2paid, default = first(v2paid))) + 1,
         lag_poly = dplyr::lag(v2x_polyarchy)) %>%
  ungroup() %>%
  group_by(tenure_id, country_id) %>%
  summarise(pop0 = first(v2xpa_popul, na_rm = T),
         ss0 = first(v2paseatshare, na_rm = T),
         term = row_number(),
         poly_first = first(v2x_polyarchy, na_rm = T),
         mean_vap = mean(vapturn, na_rm = T),
         comp_first = first(compvote, na_rm = T),
         poly_last = last(lag_poly, na_rm = T),
         mean_pop)

gov_data_con <- gov_data_con %>%
  group_by(tenure_id, country_id) %>%
  summarise(pop0 = first(pop0, na_rm = T),
            ss0 = first(ss0, na_rm = T),
            term = max(term),
            poly_first = first(poly_first, na_rm = T),
            mean_vap = mean(mean_vap, na_rm = T),
            comp_first = first(comp_first, na_rm = T),
            poly_last = last(poly_last, na_rm = T)) %>%
  mutate(poly_chg = poly_first - poly_last)
  


gov_data_con <- cbind(gov_data_con, 
                      datawizard::demean(gov_data_con,
                                         select = c("poly_chg", "term"), 
                                         group = "country_id")) %>%
  filter(poly_first >= .42)


system_party <- vparty %>%
  mutate(wpop = v2xpa_popul*v2paseatshare) %>%
  group_by(country_id, year) %>%
  summarise(
    sssum = sum(v2paseatshare, na.rm = T),
    system_pop = mean(v2xpa_popul, na.rm = T),
    system_pop_w = sum(wpop, na.rm = T)/sssum)


system_data <- left_join(system_party, data_start, by = c("year", "country_id")) %>%  
  arrange(country_id, year)
  

system_data <- cbind(system_data, 
                  datawizard::demean(system_data,
                                     select = c( "dist_mag", "exe_ele", "leg_ele", "system_pop", "system_pop_w"), 
                                    group = "country_id")) %>%
filter(v2x_polyarchy >= .42) %>%
  mutate(sys_sqr = system_pop*system_pop)
  

m1 <- lmer(vapturn ~ pop0*v2x_polyarchy_within + v2x_polyarchy_between + exe_ele  + leg_ele + ss0 + term_within + term_between + as.factor(compvote) + (1 | country_id/tenure_id), data = gov_data_sep)

summary(m1)

m2 <- lmer(mean_vap ~ *poly_chg_within + poly_chg_between + term_within + term_between + as.factor(comp_first) + (1 | country_id), data = gov_data_con)

summary(m2)




summary(stan_m2)

