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
  select(country_name, country_id, year, regturn = v2eltrnout, vapturn = v2elvaptrn, compvote = v2elcomvot, leg_ele = v2eltype_0, exe_ele = v2eltype_6, dist_mag = v2elloeldm, v2x_polyarchy)

gov_party <- vparty %>%
  filter(v2pagovsup == 0) %>%
  select(country_name, party_name = v2paenname, 
         country_id, country_text_id, year, 
         v2paid, anti_ele = v2paanteli, 
         pro_ppl = v2papeople, mani = v2paopresp, 
         v2xpa_popul, v2paseatshare)


gov_data <- left_join(gov_party, data_start, by = c("year", "country_id")) %>%  
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(tenure_id = cumsum(v2paid != dplyr::lag(v2paid, default = first(v2paid))) + 1, 
         last_reg = dplyr::lead(regturn), last_vap = dplyr::lead(vapturn),
         last_le = dplyr::lead(leg_ele),
         last_ee = dplyr::lead(exe_ele),
         last_poly = dplyr::lead(v2x_polyarchy)) %>%
  group_by(tenure_id, country_id) %>%
  summarise(country_id = first(country_id, na_rm = T),
            name = first(party_name, na_rm = T),
            e1 = first(year, na_rm = T),
            e2 = last(year, na_rm = T),
            fpop = first(v2xpa_popul, na_rm = T),
            meanpop = mean(v2xpa_popul, na_rm = T),
            fss = first(v2paseatshare, na_rm = T),
            meanss = mean(v2paseatshare, na_rm = T),
            term_num = n(),
            freg = first(regturn),
            lreg = last(last_reg, na_rm = T),
            fvap = first(vapturn),
            lvap = last(last_vap, na_rm = T),
            compvote = first(compvote, na_rm = T),
            fle = first(leg_ele),
            lle = last(last_le),
            fee = first(exe_ele),
            lee = last(last_ee),
            fdismag = first(dist_mag),
            fpoly = first(v2x_polyarchy),
            lpoly = last(last_poly)) %>%
  mutate(reg_chg = lreg - freg,
         vap_chg = lvap - fvap,
         poly_chg = lpoly - fpoly)

data_start <- vdem %>%
  select(country_name, country_id, year, regturn = v2eltrnout, vapturn = v2elvaptrn, compvote = v2elcomvot, leg_ele = v2eltype_0, exe_ele = v2eltype_6, dist_mag = v2elloeldm, v2x_polyarchy)

gov_party <- vparty %>%
  filter(v2pagovsup == 0) %>%
  select(country_name, party_name = v2paenname, 
         country_id, country_text_id, year, 
         v2paid, anti_ele = v2paanteli, 
         pro_ppl = v2papeople, mani = v2paopresp, 
         v2xpa_popul, v2paseatshare)

m1 <- lm(vap_chg ~ fpop*poly_chg + fle + fee + fdismag + meanss + as.factor(country_id), data = gov_data)
summary(m1)
m1$
