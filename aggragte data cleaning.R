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
  select(country_name, country_id, year, regturn = v2eltrnout, vapturn = v2elvaptrn, compvote = v2elcomvot, leg_ele = v2eltype_0, exe_ele = v2eltype_6, dist_mag = v2elloeldm)

gov_party <- vparty %>%
  filter(v2pagovsup == 0) %>%
  select(country_name, party_name = v2paenname, 
         country_id, country_text_id, year, 
         v2paid, anti_ele = v2paanteli, 
         pro_ppl = v2papeople, mani = v2paopresp, 
         v2xpa_popul, v2paseatshare)


gov_data <- left_join(data_start, gov_party, by = c("year", "country_id")) %>%  
  arrange(country_id, year) %>%
  mutate(tenure_id = cumsum(v2paid != dplyr::lag(v2paid, default = first(v2paid))) + 1) %>%
  group_by(tenure_id) %>%
  summarise(country_id = first(country_id, na_rm = T),
            name = first(party_name, na_rm = T),
            e1 = first(year, na_rm = T),
            e2 = last(year, na_rm = T),
            fpop = first(v2xpa_popul, na_rm = T),
            meanpop = mean(v2xpa_popul, na_rm = T),
            fss = first(v2paseatshare, na_rm = T),
            meanss = mean(v2paseatshare, na_rm = T),
            term_num = n(),
            reg)

left_join

