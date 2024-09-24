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
  select(country_id, country_text_id, year, anti_ele = v2paanteli, pro_ppl = v2papeople, mani = v2paopresp, v2xpa_popul, v2paseatshare) %>%
  mutate(xi = 1-.25*(pro_ppl), yi = 1-.25*(anti_ele), zi = 1-.25*(mani), populism = 2/((1/xi) + (1/yi) + (1/zi)), repop = 2/((1/xi) + (1/yi)))


summary(gov_party)
