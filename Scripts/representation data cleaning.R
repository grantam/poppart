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

# --- Original calculations on vparty ---

vparty2 <- vparty %>%
  mutate(medianosp_flip = 4 - v2paopresp_osp,
         vparty_medianharm = 3 / ((1 / (v2paanteli_osp + 1)) + 
                                    (1 / (v2papeople_osp + 1)) + 
                                    (1 / (medianosp_flip + 1))),
         newpop = (vparty_medianharm - 1) / 4)

party <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            sys_pop = sum(wpop, na.rm = TRUE) / votesum)

opp <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  filter(v2pagovsup == 3) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            opp_pop = sum(wpop, na.rm = TRUE) / votesum)

gov <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  filter(v2pagovsup <= 2) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            gov_pop = sum(wpop, na.rm = TRUE) / votesum)

incumbent <- vparty2 %>%
  group_by(country_id) %>%
  select(country_id, v2paenname, year, v2pagovsup, newpop, v2pariglef) %>%
  filter(v2pagovsup == 0) %>%
  mutate(ipop = lag(newpop))

party3 <- left_join(party, incumbent, by = c("year", "country_id"))
party4 <- left_join(party3, opp, by = c("year", "country_id"))
party_final <- left_join(party4, gov, by = c("year", "country_id"))

# --- Dataset expansion to include non-election years ---

n_obs <- 23600  # Highest party id times 100
df <- data.frame(id = 1:n_obs)

df <- df %>%
  mutate(group_id = id / 100,
         country_id = ceiling(group_id)) %>%
  arrange(country_id) %>%
  group_by(country_id) %>%
  mutate(year = 1924 + (row_number() - 1)) %>%
  ungroup() %>%
  select(-id, -group_id)

# Merge with party data (placeholder: use your actual data source)
# Here we assume `newparty` is already loaded or built from `vparty`
# For now, we simulate it by filtering vparty2 (you should replace this part)

# Merge with expanded df
df <- left_join(df, party_final, by = c("country_id", "year")) 

# Fill missing values within party groups
vars_to_fill <- c("country_name", "country_text_id", "sys_pop", "ipop", "v2pariglef", "opp_pop", "gov_pop", "v2paenname")

setDT(df)
df <- df[order(country_id, year)]
df[, (vars_to_fill) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)), 
   by = country_id, .SDcols = vars_to_fill] 

df <- df %>% 
  filter(is.na(country_name) == F) %>%
  select(country_id, country_name, country_text_id, year, sys_pop, v2paenname, ipop, opp_pop, gov_pop, v2pariglef)


#### Individual Level

n_obs <- 2602678  # Highest party id times 100
df_ind <- data.frame(id = 1:n_obs)

df_ind <- df_ind %>%
  mutate(group_id = id / 26,
         v2paid = ceiling(group_id)) %>%
  arrange(v2paid) %>%
  group_by(v2paid) %>%
  mutate(year = 1996 + (row_number() - 1)) %>%
  ungroup() %>%
  select(-id, -group_id)

vparty3 <- vparty2 %>%
  select(v2paid, year, newpop, country_name, country_text_id, v2paenname, pf_party_id, country_id)

df_ind <- left_join(df_ind, vparty3, by = c("v2paid", "year")) 

# Fill missing values within party groups
vars_to_fill <- c("country_name", "country_text_id", "newpop", "v2paenname", "pf_party_id", "country_id")

setDT(df_ind)
df_ind <- df_ind[order(v2paid, year)]
df_ind[, (vars_to_fill) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)), 
   by = v2paid, .SDcols = vars_to_fill] 

df_ind <- df_ind %>% 
  filter(is.na(country_name) == F) %>%
  select(v2paenname, v2paid, country_name, country_text_id, year, newpop, pf_party_id, country_id)


inst <- vdem %>%
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel)

cses <- read.csv("C:/Users/ochoc/Downloads/cses_imd.csv")

cses_clean <- cses %>%
  select(eleid = IMD1003,
         country_id = IMD1006_VDEM,
         country = IMD1006_NAM,
         iso = IMD1006_UNALPHA3,
         eletype = IMD1009,
         resid = IMD1008_RES,
         sampweight = IMD1010_1,
         demweight = IMD1010_2,
         polweight = IMD1010_3,
         r1date = IMD1011_1,
         r2date = IMD1012_1,
         age = IMD2001_1,
         age_cat = IMD2001_2,
         gender = IMD2002,
         edu = IMD2003,
         income = IMD2006,
         region = IMD2008,
         race = IMD2010,
         employ = IMD2014,
         turnout_main = IMD3001,
         turnout_r1p = IMD3001_PR_1,
         turnout_r2p = IMD3001_PR_2,
         turnout_lh = IMD3001_LH,
         turnout_uh = IMD3001_UH,
         choice_lh2 = IMD3002_LH_PL,
         choice_swtich = IMD3002_VS_1,
         choice_ideo = IMD3002_IF_CSES,
         close = IMD3005_1,
         closer = IMD3005_2,
         who = IMD3005_3,
         how_close = IMD3005_4,
         lr = IMD3006,
         IMD3008_A:IMD3009_I,
         demsat = IMD3010,
         partyrep = IMD3016_1,
         bestrep = IMD3016_2,
         IMD5000_A:IMD5000_I,
         IMD5001_A:IMD5001_I,
         rvt = IMD5006_1,
         vapt = IMD5006_2,
         compvote = IMD5007,
         IMD5052_1:IMD5056_3,
         IMD5058_1,
         numofparties = IMD5058_2,
         IMD5103_A:IMD5103_I) %>%
  filter(close <= 1) %>%
  mutate(date = as.Date(r1date, format = "%Y-%m-%d"),
         year = year(date), 
         represent = ifelse(close == 0 & closer == 0, 0, 1),
         how_close = ifelse(represent == 0, 0, how_close),
         how_rep = ifelse(close == 0 & closer == 0, 0, how_close),
         how_rep = ifelse(how_rep > 3, NA, how_rep))


cses_clean1 <- left_join(cses_clean, df, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean2 <- left_join(cses_clean1, inst, by = c("year", "country_id"))

cses_clean3 <- cses_clean2 %>%
  mutate(
    closest_party = case_when(
      who == IMD5000_A ~ IMD5103_A,
      who == IMD5000_B ~ IMD5103_B,
      who == IMD5000_C ~ IMD5103_C,
      who == IMD5000_D ~ IMD5103_D,
      who == IMD5000_E ~ IMD5103_E,
      who == IMD5000_F ~ IMD5103_F,
      who == IMD5000_G ~ IMD5103_G,
      who == IMD5000_H ~ IMD5103_H,
      who == IMD5000_I ~ IMD5103_I,
      TRUE ~ NA_integer_
    ),
    voted_party = case_when(
      choice_lh2 == IMD5000_A ~ IMD5103_A,
      choice_lh2 == IMD5000_B ~ IMD5103_B,
      choice_lh2 == IMD5000_C ~ IMD5103_C,
      choice_lh2 == IMD5000_D ~ IMD5103_D,
      choice_lh2 == IMD5000_E ~ IMD5103_E,
      choice_lh2 == IMD5000_F ~ IMD5103_F,
      choice_lh2 == IMD5000_G ~ IMD5103_G,
      choice_lh2 == IMD5000_H ~ IMD5103_H,
      choice_lh2 == IMD5000_I ~ IMD5103_I,
      TRUE ~ NA_integer_
    ))

df_ind1 <- df_ind %>%
  rename(closest_party = pf_party_id) %>%
  select(year, closest_party, country_id, newpop_closest = newpop, v2paenname_closest = v2paenname)

df_ind2 <- df_ind %>%
  rename(voted_party = pf_party_id) %>%
  select(year, voted_party, country_id, newpop_voted = newpop, v2paenname_voted = v2paenname)

cses_clean4 <- left_join(cses_clean3, df_ind1, by = c("year", "closest_party", "country_id"))

cses_clean5 <- left_join(cses_clean4, df_ind2, by = c("year", "voted_party", "country_id"))

cses_demeaned <- demean(cses_clean5, select = c("sys_pop", "ipop", "v2elloeldm", "v2pariglef", "gov_pop", "opp_pop"), by =  "country_id")

cses_test <- cses_demeaned %>%
  filter(gender <= 3, age <= 120) %>%
  mutate(how_rep = as.factor(how_rep),
         represent = as.factor(represent)) %>%
  mutate(date= as.factor(r1date),
         year_fact = as.factor(year)) %>%
  filter(gender != 3) %>%
  mutate(opp_pop_within_c = ((opp_pop_within - mean(opp_pop_within, na.rm = T))/(2*sd(opp_pop_within, na.rm = T))),
         opp_pop_between_c = ((opp_pop_between - mean(opp_pop_between, na.rm = T))/(2*sd(opp_pop_between, na.rm = T))),
         ipop_within_c = ((ipop_within - mean(ipop_within, na.rm = T))/(2*sd(ipop_within, na.rm = T))),
         ipop_between_c = ((ipop_between - mean(ipop_between, na.rm = T))/(2*sd(ipop_between, na.rm = T))),
         v2pariglef_within_c = ((v2pariglef_within - mean(v2pariglef_within, na.rm = T))/(2*sd(v2pariglef_within, na.rm = T))),
         v2pariglef_between_c = ((v2pariglef_between - mean(v2pariglef_between, na.rm = T))/(2*sd(v2pariglef_between, na.rm = T))),
         age_squ = age*age,
         age_c = ((age - mean(age, na.rm = T))/(2*sd(age, na.rm = T))),
         age_squ_c = ((age_squ - mean(age_squ, na.rm = T))/(2*sd(age_squ, na.rm = T))),
         v2elloeldm_within_c = ((v2elloeldm_within - mean(v2elloeldm_within, na.rm = T))/(2*sd(v2elloeldm_within, na.rm = T))),
         v2elloeldm_between_c = ((v2elloeldm_between - mean(v2elloeldm_between, na.rm = T))/(2*sd(v2elloeldm_between, na.rm = T))),
         gov_pop_within_c = ((gov_pop_within - mean(gov_pop_within, na.rm = T))/(2*sd(gov_pop_within, na.rm = T))),
         sys_pop_within_c = ((sys_pop_within - mean(sys_pop_within, na.rm = T))/(2*sd(sys_pop_within, na.rm = T))),
         gov_pop_between_c = ((gov_pop_between - mean(gov_pop_between, na.rm = T))/(2*sd(gov_pop_between, na.rm = T))),
         sys_pop_between_c = ((sys_pop_between - mean(sys_pop_between, na.rm = T))/(2*sd(sys_pop_between, na.rm = T))),
         numofparties_c = ((numofparties - mean(numofparties, na.rm = T))/(2*sd(numofparties, na.rm = T))))


cses_test$year_fact <- as.factor(cses_test$year_fact)
cses_test$country_id <- as.factor(cses_test$country_id)
cses_test$gender <- as.factor(cses_test$gender)
cses_test$v2elparlel <- as.factor(cses_test$v2elparlel)
cses_test$edu <- as.factor(cses_test$edu)
cses_test$how_rep <- as.factor(cses_test$how_rep)


save(cses_test, file = "cses_test.Rda")