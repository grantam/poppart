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
library(countrycode)

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

party_final <- party_final %>%
  arrange(country_id, year) %>% 
  group_by(country_id) %>%
  mutate(
    lag_ipop = dplyr::lag(ipop),
    lag_opp_pop =dplyr:: lag(opp_pop),
    lag_sys_pop = dplyr::lag(sys_pop),
    lag_gov_pop = dplyr::lag(gov_pop),
    lag_year = dplyr::lag(year)
  ) %>%
  ungroup()

# --- Dataset expansion to include non-election years ---

n_obs <- 23600
df <- data.frame(id = 1:n_obs)

df <- df %>%
  mutate(group_id = id / 100,
         country_id = ceiling(group_id)) %>%
  arrange(country_id) %>%
  group_by(country_id) %>%
  mutate(year = 1924 + (row_number() - 1)) %>%
  ungroup() %>%
  select(-id, -group_id)

# Merge with expanded df
df <- left_join(df, party_final, by = c("country_id", "year")) 

# Fill missing values within party groups
vars_to_fill <- c("country_name", "country_text_id", "sys_pop", "ipop", "v2pariglef", "opp_pop", "gov_pop", "v2paenname", "lag_sys_pop", "lag_ipop", "lag_gov_pop", "lag_opp_pop")

setDT(df)
df <- df[order(country_id, year)]
df[, (vars_to_fill) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)), 
   by = country_id, .SDcols = vars_to_fill] 

df <- df %>% 
  filter(is.na(country_name) == F) %>%
  select(country_id, country_name, country_text_id, year, sys_pop, v2paenname, ipop, opp_pop, gov_pop, v2pariglef, lag_sys_pop, lag_ipop, lag_gov_pop, lag_opp_pop)


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
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel, ccodecow = COWcode, v2ex_elechog, v2exhoshog, v2ex_elechos) %>%
  mutate(hog_direct = ifelse(v2exhoshog == 1, v2ex_elechos, v2ex_elechog)) %>%
  group_by(country_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    reset = v2x_polyarchy < 0.42,
    reset_group = cumsum(lag(reset, default = TRUE)),
    party_sys_age = sequence(rle(reset_group)$lengths) - 1
  ) %>%
  ungroup()

incomeineq <- read_csv("incomeineq.csv") %>%
  select(year, country, gini = gini_disp) %>%
  mutate(ccodecow = countrycode(country, origin = "country.name", destination = "cown"))

inst <- left_join(inst, incomeineq, by = c("year", "ccodecow"))

qog <- read.csv("C:/Users/ochoc/Downloads/qog_bas_ts_jan25.csv") %>%
  select(year, ccodecow, numofparties = gol_enep)

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
         IMD5103_A:IMD5103_I,
         unemploy_t0 = IMD5054_2,
         gdpg_t0 = IMD5052_2,
         gdppc_t0 = IMD5053_2,
         ses = IMD2016
         )%>%
  mutate(close = ifelse(close > 1, NA, close), 
         date = as.Date(r1date, format = "%Y-%m-%d"),
         year = year(date), 
         represent = ifelse(close == 0 & closer == 0, 0, 1),
         how_close = ifelse(represent == 0, 0, how_close),
         how_rep = ifelse(close == 0 & closer == 0, 0, how_close),
         how_rep = ifelse(how_rep > 3, NA, how_rep),
         partyrep = as.factor(ifelse(partyrep > 1, NA, partyrep)),
         unemploy_t0 = ifelse(unemploy_t0 > 100, NA, unemploy_t0),
         ses = as.factor(ifelse(ses > 5, NA, ses)),
         gdppc_t0 = ifelse(gdppc_t0 > 899999, NA, gdppc_t0),
         gdppc_t0 = ifelse(gdpg_t0 > 30, NA, gdppc_t0),
         age = ifelse(age > 120, NA, age),
         edu = ifelse(edu > 6, NA, edu),
         gender = ifelse(gender > 2, NA, gender),
         lr = ifelse(lr > 10, NA, lr),
         lr_missing = as.factor(ifelse(is.na(lr) == T, 1, 0)),
         lr = lr - 5,
         lr = ifelse(lr_missing == 1, 0, lr),
         col = abs(lr))


cses_clean1 <- left_join(cses_clean, df, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean1a <- left_join(cses_clean1, inst, by = c("year", "country_id"))

cses_clean2 <- left_join(cses_clean1a, qog, by = c("year", "ccodecow"))

cses_clean3 <- cses_clean2 %>%
  mutate(
    closest_party = case_when(
      who == IMD5000_A ~ IMD5103_A,
      who == IMD5000_B ~ IMD5103_B,
      who == IMD5000_C ~ IMD5103_H,
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
      TRUE ~ NA_integer_))


df_ind1 <- df_ind %>%
  rename(closest_party = pf_party_id) %>%
  select(year, closest_party, country_id, newpop_closest = newpop, v2paenname_closest = v2paenname)

df_ind2 <- df_ind %>%
  rename(voted_party = pf_party_id) %>%
  select(year, voted_party, country_id, newpop_voted = newpop, v2paenname_voted = v2paenname)

cses_clean4 <- left_join(cses_clean3, df_ind1, by = c("year", "closest_party", "country_id"))

cses_clean5 <- left_join(cses_clean4, df_ind2, by = c("year", "voted_party", "country_id"))

cses_demeaned <- demean(cses_clean5, select = c("sys_pop", "ipop", "v2pariglef", "gov_pop", "opp_pop", "unemploy_t0", "gdpg_t0", "gdppc_t0", "lag_sys_pop", "lag_ipop", "lag_gov_pop", "lag_opp_pop", "gini", "numofparties"), by =  "country_id") %>%
  mutate(age_squ = age*age,
         countryear = as.factor(paste0(country_text_id, year)))
  
cses_demeaned1 <- demean(cses_demeaned, select = c("age", "age_squ", "newpop_voted", "newpop_closest", "ses", "lr"), by = c("countryear"))

cses_test <- cses_demeaned1 %>%
  mutate(
    how_rep = as.factor(how_rep),
    represent = as.factor(represent),
    date = as.factor(r1date),
    year_fact = as.factor(year)
  ) %>%
  filter(gender != 3) %>%
  mutate(across(
    c(opp_pop_within, opp_pop_between,
      ipop_within, ipop_between,
      v2pariglef_within, v2pariglef_between,
      age_within, age_between,
      age_squ_within, age_squ_between,
      v2elloeldm,
      gov_pop_within, gov_pop_between,
      sys_pop_within, sys_pop_between,
      numofparties,
      newpop_closest_within, newpop_closest_between,
      newpop_voted_within, newpop_voted_between,
      age_squ,
      lr_within, lr_between,
      lag_sys_pop_within, lag_ipop_within, lag_gov_pop_within, lag_opp_pop_within, lag_sys_pop_between, lag_ipop_between, lag_gov_pop_between, lag_opp_pop_between,
      party_sys_age,
      numofparties_within,
      numofparties_between,
      gini_between,
      gini_within,
      unemploy_t0_between,
      unemploy_t0_within,
      lr,
      radical
    ),
    ~ (. - mean(., na.rm = TRUE)) / (2 * sd(., na.rm = TRUE)),
    .names = "{.col}_c"
  )) %>%
  mutate()



cses_test$year_fact <- as.factor(cses_test$year_fact)
cses_test$country_id <- as.factor(cses_test$country_id)
cses_test$gender <- as.factor(cses_test$gender)
cses_test$v2elparlel <- as.factor(cses_test$v2elparlel)
cses_test$edu <- as.factor(cses_test$edu)
cses_test$how_rep <- as.factor(cses_test$how_rep)


saveRDS(cses_test, file = "cses_test.Rda")
