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


populists <- read_dta("C:/Users/ochoc/Dropbox/Populism Book/03 Data/00 World Politics/Now the People Rule/Important Do Files/separate_data_set.dta") %>%
  mutate(term_character = as.character(term),
         leader_id = paste(leader, term_character))

party <- vparty %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = v2xpa_popul*v2pavote) %>%
  summarize(votesum = sum(v2pavote, na.rm = T),
         sys_pop = sum(wpop, na.rm = T)/votesum)

incumbent <- vparty %>%
  group_by(country_id) %>%
  select(country_id, v2paenname, year, v2pagovsup, v2xpa_popul) %>%
  filter(v2pagovsup == 0) %>%
  mutate(ipop = lag(v2xpa_popul))

party <- left_join(party, incumbent, by = c("year", "country_id"))

party_A <- vparty %>%
  select(year, pop_A = v2xpa_popul, IMD5103_A = pf_party_id, vote_A = v2pavote) %>%
  mutate(vote_A = vote_A/100)

party_B <- vparty %>%
  select(year, pop_B = v2xpa_popul, IMD5103_B = pf_party_id, vote_B = v2pavote) %>%
  mutate(vote_B = vote_B/100)

party_C <- vparty %>%
  select(year, pop_C = v2xpa_popul, IMD5103_C = pf_party_id, vote_C = v2pavote) %>%
  mutate(vote_C = vote_C/100)

party_D <- vparty %>%
  select(year, pop_D = v2xpa_popul, IMD5103_D = pf_party_id, vote_D = v2pavote) %>%
  mutate(vote_D = vote_D/100)

party_E <- vparty %>%
  select(year, pop_E = v2xpa_popul, IMD5103_E = pf_party_id, vote_E = v2pavote) %>%
  mutate(vote_E = vote_E/100)

party_F <- vparty %>%
  select(year, pop_F = v2xpa_popul, IMD5103_F = pf_party_id, vote_F = v2pavote) %>%
  mutate(vote_F = vote_F/100)

party_G <- vparty %>%
  select(year, pop_G = v2xpa_popul, IMD5103_G = pf_party_id, vote_G = v2pavote) %>%
  mutate(vote_G = vote_G/100)

party_H <- vparty %>%
  select(year, pop_H = v2xpa_popul, IMD5103_H = pf_party_id, vote_H = v2pavote) %>%
  mutate(vote_H = vote_H/100)

party_I <- vparty %>%
  select(year, pop_I = v2xpa_popul, IMD5103_I = pf_party_id, vote_I = v2pavote) %>%
  mutate(vote_I = vote_I/100)

inst <- vdem %>%
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel)

cses <- read.csv("C:/Users/ochoc/Downloads/cses_imd.csv")

cses_clean <- cses %>%
  select(eleid = IMD1003, country_id = IMD1006_VDEM, country = IMD1006_NAM, iso = IMD1006_UNALPHA3, eletype = IMD1009, resid = IMD1008_RES, sampweight = IMD1010_1, demweight = IMD1010_2, polweight = IMD1010_3, r1date = IMD1011_1, r2date = IMD1012_1, age = IMD2001_1, age_cat = IMD2001_2, gender = IMD2002, edu = IMD2003, income = IMD2006, region = IMD2008, race = IMD2010, employ = IMD2014, turnout_main = IMD3001, turnout_r1p = IMD3001_PR_1, turnout_r2p = IMD3001_PR_2, turnout_lh = IMD3001_LH, turnout_uh = IMD3001_UH, turnout_switch = IMD3001_TS, choice_pr1 = IMD3002_PR_2, choice_pr2 = IMD3002_LH_PL, choice_swtich = IMD3002_VS_1, choice_ideo = IMD3002_IF_CSES, close = IMD3005_1, closer = IMD3005_2, who = IMD3005_3, lr = IMD3006, IMD3008_A:IMD3009_I, demsat = IMD3010, partyrep = IMD3016_1, bestrep = IMD3016_2, IMD5000_A:IMD5000_I, IMD5001_A:IMD5001_I, rvt = IMD5006_1, vapt = IMD5006_2, compvote = IMD5007, IMD5052_1:IMD5056_3, IMD5058_1, numofparties  = IMD5058_2, IMD5103_A:IMD5103_I) %>%
    mutate(date = as.Date(r1date, format = "%Y-%m-%d"),
           year = year(date))

cses_clean <- left_join(cses_clean, party, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean <- left_join(cses_clean, inst, by = c("year", "country_id"))

cses_demeaned <- demean(cses_clean, select = c("sys_pop", "ipop"), group = "country_id")

cses_clean <- cbind(cses_clean, cses_demeaned)

cses_test <- cses_clean %>%
  filter(turnout_main <=1, gender <= 3, age <= 120, edu <= 4)

cses_test <- left_join(cses_test, party_A, by = c("year", "IMD5103_A"))
cses_test <- left_join(cses_test, party_B, by = c("year", "IMD5103_B"))
cses_test <- left_join(cses_test, party_C, by = c("year", "IMD5103_C"))
cses_test <- left_join(cses_test, party_D, by = c("year", "IMD5103_D"))
cses_test <- left_join(cses_test, party_E, by = c("year", "IMD5103_E"))
cses_test <- left_join(cses_test, party_F, by = c("year", "IMD5103_F"))
cses_test <- left_join(cses_test, party_G, by = c("year", "IMD5103_G"))
cses_test <- left_join(cses_test, party_H, by = c("year", "IMD5103_H"))
cses_test <- left_join(cses_test, party_I, by = c("year", "IMD5103_I"))

calc_weighted_diff <- function(base_var, data) {
  base_IMD <- paste0("IMD3008_", base_var)
  base_vote <- paste0("vote_", base_var)
  
  other_IMDs <- setdiff(names(select(data, starts_with("IMD3008_"))), base_IMD)
  other_votes <- setdiff(names(select(data, starts_with("vote_"))), base_vote)
  
  rowSums(
    mapply(function(IMD_col, vote_col) {
      diff = data[[base_IMD]] - data[[IMD_col]]
      weight = data[[vote_col]] * (1 - data[[base_vote]])
      ifelse(is.na(diff), NA, diff * weight)
    },
    IMD_col = other_IMDs,
    vote_col = other_votes
    ),
    na.rm = TRUE
  )
}

# Apply the function to each variable from A to I
partyscore <- cses_test %>%
  dplyr::select(IMD3008_A:IMD3008_I, vote_A:vote_I) %>%
  mutate(
    dif_A = calc_weighted_diff("A", .),
    dif_B = calc_weighted_diff("B", .),
    dif_C = calc_weighted_diff("C", .),
    dif_D = calc_weighted_diff("D", .),
    dif_E = calc_weighted_diff("E", .),
    dif_F = calc_weighted_diff("F", .),
    dif_G = calc_weighted_diff("G", .),
    dif_H = calc_weighted_diff("H", .),
    dif_I = calc_weighted_diff("I", .)
  )

cses_test <- cses_test %>%
  select(eleid:lr, demsat:ipop_within)

cses_test <- cbind(cses_test, partyscore) %>%
  mutate(polar = NA,
         polar = ifelse(who == IMD5000_A, dif_A, polar),
         polar = ifelse(who == IMD5000_B, dif_B, polar),
         polar = ifelse(who == IMD5000_C, dif_C, polar),
         polar = ifelse(who == IMD5000_D, dif_D, polar),
         polar = ifelse(who == IMD5000_E, dif_E, polar),
         polar = ifelse(who == IMD5000_F, dif_F, polar),
         polar = ifelse(who == IMD5000_G, dif_G, polar),
         polar = ifelse(who == IMD5000_H, dif_H, polar),
         polar = ifelse(who == IMD5000_I, dif_I, polar))

country_level <- cses_test %>%
  group_by(country_name, year) %>%
  summarize(mean_polar = mean(polar, na.rm = T))
