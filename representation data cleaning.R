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


populists <- read_dta("C:/Users/ochoc/Dropbox/Populism Book/03 Data/00 World Politics/Now the People Rule/Important Do Files/separate_data_set.dta") %>%
  mutate(term_character = as.character(term),
         leader_id = paste(leader, term_character))

vparty2 <- vparty %>%
  mutate(medianosp_flip = 4-v2paopresp_osp,
         vparty_medianharm = 3/((1/(v2paanteli_osp+1))+(1/(v2papeople_osp+1))+(1/(medianosp_flip+1))),
         newpop =(vparty_medianharm-1)/4)

party <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id, v2paenname) %>%
  mutate(wpop = newpop*v2paseatshare) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = T),
            sys_pop = sum(wpop, na.rm = T)/votesum)

opp <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop*v2paseatshare) %>%
  filter(v2pagovsup == 3) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = T),
            opp_pop = sum(wpop, na.rm = T)/votesum)

gov <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop*v2paseatshare) %>%
  filter(v2pagovsup >= 2) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = T),
            opp_pop = sum(wpop, na.rm = T)/votesum)


incumbent <- vparty2 %>%
  group_by(country_id) %>%
  select(country_id, v2paenname, year, v2pagovsup, newpop, v2pariglef) %>%
  filter(v2pagovsup == 0) %>%
  mutate(ipop = lag(newpop))

party <- left_join(party, incumbent, by = c("year", "country_id"))

party_A <- vparty2 %>%
  select(year, pop_A = newpop, IMD5103_A = pf_party_id, vote_A = v2pavote) %>%
  mutate(vote_A = vote_A/100)

party_B <- vparty2 %>%
  select(year, pop_B = newpop, IMD5103_B = pf_party_id, vote_B = v2pavote) %>%
  mutate(vote_B = vote_B/100)

party_C <- vparty2 %>%
  select(year, pop_C = newpop, IMD5103_C = pf_party_id, vote_C = v2pavote) %>%
  mutate(vote_C = vote_C/100)

party_D <- vparty2 %>%
  select(year, pop_D = newpop, IMD5103_D = pf_party_id, vote_D = v2pavote) %>%
  mutate(vote_D = vote_D/100)

party_E <- vparty2 %>%
  select(year, pop_E = newpop, IMD5103_E = pf_party_id, vote_E = v2pavote) %>%
  mutate(vote_E = vote_E/100)

party_F <- vparty2 %>%
  select(year, pop_F = newpop, IMD5103_F = pf_party_id, vote_F = v2pavote) %>%
  mutate(vote_F = vote_F/100)

party_G <- vparty2 %>%
  select(year, pop_G = newpop, IMD5103_G = pf_party_id, vote_G = v2pavote) %>%
  mutate(vote_G = vote_G/100)

party_H <- vparty2 %>%
  select(year, pop_H = newpop, IMD5103_H = pf_party_id, vote_H = v2pavote) %>%
  mutate(vote_H = vote_H/100)

party_I <- vparty2 %>%
  select(year, pop_I = newpop, IMD5103_I = pf_party_id, vote_I = v2pavote) %>%
  mutate(vote_I = vote_I/100)

inst <- vdem %>%
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel)

cses <- read.csv("C:/Users/ochoc/Downloads/cses_imd.csv")

cses_clean <- cses %>%
  select(eleid = IMD1003, country_id = IMD1006_VDEM, country = IMD1006_NAM, iso = IMD1006_UNALPHA3, eletype = IMD1009, resid = IMD1008_RES, sampweight = IMD1010_1, demweight = IMD1010_2, polweight = IMD1010_3, r1date = IMD1011_1, r2date = IMD1012_1, age = IMD2001_1, age_cat = IMD2001_2, gender = IMD2002, edu = IMD2003, income = IMD2006, region = IMD2008, race = IMD2010, employ = IMD2014, turnout_main = IMD3001, turnout_r1p = IMD3001_PR_1, turnout_r2p = IMD3001_PR_2, turnout_lh = IMD3001_LH, turnout_uh = IMD3001_UH, turnout_switch = IMD3001_TS, choice_pr1 = IMD3002_PR_2, choice_pr2 = IMD3002_LH_PL, choice_swtich = IMD3002_VS_1, choice_ideo = IMD3002_IF_CSES, close = IMD3005_1, closer = IMD3005_2, who = IMD3005_3, how_close = IMD3005_4, lr = IMD3006, IMD3008_A:IMD3009_I, demsat = IMD3010, partyrep = IMD3016_1, bestrep = IMD3016_2, IMD5000_A:IMD5000_I, IMD5001_A:IMD5001_I, rvt = IMD5006_1, vapt = IMD5006_2, compvote = IMD5007, IMD5052_1:IMD5056_3, IMD5058_1, numofparties  = IMD5058_2, IMD5103_A:IMD5103_I) %>%
  filter(close <= 1) %>%
  mutate(date = as.Date(r1date, format = "%Y-%m-%d"),
         year = year(date)) %>%
  mutate(IMD3008_A = ifelse(IMD3008_A < 10, IMD3008_A, NA),
         IMD3008_B = ifelse(IMD3008_B < 10, IMD3008_B, NA),
         IMD3008_C = ifelse(IMD3008_C < 10, IMD3008_C, NA),
         IMD3008_D = ifelse(IMD3008_D < 10, IMD3008_D, NA),
         IMD3008_E = ifelse(IMD3008_E < 10, IMD3008_E, NA),
         IMD3008_F = ifelse(IMD3008_F < 10, IMD3008_F, NA),
         IMD3008_G = ifelse(IMD3008_G < 10, IMD3008_G, NA),
         IMD3008_H = ifelse(IMD3008_H < 10, IMD3008_H, NA),
         IMD3008_I = ifelse(IMD3008_I < 10, IMD3008_I, NA),
         represent = ifelse(close == 0 & closer == 0, 0, 1),
         how_close = ifelse(represent == 0, 0, how_close),
         how_rep = ifelse(close == 0 & closer == 0, 0, how_close),
         how_rep = ifelse(how_rep > 3, NA, how_rep))

sums <- cses_clean %>%
  group_by(year, country) %>%
  summarise(mean = mean(represent))
  

cses_clean <- left_join(cses_clean, party, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean <- left_join(cses_clean, inst, by = c("year", "country_id"))

cses_demeaned <- demean(cses_clean, select = c("sys_pop", "ipop", "v2elloeldm", "v2pariglef"), group = "country_id")

cses_clean <- cbind(cses_clean, cses_demeaned)

cses_test <- cses_clean %>%
  filter(turnout_main <=1, gender <= 3, age <= 120) %>%
  mutate(how_rep = as.factor(how_rep),
         represent = as.factor(represent))


cses_viz <- cses_test %>%
  group_by(country, year) %>%
  mutate(
    represent = as.numeric(represent),     # Convert to integer
    how_rep = as.numeric(how_rep)          # Convert to numeric
  ) %>%
  summarize(
    total_rep = mean(represent, na.rm = TRUE),
    total_high = mean(ifelse(how_rep == 3, 1, 0), na.rm = TRUE),
    total_mid = mean(ifelse(how_rep == 2, 1, 0), na.rm = TRUE),
    total_low = mean(ifelse(how_rep == 1, 1, 0), na.rm = TRUE),
    total_zero = mean(ifelse(how_rep == 0, 1, 0), na.rm = TRUE),
    ipop = mean(ipop, na.rm = TRUE),  # Adding mean for `ipop`
    .groups = "drop"  # Removes grouping structure in final output
  )




m1 <- clmm(how_rep ~ ipop_within*v2pariglef_within + ipop_between + v2pariglef_between + age + as.factor(gender) + v2elloeldm_within + v2elloeldm_between + as.factor(v2elparlel) + as.factor(edu) + (1|country_id), data = cses_test)

summary(m1)

View(cses_test)
