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
           year = year(date)) %>%
  mutate(IMD3008_A = ifelse(IMD3008_A < 10, IMD3008_A, NA),
         IMD3008_B = ifelse(IMD3008_B < 10, IMD3008_B, NA),
         IMD3008_C = ifelse(IMD3008_C < 10, IMD3008_C, NA),
         IMD3008_D = ifelse(IMD3008_D < 10, IMD3008_D, NA),
         IMD3008_E = ifelse(IMD3008_E < 10, IMD3008_E, NA),
         IMD3008_F = ifelse(IMD3008_F < 10, IMD3008_F, NA),
         IMD3008_G = ifelse(IMD3008_G < 10, IMD3008_G, NA),
         IMD3008_H = ifelse(IMD3008_H < 10, IMD3008_H, NA),
         IMD3008_I = ifelse(IMD3008_I < 10, IMD3008_I, NA))

cses_clean <- left_join(cses_clean, party, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean <- left_join(cses_clean, inst, by = c("year", "country_id"))

cses_demeaned <- demean(cses_clean, select = c("sys_pop", "ipop"), group = "country_id")

cses_clean <- cbind(cses_clean, cses_demeaned)

cses_test <- cses_clean %>%
  filter(turnout_main <=1, gender <= 3, age <= 120, edu <= 4) %>%
  mutate(represent = ifelse(who <= 9000000, 1, 0))


m1 <- lme4::glmer(represent ~ ipop_within + ipop_between + age + as.factor(gender) + as.factor(race) + as.factor(employ) + (1|country_id), data = cses_test, family = binomial(link = "logit"), co)

















cses_test <- left_join(cses_test, party_A, by = c("year", "IMD5103_A"))
cses_test <- left_join(cses_test, party_B, by = c("year", "IMD5103_B"))
cses_test <- left_join(cses_test, party_C, by = c("year", "IMD5103_C"))
cses_test <- left_join(cses_test, party_D, by = c("year", "IMD5103_D"))
cses_test <- left_join(cses_test, party_E, by = c("year", "IMD5103_E"))
cses_test <- left_join(cses_test, party_F, by = c("year", "IMD5103_F"))
cses_test <- left_join(cses_test, party_G, by = c("year", "IMD5103_G"))
cses_test <- left_join(cses_test, party_H, by = c("year", "IMD5103_H"))
cses_test <- left_join(cses_test, party_I, by = c("year", "IMD5103_I"))

cses_test <- cses_test %>%
  mutate(dif_ab = ((abs(IMD3008_A - IMD3008_B))*(vote_B*(1-vote_A))),
         dif_ac = ((abs(IMD3008_A - IMD3008_C))*(vote_C*(1-vote_A))),
         dif_ad = ((abs(IMD3008_A - IMD3008_D))*(vote_D*(1-vote_A))),
         dif_ae = ((abs(IMD3008_A - IMD3008_E))*(vote_E*(1-vote_A))),
         dif_af = ((abs(IMD3008_A - IMD3008_F))*(vote_F*(1-vote_A))),
         dif_ag = ((abs(IMD3008_A - IMD3008_G))*(vote_G*(1-vote_A))),
         dif_ah = ((abs(IMD3008_A - IMD3008_H))*(vote_H*(1-vote_A))),
         dif_ai = ((abs(IMD3008_A - IMD3008_I))*(vote_I*(1-vote_A))),
         dif_ba = ((abs(IMD3008_B - IMD3008_A))*(vote_A*(1-vote_B))),
         dif_bc = ((abs(IMD3008_B - IMD3008_C))*(vote_C*(1-vote_B))),
         dif_bd = ((abs(IMD3008_B - IMD3008_D))*(vote_D*(1-vote_B))),
         dif_be = ((abs(IMD3008_B - IMD3008_E))*(vote_E*(1-vote_B))),
         dif_bf = ((abs(IMD3008_B - IMD3008_F))*(vote_F*(1-vote_B))),
         dif_bg = ((abs(IMD3008_B - IMD3008_G))*(vote_G*(1-vote_B))),
         dif_bh = ((abs(IMD3008_B - IMD3008_H))*(vote_H*(1-vote_B))),
         dif_bi = ((abs(IMD3008_B - IMD3008_I))*(vote_I*(1-vote_B))),
         dif_cb = ((abs(IMD3008_C - IMD3008_B))*(vote_B*(1-vote_C))),
         dif_ca = ((abs(IMD3008_C - IMD3008_A))*(vote_A*(1-vote_C))),
         dif_cd = ((abs(IMD3008_C - IMD3008_D))*(vote_D*(1-vote_C))),
         dif_ce = ((abs(IMD3008_C - IMD3008_E))*(vote_E*(1-vote_C))),
         dif_cf = ((abs(IMD3008_C - IMD3008_F))*(vote_F*(1-vote_C))),
         dif_cg = ((abs(IMD3008_C - IMD3008_G))*(vote_G*(1-vote_C))),
         dif_ch = ((abs(IMD3008_C - IMD3008_H))*(vote_H*(1-vote_C))),
         dif_ci = ((abs(IMD3008_C - IMD3008_I))*(vote_I*(1-vote_C))),
         dif_da = ((abs(IMD3008_D - IMD3008_A))*(vote_A*(1-vote_D))),
         dif_bc = ((abs(IMD3008_D - IMD3008_C))*(vote_C*(1-vote_D))),
         dif_db = ((abs(IMD3008_D - IMD3008_B))*(vote_D*(1-vote_D))),
         dif_de = ((abs(IMD3008_D - IMD3008_E))*(vote_E*(1-vote_D))),
         dif_df = ((abs(IMD3008_D - IMD3008_F))*(vote_F*(1-vote_D))),
         dif_de = ((abs(IMD3008_D - IMD3008_G))*(vote_G*(1-vote_D))),
         dif_df = ((abs(IMD3008_D - IMD3008_H))*(vote_H*(1-vote_D))),
         dif_di = ((abs(IMD3008_D - IMD3008_I))*(vote_I*(1-vote_D))),
         dif_ab = ((abs(IMD3008_E - IMD3008_B))*(vote_B*(1-vote_E))),
         dif_ec = ((abs(IMD3008_E - IMD3008_C))*(vote_C*(1-vote_E))),
         dif_ed = ((abs(IMD3008_E - IMD3008_D))*(vote_D*(1-vote_E))),
         dif_ea = ((abs(IMD3008_E - IMD3008_A))*(vote_A*(1-vote_E))),
         dif_ef = ((abs(IMD3008_E - IMD3008_F))*(vote_F*(1-vote_E))),
         dif_eg = ((abs(IMD3008_E - IMD3008_G))*(vote_G*(1-vote_E))),
         dif_eh = ((abs(IMD3008_E - IMD3008_H))*(vote_H*(1-vote_E))),
         dif_ei = ((abs(IMD3008_E - IMD3008_I))*(vote_I*(1-vote_E))),
         dif_fa = ((abs(IMD3008_F - IMD3008_A))*(vote_A*(1-vote_F))),
         dif_fc = ((abs(IMD3008_F - IMD3008_C))*(vote_C*(1-vote_F))),
         dif_fd = ((abs(IMD3008_F - IMD3008_D))*(vote_D*(1-vote_F))),
         dif_fe = ((abs(IMD3008_F - IMD3008_E))*(vote_E*(1-vote_F))),
         dif_fb = ((abs(IMD3008_F - IMD3008_B))*(vote_B*(1-vote_F))),
         dif_fg = ((abs(IMD3008_F - IMD3008_G))*(vote_G*(1-vote_F))),
         dif_fh = ((abs(IMD3008_F - IMD3008_H))*(vote_H*(1-vote_F))),
         dif_fi = ((abs(IMD3008_F - IMD3008_I))*(vote_I*(1-vote_F))),
         dif_gb = ((abs(IMD3008_G - IMD3008_B))*(vote_B*(1-vote_G))),
         dif_ga = ((abs(IMD3008_G - IMD3008_A))*(vote_A*(1-vote_G))),
         dif_gd = ((abs(IMD3008_G - IMD3008_D))*(vote_D*(1-vote_G))),
         dif_ge = ((abs(IMD3008_G - IMD3008_E))*(vote_E*(1-vote_G))),
         dif_gf = ((abs(IMD3008_G - IMD3008_F))*(vote_F*(1-vote_G))),
         dif_gh = ((abs(IMD3008_G - IMD3008_H))*(vote_H*(1-vote_G))),
         dif_gc = ((abs(IMD3008_G - IMD3008_C))*(vote_C*(1-vote_G))),
         dif_gi = ((abs(IMD3008_G - IMD3008_I))*(vote_I*(1-vote_G))),
         dif_ha = ((abs(IMD3008_H - IMD3008_A))*(vote_A*(1-vote_H))),
         dif_hc = ((abs(IMD3008_H - IMD3008_C))*(vote_C*(1-vote_H))),
         dif_hb = ((abs(IMD3008_H - IMD3008_B))*(vote_D*(1-vote_H))),
         dif_he = ((abs(IMD3008_H - IMD3008_E))*(vote_E*(1-vote_H))),
         dif_hf = ((abs(IMD3008_H - IMD3008_F))*(vote_F*(1-vote_H))),
         dif_hd = ((abs(IMD3008_H - IMD3008_D))*(vote_D*(1-vote_H))),
         dif_hf = ((abs(IMD3008_H - IMD3008_G))*(vote_G*(1-vote_H))),
         dif_hi = ((abs(IMD3008_H - IMD3008_I))*(vote_I*(1-vote_H))),
         dif_ia = ((abs(IMD3008_I - IMD3008_A))*(vote_A*(1-vote_I))),
         dif_ic = ((abs(IMD3008_I - IMD3008_C))*(vote_C*(1-vote_I))),
         dif_ib = ((abs(IMD3008_I - IMD3008_B))*(vote_D*(1-vote_I))),
         dif_ie = ((abs(IMD3008_I - IMD3008_E))*(vote_E*(1-vote_I))),
         dif_if = ((abs(IMD3008_I - IMD3008_F))*(vote_F*(1-vote_I))),
         dif_id = ((abs(IMD3008_I - IMD3008_D))*(vote_D*(1-vote_I))),
         dif_ig = ((abs(IMD3008_I - IMD3008_G))*(vote_G*(1-vote_I))),
         dif_ih = ((abs(IMD3008_I - IMD3008_H))*(vote_H*(1-vote_I))))

a_mat <- cses_test %>%
  select(dif_ab:dif_ai) %>%
  summarise(dif_A = rowSums(select(., dif_ab:dif_ai), na.rm = TRUE))

b_mat <- cses_test %>%
  select(dif_ba:dif_bi) %>%
  summarise(dif_B = rowSums(select(., dif_ba:dif_bi), na.rm = TRUE))

c_mat <- cses_test %>%
  select(dif_cb:dif_ci) %>%
  summarise(dif_C = rowSums(select(., dif_cb:dif_ci), na.rm = TRUE))

d_mat <- cses_test %>%
  select(dif_da:dif_di) %>%
  summarise(dif_D = rowSums(select(., dif_da:dif_di), na.rm = TRUE))

e_mat <- cses_test %>%
  select(dif_ec:dif_ei) %>%
  summarise(dif_E = rowSums(select(., dif_ec:dif_ei), na.rm = TRUE))

f_mat <- cses_test %>%
  select(dif_fa:dif_fi) %>%
  summarise(dif_F = rowSums(select(., dif_fa:dif_fi), na.rm = TRUE))

g_mat <- cses_test %>%
  select(dif_gb:dif_gi) %>%
  summarise(dif_G = rowSums(select(., dif_gb:dif_gi), na.rm = TRUE))

h_mat <- cses_test %>%
  select(dif_ha:dif_hi) %>%
  summarise(dif_H = rowSums(select(., dif_ha:dif_hi), na.rm = TRUE))

h_mat <- cses_test %>%
  select(dif_ha:dif_hi) %>%
  summarise(dif_H = rowSums(select(., dif_ha:dif_hi), na.rm = TRUE))

i_mat <- cses_test %>%
  select(dif_ia:dif_ih) %>%
  summarise(dif_I = rowSums(select(., dif_ia:dif_ih), na.rm = TRUE))

cses_test <- cbind(cses_test, a_mat)
cses_test <- cbind(cses_test, b_mat)
cses_test <- cbind(cses_test, c_mat)
cses_test <- cbind(cses_test, d_mat)
cses_test <- cbind(cses_test, e_mat)
cses_test <- cbind(cses_test, f_mat)
cses_test <- cbind(cses_test, g_mat)
cses_test <- cbind(cses_test, h_mat)
cses_test <- cbind(cses_test, i_mat)

cses_test <- cses_test %>%
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


ctysum <- cses_test %>%
  group_by(year, country) %>%
  summarize(cpolar = mean(polar, na.rm = T))
