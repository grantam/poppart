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

inst <- vdem %>%
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel)

cses <- read.csv("C:/Users/ochoc/Downloads/cses_imd.csv")

cses_clean <- cses %>%
  select(eleid = IMD1003, country_id = IMD1006_VDEM, country = IMD1006_NAM, iso = IMD1006_UNALPHA3, eletype = IMD1009, resid = IMD1008_RES, sampweight = IMD1010_1, demweight = IMD1010_2, polweight = IMD1010_3, r1date = IMD1011_1, r2date = IMD1012_1, age = IMD2001_1, gender = IMD2002, income = IMD2006, race = IMD2010, employ = IMD2014, turnout_main = IMD3001, turnout_r1p = IMD3001_PR_1, turnout_r2p = IMD3001_PR_2, turnout_lh = IMD3001_LH, turnout_uh = IMD3001_UH, turnout_switch = IMD3001_TS, choice_pr1 = IMD3002_PR_2, choice_pr2 = IMD3002_LH_PL, choice_swtich = IMD3002_VS_1, choice_ideo = IMD3002_IF_CSES, close = IMD3005_1, closer = IMD3005_2, who = IMD3005_3, lr = IMD3006, IMD3008_A:IMD3009_I, demsat = IMD3010, partyrep = IMD3016_1, bestrep = IMD3016_2, IMD5000_A:IMD5000_I, rvt = IMD5006_1, vapt = IMD5006_2, compvote = IMD5007, IMD5052_1:IMD5056_3, IMD5058_1, numofparties  = IMD5058_2, IMD5103_A:IMD5103_I) %>%
    mutate(date = as.Date(r1date, format = "%Y-%m-%d"),
           year = year(date))

cses_clean <- left_join(cses_clean, party, by = c("country_id", "year")) %>%
  mutate(ctid = iso)

cses_clean <- left_join(cses_clean, inst, by = c("year", "country_id"))

cses_demeaned <- demean(cses_clean, select = c("sys_pop", "ipop"), group = "country_id")

cses_clean <- cbind(cses_clean, cses_demeaned)

cses_test <- cses_clean %>%
  filter(turnout_main <=1, gender <= 3)

View(cses_test)


m1 <- glmer(turnout_main ~ sys_pop_within + sys_pop_between + factor(compvote) + numofparties + factor(v2elparlel) + v2elloeldm + v2x_polyarchy + (1 | country_id), family = binomial(link = "logit"), data = cses_test)

summary(m1)

m2 <- glmer(turnout_main ~ ipop_within +  sys_pop_within + ipop_between +  sys_pop_between + factor(compvote) + numofparties + factor(v2elparlel) + v2elloeldm + v2x_polyarchy + factor(gender) + (1 | country_id) + (1 | year), family = binomial(link = "logit"), data = cses_test)

summary(m2)
ranef(m2)
ICC(m2)

%>%
  mutate(leader_id = NA,
         leader_id = ifelse(r1date == "2017-06-25" & country == "Albania", "Edi Rama 1", leader_id),
         leader_id = ifelse(r1date == "2015-10-25" & country == "Argentina", "Cristina Fernandez 2", leader_id),
         leader_id = ifelse(r1date == "2015-10-25" & country == "Argentina", "Cristina Fernandez 2", leader_id))


