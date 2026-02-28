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
library(splines)

# =============================================================================
# HELPER: Create an expanded year panel from an ID variable
# Creates every (id, year) combination so we can forward-fill sparse data
# =============================================================================
expand_year_panel <- function(n_obs, rows_per_id, id_name, start_year) {
  data.frame(id = 1:n_obs) %>%
    mutate(group_id = id / rows_per_id,
           !!id_name := ceiling(group_id)) %>%
    arrange(.data[[id_name]]) %>%
    group_by(.data[[id_name]]) %>%
    mutate(year = start_year + (row_number() - 1)) %>%
    ungroup() %>%
    select(-id, -group_id)
}

# =============================================================================
# PART 1: V-PARTY POPULISM SCORES
# Construct a composite populism index from V-Party expert survey components
# =============================================================================

# Compute a harmonic-mean-based populism score for each party-year
# - medianosp_flip: reverse-codes the opposition to populist rhetoric variable
# - vparty_medianharm: harmonic mean of anti-elitism, people-centrism, and
#   flipped opposition scores (shifted by +1 to avoid division by zero)
# - newpop: rescales the harmonic mean to a 0-1 range
vparty2 <- vparty %>%
  mutate(medianosp_flip = 4 - v2paopresp_osp,
         vparty_medianharm = 3 / ((1 / (v2paanteli_osp + 1)) +
                                    (1 / (v2papeople_osp + 1)) +
                                    (1 / (medianosp_flip + 1))),
         newpop = (vparty_medianharm - 1) / 4)

# System-level populism: seat-share-weighted average populism across ALL parties
party <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            sys_pop = sum(wpop, na.rm = TRUE) / votesum)

# Opposition populism: weighted average populism among opposition parties only
opp <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  filter(v2pagovsup == 3) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            opp_pop = sum(wpop, na.rm = TRUE) / votesum)

# Government populism: weighted average populism among governing parties
gov <- vparty2 %>%
  group_by(country_name, country_id, year, country_text_id) %>%
  mutate(wpop = newpop * v2paseatshare) %>%
  filter(v2pagovsup <= 2) %>%
  summarize(votesum = sum(v2paseatshare, na.rm = TRUE),
            gov_pop = sum(wpop, na.rm = TRUE) / votesum)

# Incumbent head-of-government party: extract populism score and ideology
# ipop is lagged by one year within each country (prior-year incumbent populism)
incumbent <- vparty2 %>%
  group_by(country_id) %>%
  select(country_id, v2paenname, year, v2pagovsup, newpop, v2pariglef) %>%
  filter(v2pagovsup == 0) %>%
  mutate(ipop = lag(newpop))

# Merge system, incumbent, opposition, and government populism into one dataset
# Then create lagged versions of each populism measure for panel analysis
party_final <- party %>%
  left_join(incumbent, by = c("year", "country_id")) %>%
  left_join(opp, by = c("year", "country_id")) %>%
  left_join(gov, by = c("year", "country_id")) %>%
  arrange(country_id, year) %>%
  group_by(country_id) %>%
  mutate(
    lag_ipop    = dplyr::lag(ipop),
    lag_opp_pop = dplyr::lag(opp_pop),
    lag_sys_pop = dplyr::lag(sys_pop),
    lag_gov_pop = dplyr::lag(gov_pop),
    lag_year    = dplyr::lag(year)
  ) %>%
  ungroup()

# =============================================================================
# PART 2: EXPAND COUNTRY-LEVEL DATA TO A YEARLY PANEL
# Create a balanced panel (every country x year from 1924 onward),
# merge in the party data, and forward-fill missing years
# =============================================================================

# Parliamentary election indicator using V-Dem v2eltype_0 (= 1 when a lower house
# election occurred). V-Dem has fuller historical coverage than V-Party.
# Joined AFTER locf so non-election years are not forward-filled as 1.
parl_election_years <- vdem %>%
  filter(v2eltype_0 == 1) %>%
  distinct(country_id, year) %>%
  mutate(parl_election = 1L)

# 236 countries x 100 years = 23,600 rows
df <- expand_year_panel(n_obs = 23600, rows_per_id = 100,
                        id_name = "country_id", start_year = 1924)

# Merge party-level populism data onto the expanded panel
df <- left_join(df, party_final, by = c("country_id", "year"))

# Forward-fill (LOCF) within each country so non-election years
# carry the most recent election's values
vars_to_fill <- c("country_name", "country_text_id", "sys_pop", "ipop",
                   "v2pariglef", "opp_pop", "gov_pop", "v2paenname",
                   "lag_sys_pop", "lag_ipop", "lag_gov_pop", "lag_opp_pop")

setDT(df)
df <- df[order(country_id, year)]
df[, (vars_to_fill) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)),
   by = country_id, .SDcols = vars_to_fill]

# Drop rows with no country info (countries that never appear in the data),
# then join the election indicator (NA → 0 for non-election years)
df <- df %>%
  filter(!is.na(country_name)) %>%
  left_join(parl_election_years, by = c("country_id", "year")) %>%
  mutate(parl_election = replace_na(parl_election, 0L)) %>%
  select(country_id, country_name, country_text_id, year, sys_pop, v2paenname,
         ipop, opp_pop, gov_pop, v2pariglef,
         lag_sys_pop, lag_ipop, lag_gov_pop, lag_opp_pop, parl_election)

# =============================================================================
# PART 3: EXPAND INDIVIDUAL PARTY DATA TO A YEARLY PANEL
# Same approach but at the party level, for later merging to individual survey
# respondents via their party IDs
# =============================================================================

# PartyFacts founding years — used to compute party age at each election
party_age_data <- read_csv("Data/party_age.csv") %>%
  select(pf_party_id = partyfacts_id, year_founded) %>%
  mutate(year_founded = as.integer(year_founded))

# 100,103 parties x 26 years = 2,602,678 rows
df_ind <- expand_year_panel(n_obs = 2602678, rows_per_id = 26,
                            id_name = "v2paid", start_year = 1996)

# Keep only the columns needed for individual-level merges
# v2pagovsup_lag: government-support status from the PREVIOUS election for each party
# party_age: years since founding as of the election year (year - year_founded)
# new_party: 1 in the first parliamentary election year (per V-Dem v2eltype_0)
#   that occurred in the party's country on or after year_founded.
#   Using V-Dem rather than V-Party min(year) gives full historical coverage,
#   preventing old parties from being miscoded as new when V-Party coverage starts.
#   V-Dem columns are dropped immediately after the lookup — only new_party is kept.

# Step 1: all lower-house election years per country from V-Dem
vdem_parl_elections <- vdem %>%
  filter(v2eltype_0 == 1) %>%
  select(country_id, year)

# Step 2: for each party, find the earliest V-Dem election >= year_founded
first_election <- vparty2 %>%
  select(v2paid, country_id, pf_party_id) %>%
  distinct() %>%
  left_join(party_age_data, by = "pf_party_id") %>%
  left_join(vdem_parl_elections, by = "country_id") %>%
  filter(!is.na(year_founded), year >= year_founded) %>%
  group_by(v2paid) %>%
  summarise(first_election_year = min(year, na.rm = TRUE), .groups = "drop")

# Clean up — vdem_parl_elections is only needed for the lookup above
rm(vdem_parl_elections)

vparty3 <- vparty2 %>%
  arrange(v2paid, year) %>%
  group_by(v2paid) %>%
  mutate(v2pagovsup_lag = lag(v2pagovsup)) %>%
  ungroup() %>%
  select(v2paid, year, newpop, country_name, country_text_id,
         v2paenname, pf_party_id, country_id, v2pagovsup, v2pagovsup_lag) %>%
  left_join(party_age_data, by = "pf_party_id") %>%
  left_join(first_election, by = "v2paid") %>%
  mutate(
    party_age = year - year_founded,
    # new_party = 1 only in the first post-founding election; NA if year_founded missing
    new_party = case_when(
      is.na(year_founded)        ~ NA_integer_,
      year == first_election_year ~ 1L,
      TRUE                       ~ 0L
    )
  ) %>%
  select(-first_election_year)

df_ind <- left_join(df_ind, vparty3, by = c("v2paid", "year"))

# Forward-fill within each party so inter-election years have data.
# new_party is intentionally excluded: it should only be 1 in the specific first
# election year. Forward-filling would incorrectly label the party as new forever.
# Inter-election NAs are harmless since the CSES merge only touches election years.
vars_to_fill <- c("country_name", "country_text_id", "newpop",
                   "v2paenname", "pf_party_id", "country_id", "v2pagovsup", "v2pagovsup_lag",
                   "party_age", "year_founded")

setDT(df_ind)
df_ind <- df_ind[order(v2paid, year)]
df_ind[, (vars_to_fill) := lapply(.SD, function(x) na.locf(x, na.rm = FALSE)),
   by = v2paid, .SDcols = vars_to_fill]

df_ind <- df_ind %>%
  filter(!is.na(country_name)) %>%
  mutate(new_party = replace_na(new_party, 0L)) %>%
  select(v2paenname, v2paid, country_name, country_text_id, year,
         newpop, pf_party_id, country_id, v2pagovsup, v2pagovsup_lag,
         party_age, new_party, year_founded)

# =============================================================================
# PART 4: INSTITUTIONAL AND MACRO CONTROLS
# Pull in V-Dem institutional variables, income inequality, and QoG party counts
# =============================================================================

# V-Dem institutional variables: electoral system, polyarchy, head-of-government
# selection, and a computed party system age that resets when polyarchy drops
# below 0.42 (i.e., democratic breakdown resets the clock)
inst <- vdem %>%
  select(country_id, year, v2elloeldm, v2x_polyarchy, v2elparlel,
         ccodecow = COWcode, v2ex_elechog, v2exhoshog, v2ex_elechos) %>%
  mutate(hog_direct = ifelse(v2exhoshog == 1, v2ex_elechos, v2ex_elechog)) %>%
  group_by(country_id) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    reset = v2x_polyarchy < 0.42,
    reset_group = cumsum(lag(reset, default = TRUE)),
    party_sys_age = sequence(rle(reset_group)$lengths) - 1
  ) %>%
  ungroup()

# Gini coefficient (disposable income) from external income inequality dataset
incomeineq <- read_csv("Data/incomeineq.csv") %>%
  select(year, country, gini = gini_disp) %>%
  mutate(ccodecow = countrycode(country, origin = "country.name", destination = "cown"))

# Merge inequality into institutional data
inst <- left_join(inst, incomeineq, by = c("year", "ccodecow"))

# Effective number of parties from QoG (Gallagher/other index)
qog <- read.csv("C:/Users/ochoc/Downloads/qog_bas_ts_jan25.csv") %>%
  select(year, ccodecow, numofparties = gol_enep)

# =============================================================================
# PART 5: CSES INDIVIDUAL-LEVEL DATA
# Load CSES Integrated Module Dataset and clean/recode survey responses
# =============================================================================

cses <- read.csv("C:/Users/ochoc/Downloads/cses_imd.csv")

# Rename CSES variables to readable names and recode invalid values to NA
cses_clean <- cses %>%
  select(
    # Election and respondent identifiers
    eleid = IMD1003,
    country_id = IMD1006_VDEM,
    country = IMD1006_NAM,
    iso = IMD1006_UNALPHA3,
    eletype = IMD1009,
    resid = IMD1008_RES,
    # Survey weights
    sampweight = IMD1010_1,
    demweight = IMD1010_2,
    polweight = IMD1010_3,
    # Election dates
    r1date = IMD1011_1,
    r2date = IMD1012_1,
    # Demographics
    age = IMD2001_1,
    age_cat = IMD2001_2,
    gender = IMD2002,
    edu = IMD2003,
    income = IMD2006,
    region = IMD2008,
    race = IMD2010,
    employ = IMD2014,
    # Turnout and vote choice
    turnout_main = IMD3001,
    turnout_r1p = IMD3001_PR_1,
    turnout_r2p = IMD3001_PR_2,
    turnout_lh = IMD3001_LH,
    turnout_uh = IMD3001_UH,
    choice_lh2 = IMD3002_LH_PL,
    choice_swtich = IMD3002_VS_1,
    choice_ideo = IMD3002_IF_CSES,
    # Party identification
    close = IMD3005_1,       # feels close to any party (0=no, 1=yes)
    closer = IMD3005_2,      # feels closer to one party than others
    who = IMD3005_3,         # which party they feel close to (party code)
    how_close = IMD3005_4,   # strength of closeness
    # Left-right self-placement and feeling thermometers
    lr = IMD3006,
    IMD3008_A:IMD3009_I,     # party feeling thermometers (A-I) and leader evals
    demsat = IMD3010,
    partyrep = IMD3016_1,
    bestrep = IMD3016_2,
    # Election-level party codes and identifiers
    IMD5000_A:IMD5000_I,     # party codes for parties A-I
    IMD5001_A:IMD5001_I,     # party names for parties A-I
    rvt = IMD5006_1,
    vapt = IMD5006_2,
    compvote = IMD5007,
    IMD5052_1:IMD5056_3,
    IMD5058_1,
    IMD5103_A:IMD5103_I,     # ParlGov/V-Dem party IDs for parties A-I
    # Macro controls at time of election
    unemploy_t0 = IMD5054_2,
    gdpg_t0 = IMD5052_2,
    gdppc_t0 = IMD5053_2,
    ses = IMD2016
  ) %>%
  mutate(
    # Recode out-of-range values to NA for each variable
    close = ifelse(close > 1, NA, close),
    date  = as.Date(r1date, format = "%Y-%m-%d"),
    year  = year(date),
    # represent = 1 if respondent feels close to ANY party
    represent = ifelse(close == 0 & closer == 0, 0, 1),
    # how_close is 0 if no party attachment, else original value
    how_close = ifelse(represent == 0, 0, how_close),
    # how_rep: same logic, capped at 3 (values above 3 are invalid)
    how_rep = ifelse(close == 0 & closer == 0, 0, how_close),
    how_rep = ifelse(how_rep > 3, NA, how_rep),
    # Recode invalid survey responses to NA with domain-specific thresholds
    partyrep    = as.factor(ifelse(partyrep > 1, NA, partyrep)),
    unemploy_t0 = ifelse(unemploy_t0 > 100, NA, unemploy_t0),
    ses         = as.factor(ifelse(ses > 5, NA, ses)),
    gdppc_t0    = ifelse(gdppc_t0 > 899999, NA, gdppc_t0),
    gdppc_t0    = ifelse(gdpg_t0 > 30, NA, gdppc_t0),
    age         = ifelse(age > 120, NA, age),
    edu         = ifelse(edu > 6, NA, edu),
    gender      = ifelse(gender > 2, NA, gender),
    lr          = ifelse(lr > 10, NA, lr),
    # Flag whether LR is missing BEFORE centering, so we can zero-fill
    lr_missing  = as.factor(ifelse(is.na(lr), 1, 0)),
    # Center LR on 5 (so 0 = moderate) and zero-fill missing values
    lr      = lr - 5,
    lr      = ifelse(lr_missing == 1, 0, lr),
    lr_sq = lr^2
  ) %>%
  # Recode CSES special missing codes (96-99) to NA in feeling thermometers
  mutate(across(IMD3008_A:IMD3008_I,
                ~ ifelse(. %in% c(96, 97, 98, 99), NA, .)))

# --- Compute feeling thermometer summaries and party favorability ---

# Column names for the 9 party feeling thermometers
vars <- paste0("IMD3008_", LETTERS[1:9])

cses_clean <- cses_clean %>%
  # rowwise() needed for c_across to compute per-respondent stats
  rowwise() %>%
  mutate(
    feel_avg = mean(c_across(all_of(vars)), na.rm = TRUE),
    feel_max = max(c_across(all_of(vars)), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # When all thermometers are NA, mean/max return NaN/-Inf; convert to NA
  mutate(
    feel_max = ifelse(feel_max == -Inf, NA, feel_max),
    feel_avg = ifelse(feel_avg == -Inf, NA, feel_avg),
    # Match respondent's closest party (who) to the corresponding thermometer
    own_party_feel = case_when(
      who == IMD5000_A ~ IMD3008_A,
      who == IMD5000_B ~ IMD3008_B,
      who == IMD5000_C ~ IMD3008_C,
      who == IMD5000_D ~ IMD3008_D,
      who == IMD5000_E ~ IMD3008_E,
      who == IMD5000_F ~ IMD3008_F,
      who == IMD5000_G ~ IMD3008_G,
      who == IMD5000_H ~ IMD3008_H,
      who == IMD5000_I ~ IMD3008_I,
      TRUE ~ NA_real_
    ),
    # Party favorability: own party's thermometer if affiliated, else highest
    party_fav = ifelse(!is.na(own_party_feel), own_party_feel, feel_max)
  )

# =============================================================================
# PART 6: MERGE CSES WITH COUNTRY, INSTITUTIONAL, AND PARTY DATA
# Join everything together and map party codes to V-Dem party IDs
# =============================================================================

# Merge country-level populism panel, institutional controls, and QoG
# Then map respondent's closest party and voted party to V-Dem party IDs
cses_clean3 <- cses_clean %>%
  left_join(df, by = c("country_id", "year")) %>%
  mutate(ctid = iso) %>%
  left_join(inst, by = c("year", "country_id")) %>%
  left_join(qog, by = c("year", "ccodecow")) %>%
  mutate(
    # Map closest party code to its V-Dem/ParlGov party ID
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
    # Map lower-house vote choice to its V-Dem/ParlGov party ID
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
    )
  )

# Prepare party-level populism scores for merging by closest/voted party
df_ind1 <- df_ind %>%
  rename(closest_party = pf_party_id) %>%
  select(year, closest_party, country_id,
         newpop_closest = newpop, v2paenname_closest = v2paenname,
         v2pagovsup_closest = v2pagovsup,
         v2pagovsup_closest_lag = v2pagovsup_lag,
         party_age_closest = party_age,
         new_party_closest = new_party,
         year_founded_closest = year_founded)

df_ind2 <- df_ind %>%
  rename(voted_party = pf_party_id) %>%
  select(year, voted_party, country_id,
         newpop_voted = newpop, v2paenname_voted = v2paenname)

# Merge populism scores for each respondent's closest and voted party
cses_merged <- cses_clean3 %>%
  left_join(df_ind1, by = c("year", "closest_party", "country_id")) %>%
  left_join(df_ind2, by = c("year", "voted_party", "country_id")) %>%
  mutate(
    newpop_closest_missing = ifelse(is.na(newpop_closest), 1, 0),
    # Trichotomous partisan type: non-partisan, non-populist partisan, populist partisan
    # Uses median split of newpop_closest among partisans to define "populist party"
    partisan_type = case_when(
      is.na(newpop_closest) ~ "no_party",
      newpop_closest >= median(newpop_closest, na.rm = TRUE) ~ "pop_partisan",
      TRUE ~ "nonpop_partisan"
    ),
    partisan_type = factor(partisan_type,
                           levels = c("nonpop_partisan", "pop_partisan", "no_party")),
    # Party alignment: classifies respondent's closest party as government,
    # opposition, or no party based on V-Party government support variable
    # v2pagovsup: 0 = head of govt, 1 = senior coalition, 2 = junior coalition, 3 = opposition
    party_alignment = case_when(
      is.na(v2pagovsup_closest) ~ "no_party",
      v2pagovsup_closest <= 2    ~ "government",
      v2pagovsup_closest == 3    ~ "opposition"
    ),
    party_alignment = factor(party_alignment,
                             levels = c("no_party", "government", "opposition")),
    # Lagged alignment: role of respondent's closest party in the PREVIOUS election
    party_alignment_lag = case_when(
      is.na(v2pagovsup_closest_lag) ~ "no_party",
      v2pagovsup_closest_lag <= 2   ~ "government",
      v2pagovsup_closest_lag == 3   ~ "opposition"
    ),
    party_alignment_lag = factor(party_alignment_lag,
                                 levels = c("no_party", "government", "opposition"))
  )

# =============================================================================
# PART 7: DEMEANING AND STANDARDIZATION
# Decompose variables into within- and between-country (and country-year)
# components, then Gelman-standardize (divide by 2 SD) for comparability
# =============================================================================

# First demeaning pass: separate within- vs between-COUNTRY variation
# for macro-level variables
cses_demeaned <- demean(cses_merged,
  select = c("sys_pop", "ipop", "v2pariglef", "gov_pop", "opp_pop",
             "unemploy_t0", "gdpg_t0", "gdppc_t0",
             "lag_sys_pop", "lag_ipop", "lag_gov_pop", "lag_opp_pop",
             "gini", "numofparties"),
  by = "country_id"
) %>%
  mutate(age_squ = age * age,
         countryear = as.factor(paste0(country_text_id, year)))

# Second demeaning pass: separate within- vs between-COUNTRY-YEAR variation
# for individual-level variables (removes election-specific means)
cses_demeaned <- demean(cses_demeaned,
  select = c("age", "age_squ", "newpop_voted", "newpop_closest",
             "ses", "lr", "lr_sq", "party_age_closest"),
  by = "countryear"
)

# Variables to Gelman-standardize: (x - mean) / (2 * SD)
# This puts continuous predictors on a comparable scale where a 1-unit change
# corresponds to a 2-SD shift, making coefficients directly comparable
# to binary predictors
vars_to_standardize <- c(
  "opp_pop_within", "opp_pop_between",
  "ipop_within", "ipop_between",
  "v2pariglef_within", "v2pariglef_between",
  "age_within", "age_between",
  "age_squ_within", "age_squ_between",
  "v2elloeldm",
  "gov_pop_within", "gov_pop_between",
  "sys_pop_within", "sys_pop_between",
  "numofparties",
  "newpop_closest_within", "newpop_closest_between",
  "newpop_voted_within", "newpop_voted_between",
  "age_squ",
  "lr_within", "lr_between",
  "lag_sys_pop_within", "lag_ipop_within",
  "lag_gov_pop_within", "lag_opp_pop_within",
  "lag_sys_pop_between", "lag_ipop_between",
  "lag_gov_pop_between", "lag_opp_pop_between",
  "party_sys_age",
  "numofparties_within", "numofparties_between",
  "gini_between", "gini_within",
  "unemploy_t0_between", "unemploy_t0_within",
  "lr_sq_within", "lr_sq_between",
  "party_age_closest_within", "party_age_closest_between"
)

cses_test <- cses_demeaned %>%
  # Convert categorical variables to factors for modeling
  mutate(
    across(c(how_rep, represent, v2elparlel, edu, gender),
           as.factor),
    date      = as.factor(r1date),
    year_fact = as.factor(year)
  ) %>%
  # Remove respondents with gender code 3 (other/missing)
  filter(gender != 3) %>%
  # Gelman-standardize all continuous predictors
  mutate(
    across(all_of(vars_to_standardize),
           ~ (. - mean(., na.rm = TRUE)) / (2 * sd(., na.rm = TRUE)),
           .names = "{.col}_c")
  ) %>%
  # Keep only one row per respondent-election (deduplicate)
  distinct(eleid, resid, .keep_all = TRUE) %>%
  # Convert remaining ID variables to factors
  mutate(
    year_fact  = as.factor(year_fact),
    country_id = as.factor(country_id),
    how_rep    = as.factor(how_rep)
  ) %>%
  # Drop respondents who appear more than once in the same election
  add_count(resid, eleid) %>%
  filter(n == 1) %>%
  # Create a within-country time variable (years since first election)
  group_by(country_id) %>%
  mutate(time = year - min(year)) %>%
  ungroup()

# =============================================================================
# PART 8: TIME SPLINES AND SAVE
# Add natural cubic spline basis for flexible time trends in models
# =============================================================================

# Natural cubic spline with 3 degrees of freedom for smooth time trends
spline_basis <- ns(cses_test$time, df = 3)
cses_test$spline1 <- spline_basis[, 1]
cses_test$spline2 <- spline_basis[, 2]
cses_test$spline3 <- spline_basis[, 3]

# Save final analysis-ready dataset
saveRDS(cses_test, file = "Data/cses_test.Rda")
