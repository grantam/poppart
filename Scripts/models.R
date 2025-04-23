cses_test <- readRDS("cses_test.Rda")

m1 <- clmm(how_rep ~ sys_pop_within_c +
             sys_pop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)


m2 <- clmm(how_rep ~ opp_pop_within_c +
             opp_pop_between_c +
             ipop_within_c +
             ipop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

m3 <- clmm(how_rep ~ opp_pop_within_c +
             opp_pop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

m4 <- clmm(how_rep ~ ipop_within_c +
             ipop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

m5 <- clmm(how_rep ~ gov_pop_within_c +
             gov_pop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

m6 <- clmm(how_rep ~ gov_pop_within_c +
             gov_pop_between_c +
             opp_pop_within_c +
             opp_pop_between_c +
             v2pariglef_within_c +
             v2pariglef_between_c +
             v2elloeldm_within_c +
             v2elloeldm_between_c +
             numofparties_c +
             age_squ_c +
             as.factor(gender) +
             as.factor(v2elparlel) +
             as.factor(edu) +
             (1| country_id) +
             (1 | country_id:year_fact),
           data = cses_test)

summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m6)
