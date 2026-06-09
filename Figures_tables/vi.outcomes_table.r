##cancer outcomes####
##person years at risk

df<- df %>% mutate(pyar_end = case_when(death==0 & is.na(TRANSFER_OUT_CODE)~ as.Date("2023-12-31"),
                                        death==1 | !is.na(TRANSFER_OUT_CODE)~ pmin(as.Date(death_date),as.Date(DATE_TRANSFER_OUT), na.rm=T),
                                        T~NA)) %>%
  mutate(pyar_end = case_when(pyar_end > as.Date("2023-12-31") ~as.Date("2023-12-31"),
                              T~ pyar_end) ) %>% 
  mutate(py_risk = interval(as.Date(date_end_pregnancy),pyar_end)/ years(1)) %>%
  mutate(follow_months = interval(as.Date(date_end_pregnancy),end_follow)/ months(1))


n_lb <-  df %>% group_by(hdfa_preg) %>% count() %>% mutate(var = "Number of singleton live births") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = n)
cancer_by_exposed <- df%>%group_by(hdfa_preg) %>% 
  summarise(cancer_outcome = sum(cancer_outcome)) %>%
  mutate(var = "Number with childhood cancer during follow up period") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = cancer_outcome)
pyar_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = round_half_up(sum(py_risk),1))  %>% 
  mutate(var = "Person years at risk") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = pyar_all_ages)
iqr_follows <- df %>% group_by(hdfa_preg) %>% 
  summarise(med_follow_months = median(follow_months), 
            QR1 = quantile(follow_months, probs=0.25), 
            QR3 = quantile(follow_months, probs=0.75), ) %>%
  mutate(median_w_IQR = paste0(round_half_up(med_follow_months,1), 
                               " (", round_half_up(QR1,1), ", ", round_half_up(QR3, 1), ")")) %>%
  select(hdfa_preg, median_w_IQR) %>%
  pivot_wider(names_from = hdfa_preg, values_from=median_w_IQR) %>%
  mutate(var = "Median duration of follow up (IQR), months")


rate_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = sum(py_risk), cancer_outcome = sum(cancer_outcome))  %>% 
  ungroup() %>%
  mutate(rateper = cancer_outcome/pyar_all_ages*100000)


tmp <-cbind(rate_by_exposed$cancer_outcome[1],rate_by_exposed$pyar_all_ages[1])
rate_exposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                        conf.level = 0.95) *100000
tmp <-cbind(rate_by_exposed$cancer_outcome[2],rate_by_exposed$pyar_all_ages[2])
rate_unexposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                          conf.level = 0.95) *100000
rate_unexposed$hdfa_preg <- "unexposed_hdfa"
rate_exposed$hdfa_preg <- "exposed_hdfa"
rates <- rbind(rate_exposed, rate_unexposed) %>%
  mutate(rate_per =  paste0(sprintf("%0.1f",round_half_up(est,1)), " (",
                            sprintf("%0.1f", round_half_up(lower,1)), ", ", 
                            sprintf("%0.1f", round_half_up(upper,1)), ")")) %>%
  select(hdfa_preg, rate_per) %>%
  mutate(var= "Childhood cancer rate (per 100,000 PYAR) (95% CI)") %>%
  pivot_wider(names_from = hdfa_preg, values_from = rate_per)

overall_rates <- rbind(n_lb, cancer_by_exposed, pyar_by_exposed, rates , iqr_follows)
names(overall_rates) <- c("", "Singleton live births treated with high dose folic acid", "Singleton live births not treated with high dose folic acid")

##compute which age band incidence is in
##compute end dates for date entering each age band
##compute PYAR per age band
df<- df %>%
  mutate(reached_1st  = case_when(interval(date_end_pregnancy, end_follow)/years(1) >=1 ~1,T~0 ), 
    reached_5th  = case_when(interval(date_end_pregnancy, end_follow)/years(1) >=5 ~1,T~0 ), 
         reached_10th  = case_when(interval(date_end_pregnancy, end_follow)/years(1) >=10 ~1,T~0 )) %>%
  mutate(age_yr_at_diag = interval(date_end_pregnancy, incidence_date) /years(1)) %>% 
 #count incidence in each age range
   mutate(incid_0_1 = case_when(cancer_outcome==1 & age_yr_at_diag <1 ~1, T~0), 
         incid_1_4 = case_when(cancer_outcome==1 & age_yr_at_diag>=1 & cancer_outcome==1 & age_yr_at_diag <5 ~1, T~0), 
         incid_5_9 = case_when(cancer_outcome==1 & age_yr_at_diag >=5 & age_yr_at_diag <10 ~1, T~0), 
         incid_10_14 = case_when(cancer_outcome==1 & age_yr_at_diag >=10 & age_yr_at_diag <15 ~1, T~0) ) %>%
  #compute last date to count within each time period
  mutate(end_0_1 = case_when(reached_1st==1 &
                               reached_1st==1 ~ as.Date(date_end_pregnancy) +365.25,
                             T~ end_follow), 
         end_1_4 = case_when(reached_1st==0~NA,
                             reached_5th==1 &
                               reached_5th==1 ~ as.Date(date_end_pregnancy) +365.25*(5),
                             T~ end_follow), 
         end_5_9 = case_when(reached_5th==0 ~NA, 
                             reached_10th==1 ~ as.Date(date_end_pregnancy) + 365.25*(10), 
                             T~ end_follow),
         end_10_14 = case_when(reached_10th==0 ~NA, 
                               T~ end_follow))%>%
 #compute PYAR atributable to each age range
   mutate(pyar_0_1= interval(as.Date(date_end_pregnancy),end_0_1)/years(1), 
    pyar_1_4= interval(end_0_1,end_1_4)/years(1), 
         pyar_5_9= interval(end_1_4,end_5_9)/years(1), 
         pyar_10_14= interval(end_5_9,end_10_14)/years(1) ) %>%
  mutate(age_end_follow = interval(date_end_pregnancy,end_follow )/years(1))


##create values for each part of table
#0-1
n_lb <- n_lb %>% mutate(var="   Number of children retained to follow up at the start of this age group")
cancer_by_exposed <- df %>% group_by(hdfa_preg) %>% 
  summarise(incid_0_1 = sum(incid_0_1, na.rm=TRUE)) %>%
  mutate(var = "   Number diagnosed with childhood cancer within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = incid_0_1)
pyar_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = round_half_up(sum(pyar_0_1),1))  %>% 
  mutate(var = "   Person years at risk within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = pyar_all_ages)
rate_by_exposed0_1 <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_0_1 = sum(pyar_0_1), incid_0_1 = sum(incid_0_1 ))  %>% 
  ungroup() %>%
  mutate(rateper = incid_0_1 /pyar_0_1*100000)
tmp <-cbind(rate_by_exposed0_1$incid_0_1 [1],rate_by_exposed0_1$pyar_0_1[1])
rate_exposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                        conf.level = 0.95) *100000

tmp <-cbind(rate_by_exposed0_1$incid_0_1[2],rate_by_exposed0_1$pyar_0_1[2])
rate_unexposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                          conf.level = 0.95) *100000
rate_unexposed$hdfa_preg <- "unexposed_hdfa"
rate_exposed$hdfa_preg <- "exposed_hdfa"
rates <- rbind(rate_exposed, rate_unexposed) %>%
  mutate(rate_per = paste0(round_half_up(est,1), " (", round_half_up(lower,1), ", ", round_half_up(upper,1), ")")) %>%
  select(hdfa_preg, rate_per) %>%
  mutate(var= "   Childhood cancer rate within this age group (per 100,000 PYAR) (95% CI)") %>%
  pivot_wider(names_from = hdfa_preg, values_from = rate_per)
top_table <- as.data.frame(cbind(" Age <1 year", "", ""))
names(top_table) <- c("var", "exposed_hdfa", "unexposed_hdfa")

stats_0_1 <- rbind(top_table, n_lb, cancer_by_exposed, pyar_by_exposed, rates) %>%
  select(var, exposed_hdfa, unexposed_hdfa)
names(stats_0_1) <- c("", "", "")
#1-4
n_lb <- df %>% filter(age_end_follow >=1) %>% 
  group_by(hdfa_preg) %>% count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n)%>%
  mutate(var="   Number of children retained to follow up at the start of this age group")
cancer_by_exposed <- df %>% group_by(hdfa_preg) %>% 
  summarise(incid_1_4 = sum(incid_1_4, na.rm=TRUE)) %>%
  mutate(var = "   Number diagnosed with childhood cancer within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = incid_1_4)
pyar_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = round_half_up(sum(pyar_1_4, na.rm=T),1))  %>% 
  mutate(var = "   Person years at risk within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = pyar_all_ages)
rate_by_exposed1_4 <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_1_4 = sum(pyar_1_4, na.rm=T), incid_1_4 = sum(incid_1_4, na.rm=T ))  %>% 
  ungroup() %>%
  mutate(rateper = incid_1_4 /pyar_1_4*100000)
tmp <-cbind(rate_by_exposed1_4$incid_1_4 [1],rate_by_exposed1_4$pyar_1_4[1])
rate_exposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                        conf.level = 0.95) *100000

tmp <-cbind(rate_by_exposed1_4$incid_1_4[2],rate_by_exposed1_4$pyar_1_4[2])
rate_unexposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                          conf.level = 0.95) *100000
rate_unexposed$hdfa_preg <- "unexposed_hdfa"
rate_exposed$hdfa_preg <- "exposed_hdfa"
rates <- rbind(rate_exposed, rate_unexposed) %>%
  mutate(rate_per = paste0(sprintf("%0.1f",round_half_up(est,1)), " (", 
                           sprintf("%0.1f",round_half_up(lower,1)), ", ", 
                           sprintf("%0.1f",round_half_up(upper,1)), ")")) %>%
  select(hdfa_preg, rate_per) %>%
  mutate(var= "   Childhood cancer rate within this age group (per 100,000 PYAR) (95% CI)") %>%
  pivot_wider(names_from = hdfa_preg, values_from = rate_per)
top_table <- as.data.frame(cbind(" Age 1-4 years", "", ""))
names(top_table) <- c("var", "exposed_hdfa", "unexposed_hdfa")

stats_1_4 <- rbind(top_table, n_lb, cancer_by_exposed, pyar_by_exposed, rates) %>%
  select(var, exposed_hdfa, unexposed_hdfa)
names(stats_1_4) <- c("", "", "")

##5-9yrs
n_lb <- df %>% filter(age_end_follow >=5) %>% 
  group_by(hdfa_preg) %>% count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n)%>%
  mutate(var="   Number of children retained to follow up at the start of this age group")
cancer_by_exposed <- df%>%group_by(hdfa_preg) %>% 
  summarise(incid_5_9 = sum(incid_5_9, na.rm=TRUE)) %>%
  mutate(var = "   Number diagnosed with childhood cancer within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = incid_5_9)
pyar_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = round_half_up(sum(pyar_5_9, na.rm=T),1))  %>% 
  mutate(var = "   Person years at risk within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = pyar_all_ages)
rate_by_exposed_5_9 <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_5_9 = sum(pyar_5_9, na.rm=T), incid_5_9 = sum(incid_5_9))  %>% 
  ungroup() %>%
  mutate(rateper = incid_5_9 /pyar_5_9*100000)

tmp <-cbind(rate_by_exposed_5_9 $incid_5_9 [1],rate_by_exposed_5_9 $pyar_5_9[1])
rate_exposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                        conf.level = 0.95) *100000

tmp <-cbind(rate_by_exposed_5_9$incid_5_9[2],rate_by_exposed_5_9$pyar_5_9[2])
rate_unexposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                          conf.level = 0.95) *100000
rate_unexposed$hdfa_preg <- "unexposed_hdfa"
rate_exposed$hdfa_preg <- "exposed_hdfa"
rates <- rbind(rate_exposed, rate_unexposed) %>%
  mutate(rate_per = paste0(sprintf("%0.1f",round_half_up(est,1)), " (",
                           sprintf("%0.1f",round_half_up(lower,1)), ", ", 
                           sprintf("%0.1f",round_half_up(upper,1)), ")")) %>%
  select(hdfa_preg, rate_per) %>%
  mutate(var= "   Childhood cancer rate within this age group (per 100,000 PYAR) (95% CI)") %>%
  pivot_wider(names_from = hdfa_preg, values_from = rate_per)

top_table <- as.data.frame(cbind(" Age 5-9 years inclusive", "", ""))
names(top_table) <- c("var",  "exposed_hdfa" ,  "unexposed_hdfa")

stats_5_9 <- rbind(top_table, n_lb, cancer_by_exposed, pyar_by_exposed, rates) %>%
  select(var, exposed_hdfa, unexposed_hdfa)

names(stats_5_9) <- c("", "", "")

##10-13yrs
n_lb <- df %>% filter(age_end_follow >=10) %>% 
  group_by(hdfa_preg) %>% count() %>%
  pivot_wider(names_from = hdfa_preg, values_from = n)%>%
  mutate(var="   Number of children retained to follow up at the start of this age group")
cancer_by_exposed <- df%>%group_by(hdfa_preg) %>% 
  summarise(incid_10_14 = sum(incid_10_14, na.rm=TRUE)) %>%
  mutate(var = "   Number diagnosed with childhood cancer within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = incid_10_14)

pyar_by_exposed <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_all_ages = round_half_up(sum(pyar_10_14, na.rm=T),1))  %>% 
  mutate(var = "   Person years at risk within this age group") %>% 
  pivot_wider(names_from = hdfa_preg, values_from = pyar_all_ages)
rate_by_exposed_10_14 <- df %>% group_by(hdfa_preg) %>%
  summarise(pyar_10_14 = sum(pyar_10_14, na.rm=T), incid_10_14 = sum(incid_10_14))  %>% 
  ungroup() %>%
  mutate(rateper = incid_10_14/pyar_10_14*100000)

tmp <-cbind(rate_by_exposed_10_14$incid_10_14[1],rate_by_exposed_10_14$pyar_10_14[1])
rate_exposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                        conf.level = 0.95) *100000

tmp <-cbind(rate_by_exposed_10_14$incid_10_14[2],rate_by_exposed_10_14$pyar_10_14[2])
rate_unexposed <-epi.conf(tmp, ctype = "inc.rate", method = "exact", N = 1000, design = 1, 
                          conf.level = 0.95) *100000
rate_unexposed$hdfa_preg <- "unexposed_hdfa"
rate_exposed$hdfa_preg <- "exposed_hdfa"
rates <- rbind(rate_exposed, rate_unexposed) %>%
  mutate(rate_per = paste0(sprintf("%0.1f",round_half_up(est,1)), " (",
                           sprintf("%0.1f",round_half_up(lower,1)), ", ",
                           sprintf("%0.1f",round_half_up(upper,1)), ")")) %>%
  select(hdfa_preg, rate_per) %>%
  mutate(var= "   Childhood cancer rate within this age group (per 100,000 PYAR) (95% CI)") %>%
  pivot_wider(names_from = hdfa_preg, values_from = rate_per)

top_table <- as.data.frame(cbind(" Age 10-13 years inclusive", "", "") )
names(top_table) <- c("var",  "exposed_hdfa" ,  "unexposed_hdfa")

stats_10_14 <- rbind(top_table, n_lb, cancer_by_exposed, pyar_by_exposed, rates)%>%
  select(var, exposed_hdfa, unexposed_hdfa)
names(stats_10_14) <- c("", "", "")

