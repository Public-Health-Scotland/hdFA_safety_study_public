##a flags to main dataset
df<- df %>% mutate(total_hdfa_presc_grp = case_when(total_prescr ==0 ~"0", 
                                                    total_prescr==1 ~"1", 
                                                    total_prescr==2 ~"2", 
                                                    total_prescr>=3 ~ "\u2265 3")) %>%
  mutate(total_hdfa_presc_grp = factor(total_hdfa_presc_grp, c("0", "1", "2", "\u2265 3")) )

t_n_prescr <- df %>% filter(hdfa_preg=="exposed_hdfa") %>% group_by(total_hdfa_presc_grp) %>% count() %>%
  ungroup() %>%  mutate(`%` = round(n/sum(n)*100,1)) %>%
  rename(`Number of prescriptions dispensed during exposure window` = total_hdfa_presc_grp)
names(t_n_prescr) <- c("Number of prescriptions dispensed during exposure window", "", " ")

###determining week of first prescription
hdfa_pis <- readRDS(paste0(folder_data_path,"processed_extracts/PIS_hdfa_flags.rds"))
hdfa_pis <- hdfa_pis %>% mutate(wks_first_presc = as.numeric(wks_first_presc)) %>% 
  select(pregnancy_id, wks_first_presc)%>% rename(p_wks_first_presc = wks_first_presc )


hepma_hdfa <- readRDS(paste0(folder_data_path, "processed_extracts/hepma_hdfa_flags.rds")) 
hepma_hdfa <- hepma_hdfa %>% 
  mutate(wks_first_presc = floor((as.Date(first_prescr) -as.Date(est_date_conception))/7)+2) %>%
  filter(hdFA_hepma_in_preg==1 & wks_first_presc >=(-10))  %>% 
  select(pregnancy_id, wks_first_presc) %>% rename(h_wks_first_presc = wks_first_presc )

#join to main data and group weeks
df <- df %>% left_join(hdfa_pis)
df <- df %>% left_join(hepma_hdfa)
df <- df %>%
  mutate(first_hdfa_week = pmin(h_wks_first_presc, p_wks_first_presc, na.rm = T)) %>%
  mutate(first_hdfa_time = case_when(first_hdfa_week >=(-10) & first_hdfa_week <2 ~ "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)", 
                                     first_hdfa_week >=2 & first_hdfa_week <12 ~ "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                     first_hdfa_week >=12 & first_hdfa_week <24 ~ "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                     first_hdfa_week >=24 ~ "Third trimester (\u226524\u207A\u2070)")) %>%
  mutate(first_hdfa_time= factor(first_hdfa_time, 
                                 c( "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)",
                                    "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                    "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                    "Third trimester (\u226524\u207A\u2070)")))

####
PIS_hdFA_all<- readRDS(paste0(folder_data_path, "processed_extracts/PIS_hdFA_processed.rds") )
#names(slipbd1)
slipbd1 <- df %>%
  select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy, hdfa_preg)


###join pregs and hdfa and just keep ones with a flag#
pregs_hdfa <- left_join(slipbd1, PIS_hdFA_all) %>% filter(hdFA_PIS==1)

### determine whether the hdfa prescription is relevant to this pregnancy.
pregs_hdfa <- pregs_hdfa %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception) + (7*10)) %>%
  mutate(hdFA_PIS_to_conception = case_when(date_to_use >= start_window & date_to_use <  as.Date(est_date_conception)  ~1 , T~0), 
         hdFA_PIS_to_12wks = case_when(date_to_use >= as.Date(est_date_conception) & date_to_use < end_window ~1 , T~0), 
         hdFA_PIS_in_preg = case_when(date_to_use >= start_window & date_to_use <= date_end_pregnancy ~1 , T~0), 
         hdFA_PIS_after_12wks=case_when(date_to_use >= end_window & date_to_use <= date_end_pregnancy ~1 , T~0)  ) 
table(pregs_hdfa$hdFA_PIS_in_preg, pregs_hdfa$hdFA_PIS_to_12wks)

##filter to remove prescriptions not relevant to pregnancy
pregs_hdfa <- pregs_hdfa %>% filter(hdFA_PIS_in_preg==1)

##count of tablets in relevant prescriptions
hdfa_summary <- pregs_hdfa %>%
  mutate(dispensed_quantity_tabs = case_when(dispensed_strength_per_uo_m=="ML"~0, T~dispensed_quantity)) %>%
  mutate(liquid_prescr = case_when(dispensed_strength_per_uo_m=="ML" ~1, T~0)) %>%
  mutate(censored_due_to_n = case_when(dispensed_quantity_tabs < 7 |dispensed_quantity_tabs >200 ~1, T~0)) %>%
  mutate(dispensed_quantity_tabs_censor= case_when(dispensed_quantity_tabs < 7 |dispensed_quantity_tabs >200 ~ 0,
                                                   T~dispensed_quantity_tabs)) %>%
  group_by(pregnancy_id, mother_upi) %>%
  mutate(n_hdfa_12 = sum(hdFA_PIS_to_12wks),n_hdfa_any = sum(hdFA_PIS_in_preg )) %>% 
  summarise(est_date_conception = first(est_date_conception),
            first_prescr = min_(date_to_use), 
            last_prescr = max_(date_to_use),
            hdFA_PIS_to_conception  = max_(hdFA_PIS_to_conception ),
            hdFA_PIS_to_12wks = max_(hdFA_PIS_to_12wks), 
            hdFA_PIS_in_preg = max_(hdFA_PIS_in_preg),
            hdFA_PIS_after_12wks = max_(hdFA_PIS_after_12wks),
            n_hdfa_12 = max(n_hdfa_12), 
            n_hdfa_any = max(n_hdfa_any), 
            n_liquid_hdfa = sum(liquid_prescr),
            n_dispensed_tabs = sum(dispensed_quantity_tabs),
            n_dispensed_tabs_censoring = sum(dispensed_quantity_tabs_censor)
  ) %>% ungroup() 

hdfa_summary <- hdfa_summary  %>%  mutate(wks_first_presc_p = floor((as.Date(first_prescr) -as.Date(est_date_conception))/7)+2)


###hepma
hepma_extract_hdfa <- readRDS(paste0(folder_data_path, "extracts/hepma_hdfa_raw.rds"))


hepma_extract_hdfa <-hepma_extract_hdfa %>% mutate(hdfa_HEPMA=1) %>%
  filter(prescription_has_no_associated_admin=="N")
hepma_extract_hdfa <-hepma_extract_hdfa %>% 
  mutate(admin_not_given = case_when(admin_reason_not_given %in%
                                       c("PATIENT SELF ADMINISTERED", "SELF ADMINISTERED", 
                                         "SELF ADMINISTERED -DAY SURGERY ONLY","SELF ADMINISTERS" )~"N", T~admin_not_given)) %>%
  filter(admin_not_given=="N")

hepma_counts <- hepma_extract_hdfa %>% 
  mutate(admin_given_date_time = 
           case_when(is.na(admin_given_date_time)~ presc_start_date_time, T~admin_given_date_time)) %>%
  arrange(patient_upi_number, presc_unique_id, admin_given_date_time) %>%
  group_by(patient_upi_number, presc_unique_id) %>% 
  summarise(count_admins = n(), 
            presc_start_date_time = first_(presc_start_date_time),
            admin_given_date_time = first_(admin_given_date_time)) %>%
  mutate(hdfa_HEPMA=1) %>% ungroup()

pregs_hdfa <- left_join(slipbd1, hepma_counts  , by = c("mother_upi" = "patient_upi_number")) %>% 
  filter(hdfa_HEPMA==1)

#determine week of prescribing
pregs_hdfa <- pregs_hdfa %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception) + (7*10) , 
                                    presc_start= as.Date(admin_given_date_time) ) %>%
  mutate(hdFA_hepma_to_conception = case_when(presc_start >= start_window & presc_start < as.Date(est_date_conception) ~1 , T~0), 
         hdFA_hepma_to_12wks = case_when(presc_start >= as.Date(est_date_conception) & presc_start < end_window ~1 , T~0), 
         hdFA_hepma_after_12wks = case_when(presc_start >= end_window & presc_start < date_end_pregnancy ~1 , T~0), 
         hdFA_hepma_in_preg = case_when(presc_start >= start_window & presc_start <= date_end_pregnancy ~1 , T~0)) %>%
  filter(hdFA_hepma_in_preg==1) 


pregs_hdfa_minimal <- pregs_hdfa %>%
  group_by(pregnancy_id) %>%
  mutate(n_hdfa_12 = sum(hdFA_hepma_to_12wks)+sum(hdFA_hepma_to_conception),n_hdfa_any = sum(hdFA_hepma_in_preg)) %>%
  summarise(first_prescr_h = min_(presc_start),
            est_date_conception=first(est_date_conception),
            hdFA_hepma_to_conception =max_(hdFA_hepma_to_conception),
            hdFA_hepma_to_12wks = max_(hdFA_hepma_to_12wks),
            hdFA_hepma_after_12wks = max_(hdFA_hepma_after_12wks),
            hdFA_hepma_in_preg = max_(hdFA_hepma_in_preg), 
            n_hdfa_hepma_12 = max_(n_hdfa_12), 
            n_hdfa_hepma_preg = max_(n_hdfa_any ), 
            n_hepma_dispensed_tabs = sum(count_admins)) %>% ungroup()


pregs_hdfa_minimal<- pregs_hdfa_minimal  %>% 
  mutate(wks_first_presc_h = floor((as.Date(first_prescr_h) -as.Date(est_date_conception))/7)+2)

names(  hdfa_summary)

df_tabs <-left_join(slipbd1
               , hdfa_summary)
df_tabs<- left_join(df_tabs, pregs_hdfa_minimal)

#
df_tabs <- df_tabs %>% filter(hdFA_PIS_in_preg==1 | hdFA_hepma_in_preg==1)
df_tabs <- df_tabs %>% 
  mutate(n_dispensed_tabs = replace_na(n_dispensed_tabs,0), 
         n_hepma_dispensed_tabs = replace_na(n_hepma_dispensed_tabs,0) ) %>%
  mutate(total_tablets = n_dispensed_tabs+n_hepma_dispensed_tabs) %>%
  mutate(total_tablets_censor=  n_dispensed_tabs_censoring +n_hepma_dispensed_tabs) %>%
  mutate(n_hdfa_hepma_preg = replace_na(n_hdfa_hepma_preg,0),
         n_hdfa_any = replace_na(n_hdfa_any,0)) %>%
  mutate(total_prescr = n_hdfa_hepma_preg + n_hdfa_any) %>%
  mutate(wk_first_prescr = pmin(wks_first_presc_h , wks_first_presc_p, na.rm=T)) %>%
  mutate(group_prescr = case_when(total_prescr==1 ~"1", 
                                  total_prescr==2 ~"2",
                                  total_prescr>=3 ~">=3" ), 
         group_tabs = case_when(total_tablets==0 ~ "0 missing", 
                                total_tablets<28 ~ "1-27",
                                total_tablets>=28 &total_tablets<56 ~ "28-55",
                                total_tablets>=56 & total_tablets <84 ~ "56-83", 
                                total_tablets>=84  & total_tablets <112~"84-112", 
                                total_tablets>=112 ~ "112+")) 

#group weeks
df_tabs <- df_tabs %>%
  mutate(first_hdfa_time = case_when(wk_first_prescr >=(-10) & wk_first_prescr <2 ~ "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)", 
                                     wk_first_prescr >=2 & wk_first_prescr <12 ~ "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                     wk_first_prescr >=12 & wk_first_prescr <24 ~ "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                     wk_first_prescr >=24 ~ "Third trimester (\u226524\u207A\u2070)"))%>%
  mutate(first_hdfa_time= factor(first_hdfa_time, 
                                 c( "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)",
                                    "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                    "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                    "Third trimester (\u226524\u207A\u2070)")))


df_tabs <- df_tabs %>% mutate(total_hdfa_presc_grp = case_when(total_prescr ==0 ~"0", 
                                                    total_prescr==1 ~"1", 
                                                    total_prescr==2 ~"2", 
                                                    total_prescr>=3 ~ "\u2265 3")) %>%
  mutate(total_hdfa_presc_grp = factor(total_hdfa_presc_grp, c("0", "1", "2", "\u2265 3")) )

t_n_prescr <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% group_by(total_hdfa_presc_grp) %>% count() %>%
  ungroup() %>%  mutate(`%` = round(n/sum(n)*100,1)) %>%
  rename(`Number of prescriptions dispensed during exposure window` = total_hdfa_presc_grp)
names(t_n_prescr) <- c("Number of prescriptions dispensed during exposure window", "", " ")

###determine week of prescription
hdfa_pis <- readRDS(paste0(folder_data_path,"processed_extracts/PIS_hdfa_flags.rds"))

hdfa_pis <- hdfa_pis %>% mutate(wks_first_presc = as.numeric(wks_first_presc)) %>% 
  select(pregnancy_id, wks_first_presc)%>% rename(p_wks_first_presc = wks_first_presc )


hepma_hdfa <- readRDS(paste0(folder_data_path, "processed_extracts/hepma_hdfa_flags.rds")) 
hepma_hdfa <- hepma_hdfa %>% 
  mutate(wks_first_presc = floor((as.Date(first_prescr) -as.Date(est_date_conception))/7)+2) %>%
  filter(hdFA_hepma_in_preg==1 & wks_first_presc >=(-10))  %>% 
  select(pregnancy_id, wks_first_presc) %>% rename(h_wks_first_presc = wks_first_presc )
df_tabs <- df_tabs %>% left_join(hdfa_pis)
df_tabs <- df_tabs %>% left_join(hepma_hdfa)

df_tabs <- df_tabs %>%
  mutate(first_hdfa_week = pmin(h_wks_first_presc, p_wks_first_presc, na.rm = T)) %>%
  mutate(first_hdfa_time = case_when(first_hdfa_week >=(-10) & first_hdfa_week <2 ~ "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)", 
                                     first_hdfa_week >=2 & first_hdfa_week <12 ~ "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                     first_hdfa_week >=12 & first_hdfa_week <24 ~ "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                     first_hdfa_week >=24 ~ "Third trimester (\u226524\u207A\u2070)")) %>%
  mutate(first_hdfa_time= factor(first_hdfa_time, 
                                 c( "Pre-conception (-10\u207A\u2070 to 1\u207A\u2076)",
                                    "First trimester (2\u207A\u2070 to 11\u207A\u2076)", 
                                    "Second trimester (12\u207A\u2070 to 23\u207A\u2076)",
                                    "Third trimester (\u226524\u207A\u2070)"))) 

##crosstabs n prescribed and first trimester prescribed
t_time_n_prescr <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  group_by(first_hdfa_time, total_hdfa_presc_grp) %>% count() %>%
  ungroup() %>%
  pivot_wider(names_from=total_hdfa_presc_grp, values_from = n) %>%
  mutate(total = `1`  + `2` +`≥ 3`)
t_n_prescr_total <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  mutate(first_hdfa_time = "Total") %>%
  group_by(first_hdfa_time, total_hdfa_presc_grp) %>% count() %>%
  ungroup() %>%
  pivot_wider(names_from=total_hdfa_presc_grp, values_from = n) %>%
  mutate(total = `1`  + `2` +`≥ 3`)

t_time_n_prescr <- rbind(t_time_n_prescr,t_n_prescr_total  )
##total pregs with known table numbers
n_known_n_tabs <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  filter(n_liquid_hdfa==0 & total_tablets>0) %>%
  group_by(first_hdfa_time) %>% count()
n_known_total <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  mutate(first_hdfa_time = "Total") %>%
  filter(n_liquid_hdfa==0 & total_tablets>0) %>%
  group_by(first_hdfa_time) %>% count()
n_known_n_tabs <- rbind(n_known_n_tabs, n_known_total)
##median and IQR of tablets by trimester of first prescribed. 
n_tabs_summary <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 
  group_by(first_hdfa_time) %>% 
  summarise(median = median(total_tablets), 
            QR1 = quantile(total_tablets, probs=0.25), 
            QR3 = quantile(total_tablets, probs=0.75),)
n_tabs_sum_totals <- df_tabs %>% filter(hdfa_preg=="exposed_hdfa") %>% 

  mutate(first_hdfa_time = "Total") %>%
  group_by(first_hdfa_time) %>%
  summarise(median = median(total_tablets), 
            QR1 = quantile(total_tablets, probs=0.25), 
            QR3 = quantile(total_tablets, probs=0.75),)
n_tabs_summary <-rbind(n_tabs_summary, n_tabs_sum_totals )
##join tables into one. (we will take care of cell merges and table headers in the excel writing script)
tab_n_prescrib <- left_join(t_time_n_prescr,n_known_n_tabs) %>%
  rename(`Number births with known total quantity of hdFA tablets`  = n )
tab_n_prescrib <- left_join(tab_n_prescrib, n_tabs_summary)%>%
  rename(`Timing of first prescription` = first_hdfa_time) %>%
  mutate(Median_IQR = paste0(median, " (", QR1, ", ",QR3, ")" )) %>% select(-c(median, QR1, QR3))


names(tab_n_prescrib) <-
  c("Gestation the first prescription in the exposure window was dispensed (completed weeks)\u1d43",
    "1 prescription"  , "2 prescriptions" ,"≥ 3 prescriptions", "Total", 
    "Number of singleton live births with known total quantity of hdFA\u1d9c tablets dispensed during the exposure period", 
    "Median (IQR\u1d49)")
