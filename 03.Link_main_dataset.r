###Create final ataset
source("00.setup.r")
##Load slipbd
slipbd1 <- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))
preg_ids <-slipbd1 %>% select(pregnancy_id)

##load exposure and outcome
df_pis_fa <-  readRDS(paste0(folder_data_path,"processed_extracts/PIS_hdfa_flags.rds")) %>% select(-est_date_conception)
df_hepma_fa <- readRDS(paste0(folder_data_path, "processed_extracts/hepma_hdfa_flags.rds"))

##combine to single flag & total counts of prescribed
df <- left_join(preg_ids, df_hepma_fa)
df_pis_fa <-df_pis_fa  %>% select(-mother_upi, -first_prescr, -last_prescr)

df_pis_fa <- df_pis_fa %>% 
  mutate(wks_first_presc = as.numeric(wks_first_presc))

df <- left_join(df ,df_pis_fa)

##fill in NAs with 0 and take max
df <- df %>% 
  mutate_at(vars(hdFA_hepma_to_conception, hdFA_hepma_to_12wks, hdFA_hepma_after_12wks, hdFA_hepma_in_preg,
                 n_hdfa_hepma_12, n_hdfa_hepma_preg,
                hdFA_PIS_to_conception,  hdFA_PIS_to_12wks, hdFA_PIS_after_12wks, hdFA_PIS_in_preg,  n_hdfa_12, n_hdfa_any), ~tidyr::replace_na(., 0)) %>%
  mutate(total_prescr_12wks =n_hdfa_12 +n_hdfa_hepma_12, 
         total_prescr =n_hdfa_any + n_hdfa_hepma_preg) %>%
  mutate(hdfa_conception = pmax(hdFA_hepma_to_conception, hdFA_PIS_to_conception),
         hfda_12weeks = pmax(hdFA_hepma_to_12wks, hdFA_PIS_to_12wks), 
         hdfa_after_12wks = pmax(hdFA_PIS_after_12wks, hdFA_hepma_after_12wks),
         hdfa_preg = pmax(hdFA_PIS_in_preg , hdFA_hepma_in_preg)) %>%
  select(pregnancy_id, total_prescr_12wks, total_prescr,hdfa_conception,  hfda_12weeks, hdfa_after_12wks, hdfa_preg  )

df<- left_join(slipbd1, df)

#Cancer outcomes####
cancer_outcome <- read_parquet(paste0(folder_data_path,"extracts/temp_smr06_child.parquet"))
source("cancer_groupings.r")
cancer_outcome <- cancer_outcome %>% 
  select(upi_number, incidence_date, date_of_birth, sex, death_certificate_only,
         icd10s_cancer_site, icdo2_icdo2, type_icdo,
         morph_morphology, type_icdo3, group, group_desc) %>% mutate(cancer_outcome=1) %>%
  rename(cancer_group = group, cancer_group_desc = group_desc)

df_cancer <- left_join(slipbd1 %>% 
                         select(pregnancy_id, baby_upi, date_end_pregnancy ), 
                       cancer_outcome, by = c("baby_upi" = "upi_number"))

df_cancer<- df_cancer %>% filter(!is.na(incidence_date)) %>%
  arrange(baby_upi, incidence_date) %>% group_by(baby_upi) %>% slice(1) %>% ungroup() %>%
  mutate(time_to_event = as.Date(incidence_date)-as.Date(date_end_pregnancy))%>%
  select(-date_end_pregnancy, -sex, -death_certificate_only)

df<- left_join(df, df_cancer)
##Comorbidity
df_comorb <- readRDS(paste0(folder_data_path, "processed_extracts/all_comorbs.rds") )

df<- left_join(df, df_comorb)

##methotrexate####
df_mtx_pis<- readRDS("/conf/FolicAcid/data/processed_extracts/methotrextate_PIS.rds")
#(there were no exposures to methotrexate in hepma)
df<- left_join(df, df_mtx_pis)

###ASM exposure####
df_asm_PIS <- readRDS("/conf/FolicAcid/data/processed_extracts/asm_PIS.rds")
df_asm_hepma <- readRDS("/conf/FolicAcid/data/processed_extracts/asm_hepma.rds")

df_asm <- preg_ids %>% left_join(df_asm_PIS)
df_asm <- left_join(df_asm, df_asm_hepma)

df_asm <- df_asm %>%
  mutate_at(vars( asm_PIS_conception, asm_PIS_to_12_wks, asm_PIS_after_12wks, 
                  asm_anytime, asm_hpm_conception, asm_hpm_to_12_wks, asm_hpm_after_12wks, asm_hpm_anytime),
            ~tidyr::replace_na(., 0)) 

df_asm <- df_asm %>%
  mutate(asm_conception = pmax(asm_PIS_conception, asm_hpm_conception), 
         asm_to_12wks = pmax(asm_hpm_to_12_wks, asm_PIS_to_12_wks), 
         asm_after_12_wks = pmax(asm_hpm_after_12wks, asm_PIS_after_12wks), 
         asm_anytime = pmax(asm_anytime, asm_hpm_anytime)) %>%
  select(pregnancy_id, asm_conception, asm_to_12wks, asm_after_12_wks, asm_anytime)

df<- left_join(df, df_asm)


rm(df_asm_hepma, df_asm_PIS, df_asm)

##Censoring####
###deaths####
nrs_deaths <- readRDS(paste0(folder_data_path, "extracts/nrs_deaths_child.rds"))
child_deaths <- nrs_deaths %>% select(upi_number, date_of_death) %>%
  rename(child_date_of_death = date_of_death)
df<- left_join(df, child_deaths, by= c("baby_upi"= "upi_number" ))


###CHI transfer####
transfers <- readRDS(paste0(folder_data_path, "extracts/CHILI_babies2_first_transfer.rds"))
names(transfers)
transfers <-transfers %>% select(pregnancy_id, baby_upi, TRANSFER_OUT_CODE, DATE_TRANSFER_OUT, DATE_OF_DEATH, destination) %>%
  rename(chi_date_of_death = DATE_OF_DEATH, transfer_destination = destination)
df<- left_join(df, transfers)

###Congenital anomaly ###
anom <- readRDS(paste0(folder_data_path, "processed_extracts/linked_congenital_conditions.rds"))
df <- left_join(df, anom)

###Tidy up - Add groupings sort NAs####
df <- df %>% mutate(cancer_outcome = case_when(is.na(cancer_outcome) ~ 0, T~ cancer_outcome))

df <- df %>% mutate(hdfa_preg = case_when(hdfa_preg==0 ~"unexposed_hdfa", 
                                          hdfa_preg==1 ~"exposed_hdfa",))
df <- df %>% 
  mutate(death_date= case_when(!is.na(date_infant_death)~date_infant_death, 
                               !is.na(child_date_of_death)~child_date_of_death)) %>%
  mutate(time_to_death = as.Date(death_date) - as.Date(date_end_pregnancy)) %>%
  mutate(death = case_when(!is.na(time_to_death)~1, T~0)) 

df<- df %>% 
  mutate(age_group = case_when(maternal_age_conception < 20 ~ "<20", 
                               maternal_age_conception >=20 & maternal_age_conception <25 ~ "20-24",
                               maternal_age_conception >=25 & maternal_age_conception <30 ~ "25-29",
                               maternal_age_conception >=30 & maternal_age_conception <35 ~ "30-34",
                               maternal_age_conception >=35 & maternal_age_conception <40 ~ "35-39",
                               maternal_age_conception >=40 ~ "40+",
                               T~"Unknown"))
df<- df %>% 
  mutate(bw_group = case_when(birthweight < 1500 ~ "<1500", 
                              birthweight >= 1500 & birthweight <2500 ~ "1500-2499",
                              birthweight >= 2500 & birthweight <4000 ~ "2500-3999",
                              birthweight >= 4000 ~ ">4000", T~"Unknown"))
##bmi
df<- df %>% 
  mutate(bmi_group = case_when(maternal_bmi < 18.5 ~"Underweight", 
                               maternal_bmi >= 18.5 & maternal_bmi < 25 ~"Healthy weight", 
                               maternal_bmi >= 25 & maternal_bmi < 30 ~"Overweight",
                               maternal_bmi >= 30 ~"Obese", T~ "Unknown" )) %>%
  mutate(obese = case_when(maternal_bmi <30 ~0, 
                           maternal_bmi >= 30 ~1,
                           T~ NA))
##comorbidity 
df <- df %>%
  mutate_at(vars(vte, hypertension, demyelinating_NMD, haematological,thyroid, 
                 asthma, GI_dis, liver, Imm_mediated_joint_ct, kidney_dis, dis_pelvic_genital_tract, 
                 skin), ~replace_na(., 0)) %>%
  mutate(any_comorb = pmax(vte, hypertension, demyelinating_NMD, haematological,thyroid, 
                           asthma, GI_dis, liver, Imm_mediated_joint_ct, kidney_dis, dis_pelvic_genital_tract, 
                           skin))


##followup
df <- df %>% mutate(start_follow = date_end_pregnancy) %>%
  mutate(cancer_after_transfer = case_when(incidence_date > DATE_TRANSFER_OUT ~1, T~0)) %>% 
  ##clean dates to remove all but the first end point
  mutate(clean_incidence_date= case_when(cancer_after_transfer==1 ~NA, T~incidence_date)) %>% #remove cancer incidence if after transfer
  mutate(clean_transfer_date = case_when(DATE_TRANSFER_OUT > incidence_date ~ NA,
                                         DATE_TRANSFER_OUT > death_date ~ NA, # transfer out record occasionally lags NRS date death - take NR date as more reliable
                                         T~DATE_TRANSFER_OUT)) %>%
  mutate(end_follow_type = case_when(!is.na(clean_incidence_date) ~ "cancer", 
                                     !is.na(clean_transfer_date) ~ "censor - emigration", 
                                     !is.na(death_date) ~ "censor - death", T~"end study") ) %>%
  mutate(end_follow = case_when(end_follow_type =="cancer" ~ incidence_date, 
                                end_follow_type == "censor - emigration" ~ clean_transfer_date, 
                                end_follow_type == "censor - death" ~ death_date,
                                end_follow_type == "end study" ~ as.Date("2023/12/31"))) %>%
  mutate(follow_days = as.Date(end_follow) - as.Date(date_end_pregnancy))


df <- df %>% mutate(maternal_simd = case_when(maternal_simd_end_preg=="Unknown" ~ maternal_simd_booking, 
                                              T~maternal_simd_end_preg))


follow_avg <- df %>% group_by(hdfa_preg) %>% summarise(cancer_n = sum(cancer_outcome), 
                                                       followup = sum(follow_days))


###fill in SIMD and ethnicity
missing_simd <- df %>% filter(maternal_simd=="Unknown")

filler_records <- df %>% filter(maternal_simd != "Unknown" & mother_upi %in% missing_simd$mother_upi)
##only 14 pregnancies

filler_records <- filler_records %>% select(pregnancy_id, mother_upi, maternal_simd, date_end_pregnancy)

fixable <- missing_simd %>% filter(mother_upi %in% filler_records$mother_upi) %>% 
  select(pregnancy_id, mother_upi, maternal_simd, date_end_pregnancy)

df_fixit <- rbind(fixable, filler_records)
df_fixit <- df_fixit %>% arrange(mother_upi, date_end_pregnancy)

df_fixit <- df_fixit %>% group_by(mother_upi) %>%
  mutate(imputed_simd = case_when(maternal_simd=="Unknown" ~ lag(maternal_simd), T~maternal_simd))  %>%
  mutate(imputed_simd = case_when(imputed_simd=="Unknown" ~ lag(imputed_simd), T~imputed_simd)) %>% ungroup()
###get the imputed simd 
df_imputed <- df_fixit %>% filter(maternal_simd=="Unknown") %>%
  select(pregnancy_id, imputed_simd)
df <- left_join(df,df_imputed) %>% 
  mutate(maternal_simd= case_when(!is.na(imputed_simd) ~ imputed_simd, T~maternal_simd))

##ethnicity
missing_and_old <- c("99", "1D", "1J", "3A", "3B", "3C", "3D", "4A", "4B", "4C", "4Z")
##do just missing first
missing_eth <- df %>% filter(maternal_ethnicity=="99")

filler_records <- df %>% filter(maternal_ethnicity != "99" & mother_upi %in% missing_eth$mother_upi)


filler_records <- filler_records %>% select(pregnancy_id, mother_upi, maternal_ethnicity, date_end_pregnancy)

fixable <- missing_eth %>% filter(mother_upi %in% filler_records$mother_upi) %>% 
  select(pregnancy_id, mother_upi, maternal_ethnicity, date_end_pregnancy)
nrow(fixable)

df_fixit <- rbind(fixable, filler_records)
##count group size and record order
df_fixit <- df_fixit %>% arrange(mother_upi, date_end_pregnancy) %>%
  group_by(mother_upi) %>% mutate(row_no = row_number(), n_records= n()) %>% ungroup()

count_chi <- df_fixit %>% ungroup() %>% group_by((mother_upi)) %>%count()

##one rond of lag and lead should take care of the pairs - larger groups may need more
df_fixit1 <- df_fixit %>% group_by(mother_upi) %>%
  ##take lagged ethnicity when it is 99
  mutate(imputed_eth = case_when(maternal_ethnicity=="99" ~ lag(maternal_ethnicity), 
                                 T~maternal_ethnicity))  %>%
  ##then take the lead when ethnicity is still 99 or is missing.
    mutate(imputed_eth = case_when(imputed_eth=="99" | is.na(imputed_eth) ~lead(maternal_ethnicity), 
                     T~imputed_eth)) %>% 
   mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no< (n_records-1)~ lead(maternal_ethnicity,2),
                     T~imputed_eth)) %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no>2~ lag(maternal_ethnicity,2),
                     T~imputed_eth))  %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no< (n_records-2)~ lead(maternal_ethnicity,3),
                     T~imputed_eth)) %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no>3~ lag(maternal_ethnicity,3),
                     T~imputed_eth))  %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no< (n_records-3)~ lead(maternal_ethnicity,4),
                     T~imputed_eth)) %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no>4~ lag(maternal_ethnicity,4),
                     T~imputed_eth))  %>% 
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no< (n_records-4)~ lead(maternal_ethnicity,5),
                     T~imputed_eth)) %>%
  mutate(imputed_eth = 
           case_when((imputed_eth=="99"| is.na(imputed_eth)) & row_no>5~ lag(maternal_ethnicity,5),
                     T~imputed_eth))  %>% 
  
  ungroup() %>%
  group_by(mother_upi) %>% 
  mutate(max_eth = max(imputed_eth))

df_imputed <- df_fixit1 %>% ungroup() %>%
  filter(maternal_ethnicity=="99" | is.na(maternal_ethnicity)) %>%
  select(pregnancy_id, imputed_eth)
df <- left_join(df,df_imputed) %>% 
  mutate(maternal_ethnicityinfilled= case_when(!is.na(imputed_eth) ~ imputed_eth, T~maternal_ethnicity))

##check endpoints
df <-df %>% mutate(any_child_cancer = case_when(!is.na(incidence_date)~1, T~0), 
                   cancer_outcome = case_when(end_follow_type !="cancer" ~0, T~cancer_outcome))

saveRDS(df, paste0(folder_data_path, "working_data/main_dataset.rds"))
