hepma_extract_asm <- dbGetQuery(denodo_connection, "SELECT patient_chi_number, patient_upi_number, patient_date_of_birth,
                              patient_sex, patient_sex_desc,
                              reporting_date, presc_start_date_time, admin_given_date_time,
                              prescription_has_no_associated_admin, admin_not_given, admin_reason_not_given,
                              dmd_bnf_code,
                              dmd_code, dmd_vmp_name, dmd_vtm_name, dmd_atc_code,
                              dmd_atc_code_description, medication_name,
                              med_instruction, medication_formulation, med_strength,
                              dmd_ddd_conversion_factor, treatment_health_board_name,
                              presc_unique_id, admin_unique_id
                            FROM hepma.hepma_administration_prescription_analysis
                            WHERE presc_start_date_time >= '2020-01-01'
                            AND presc_start_date_time <= '2023-12-31'
                           AND treatment_health_board_name NOT IN ('STATE HOSPITAL')
                           AND (dmd_bnf_code LIKE '0408%'
                           OR (dmd_bnf_code LIKE '0402%'
                                 AND dmd_vtm_name IN ('Sodium valproate','Valproic acid')))") 



###filter to only those in cohort.
preg_details <-  read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet")) %>% select(pregnancy_id, mother_upi)
hepma_extract_asm <- hepma_extract_asm %>% filter(patient_chi_number %in% preg_details$mother_upi | 
                                                      patient_upi_number %in% preg_details$mother_upi)

saveRDS(hepma_extract_asm, paste0(folder_data_path, "extracts/hepma_asm_raw.rds"))
hepma_extract_asm <- readRDS(paste0(folder_data_path, "extracts/hepma_asm_raw.rds"))


###ASMs####
###Flag exposure in three time periods - before conception, oconception to 11+6 and 12+ weeks 
hepma_extract_asm  <-hepma_extract_asm  %>% 
  mutate(asm_hepma = 1) %>%
  select(patient_upi_number, presc_start_date_time, admin_given_date_time, 
         prescription_has_no_associated_admin, admin_not_given, 
         admin_reason_not_given, presc_unique_id,
         asm_hepma, dmd_vtm_name) %>%
  filter(prescription_has_no_associated_admin=="N")
table(hepma_extract_asm$admin_reason_not_given)

hepma_extract_asm <-hepma_extract_asm  %>% 
  mutate(admin_not_given = 
           case_when(admin_reason_not_given %in%
                       c("PATIENT SELF ADMINISTERED", "SELF ADMINISTERED", 
                         "SELF ADMINISTERED -DAY SURGERY ONLY","SELF ADMINISTERS" )~"N", T~admin_not_given)) %>%
  filter(admin_not_given=="N")%>%
  mutate(admin_given_date_time = 
           case_when(is.na(admin_given_date_time)~ presc_start_date_time, T~admin_given_date_time)) %>%
  arrange(patient_upi_number, presc_unique_id, admin_given_date_time) %>%
  group_by(presc_unique_id) %>% slice(1)%>% mutate(hdfa_HEPMA=1)


slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)

###join pregs and hdfa and just keep ones with a flag
pregs_asm <- left_join(slipbd1, hepma_extract_asm,
                       by=c("mother_upi" = "patient_upi_number") ) %>%
                         filter(asm_hepma==1) 

###now determine whether the hdfa prescription is relevant to this pregnancy.
pregs_asm  <- pregs_asm  %>%
  mutate(admin_date = as.Date(admin_given_date_time)) %>%
  mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception)) %>%
  mutate(start_window2 = as.Date(est_date_conception) , 
         end_window2 = as.Date(est_date_conception)+ (7*10)) %>%
  mutate(asm_hpm_conception = case_when(admin_date >= start_window & admin_date <= end_window ~1 , T~0), 
         asm_hpm_to_12_wks = case_when(admin_date >= start_window2 & admin_date <= end_window2 ~1 , T~0), 
         asm_hpm_after_12wks=case_when(admin_date >=end_window2 & admin_date <= date_end_pregnancy ~1 , T~0))%>%
  mutate(asm_hpm_anytime = 
           case_when(asm_hpm_conception==1| asm_hpm_to_12_wks==1 | asm_hpm_after_12wks==1 ~1 , T~0))%>%
  filter(asm_hpm_anytime==1)


pregs_asm <- pregs_asm %>% 
  group_by(pregnancy_id)%>%
  summarise(asm_hpm_conception = max(asm_hpm_conception), 
            asm_hpm_to_12_wks = max(asm_hpm_to_12_wks), 
            asm_hpm_after_12wks=max(asm_hpm_after_12wks), 
            asm_hpm_anytime = max(asm_hpm_anytime) ) %>% ungroup() 

saveRDS(pregs_asm, paste0(folder_data_path, "processed_extracts/asm_hepma.rds"))




###methotrexate####
hepma_extract_mtx <- dbGetQuery(denodo_connection, "SELECT patient_chi_number, patient_upi_number, patient_date_of_birth,
                              patient_sex, patient_sex_desc,
                              reporting_date, presc_start_date_time, admin_given_date_time,
                              prescription_has_no_associated_admin, admin_not_given, admin_reason_not_given,
                              dmd_bnf_code,
                              dmd_code, dmd_vmp_name, dmd_vtm_name, dmd_atc_code,
                              dmd_atc_code_description, medication_name,
                              med_instruction, medication_formulation, med_strength,
                              dmd_ddd_conversion_factor, treatment_health_board_name,
                              presc_unique_id, admin_unique_id
                            FROM hepma.hepma_administration_prescription_analysis
                            WHERE presc_start_date_time >= '2020-01-01'
                            AND presc_start_date_time <= '2023-12-31'
                           AND treatment_health_board_name NOT IN ('STATE HOSPITAL')
                           AND dmd_vtm_name ='Methotrexate'") 


###filter to only those in cohort.
preg_details <-  read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet")) %>% select(pregnancy_id, mother_upi)
hepma_extract_mtx <- hepma_extract_mtx %>% filter(patient_chi_number %in% preg_details$mother_upi | 
                                                    patient_upi_number %in% preg_details$mother_upi)

saveRDS(hepma_extract_mtx, paste0(folder_data_path, "extracts/hepma_mtx_raw.rds"))

hepma_extract_mtx <- readRDS( paste0(folder_data_path, "extracts/hepma_mtx_raw.rds"))
###Flag exposure in three time periods - before conception, oconception to 11+6 and 12+ weeks 

hepma_extract_mtx  <-hepma_extract_mtx   %>% 
  mutate(mtx_hepma = 1) %>%
  select(patient_upi_number, presc_start_date_time, admin_given_date_time, 
         prescription_has_no_associated_admin, admin_not_given, 
         admin_reason_not_given, presc_unique_id,
         mtx_hepma, dmd_vtm_name) %>%
  filter(prescription_has_no_associated_admin=="N")
table(hepma_extract_mtx$admin_reason_not_given)

hepma_extract_mtx  <-hepma_extract_mtx %>% 
  mutate(admin_not_given = 
           case_when(admin_reason_not_given %in%
                       c("PATIENT SELF ADMINISTERED", "SELF ADMINISTERED", 
                         "SELF ADMINISTERED -DAY SURGERY ONLY","SELF ADMINISTERS" )~"N", T~admin_not_given)) %>%
  filter(admin_not_given=="N")%>%
  mutate(admin_given_date_time = 
           case_when(is.na(admin_given_date_time)~ presc_start_date_time, T~admin_given_date_time)) %>%
  arrange(patient_upi_number, presc_unique_id, admin_given_date_time) %>%
  group_by(presc_unique_id) %>% slice(1)%>% mutate(hdfa_HEPMA=1)


slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)

###join pregs and hdfa and just keep ones with a flag
pregs_mtx <- left_join(slipbd1, hepma_extract_mtx,
                       by=c("mother_upi" = "patient_upi_number") ) %>%
  filter(mtx_hepma==1) 

###now determine whether the hdfa prescription is relevant to this pregnancy.
## from 12 weeks prior to conception date to 12weeks gestation (ie 10wks after conception)
##also flag any in pregnancy including after 12 wks

pregs_mtx <- pregs_mtx  %>%
  mutate(admin_date = as.Date(admin_given_date_time)) %>%
  mutate(start_window = as.Date(est_date_conception) - (7*12), 
         end_window = as.Date(est_date_conception)) %>%
  mutate(start_window2 = as.Date(est_date_conception) , 
         end_window2 = as.Date(est_date_conception)+ (7*10)) %>%
  mutate(mtx_hpm_conception = case_when(admin_date >= start_window & admin_date <= end_window ~1 , T~0), 
         mtx_hpm_to_12_wks = case_when(admin_date >= start_window2 & admin_date <= end_window2 ~1 , T~0), 
         mtx_hpm_after_12wks=case_when(admin_date >=end_window2 & admin_date <= date_end_pregnancy ~1 , T~0))%>%
  mutate(mtx_hpm_anytime = 
           case_when(mtx_hpm_conception==1| mtx_hpm_to_12_wks==1 | mtx_hpm_after_12wks==1 ~1 , T~0))%>%
  filter(mtx_hpm_anytime==1)
###NO hepma methotrextate pescriptions were during pregnancy