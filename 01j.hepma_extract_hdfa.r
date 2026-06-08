
hepma_extract_hdfa <- dbGetQuery(denodo_connection, "SELECT patient_chi_number, patient_upi_number, patient_date_of_birth,
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
                            AND (dmd_vtm_name IN ('FOLIC ACID','Folic acid') OR 
                                dmd_vmp_name LIKE 'Folic acid%') ") 

hepma_extract_hdfa <- hepma_extract_hdfa %>% filter(med_strength!="400 MICROGRAM" & med_strength!="400 MICROGRAMS")

###filter to only those in cohort.
preg_details <-  read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet")) %>% select(pregnancy_id, mother_upi)
hepma_extract_hdfa <- hepma_extract_hdfa %>% filter(patient_chi_number %in% preg_details$mother_upi | 
                                                      patient_upi_number %in% preg_details$mother_upi)

saveRDS(hepma_extract_hdfa, paste0(folder_data_path, "extracts/hepma_hdfa_raw.rds"))
hepma_extract_hdfa <- readRDS(paste0(folder_data_path, "extracts/hepma_hdfa_raw.rds"))


###processing to summarise if admin EVER given or not 
# processing file for HEPMA data

hepma_extract_hdfa <-hepma_extract_hdfa %>% mutate(hdfa_HEPMA=1) %>%
  filter(prescription_has_no_associated_admin=="N")
hepma_extract_hdfa <-hepma_extract_hdfa %>% 
  mutate(admin_not_given = case_when(admin_reason_not_given %in%
                                       c("PATIENT SELF ADMINISTERED", "SELF ADMINISTERED", 
                                         "SELF ADMINISTERED -DAY SURGERY ONLY","SELF ADMINISTERS" )~"N", T~admin_not_given)) %>%
  filter(admin_not_given=="N")
table(hepma_extract_hdfa$prescription_has_no_associated_admin)

hepma_sliced <-hepma_extract_hdfa %>% 
  mutate(admin_given_date_time = 
           case_when(is.na(admin_given_date_time)~ presc_start_date_time, T~admin_given_date_time)) %>%
arrange(patient_upi_number, presc_unique_id, admin_given_date_time) %>%
  group_by(presc_unique_id) %>% slice(1)%>% mutate(hdfa_HEPMA=1)

slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 
names(slipbd1)
slipbd1 <- slipbd1 %>% select(pregnancy_id, mother_upi, baby_upi, est_date_conception, date_end_pregnancy)

names(hepma_extract_hdfa)
s
###join pregs and hdfa and just keep ones with a flag
pregs_hdfa <- left_join(slipbd1, hepma_sliced , by = c("mother_upi" = "patient_upi_number")) %>% 
  filter(hdfa_HEPMA==1)

###now determine whether the hdfa prescription is relevant to this pregnancy.
## from 12 weeks prior to conception date to 12weeks gestation (ie 10wks after conception)
##also flag any in pregnancy including after 12 wks
pregs_hdfa <- pregs_hdfa %>% mutate(start_window = as.Date(est_date_conception) - (7*12), 
                                    end_window = as.Date(est_date_conception) + (7*10) , 
                                    presc_start= as.Date(admin_given_date_time) ) %>%
      mutate(hdFA_hepma_to_conception = case_when(presc_start >= start_window & presc_start < as.Date(est_date_conception) ~1 , T~0), 
      hdFA_hepma_to_12wks = case_when(presc_start >= as.Date(est_date_conception) & presc_start < end_window ~1 , T~0), 
           hdFA_hepma_after_12wks = case_when(presc_start >= end_window & presc_start < date_end_pregnancy ~1 , T~0), 
         hdFA_hepma_in_preg = case_when(presc_start >= start_window & presc_start <= date_end_pregnancy ~1 , T~0)) %>%
  filter(hdFA_hepma_in_preg==1) 
table(pregs_hdfa$hdFA_hepma_to_12wks, pregs_hdfa$hdFA_hepma_in_preg)

table(pregs_hdfa$hdFA_hepma_to_12wks, pregs_hdfa$prescription_has_no_associated_admin)

pregs_hdfa_minimal <- pregs_hdfa %>%
 group_by(pregnancy_id) %>%
  mutate(n_hdfa_12 = sum(hdFA_hepma_to_12wks)+sum(hdFA_hepma_to_conception),n_hdfa_any = sum(hdFA_hepma_in_preg)) %>%
  summarise(first_prescr = min_(presc_start),
            est_date_conception=first(est_date_conception),
            hdFA_hepma_to_conception =max_(hdFA_hepma_to_conception),
            hdFA_hepma_to_12wks = max_(hdFA_hepma_to_12wks),
            hdFA_hepma_after_12wks = max_(hdFA_hepma_after_12wks),
            hdFA_hepma_in_preg = max_(hdFA_hepma_in_preg), 
            n_hdfa_hepma_12 = max_(n_hdfa_12), 
            n_hdfa_hepma_preg = max_(n_hdfa_any )) %>% ungroup()

saveRDS(pregs_hdfa_minimal, paste0(folder_data_path, "processed_extracts/hepma_hdfa_flags.rds"))


