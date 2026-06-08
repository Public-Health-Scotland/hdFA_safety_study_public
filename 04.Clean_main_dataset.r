#05/Create clean cohort###
#THis file creates a clean cohort for analysis
##removing individuals with no chi or non scottish postcode
##ethnicity cleaned and grouped
source("00.setup.r")
df<- readRDS(paste0(folder_data_path, "working_data/main_dataset.rds"))

## data prep####
##find non scottish postcodes
scottish_pc <- c("AB", "DD", "DG", "EH", "FK", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", 
                 "IV", "HS" , "KA", "KW", "KY", "ML", "PA", "PH", "TD", "ZE", "NK")# also allow nk for not known
df <- df %>% 
  mutate(non_scot_pc_booking = case_when(!is.na(maternal_postcode_booking) &
                                           !substr(maternal_postcode_booking,1,2) %in% scottish_pc~1, T~0)) %>%
  mutate(non_scot_pc_end= case_when(!is.na(maternal_postcode_end_preg) &
                                      !substr(maternal_postcode_end_preg,1,2) %in% scottish_pc~1, T~0)) 
##filter mat age, postcode, valid chis, gestation>=20
df <- df %>%  mutate(check_mother_chi = chi_check(mother_upi), check_baby_chi = chi_check(baby_upi)) %>%
  filter(check_mother_chi=="Valid CHI" & check_baby_chi=="Valid CHI") %>%
  filter(maternal_age_conception >=18 & maternal_age_conception <=49) %>%
  filter(gest_end_pregnancy>=20)%>%
  filter(non_scot_pc_booking==0 & non_scot_pc_end==0)
df <-df %>% mutate(any_indicator = case_when(ind_ntd==1 | ind_coeliac==1 | obese==1 |
                                               ind_sickle_cell==1 |ind_thalassaemia==1 |
                                               ind_preexist_diabetes==1|mtx_flag==1| asm_flag==1 ~"Yes" ,
                                             T~"No"))
df <- df %>%
  mutate(first_hdfa_week = pmin(h_wks_first_presc, p_wks_first_presc, na.rm = T))

df <- df %>% 
  ##use infilled ethnicity
  mutate(mat_ethnicity_mapped = case_when(maternal_ethnicityinfilled %in% c("4D",  "4Y") ~ "4X", 
                                                 maternal_ethnicityinfilled %in% c("5C", "5D", "5Y") ~ "5X", 
                                                 maternal_ethnicityinfilled %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                 maternal_ethnicityinfilled=="5Z" ~"6Z",
                                                 maternal_ethnicityinfilled %in% c("1E", "1F") ~ "1B",
                                                 maternal_ethnicityinfilled== "1J" ~ "1C",
                                                 T~ maternal_ethnicityinfilled)) %>%
  mutate(maternal_ethnicity_desc = ethnicity_labels(mat_ethnicity_mapped)) %>%
  ##broad groups
  mutate(mat_ethnicity_broad_groups = 
           case_when(grepl("white", maternal_ethnicity_desc, ignore.case=T) |
                       maternal_ethnicity_desc %in% c("Scottish", "Gypsy/Traveller", "Other British","Polish") |
                       grepl("Irish", maternal_ethnicity_desc, ignore.case=T) ~ "White", 
                     maternal_ethnicity_desc %in% 
                       c("African, Scottish African or British African", "African, African Scottish or African British",
                         "OLD_CODEAfrican")  ~
                       "African, Scottish African or British African",
                     grepl("Bangladeshi", maternal_ethnicity_desc, ignore.case=T)|
                       grepl("Asian", maternal_ethnicity_desc, ignore.case=T)|
                       grepl("Chinese", maternal_ethnicity_desc, ignore.case=T)|
                       grepl("Indian", maternal_ethnicity_desc, ignore.case=T)|
                       grepl("Pakistani", maternal_ethnicity_desc, ignore.case=T) ~
                       "Asian, Scottish Asian or British Asian",
                     maternal_ethnicity_desc %in%
                       c("Caribbean, Caribbean Scottish or Caribbean British",
                         "Caribbean or Black", "OLD_CODEAny other black background", 
                         "Black, Black Scottish or Black British" ) ~"Caribbean or Black",
                     grepl("mixed", maternal_ethnicity_desc, ignore.case=T)  ~"Mixed or multiple ethnic groups",
                     grepl("Arab", maternal_ethnicity_desc, ignore.case=T)|
                       maternal_ethnicity_desc %in% c("Other ethnic group", "OLD_CODEAny other ethnic background") ~
                       "Other ethnic group",
                     T~ "Unknown/unclassified")) %>%
  mutate(year_conception = year(est_date_conception)) %>%
  mutate(cancer_history = case_when(is.na(cancer_history) ~ 0, T~cancer_history)) %>%
  mutate(maternal_smoking = case_when(is.na(maternal_smoking) ~ "Unknown", 
                                      maternal_smoking=="smoker" ~ "Current smoker", 
                                      maternal_smoking=="ex-smoker" ~ "Former smoker", 
                                      maternal_smoking=="non-smoker" ~ "Never smoked", 
                                      T~maternal_smoking)) %>% 
  mutate(maternal_simd = case_when(is.na(maternal_simd) ~"Unknown", T~maternal_simd)) %>%
  mutate(maternal_simd = paste0("SIMD ", maternal_simd)) %>%
  mutate(baby_sex = case_when(is.na(baby_sex) ~ "Unknown", T~baby_sex)) %>%
   mutate(ind_sickle_cell = replace_na(ind_sickle_cell , 0), 
         ind_ntd = replace_na(ind_ntd, 0), 
         ind_coeliac = replace_na(ind_coeliac, 0), 
         ind_preexist_diabetes = replace_na(ind_preexist_diabetes,0),
         ind_thalassaemia = replace_na(ind_thalassaemia,0),
         obese = replace_na(obese, 0)) %>%
  ##flag for ASM and MTX only when taken in peio before pregnancy and 0-12 weeks
  mutate(mtx_flag = case_when(mtx_PIS_conception==1 & mtx_PIS_to_12_wks==1 ~1, T~0),
         asm_flag = case_when(asm_conception==1 & asm_to_12wks==1 ~ 1, T~0))

##fix death after end follow dates
df<-df %>% 
  mutate(end_follow_type = 
           case_when( death_date > as.Date("2023-12-31") &
                        end_follow_type=="censor - death" ~"end_study", 
                      T~end_follow_type) ) %>%
  mutate(end_follow = case_when(end_follow >as.Date("2023-12-31") ~ as.Date("2023-12-31"),
                                         T~end_follow)) %>%
  mutate(clean_death_date = case_when( death_date > as.Date("2023-12-31")~NA, T~death_date))  %>% 
  mutate(follow_days = as.Date(end_follow)-as.Date(date_end_pregnancy))

saveRDS(df,paste0(folder_data_path, "working_data/main_dataset_clean.rds"))

df <- readRDS(paste0(folder_data_path, "working_data/main_dataset_clean.rds"))
df <- df %>% select(-c( p_wks_first_presc, p_first_prescr, h_wks_first_presc ,h_first_prescr,
                        first_hdfa_week, date_first_prescr ))

hdfa_pis <- readRDS(paste0(folder_data_path, "processed_extracts/PIS_hdfa_flag_details.rds") )

hdfa_pis <- hdfa_pis %>% mutate(wks_first_presc = as.numeric(wks_first_presc_p)) %>% 
  select(pregnancy_id,wks_first_presc, first_prescr)%>% rename(p_first_prescr = first_prescr, p_wks_first_presc = wks_first_presc)


hepma_hdfa <- readRDS(paste0(folder_data_path, "processed_extracts/hepma_hdfa_flags.rds")) 
hepma_hdfa <- hepma_hdfa %>% 
  mutate(wks_first_presc = floor((as.Date(first_prescr) -as.Date(est_date_conception))/7)+2) %>%
  filter(hdFA_hepma_in_preg==1 & wks_first_presc >=(-10))  %>% 
  select(pregnancy_id,wks_first_presc,  first_prescr) %>% 
  rename(h_wks_first_presc= wks_first_presc,  h_first_prescr= first_prescr)
df <- df %>% left_join(hdfa_pis)
df <- df %>% left_join(hepma_hdfa)
df <- df %>%
  mutate(first_hdfa_week = pmin(h_wks_first_presc, p_wks_first_presc, na.rm = T)) %>%
  mutate(date_first_prescr = pmin(h_first_prescr, p_first_prescr, na.rm=T)) 

df <- df %>% select(-c(h_wks_first_presc, p_wks_first_presc,h_first_prescr, p_first_prescr))
saveRDS(df,paste0(folder_data_path, "working_data/main_dataset_clean.rds"))
####


