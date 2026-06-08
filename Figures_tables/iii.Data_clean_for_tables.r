###demographics Table  data setup
df<- readRDS(paste0(folder_data_path, "working_data/main_dataset.rds"))

## data prep####
##remove invalid chis
df <- df %>%  mutate(check_mother_chi = chi_check(mother_upi), check_baby_chi = chi_check(baby_upi)) %>%
  filter(check_mother_chi=="Valid CHI" & check_baby_chi=="Valid CHI") %>%
  filter(maternal_age_conception >=18 & maternal_age_conception <=49) %>%
  filter(gest_end_pregnancy>=20)
scottish_pc <- c("AB", "DD", "DG", "EH", "FK", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", 
                 "IV", "HS" , "KA", "KW", "KY", "ML", "PA", "PH", "TD", "ZE", "NK")# also allow nk for not known
df <- df %>% 
  mutate(non_scot_pc_booking = case_when(!is.na(maternal_postcode_booking) &
                                           !substr(maternal_postcode_booking,1,2) %in% scottish_pc~1, T~0)) %>%
  mutate(non_scot_pc_end= case_when(!is.na(maternal_postcode_end_preg) &
                                      !substr(maternal_postcode_end_preg,1,2) %in% scottish_pc~1, T~0)) %>%
  filter(non_scot_pc_booking==0 & non_scot_pc_end==0)
#remove cancer outcomes after emigration date
df<- df %>% mutate(cancer_outcome = case_when(incidence_date > DATE_TRANSFER_OUT~0, T~cancer_outcome))
##check deaths after censor date
df<- df %>%
  mutate(end_follow_type = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31") & end_follow_type=="censor - death" ~ "end study", 
                     T~end_follow_type),
         end_follow = 
           case_when(as.Date(end_follow) > as.Date("2023-12-31")  ~ as.POSIXct("2023-12-31"), T~end_follow)  )

##ethnicity mapping

df <- df %>% mutate(mat_ethnicity_mapped = case_when(maternal_ethnicity %in% c("4D",  "4Y") ~ "4X", 
                                                     maternal_ethnicity %in% c("5C", "5D", "5Y") ~ "5X", 
                                                     maternal_ethnicity %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                     maternal_ethnicity=="5Z" ~"6Z",
                                                     maternal_ethnicity %in% c("1E", "1F") ~ "1B",
                                                     maternal_ethnicity== "1J" ~ "1C",
                                                     T~ maternal_ethnicity)) %>%
  ##for comparisons - also do infilled ethnicity
  mutate(mat_ethnicity_mapped_infill = case_when(maternal_ethnicityinfilled %in% c("4D",  "4Y") ~ "4X", 
                                                 maternal_ethnicityinfilled %in% c("5C", "5D", "5Y") ~ "5X", 
                                                 maternal_ethnicityinfilled %in% c("1E", "1F", "1G", "1H") ~ "1B", 
                                                 maternal_ethnicityinfilled=="5Z" ~"6Z",
                                                 maternal_ethnicityinfilled %in% c("1E", "1F") ~ "1B",
                                                 maternal_ethnicityinfilled== "1J" ~ "1C",
                                                 T~ maternal_ethnicityinfilled)) %>%
  mutate(maternal_ethnicity_desc = ethnicity_labels(mat_ethnicity_mapped)) %>% 
  mutate(maternal_ethnicity_desc_infill = ethnicity_labels(mat_ethnicity_mapped_infill)) %>% 
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

  mutate(ind_sickle_cell = case_when(is.na(ind_sickle_cell) ~ 0, T~ind_sickle_cell)  ) %>%

  mutate(mtx_flag = case_when(mtx_PIS_conception==1 & mtx_PIS_to_12_wks==1 ~1, T~0),
         asm_flag = case_when(asm_conception==1 & asm_to_12wks==1 ~ 1, T~0))


df <- df %>% mutate(mat_ethnicity_broad_groups = 
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
                                  maternal_ethnicity_desc %in% c("Other ethnic group", "OLD_CODEAny other ethnic background") ~"Other ethnic group",
                                T~ "Unknown/unclassified")) %>%
  
  mutate(mat_ethnicity_broad_groups_fill = 
           case_when(grepl("white", maternal_ethnicity_desc_infill, ignore.case=T) |
                       maternal_ethnicity_desc_infill %in% c("Scottish", "Gypsy/Traveller", "Other British","Polish") |
                       grepl("Irish", maternal_ethnicity_desc_infill, ignore.case=T) ~ "White", 
                     maternal_ethnicity_desc_infill %in% 
                       c("African, Scottish African or British African", "African, African Scottish or African British",
                         "OLD_CODEAfrican")  ~
                       "African, Scottish African or British African",
                     grepl("Bangladeshi", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Asian", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Chinese", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Indian", maternal_ethnicity_desc_infill, ignore.case=T)|
                       grepl("Pakistani", maternal_ethnicity_desc_infill, ignore.case=T) ~
                       "Asian, Scottish Asian or British Asian",
                     maternal_ethnicity_desc_infill %in%
                       c("Caribbean, Caribbean Scottish or Caribbean British",
                         "Caribbean or Black", "OLD_CODEAny other black background", 
                         "Black, Black Scottish or Black British" ) ~"Caribbean or Black",
                     grepl("mixed", maternal_ethnicity_desc_infill, ignore.case=T)  ~"Mixed or multiple ethnic groups",
                     grepl("Arab", maternal_ethnicity_desc_infill, ignore.case=T)|
                       maternal_ethnicity_desc_infill %in% c("Other ethnic group", "OLD_CODEAny other ethnic background") ~"Other ethnic group",
                     T~ "Unknown/unclassified"))



df <- df %>% mutate(cancer_history = convert_YN(cancer_history), 
                    any_comorb = convert_YN(any_comorb),
                    ind_ntd= convert_YN(ind_ntd),
                    ind_preexist_diabetes=  convert_YN(ind_preexist_diabetes),
                    ind_coeliac=  convert_YN(ind_coeliac),
                    ind_thalassaemia = convert_YN(ind_thalassaemia),
                    ind_sickle_cell= convert_YN(ind_sickle_cell),
                    obese= convert_YN(obese),
                    asm_anytime= convert_YN(asm_anytime),
                    mtx_anytime= convert_YN(mtx_anytime) ,
                    asm_flag= convert_YN(asm_flag),
                    asm_conception = convert_YN(asm_conception),
                    asm_to_12wks = convert_YN(asm_to_12wks),
                    asm_after_12_wks = convert_YN(asm_after_12_wks),
                    mtx_flag= convert_YN(mtx_flag),
                    mtx_PIS_conception = convert_YN(mtx_PIS_conception),
                    mtx_PIS_to_12_wks = convert_YN(mtx_PIS_to_12_wks),
                    mtx_PIS_after_12wks = convert_YN(mtx_PIS_after_12wks),
                    any_congenital_condition = convert_YN(any_congenital_condition)) 