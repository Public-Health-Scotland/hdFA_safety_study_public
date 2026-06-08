##Read in SMR01 extract for a 5-year lookback from conception date
##Idensify preexisting conditions and confounders

mother_details <- read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet"))
names(mother_details)
#remember end date is end of checking period for being in scotland (ie the conception date)
mother_details <- mother_details %>%
  mutate(LOOKBACK_YR = year(end_date) -5) %>%
  mutate(LOOKBACK_END = year(end_date) )

mother_cohort<- mother_details %>% select( pregnancy_id,mother_upi,end_date, LOOKBACK_YR, LOOKBACK_END) %>%
    rename(MOTHER_UPI = mother_upi, PREGNANCY_ID = pregnancy_id, CONCEPTION_DATE = end_date)

dbWriteTable(SMRAConnection, "HDFA_MUM", mother_cohort)

data_smr01_temp_1 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT T1.PREGNANCY_ID, T1.CONCEPTION_DATE, T2.UPI_NUMBER, T2.LINK_NO,
      T2.CIS_MARKER, T2.ADMISSION_DATE, T2.DISCHARGE_DATE,
    T2.HBTREAT_CURRENTDATE,T2.LOCATION, T2.ADMISSION_TYPE,
    SUBSTR(T2.MAIN_CONDITION, 1, 4) AS MAIN_CONDITION,
    SUBSTR(T2.OTHER_CONDITION_1, 1, 4) AS OTHER_CONDITION_1,
    SUBSTR(T2.OTHER_CONDITION_2, 1, 4) AS OTHER_CONDITION_2,
    SUBSTR(T2.OTHER_CONDITION_3, 1, 4) AS OTHER_CONDITION_3,
    SUBSTR(T2.OTHER_CONDITION_4, 1, 4) AS OTHER_CONDITION_4,
    SUBSTR(T2.OTHER_CONDITION_5, 1, 4) AS OTHER_CONDITION_5,
    T2.HBRES_CURRENTDATE, T2.DOB, T2.ETHNIC_GROUP, T2.DR_POSTCODE, T2.POSTCODE
   FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_MUM" T1 
    LEFT JOIN
    ANALYSIS.SMR01_PI T2
    ON T1.MOTHER_UPI = T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.DISCHARGE_DATE)) >= T1.LOOKBACK_YR AND 
    TO_NUMBER(EXTRACT(year FROM T2.ADMISSION_DATE)) <= T1.LOOKBACK_END
    ORDER BY T2.link_no, T2.admission_date, T2.discharge_date, T2.admission, T2.discharge, T2.uri')
  )
) %>%
  clean_names() %>% unique()
data_smr01_temp_1 <- data_smr01_temp_1 %>% unique()
write_parquet(data_smr01_temp_1, paste0(folder_data_path, "temp_smr01_raw_mothers.parquet"))

###Limit dates 
smr01<- data_smr01_temp_1 %>%
  mutate(lookback_start = conception_date -years(5))
smr01<- smr01 %>% filter(discharge_date >= lookback_start & admission_date <=conception_date)
write_parquet(smr01, paste0(folder_data_path, "extracts/temp_smr01_raw_mothers.parquet"))

##flagging indications####
smr01_indicator_flags <- smr01 %>%
  mutate(NTD = case_when(substr(main_condition,1,3) %in% NTD~1, 
                         substr(other_condition_1,1,3) %in% NTD~1,
                         substr(other_condition_2,1,3) %in% NTD~1,
                         substr(other_condition_3,1,3) %in% NTD~1,
                         substr(other_condition_4,1,3) %in% NTD~1,
                         substr(other_condition_5,1,3) %in% NTD~1, T~0),
         coeliac =case_when(substr(main_condition,1,3) == Coeliac~1, 
                            substr(other_condition_1,1,3) == Coeliac~1, 
                            substr(other_condition_2,1,3) == Coeliac~1, 
                            substr(other_condition_3,1,3) == Coeliac~1, 
                            substr(other_condition_4,1,3) == Coeliac~1, 
                            substr(other_condition_5,1,3) == Coeliac~1, T~0),
         sickle_cell =case_when(substr(main_condition,1,3) == Sickle_cell~1, 
                                substr(other_condition_1,1,3) == Sickle_cell~1, 
                                substr(other_condition_2,1,3) == Sickle_cell~1, 
                                substr(other_condition_3,1,3) == Sickle_cell~1, 
                                substr(other_condition_4,1,3) == Sickle_cell~1, 
                                substr(other_condition_5,1,3) == Sickle_cell~1, T~0),
         preexist_diabetes = case_when(substr(main_condition,1,3) %in% Diabetes |
                                         substr(main_condition,1,4) %in% Diabetes ~1, 
                                       substr(other_condition_1,1,3) %in% Diabetes |
                                         substr(other_condition_1,1,4) %in% Diabetes~1, 
                                       substr(other_condition_2,1,3) %in% Diabetes |
                                         substr(other_condition_2,1,4) %in% Diabetes~1, 
                                       substr(other_condition_3,1,3) %in% Diabetes |
                                         substr(other_condition_3,1,4) %in% Diabetes~1, 
                                       substr(other_condition_4,1,3) %in% Diabetes |
                                         substr(other_condition_4,1,4) %in% Diabetes~1, 
                                       substr(other_condition_5,1,3) %in% Diabetes |
                                         substr(other_condition_5,1,4) %in% Diabetes~1, T~0),
         thalassaemia  = case_when(substr(main_condition,1,4) == Thalassaemia~1, 
                                  substr(other_condition_1,1,4) == Thalassaemia~1, 
                                  substr(other_condition_2,1,4) == Thalassaemia~1, 
                                  substr(other_condition_3,1,4) == Thalassaemia~1, 
                                  substr(other_condition_4,1,4) == Thalassaemia~1, 
                                  substr(other_condition_5,1,4) == Thalassaemia~1, T~0)  ) %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise(ntd = max(NTD), 
            coeliac = max(coeliac),
            sickle_cell =  max(sickle_cell),
            thalassaemia = max(thalassaemia),
            preexist_diabetes = max(preexist_diabetes))

saveRDS(smr01_indicator_flags,paste0(folder_data_path,"extracts/smr01_indicators.rds"))
##Flags for relevant comorbidities####
smr01_comorb_flags <- smr01 %>%
  mutate(vte = case_when(substr(main_condition,1,3) %in% VTE~1, 
                         substr(other_condition_1,1,3) %in% VTE~1,
                         substr(other_condition_2,1,3) %in% VTE~1,
                         substr(other_condition_3,1,3) %in% VTE~1,
                         substr(other_condition_4,1,3) %in% VTE~1,
                         substr(other_condition_5,1,3) %in% VTE~1, T~0),
         hypertension = case_when(substr(main_condition,1,3) %in% Hypertension~1, 
                         substr(other_condition_1,1,3) %in% Hypertension~1,
                         substr(other_condition_2,1,3) %in% Hypertension~1,
                         substr(other_condition_3,1,3) %in% Hypertension~1,
                         substr(other_condition_4,1,3) %in% Hypertension~1,
                         substr(other_condition_5,1,3) %in% Hypertension~1, T~0),
         demyelinating_NMD =case_when(substr(main_condition,1,3) %in% Demyelinating_NMD~1, 
                                substr(other_condition_1,1,3)  %in% Demyelinating_NMD~1, 
                                substr(other_condition_2,1,3)  %in% Demyelinating_NMD~1, 
                                substr(other_condition_3,1,3)  %in% Demyelinating_NMD~1, 
                                substr(other_condition_4,1,3) %in% Demyelinating_NMD~1, 
                                substr(other_condition_5,1,3) %in% Demyelinating_NMD~1, T~0),
         haematological = case_when(substr(main_condition,1,3) %in% Haematological |
                                         substr(main_condition,1,4) %in% Haematological ~1, 
                                       substr(main_condition,1,3) %in% Haematological |
                                      substr(other_condition_1,1,3) %in% Haematological |
                                      substr(other_condition_1,1,4) %in% Haematological ~1, 
                                    substr(other_condition_2,1,3) %in% Haematological |
                                      substr(other_condition_2,1,4) %in% Haematological~1, 
                                    substr(other_condition_3,1,3) %in% Haematological |
                                      substr(other_condition_3,1,4) %in% Haematological~1, 
                                    substr(other_condition_4,1,3) %in% Haematological |
                                      substr(other_condition_4,1,4) %in% Haematological~1, 
                                    substr(other_condition_5,1,3) %in% Haematological |
                                      substr(other_condition_5,1,4) %in% Haematological~1, T~0),
         thyroid = case_when(substr(main_condition,1,3) %in% Thyroid |
                                      substr(main_condition,1,4) %in% Thyroid ~1,
                             substr(other_condition_1,1,3) %in% Thyroid |
                                      substr(other_condition_1,1,4) %in% Thyroid ~1, 
                                    substr(other_condition_2,1,3) %in% Thyroid |
                                      substr(other_condition_2,1,4) %in% Thyroid~1, 
                                    substr(other_condition_3,1,3) %in% Thyroid |
                                      substr(other_condition_3,1,4) %in% Thyroid~1, 
                                    substr(other_condition_4,1,3) %in% Thyroid |
                                      substr(other_condition_4,1,4) %in% Thyroid ~1, 
                                    substr(other_condition_5,1,3) %in% Thyroid |
                                      substr(other_condition_5,1,4) %in% Thyroid ~1, T~0),
         asthma = case_when(substr(main_condition,1,3) %in% Asthma |
                               substr(main_condition,1,4) %in% Asthma ~1, 
                             substr(other_condition_1,1,3) %in% Asthma |
                               substr(other_condition_1,1,4) %in% Asthma ~1, 
                             substr(other_condition_2,1,3) %in% Asthma |
                               substr(other_condition_2,1,4) %in% Asthma~1, 
                             substr(other_condition_3,1,3) %in% Asthma |
                               substr(other_condition_3,1,4) %in% Asthma~1, 
                             substr(other_condition_4,1,3) %in% Asthma |
                               substr(other_condition_4,1,4) %in% Asthma ~1, 
                             substr(other_condition_5,1,3) %in% Asthma |
                               substr(other_condition_5,1,4) %in% Asthma ~1, T~0),
         GI_dis = case_when(substr(main_condition,1,3) %in% GI_diseases |
                              substr(main_condition,1,4) %in% GI_diseases ~1, 
                            substr(other_condition_1,1,3) %in% GI_diseases |
                              substr(other_condition_1,1,4) %in% GI_diseases ~1, 
                            substr(other_condition_2,1,3) %in% GI_diseases |
                              substr(other_condition_2,1,4) %in% GI_diseases ~1, 
                            substr(other_condition_3,1,3) %in% GI_diseases |
                              substr(other_condition_3,1,4) %in% GI_diseases ~1, 
                            substr(other_condition_4,1,3) %in% GI_diseases |
                              substr(other_condition_4,1,4) %in% GI_diseases ~1, 
                            substr(other_condition_5,1,3) %in% GI_diseases |
                              substr(other_condition_5,1,4) %in% GI_diseases ~1, T~0),
         liver =case_when(substr(main_condition,1,3) %in% Liver~1, 
                                      substr(other_condition_1,1,3) %in% Liver~1, 
                                      substr(other_condition_2,1,3) %in% Liver~1, 
                                      substr(other_condition_3,1,3) %in% Liver~1, 
                                      substr(other_condition_4,1,3) %in% Liver~1, 
                                      substr(other_condition_5,1,3) %in% Liver~1, T~0),
         Imm_mediated_joint_ct =case_when(substr(main_condition,1,3) %in% Immune_mediated_joint_connective_tissue~1, 
                          substr(other_condition_1,1,3)  %in% Immune_mediated_joint_connective_tissue~1, 
                          substr(other_condition_2,1,3) %in%Immune_mediated_joint_connective_tissue ~1, 
                          substr(other_condition_3,1,3) %in% Immune_mediated_joint_connective_tissue ~1, 
                          substr(other_condition_4,1,3)  %in% Immune_mediated_joint_connective_tissue ~1, 
                          substr(other_condition_5,1,3) %in% Immune_mediated_joint_connective_tissue~1, T~0),
         kidney_dis =case_when(substr(main_condition,1,3) %in% Kidney_diseases~1, 
                      substr(other_condition_1,1,3)  %in% Kidney_diseases~1, 
                      substr(other_condition_2,1,3) %in% Kidney_diseases ~1, 
                      substr(other_condition_3,1,3) %in% Kidney_diseases ~1, 
                      substr(other_condition_4,1,3)  %in%Kidney_diseases ~1, 
                      substr(other_condition_5,1,3) %in% Kidney_diseases~1, T~0),
         dis_pelvic_genital_tract  =case_when(substr(main_condition,1,3) %in% Disorders_pelvic_genital_tract~1, 
                               substr(other_condition_1,1,3)  %in% Disorders_pelvic_genital_tract~1, 
                               substr(other_condition_2,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                               substr(other_condition_3,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                               substr(other_condition_4,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                               substr(other_condition_5,1,3)  %in% Disorders_pelvic_genital_tract ~1, T~0),
         skin = case_when(substr(main_condition,1,3) %in% Skin |
                              substr(main_condition,1,4) %in% Skin ~1, 
                            substr(other_condition_1,1,3) %in% Skin |
                              substr(other_condition_1,1,4) %in% Skin ~1, 
                            substr(other_condition_2,1,3) %in% Skin |
                              substr(other_condition_2,1,4) %in% Skin ~1, 
                            substr(other_condition_3,1,3) %in% Skin |
                              substr(other_condition_3,1,4) %in% Skin ~1, 
                            substr(other_condition_4,1,3) %in% Skin |
                              substr(other_condition_4,1,4) %in% Skin ~1, 
                            substr(other_condition_5,1,3) %in% Skin |
                              substr(other_condition_5,1,4) %in% Skin ~1, T~0)    ) %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise(vte=max(vte), 
            hypertension = max(hypertension), 
            demyelinating_NMD= max(demyelinating_NMD), 
            haematological = max(haematological),
            thyroid=max(thyroid),
            asthma = max(asthma),
            GI_dis = max(GI_dis), 
            liver = max(liver),
            Imm_mediated_joint_ct = max(Imm_mediated_joint_ct),
            kidney_dis = max(kidney_dis),
            dis_pelvic_genital_tract = max(dis_pelvic_genital_tract),
            skin= max(skin)
    
  ) %>% ungroup()


saveRDS(smr01_comorb_flags,paste0(folder_data_path,"extracts/smr01_comorbs.rds"))

