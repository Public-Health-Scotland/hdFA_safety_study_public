##SMR02 extract for comorbidities##

##previous records 
data_smr02 <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT T1.PREGNANCY_ID,  T1.CONCEPTION_DATE, T2.UPI_NUMBER, T2.ADMISSION_DATE, T2.DISCHARGE_DATE,
    T2.HBTREAT_CURRENTDATE,T2.LOCATION, T2.ADMISSION_TYPE, T2.ADMISSION_REASON,
    T2.INDICATION_FOR_OPERATIVE_DEL,
    DRUG_MISUSE, 
    DRUGS_USED_1, DRUGS_USED_2, DRUGS_USED_3, DRUGS_USED_4, 
    WEEKLY_ALCOHOL_CONSUMPTION, INJECTED_ILLICIT_DRUGS,
    SUBSTR(T2.MAIN_CONDITION, 1, 4) AS MAIN_CONDITION,
    SUBSTR(T2.OTHER_CONDITION_1, 1, 4) AS OTHER_CONDITION_1,
    SUBSTR(T2.OTHER_CONDITION_2, 1, 4) AS OTHER_CONDITION_2,
    SUBSTR(T2.OTHER_CONDITION_3, 1, 4) AS OTHER_CONDITION_3,
    SUBSTR(T2.OTHER_CONDITION_4, 1, 4) AS OTHER_CONDITION_4,
    SUBSTR(T2.OTHER_CONDITION_5, 1, 4) AS OTHER_CONDITION_5,
    T2.HBRES_CURRENTDATE, T2.DOB, T2.ETHNIC_GROUP, T2.DIABETES,
    T2.CONDITION_ON_DISCHARGE
    FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_MUM" T1 
    LEFT JOIN
    ANALYSIS.SMR02_PI T2
    ON T1.MOTHER_UPI = T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.DISCHARGE_DATE)) >= T1.LOOKBACK_YR AND 
    TO_NUMBER(EXTRACT(year FROM T2.ADMISSION_DATE)) <= T1.LOOKBACK_END
    ORDER BY T2.admission_date, T2.discharge_date')
  )
) %>%
  clean_names() %>% unique()



data_smr02<- data_smr02 %>% unique()


###Limit dates more accurately in SMR02####
data_smr02<- data_smr02%>%
  mutate(lookback_start = conception_date -years(5))
data_smr02<- data_smr02 %>% filter(discharge_date >= lookback_start & admission_date <=conception_date)

write_parquet(data_smr02, paste0(folder_data_path, "extracts/temp_smr02_raw_mothers.parquet"))
data_smr02 <- read_parquet(paste0(folder_data_path, "extracts/temp_smr02_raw_mothers.parquet"))

smr02_indicator_flags <- data_smr02 %>%
  mutate(preexist_diabetes = case_when(diabetes==1 ~1,
                             substr(indication_for_operative_del,1,3) %in% Diabetes |
                              substr(indication_for_operative_del,1,4) %in% Diabetes ~1,
                              substr(main_condition,1,3) %in% Diabetes |
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
         NTD = case_when(substr(indication_for_operative_del,1,3) %in% NTD~1,
                         substr(main_condition,1,3) %in% NTD~1, 
                         substr(other_condition_1,1,3) %in% NTD~1,
                         substr(other_condition_2,1,3) %in% NTD~1,
                         substr(other_condition_3,1,3) %in% NTD~1,
                         substr(other_condition_4,1,3) %in% NTD~1,
                         substr(other_condition_5,1,3) %in% NTD~1, T~0),
         coeliac =case_when(
           substr(indication_for_operative_del,1,3) == Coeliac ~1, 
           substr(main_condition,1,3) == Coeliac~1, 
                            substr(other_condition_1,1,3) == Coeliac~1, 
                            substr(other_condition_2,1,3) == Coeliac~1, 
                            substr(other_condition_3,1,3) == Coeliac~1, 
                            substr(other_condition_4,1,3) == Coeliac~1, 
                            substr(other_condition_5,1,3) == Coeliac~1, T~0),
         sickle_cell =case_when(substr(indication_for_operative_del,1,3) == Sickle_cell~1, 
           substr(main_condition,1,3) == Sickle_cell~1, 
                                substr(other_condition_1,1,3) == Sickle_cell~1, 
                                substr(other_condition_2,1,3) == Sickle_cell~1, 
                                substr(other_condition_3,1,3) == Sickle_cell~1, 
                                substr(other_condition_4,1,3) == Sickle_cell~1, 
                                substr(other_condition_5,1,3) == Sickle_cell~1, T~0),
         thalassaemia  = case_when(substr(indication_for_operative_del,1,4) == Thalassaemia~1, 
                                  substr(main_condition,1,4) == Thalassaemia~1, 
                                   substr(other_condition_1,1,4) == Thalassaemia~1, 
                                   substr(other_condition_2,1,4) == Thalassaemia~1, 
                                   substr(other_condition_3,1,4) == Thalassaemia~1, 
                                   substr(other_condition_4,1,4) == Thalassaemia~1, 
                                   substr(other_condition_5,1,4) == Thalassaemia~1, T~0) 
         ) %>%
 group_by(upi_number, pregnancy_id) %>%
  summarise(ntd = max(NTD), 
            coeliac = max(coeliac),
            sickle_cell =  max(sickle_cell),
            thalassaemia = max(thalassaemia),
            preexist_diabetes = max(preexist_diabetes))

saveRDS(smr02_indicator_flags, paste0(folder_data_path,"extracts/smr02_indicators.rds"))

##Flags for relevant comorbidities####
smr02_comorb_flags <- data_smr02 %>%
  mutate(vte = case_when(substr(indication_for_operative_del,1,3) %in% VTE~1, 
                         substr(main_condition,1,3) %in% VTE~1, 
                         substr(other_condition_1,1,3) %in% VTE~1,
                         substr(other_condition_2,1,3) %in% VTE~1,
                         substr(other_condition_3,1,3) %in% VTE~1,
                         substr(other_condition_4,1,3) %in% VTE~1,
                         substr(other_condition_5,1,3) %in% VTE~1, T~0),
         hypertension = case_when(substr(indication_for_operative_del,1,3) %in% Hypertension~1, 
           substr(main_condition,1,3) %in% Hypertension~1, 
                                  substr(other_condition_1,1,3) %in% Hypertension~1,
                                  substr(other_condition_2,1,3) %in% Hypertension~1,
                                  substr(other_condition_3,1,3) %in% Hypertension~1,
                                  substr(other_condition_4,1,3) %in% Hypertension~1,
                                  substr(other_condition_5,1,3) %in% Hypertension~1, T~0),
         demyelinating_NMD =case_when(substr(indication_for_operative_del,1,3) %in% Demyelinating_NMD~1, 
                                   substr(main_condition,1,3) %in% Demyelinating_NMD~1, 
                                      substr(other_condition_1,1,3)  %in% Demyelinating_NMD~1, 
                                      substr(other_condition_2,1,3)  %in% Demyelinating_NMD~1, 
                                      substr(other_condition_3,1,3)  %in% Demyelinating_NMD~1, 
                                      substr(other_condition_4,1,3) %in% Demyelinating_NMD~1, 
                                      substr(other_condition_5,1,3) %in% Demyelinating_NMD~1, T~0),
         
         ##heamatological defintion to xclude anaemia when condition on discharge==3 (or 2?)
         haematological = case_when(
           condition_on_discharge %in% c(2,3) & substr(indication_for_operative_del,1,3) %in% haem_excl_anaemia |
            condition_on_discharge %in% c(2,3) & substr(indication_for_operative_del,1,4) %in% haem_excl_anaemia |
           substr(indication_for_operative_del,1,3) %in% Haematological & !(condition_on_discharge %in% c(2,3)) |
           substr(indication_for_operative_del,1,4) %in% Haematological & !(condition_on_discharge %in% c(2,3)) ~1, 
           
           condition_on_discharge %in% c(2,3) & substr(main_condition,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(main_condition,1,4) %in% haem_excl_anaemia |
             !(condition_on_discharge %in% c(2,3)) & substr(main_condition,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) & substr(main_condition,1,4) %in% Haematological ~1, 
                                    
           condition_on_discharge %in% c(2,3) & substr(other_condition_1,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(other_condition_1,1,4) %in% haem_excl_anaemia |           
           
              !(condition_on_discharge %in% c(2,3)) & substr(other_condition_1,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) &substr(other_condition_1,1,4) %in% Haematological ~1, 
           condition_on_discharge %in% c(2,3) & substr(other_condition_2,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(other_condition_2,1,4) %in% haem_excl_anaemia |           
             !(condition_on_discharge %in% c(2,3)) & substr(other_condition_2,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) & substr(other_condition_2,1,4) %in% Haematological~1, 
           condition_on_discharge %in% c(2,3) & substr(other_condition_3,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(other_condition_3,1,4) %in% haem_excl_anaemia |           
             
           !(condition_on_discharge %in% c(2,3)) & substr(other_condition_3,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) &   substr(other_condition_3,1,4) %in% Haematological~1, 
           condition_on_discharge %in% c(2,3) & substr(other_condition_4,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(other_condition_4,1,4) %in% haem_excl_anaemia |           
             !(condition_on_discharge %in% c(2,3)) &  substr(other_condition_4,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) &   substr(other_condition_4,1,4) %in% Haematological~1, 
           condition_on_discharge %in% c(2,3) & substr(other_condition_5,1,3) %in% haem_excl_anaemia |
             condition_on_discharge %in% c(2,3) & substr(other_condition_5,1,4) %in% haem_excl_anaemia |           
             !(condition_on_discharge %in% c(2,3)) &  substr(other_condition_5,1,3) %in% Haematological |
             !(condition_on_discharge %in% c(2,3)) &   substr(other_condition_5,1,4) %in% Haematological~1, T~0),
         thyroid = case_when(substr(indication_for_operative_del,1,3) %in% Thyroid |
                               substr(indication_for_operative_del,1,4) %in% Thyroid ~1,
                              substr(main_condition,1,3) %in% Thyroid |
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
         asthma = case_when(substr(indication_for_operative_del,1,3) %in% Asthma |
                              substr(indication_for_operative_del,1,4) %in% Asthma ~1, 
                      substr(main_condition,1,3) %in% Asthma |
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
         GI_dis = case_when(substr(indication_for_operative_del,1,3) %in% GI_diseases |
                            substr(indication_for_operative_del,1,4) %in% GI_diseases ~1, 
                      substr(main_condition,1,3) %in% GI_diseases |
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
         liver =case_when(substr(indication_for_operative_del,1,3) %in% Liver~1,
                      substr(main_condition,1,3) %in% Liver~1, 
                          substr(other_condition_1,1,3) %in% Liver~1, 
                          substr(other_condition_2,1,3) %in% Liver~1, 
                          substr(other_condition_3,1,3) %in% Liver~1, 
                          substr(other_condition_4,1,3) %in% Liver~1, 
                          substr(other_condition_5,1,3) %in% Liver~1, T~0),
         Imm_mediated_joint_ct =case_when(
           substr(indication_for_operative_del,1,3) %in% Immune_mediated_joint_connective_tissue~1, 
           substr(main_condition,1,3) %in% Immune_mediated_joint_connective_tissue~1, 
                                          substr(other_condition_1,1,3)  %in% Immune_mediated_joint_connective_tissue~1, 
                                          substr(other_condition_2,1,3) %in%Immune_mediated_joint_connective_tissue ~1, 
                                          substr(other_condition_3,1,3) %in% Immune_mediated_joint_connective_tissue ~1, 
                                          substr(other_condition_4,1,3)  %in% Immune_mediated_joint_connective_tissue ~1, 
                                          substr(other_condition_5,1,3) %in% Immune_mediated_joint_connective_tissue~1, T~0),
         kidney_dis =case_when(
           substr(indication_for_operative_del,1,3) %in% Kidney_diseases~1, 
           substr(main_condition,1,3) %in% Kidney_diseases~1, 
                               substr(other_condition_1,1,3)  %in% Kidney_diseases~1, 
                               substr(other_condition_2,1,3) %in% Kidney_diseases ~1, 
                               substr(other_condition_3,1,3) %in% Kidney_diseases ~1, 
                               substr(other_condition_4,1,3)  %in%Kidney_diseases ~1, 
                               substr(other_condition_5,1,3) %in% Kidney_diseases~1, T~0),
         dis_pelvic_genital_tract  =case_when(
           substr(indication_for_operative_del,1,3) %in% Disorders_pelvic_genital_tract~1, 
           substr(main_condition,1,3) %in% Disorders_pelvic_genital_tract~1, 
                                              substr(other_condition_1,1,3)  %in% Disorders_pelvic_genital_tract~1, 
                                              substr(other_condition_2,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                                              substr(other_condition_3,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                                              substr(other_condition_4,1,3) %in% Disorders_pelvic_genital_tract ~1, 
                                              substr(other_condition_5,1,3)  %in% Disorders_pelvic_genital_tract ~1, T~0),
         skin = case_when(
           substr(indication_for_operative_del,1,3) %in% Skin |
             substr(indication_for_operative_del,1,4) %in% Skin ~1,   
           substr(main_condition,1,3) %in% Skin |
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


saveRDS(smr02_comorb_flags,paste0(folder_data_path,"extracts/smr02_comorbs.rds"))


###index delivery record####
##extract delivery record for pre-pregnancy diabetes and maternal indications ONLY
#
###upload the SMR02 dates from SLIPD to match the records

slipbd1 <- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))

mother_cohort <- slipbd1 %>% 
  select(pregnancy_id, mother_upi, mother_dob, est_date_conception, date_end_pregnancy, smr02_admission_date, smr02_date_of_delivery) %>% 
  unique() %>%
  mutate(valid_chi = chi_check(mother_upi)) %>% filter(valid_chi == "Valid CHI")%>%
  mutate(ADMISSION_YEAR =  year(smr02_admission_date)) %>% 
  select( pregnancy_id,mother_upi, ADMISSION_YEAR,smr02_admission_date, smr02_date_of_delivery) %>%
  rename(MOTHER_UPI = mother_upi, PREGNANCY_ID = pregnancy_id,
        SMR02_ADMISSION_DATE =  smr02_admission_date, SMR02_DELIVERY_DATE = smr02_date_of_delivery)

dbRemoveTable(SMRAConnection,"HDFA_MUM")
dbWriteTable(SMRAConnection, "HDFA_MUM", mother_cohort)


data_smr02_del <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT T1.PREGNANCY_ID,  T1.SMR02_ADMISSION_DATE, T1.SMR02_DELIVERY_DATE,
      T2.UPI_NUMBER, T2.ADMISSION_DATE, T2.DISCHARGE_DATE,
      T2.DATE_OF_DELIVERY,
    T2.HBTREAT_CURRENTDATE,T2.LOCATION, T2.ADMISSION_TYPE, T2.ADMISSION_REASON,
    T2.INDICATION_FOR_OPERATIVE_DEL,
    SUBSTR(T2.MAIN_CONDITION, 1, 4) AS MAIN_CONDITION,
    SUBSTR(T2.OTHER_CONDITION_1, 1, 4) AS OTHER_CONDITION_1,
    SUBSTR(T2.OTHER_CONDITION_2, 1, 4) AS OTHER_CONDITION_2,
    SUBSTR(T2.OTHER_CONDITION_3, 1, 4) AS OTHER_CONDITION_3,
    SUBSTR(T2.OTHER_CONDITION_4, 1, 4) AS OTHER_CONDITION_4,
    SUBSTR(T2.OTHER_CONDITION_5, 1, 4) AS OTHER_CONDITION_5,
    T2.HBRES_CURRENTDATE, T2.DOB, T2.ETHNIC_GROUP, T2.DIABETES,
    T2.CONDITION_ON_DISCHARGE
    FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_MUM" T1 
    LEFT JOIN
    ANALYSIS.SMR02_PI T2
    ON T1.MOTHER_UPI = T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.ADMISSION_DATE)) = T1.ADMISSION_YEAR
    ORDER BY T2.admission_date, T2.discharge_date')
  )
) %>%
  clean_names() %>% unique()

data_smr02_del2 <- data_smr02_del %>% filter(admission_date==smr02_admission_date)


##do NOT flag NTD on delivery record due to possibility of mistakenly recording child's ntd
#
smr02_delivery_indicator_flags <- 
  data_smr02_del2  %>%
  mutate(preexist_diabetes = case_when(diabetes==1 ~1,
                                       substr(indication_for_operative_del,1,3) %in% Diabetes |
                                         substr(indication_for_operative_del,1,4) %in% Diabetes ~1,
                                       substr(main_condition,1,3) %in% Diabetes |
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
      coeliac =case_when(
           substr(indication_for_operative_del,1,3) == Coeliac ~1, 
           substr(main_condition,1,3) == Coeliac~1, 
           substr(other_condition_1,1,3) == Coeliac~1, 
           substr(other_condition_2,1,3) == Coeliac~1, 
           substr(other_condition_3,1,3) == Coeliac~1, 
           substr(other_condition_4,1,3) == Coeliac~1, 
           substr(other_condition_5,1,3) == Coeliac~1, T~0),
         sickle_cell =case_when(substr(indication_for_operative_del,1,3) == Sickle_cell~1, 
                                substr(main_condition,1,3) == Sickle_cell~1, 
                                substr(other_condition_1,1,3) == Sickle_cell~1, 
                                substr(other_condition_2,1,3) == Sickle_cell~1, 
                                substr(other_condition_3,1,3) == Sickle_cell~1, 
                                substr(other_condition_4,1,3) == Sickle_cell~1, 
                                substr(other_condition_5,1,3) == Sickle_cell~1, T~0),
         thalassaemia  = case_when(substr(indication_for_operative_del,1,4) == Thalassaemia~1, 
                                   substr(main_condition,1,4) == Thalassaemia~1, 
                                   substr(other_condition_1,1,4) == Thalassaemia~1, 
                                   substr(other_condition_2,1,4) == Thalassaemia~1, 
                                   substr(other_condition_3,1,4) == Thalassaemia~1, 
                                   substr(other_condition_4,1,4) == Thalassaemia~1, 
                                   substr(other_condition_5,1,4) == Thalassaemia~1, T~0) 
  ) %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise(coeliac = max(coeliac),
            sickle_cell =  max(sickle_cell),
            thalassaemia = max(thalassaemia),
            preexist_diabetes = max(preexist_diabetes))

table(smr02_delivery_indicator_flags$coeliac)
table(smr02_delivery_indicator_flags$ntd)
table(smr02_delivery_indicator_flags$thalassaemia)

table(smr02_delivery_indicator_flags$sickle_cell)
table(smr02_delivery_indicator_flags$preexist_diabetes)

table(smr02_indicator_flags$ntd)
table(smr02_indicator_flags$coeliac)

saveRDS(smr02_delivery_indicator_flags, paste0(folder_data_path,"extracts/smr02_indicators_del.rds"))

