data_smr06_mothers <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT 2.UPI_NUMBER, T2.DATE_OF_REG, T2.INCIDENCE_DATE,
      T2.ENCR_INCIDENCE_DATE, T2.SITE_ICD9 ,T2.ICD10S_CANCER_SITE,
      T2.ICDO2_ICDO2, T2.TYPE_ICDO, T2.MORPH_MORPHOLOGY,T2.TYPE_ICDO3,
      T2.PE_PATIENT_ID, T2.TUMOUR_NO, T2.RECORD_TYPE, T2.DATE_OF_BIRTH,
        T2.SEX, T2.DEATHRECID , T2.DEATH_CERTIFICATE_ONLY, T2.AGE_IN_YEARS, T2.AGE_IN_MONTHS
          FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_MUM" T1 
    LEFT JOIN
    ANALYSIS.SMR06_PI T2
    ON T1.MOTHER_UPI = T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.INCIDENCE_DATE)) <= 2022')
  )
) %>%
  clean_names() %>% unique()

data_smr06_mothers <-read_parquet( paste0(folder_data_path, "extracts/temp_smr06_mother.parquet"))

data_smr06_mothers <- data_smr06_mothers %>% mutate(chi_dob = phsmethods::dob_from_chi(upi_number))
table(data_smr06_mothers$chi_dob != data_smr06_mothers$date_of_birth)


preg_details <- read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet"))

icd10_codes <- c(paste0("C0", 0:9), paste0("C", 10:98))
child_icd10 <- c(paste0("C0", 0:9), paste0("C", 10:98), "D32", "D33", "D352", "D353", "D354", "D42", "D43", "D443", "D444", "D445")
###all records have ICD10 so only need to filter on icd10

data_smr06_mothers2 <- data_smr06_mothers %>%
  filter((substr(data_smr06_mothers$icd10s_cancer_site,1,3) %in% icd10_codes  & age_in_years >=15) |
  (age_in_years <15 & substr(data_smr06_mothers$icd10s_cancer_site,1,3) %in% child_icd10) |
           (age_in_years <15 & substr(data_smr06_mothers$icd10s_cancer_site,1,4) %in% child_icd10)) %>%
  filter(substr(icd10s_cancer_site,1,3) !="C44")

write_parquet(data_smr06_mothers2 , paste0(folder_data_path, "extracts/temp_smr06_mother.parquet"))

####Child extract####  
baby_upi <- read_parquet(paste0(folder_data_path, "baby_upi_valid.parquet"))
child_cohort<- baby_upi %>% rename(UPI_NUMBER = baby_upi, DATE_OF_BIRTH = start_date)

dbWriteTable(SMRAConnection, "HDFA_CHILD", child_cohort)

data_smr06_child <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT T2.UPI_NUMBER, T2.DATE_OF_REG, T2.INCIDENCE_DATE,
      T2.ENCR_INCIDENCE_DATE, T2.SITE_ICD9 ,T2.ICD10S_CANCER_SITE,
      T2.ICDO2_ICDO2, T2.TYPE_ICDO, T2.MORPH_MORPHOLOGY,T2.TYPE_ICDO3,
      T2.PE_PATIENT_ID, T2.TUMOUR_NO, T2.RECORD_TYPE, T2.DATE_OF_BIRTH,
        T2.SEX, T2.DEATHRECID , T2.DEATH_CERTIFICATE_ONLY,
         T2.AGE_IN_YEARS, T2.AGE_IN_MONTHS
          FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_CHILD" T1 
    LEFT JOIN
    ANALYSIS.SMR06_PI T2
    ON T1.UPI_NUMBER = T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.INCIDENCE_DATE)) >= 2010 AND
    TO_NUMBER(EXTRACT(year FROM T2.INCIDENCE_DATE)) <= 2023')
  )
) %>%
  clean_names() %>% unique()

data_smr06_child  <- data_smr06_child  %>% filter(substr(icd10s_cancer_site,1,3) %in% child_icd10 |
                                                    substr(icd10s_cancer_site,1,4) %in% child_icd10  )
write_parquet(data_smr06_child, paste0(folder_data_path, "extracts/temp_smr06_child.parquet"))

data_smr06_child<- read_parquet(paste0(folder_data_path, "extracts/temp_smr06_child.parquet"))
#### Mother history of cancer - refine by date before pregnancy###
data_smr06_mothers <-read_parquet( paste0(folder_data_path, "extracts/temp_smr06_mother.parquet")) 

preg_details <- read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet"))%>%
  rename(est_conception_date = end_date) %>% select(-start_date)

##only need to link where the upi is present at all in the cancer data
preg_df <- preg_details %>% filter(mother_upi %in% data_smr06_mothers$upi_number)

df <- left_join(preg_df , data_smr06_mothers, by= c("mother_upi" = "upi_number"))

df <- df %>% filter(est_conception_date >= incidence_date) %>%
  filter(incidence_date >= mother_dob) %>% 
  mutate(age_incidence = as.numeric((as.Date(incidence_date) - as.Date(mother_dob))/365.25)) %>%
  mutate(age_incidence = floor(age_incidence))
table(df$age_incidence)


###all child to compute overall rates
smr06_all_child <- as_tibble(
dbGetQuery(
  SMRAConnection, paste0(
    'SELECT  T2.UPI_NUMBER, T2.DATE_OF_REG, T2.INCIDENCE_DATE,
      T2.ENCR_INCIDENCE_DATE, T2.SITE_ICD9 ,T2.ICD10S_CANCER_SITE,
      T2.ICDO2_ICDO2, T2.TYPE_ICDO, T2.MORPH_MORPHOLOGY,T2.TYPE_ICDO3,
      T2.PE_PATIENT_ID, T2.TUMOUR_NO, T2.RECORD_TYPE, T2.DATE_OF_BIRTH,
        T2.SEX, T2.DEATHRECID , T2.DEATH_CERTIFICATE_ONLY,
         T2.AGE_IN_YEARS, T2.AGE_IN_MONTHS, T2.OUT_OF_SCOTLAND, T2.FETAL_TUMOUR, T2.POSTCODE
    FROM
    ANALYSIS.SMR06_PI T2
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.INCIDENCE_DATE)) >= 2010 AND
    TO_NUMBER(EXTRACT(year FROM T2.INCIDENCE_DATE)) <= 2023 AND 
    TO_NUMBER(AGE_IN_YEARS) <=16')
)
) %>%
  clean_names() %>% unique()


smr06_all_child  <- smr06_all_child  %>% filter(substr(icd10s_cancer_site,1,3) %in% child_icd10 |
                                                    substr(icd10s_cancer_site,1,4) %in% child_icd10  )
table(is.na(smr06_all_child$date_of_birth))
smr06_all_child <- smr06_all_child %>% filter(date_of_birth >=as.Date("2011-01-01"))

saveRDS(smr06_all_child , paste0(folder_data_path, "extracts/smr06_child_comparison.rds"))
