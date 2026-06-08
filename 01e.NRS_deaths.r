###Deaths extracts###
###Babies####
snrs_deaths <- as_tibble(
  dbGetQuery(
    SMRAConnection, paste0(
      'SELECT   T2.UPI_NUMBER, T2.DATE_OF_DEATH,
      T1.DATE_OF_BIRTH AS SLIPBD_DOB,
      T2.DATE_OF_BIRTH AS NRS_DOB,
     UNDERLYING_CAUSE_OF_DEATH ,
     AGE, AGE_UNITS,
    CAUSE_OF_DEATH_CODE_0 ,  CAUSE_OF_DEATH_CODE_1 ,
    CAUSE_OF_DEATH_CODE_2 ,  CAUSE_OF_DEATH_CODE_3 ,
    CAUSE_OF_DEATH_CODE_4 ,  CAUSE_OF_DEATH_CODE_5 
    CAUSE_OF_DEATH_CODE_6 ,   CAUSE_OF_DEATH_CODE_7 ,
    CAUSE_OF_DEATH_CODE_8 , CAUSE_OF_DEATH_CODE_9 
    FROM
  ',  toupper(Sys.info()[["user"]]),'."HDFA_CHILD" T1 
    LEFT JOIN
    ANALYSIS.GRO_DEATHS_C T2
    ON T1.UPI_NUMBER= T2.UPI_NUMBER  
    WHERE
    TO_NUMBER(EXTRACT(year FROM T2.DATE_OF_DEATH)) >= 2010')
  )
) %>%
  clean_names() %>% unique()

# restrict deaths to under 15 years
nrs_deaths <- nrs_deaths %>% 
  filter(age <= 15)

dups <- nrs_deaths %>% group_by(upi_number) %>% count() %>% filter(n>1)
#no duplicates

saveRDS(nrs_deaths, paste0(folder_data_path, "extracts/nrs_deaths_child.rds"))

