#extract SLIPBD####
#Live births only
slipbd <- dbGetQuery(denodo_connection,
                        "SELECT * FROM slipbd.slipbd_all_pregnancies 
                        WHERE fetus_outcome1='Live birth'
                        OR fetus_outcome2='Live birth'")
#cohort dates
slipbd1 <- slipbd %>% 
   filter(as.Date(est_date_conception) >= as.Date("2010-04-01") &
            as.Date(est_date_conception) <= as.Date("2022-12-31"))
#filter singletons only
slipbd1 <- slipbd1 %>% filter(total_births_this_pregnancy==1) 

saveRDS(slipbd1, paste0(folder_data_path, "extracts/slipbd_extract.rds"))
slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))
##Save mother and baby UPI lists, and baby triplicate ID
mother_upi <- slipbd1 %>% select(mother_upi) %>% unique()
baby_upi <- slipbd1 %>% select(baby_upi, nrs_triplicate_id) %>% unique()

saveRDS(mother_upi, paste0(folder_data_path, "mother_upi_full.rds"))
saveRDS(baby_upi, paste0(folder_data_path, "baby_upi_full.rds"))

##save lists for CHILI
#mother UPI and pregnancy date
#only for those with a valid CHI
mother_details <- slipbd1 %>% select(pregnancy_id, mother_upi, mother_dob, est_date_conception) %>% 
  unique() %>%
  mutate(valid_chi = chi_check(mother_upi)) %>% filter(valid_chi == "Valid CHI")%>%
  mutate(start_date =  as.Date(est_date_conception) - 90, end_date =  as.Date(est_date_conception)) %>%
  select(pregnancy_id, mother_upi, mother_dob, start_date, end_date)
         
         
baby_upi <- slipbd1 %>% select(baby_upi,  nrs_triplicate_id, date_end_pregnancy) %>% unique() %>%
  mutate(valid_chi = chi_check(baby_upi)) %>% filter(valid_chi == "Valid CHI") %>%
  mutate(start_date = date_end_pregnancy, end_date = as.Date("2023-12-31")) %>%
  select(baby_upi, nrs_triplicate_id, start_date, end_date)


write_parquet(mother_details, paste0(folder_data_path, "mother_upi_valid.parquet"))
write_parquet(baby_upi, paste0(folder_data_path, "baby_upi_valid.parquet"))

mother_details <- 
  read_parquet(paste0(folder_data_path, "mother_upi_valid.parquet"))
mother_upi <-mother_details %>% select(mother_upi) %>% unique()
write.csv(mother_upi, paste0(folder_data_path, "lookups/mother_upis.csv"))

baby_upi <-
  read_parquet(paste0(folder_data_path, "baby_upi_valid.parquet"))
