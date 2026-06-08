###Prep for SLICCD linkage#######
##we want to know if women have a previous baby on register with NTD
## or if they themselves are on it (the reg is back to 2000 - so some women later in the cohort could b on it themselves)
slipbd1<- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds"))

##Mothers self check
mothers2 <- slipbd1 %>% filter(mother_dob >= as.Date("2000-01-01")) %>% select(mother_upi, mother_dob)

##Mothers self check
mothers1 <- slipbd1 %>%  select(mother_upi, mother_dob, baby_upi, date_end_pregnancy) %>%
  rename(baby_dob = date_end_pregnancy)

saveRDS(mothers1 , paste0(folder_data_path,"CHI_pairs_for_SLICCD.rds"))

saveRDS(mothers2 , paste0(folder_data_path,"Mum_self_match_CHI_for_SLICCD.rds"))
mothers3 <- mothers1 %>%  select(mother_upi, mother_dob) 
saveRDS(mothers3 , paste0(folder_data_path,"All_mothersCHI__SLICCD.rds"))
