#############################
#############################
chi_mothers <- read_parquet(paste0(folder_data_path, "extracts/CHI25064extract_mothers.parquet"))
deaths_transfers <- left_join(chi_babies, nrs_deaths, by = c("UPI_NUMBER" = "upi_number"))
chi_babies2 <- read_parquet(paste0(folder_data_path, "extracts/CHI25064extract_babies_v2.parquet"))

count_chis <- chi_babies2 %>% filter(!is.na(DATE_TRANSFER_OUT)) %>% group_by(UPI_NUMBER) %>% count()
table(count_chis$n)

transfers_only <- chi_babies2 %>% filter(!is.na(DATE_TRANSFER_OUT))
length(unique(transfers_only$UPI_NUMBER))
length(unique(transfers_only$UPI_NUMBER))/length(unique(chi_babies2$UPI_NUMBER))


table(year(transfers_only$DATE_TRANSFER_OUT))
#
transfers_only <- transfers_only %>%
  mutate(destination = case_when(NEW_AREA_CODE %in% c("CYM", "ENG", "NI", "IM", "IMM", "EMB")~ "out of scotland", 
                                 TRANSFER_OUT_CODE =="E" ~  "out of scotland", 
                                 TRANSFER_OUT_CODE %in% c("F", "S") ~ "enlistment/services dependant",
                                 T~"unknown"))


first_transfers <- transfers_only %>% arrange(UPI_NUMBER, DATE_TRANSFER_OUT) %>%
  group_by(UPI_NUMBER) %>% slice(1) %>% ungroup()
  table(first_transfers$destination)
first_known_dest <- transfers_only %>% filter(destination!="unknown") %>% 
  arrange(UPI_NUMBER, DATE_TRANSFER_OUT) %>%
  group_by(UPI_NUMBER) %>% slice(1) %>% ungroup() 

multi_transfers <- transfers_only %>% group_by(UPI_NUMBER) %>%
  mutate(count_transfers = n()) %>% ungroup() %>% filter(count_transfers>1)
##deduplicate records

multi_transfers2 <- multi_transfers %>%
  mutate(destination = case_when(NEW_AREA_CODE %in% c("CYM", "ENG", "NI", "IM", "IMM", "EMB")~ "1 out of scotland", 
                                 TRANSFER_OUT_CODE =="E" ~  "1 out of scotland", 
                                 TRANSFER_OUT_CODE %in% c("F", "S") ~ "2 enlistment/services dependant",
                                 T~"3 unknown")) %>% 
  group_by(UPI_NUMBER, DATE_TRANSFER_OUT) %>% arrange(DATE_TRANSFER_OUT, destination) %>% slice(1) %>% ungroup() %>%
  select(-count_transfers)
  

transfers_only <- transfers_only %>% filter(!(UPI_NUMBER %in% multi_transfers2$UPI_NUMBER))
transfers_only <- rbind(transfers_only,multi_transfers2) 

transfers_only <- transfers_only %>% 
  mutate(destination = case_when(NEW_AREA_CODE %in% c("CYM", "ENG", "NI", "IM", "IMM", "EMB")~ "1 out of scotland", 
                                 TRANSFER_OUT_CODE =="E" ~  "1 out of scotland", 
                                 TRANSFER_OUT_CODE %in% c("F", "S") ~ "2 enlistment/services dependant",
                                 T~"3 unknown"))

first_transfers <- transfers_only %>% arrange(UPI_NUMBER, DATE_TRANSFER_OUT) %>%
  group_by(UPI_NUMBER) %>% slice(1) %>% ungroup()

first_known_dest <- transfers_only %>% filter(destination!="3 unknown") %>% 
  arrange(UPI_NUMBER, DATE_TRANSFER_OUT) %>%
  group_by(UPI_NUMBER) %>% slice(1) %>% ungroup() 

multi_transfers3 <- transfers_only %>% group_by(UPI_NUMBER) %>%
  mutate(count_transfers = n()) %>% ungroup() %>% filter(count_transfers>1)

multi_transfers3 <- multi_transfers3 %>% group_by(UPI_NUMBER) %>% 
  mutate(difference = DATE_TRANSFER_OUT-lag(DATE_TRANSFER_OUT))
table(multi_transfers3$difference)
hist(as.numeric(multi_transfers3$difference), xlim = c(0,365), breaks=2000)
table(multi_transfers3$difference<30)

slipbd1 <- readRDS(paste0(folder_data_path, "extracts/slipbd_extract.rds")) 

slipbd1 <-slipbd1 %>% select(pregnancy_id, baby_upi, date_end_pregnancy)

chi_transfers2 <-left_join(slipbd1, first_transfers, by = c("baby_upi" = "UPI_NUMBER")) %>%
  mutate(destination = case_when(is.na(DATE_TRANSFER_OUT) ~ "No transfer" , T~ destination)) %>%
  mutate(start_date = date_end_pregnancy)  %>% select(-date_end_pregnancy)
saveRDS(chi_transfers2, paste0(folder_data_path, "extracts/CHILI_babies2_first_transfer.rds"))




