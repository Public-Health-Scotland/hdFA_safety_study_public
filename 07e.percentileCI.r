##
folder_data_path <- "/conf/FolicAcid/data/"

library(dplyr)
library(ggplot2)
library(tidyr)
bootres1_50 <- readRDS("/conf/FolicAcid/data/outputs/bootres1_50.rds")
bootres51_100 <- readRDS("/conf/FolicAcid/data/outputs/bootres51_100.rds")
bootres101_200 <- readRDS("/conf/FolicAcid/data/outputs/bootres101_200.rds")
bootres201_300 <- readRDS("/conf/FolicAcid/data/outputs/bootres201_300.rds")
bootres301_400 <- readRDS("/conf/FolicAcid/data/outputs/bootres301_400.rds")
bootres401_500 <- readRDS("/conf/FolicAcid/data/outputs/bootres401_500.rds")

bootres <- rbind(bootres1_50, bootres51_100, bootres101_200, bootres201_300, 
                 bootres301_400, bootres401_500)
saveRDS(bootres,"/conf/FolicAcid/data/outputs/main_bootres.rds" )
#cumulative incidnce###
bootres <- bootres %>% mutate(CumIncTrt = 1-Treated, 
                              CumIncUntrt = 1- Untreated)
trt_cuminc <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(CumIncTrt = list(quibble(CumIncTrt , c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(CumIncTrt ) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CumInc CI2.5%` = `0.025`, `CumInc CI97.5%` =  `0.975`,  `CumInc 50%` =`0.5`) %>%
  mutate(randf="Treated")

untrt_cuminc <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(CumIncUntrt = list(quibble(CumIncUntrt , c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(CumIncUntrt )%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CumInc CI2.5%` = `0.025`, `CumInc CI97.5%` =  `0.975`,  `CumInc 50%` =`0.5`) %>%
  mutate(randf="Untreated")

bs_cuminc <- rbind(trt_cuminc , untrt_cuminc )

##CIs####


quibble <- function(x, q = c(0.025,0.5, 0.975), dropNA = TRUE) {
  tibble(x = quantile(x, q, na.rm = dropNA), q = q)
}
trt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Treated = list(quibble(Treated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Treated) %>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Treated")

untrt <- bootres %>% 
  group_by(time_interval) %>% 
  summarise(Untreated = list(quibble(Untreated, c(0.025,0.5, 0.975), dropNA = TRUE))) %>% 
  tidyr::unnest(Untreated)%>% 
  pivot_wider(names_from = q, values_from = x) %>%
  rename(`CI2.5%` = `0.025`, `CI97.5%` =  `0.975`,  `50%` =`0.5`) %>%
  mutate(randf="Untreated")

bsCI <- rbind(trt, untrt)

results <- readRDS(paste0(folder_data_path, "outputs/predictions_fitwts_spline.rds"))


