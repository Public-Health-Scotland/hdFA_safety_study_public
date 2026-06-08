##
folder_data_path <- "/conf/FolicAcid/data/"


library(dplyr)
library(ggplot2)
library(tidyr)
bootres <- readRDS( paste0(folder_data_path, "outputs/sens_ASM_bootres1_500.rds"))

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

results <- readRDS(paste0(folder_data_path, "outputs/sens_ASM_predictions_fitwts_spline.rds"))


results <- left_join(results,bsCI)


p <- ggplot() +
  geom_line(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+
  geom_point(aes(x=results$time_interval, y = results$`50%`, colour= results$randf))+
  #  geom_line(aes(x=time, y = ci_lower,colour = group))+
  geom_ribbon(aes(x=results$time_interval, ymin= results$`CI2.5%`, ymax= results$`CI97.5%`,
                  colour= results$randf), linetype=2, alpha=0.1) +
    xlab("Years") +
  ylab("Probability of cancer free survival") +
  ggtitle("Bootstrapped predicted IPTW Survival Curves: mean and 95% CI") +
  labs(colour="Treatment group") +
  theme_bw() 
p
