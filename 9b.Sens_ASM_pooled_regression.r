##pooled regression
source("00.setup.r")
library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)

df_long <- readRDS(paste0(folder_data_path, "working_data/sensitivity_ASM_long_data.rds"))

df_long<-df_long %>%
  group_by(pregnancy_id)%>%
  mutate(
    cancer_overall = max(event)
  ) %>% ungroup()


# Create 'baseline' - data collected at visit 0
baseline <-df_long %>%
  dplyr::filter(as.numeric(time_interval) == 0)


# Weighted models different forms for time -----------------------------
## linear
#fit_wts <- glm(event ~ 
#                        time_interval +  treated,
#                      weights = stabil_weights, data = df_long,  family=binomial())
#summary(fit_wts)
#saveRDS(fit_wts, paste0(folder_data_path, "outputs/sens_ASM_model_lineartime.rds"))


##factor time
#fit_wts_f <- glm(event ~ 
                 #as.factor(time_interval) +  treated,
#               weights =  stabil_weights, data = df_long,  family=binomial())
#summary(fit_wts_f)
#saveRDS(fit_wts_f, paste0(folder_data_path, "outputs/sens_ASM_model_factortime.rds"))

##splines####
fit_wts_spline <- glm(event ~ 
                        time_interval + rcs(time_interval,3)+ treated,
                      weights = stabil_weights,  data = df_long,  family=binomial())#

summary(fit_wts_spline)
#AIC: 339.62
saveRDS(fit_wts_spline, paste0(folder_data_path, "outputs/sens_ASM_model_spline.rds"))
