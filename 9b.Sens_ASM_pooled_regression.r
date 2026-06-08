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

## Check events per interval

# Weighted models different forms for time -----------------------------
## Form of time,
##try linear
fit_wts <- glm(event ~ 
                        time_interval +  treated,
                      weights = stabil_weights, data = df_long,  family=binomial())
summary(fit_wts)
#AIC: 347.73
saveRDS(fit_wts, paste0(folder_data_path, "outputs/sens_ASM_model_lineartime.rds"))

##linear time, double robust
fit_wts_db <- glm(event ~ 
                    time_interval +  treated +
                    year_conception + as.numeric(maternal_age_conception)+
                    any_comorb,
                  weights = stabil_weights, data = df_long,  family=binomial())
summary(fit_wts_db)
#AIC 347.62
saveRDS(fit_wts_db, paste0(folder_data_path, "outputs/sens_ASM_model_lineartime_double.rds"))

##factor time
#fit_wts_f <- glm(event ~ 
                 #as.factor(time_interval) +  treated,
#               weights =  stabil_weights, data = df_long,  family=binomial())
#summary(fit_wts_f)
#AIC:  343.93
#saveRDS(fit_wts_f, paste0(folder_data_path, "outputs/sens_ASM_model_factortime.rds"))

##splines####
fit_wts_spline <- glm(event ~ 
                        time_interval + rcs(time_interval,3)+ treated,
                      weights = stabil_weights,  data = df_long,  family=binomial())#

summary(fit_wts_spline)
#IC: 339.62
saveRDS(fit_wts_spline, paste0(folder_data_path, "outputs/sens_ASM_model_spline.rds"))
