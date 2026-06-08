##pooled regression
## need  > 20k memory to run both weighted and uinweighted model full dataset
##works with 30k , possibly dont need quite that much
source("00.setup.r")
library(lmtest)
library(survival)
library(sandwich)
library(reshape2)
library(rms)


df_long2 <- readRDS(paste0(folder_data_path, "working_data/timeseries_1yr_start_birth_details.rds"))
table(df_long2$time_interval)

#
# Create 'cancr_Overall' - an indicator 

df_long2<-df_long2 %>%
  group_by(pregnancy_id)%>%
  mutate(
    cancer_overall = max(event)
  ) %>% ungroup()

# Create 'baseline' - data collected at visit 0
baseline <-df_long2 %>%
  dplyr::filter(as.numeric(time_interval) == 0)


# Weighted models different forms for time -----------------------------

## Stabilised weight models -----------------------------
##different forms of time compare aic
#fit_wts <- glm(event ~ 
#                 time_interval +  treated,
#               weights = stabil_weights, data = df_long2,  family=binomial())
#summary(fit_wts)
#fit_wts_x <- glm(event ~ 
#                 time_interval*treated,
#               weights = stabil_weights, data = df_long2,  family=binomial())
#summary(fit_wts_x)
#saveRDS(fit_wts, paste0(folder_data_path, "outputs/model_stabwt_lineartime.rds"))
#factor time
#fit_wts_f <- glm(event ~ 
#                   as.factor(time_interval) +  treated,
#                 weights = stabil_weights, data = df_long2,  family=binomial())
#summary(fit_wts_f)

#time with splines

#AIC  14821 same as linear time
fit_wts_spline <- glm(event ~ 
                        time_interval + rcs(time_interval,4)+ treated,
                      weights = stabil_weights, data = df_long2,  family=binomial())#
summary(fit_wts_spline)
#
#AIC: 14815(4 knots) loweest AIC

saveRDS(fit_wts_spline, paste0(folder_data_path, "outputs/model_stabwts_lineartime_spline.rds"))
